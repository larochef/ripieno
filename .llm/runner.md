# Runner

`runner/` is the engine every model runs on the GPU with: Scala on the JVM, HIP kernels compiled for one GPU (gfx1151, the Radeon of a Strix Halo). Its engine packages (`native`, `tensor`, `ops`, `formats`, `weights`) and kernels were copied from drift's runner; models live in `models/`.

## The GPU is shared with the desktop

- **No model runs on this GPU through a stock framework** (PyTorch, TensorFlow, JAX, ONNX Runtime with a GPU provider). One long launch starves the desktop's graphics ring; past a few seconds the ring times out, its reset fails and the session is lost. Reference implementations run on the processor only, with `CUDA_VISIBLE_DEVICES=""`
- **Every launch stays short**: 50 ms at most (`Gpu.LongestLaunchMillis`). A hipBLAS product is cut along its rows by `HipBlas.launchRows`; a kernel that could run long is cut the same way, as `HipAttention` does with `TiledLaunchWork`. A new model comes with a test under `HipRuntime.Trace.watch` asserting its longest launch, as `ViolinDiffusionGpuTests` has
- The work of a launch is bounded where it is launched, by a constant derived from a measured speed and saying so: `HipBlas.LaunchWork` and `LaunchWorkIntoF32` for products (an F32 product's speed varies fifteenfold with its shape: the budget is the slowest shape's), `PatchValuesPerLaunch` and `OverlapWorkPerLaunch` in `HipConvolution`. A kernel over a whole signal takes a `first`/`count` pair so that it can be launched in pieces
- Device memory is written to once, in small pieces, when it is allocated (`HipBase.fresh`): the first launch to write to fresh memory also has it mapped, and took 40 to 80 ms for 1 ms of work
- A single launch's time also includes waiting for whatever else uses the GPU, the desktop first: one launch in a hundred takes ten times its usual time. Judge a kernel by its average, and a model by its longest launch after one unwatched run (`ViolinDecoderGpuTests`)
- A new kernel is first run on tiny shapes; after any new kind of GPU work, read `journalctl -k | grep amdgpu`: a `ring ... timeout` there is the failure to look for
- GPU tests (`runner.gpuTest`) only touch the GPU with `RUNNER_GPU_TESTS=1`, and under a `timeout`. Without it they pass as skipped, so `./mill __.test` is harmless

## Engine

- One `HipRuntime` and one `HipOps` per process, used from one thread: allocations, the spare pool and the scratch region are not synchronised
- Nothing is garbage-collected on the device: what is allocated is released, or held by a `TensorScope`. `release` ignores views (`rows`, `prefix` of another tensor)
- Activations are F32, `[rows, width]`, row-major; linear weights are `[out, in]`. `linear` rounds through F16: results beyond ±65,504 overflow unless `wideProducts` is set, and only `conv1d` keeps F32 sums
- Weights are read as their authors publish them: safetensors or a TensorFlow checkpoint mapped and pinned (`weights/`), or a PyTorch checkpoint read without running its pickles (`TorchCheckpoint`); a model reshapes what it needs on the host at load (`VioletWeights`). No converted copy of published weights is hosted anywhere: the server downloads them from where they are published (`Downloads`)
- The ROCm tree is the TheRock build the server downloads into Ripieno's cache (`Download.Rocm`), handed to the runner as `RUNNER_ROCM_ROOT`. The runner carries its kernels' sources and compiles them with that tree the first time (`KernelCache`): the build needs no ROCm
- `RUNNER_HIP_TRACE=1` times every launch (a device synchronisation after each) and prints the table when the process ends: for measuring, never for running

## Porting a model

- The reference implementation, run on the processor, is the oracle: record the inputs and outputs of each stage and compare stage by stage, then the whole; once the port matches, its tests no longer need the reference
- What does not change while a window is sampled is computed once per window, not once per step (`ViolinDiffusion.condition`)
