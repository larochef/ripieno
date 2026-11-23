# Build System

Mill, build file `build.mill`.

## THE MILL LOCK: CHECK IT BEFORE EVERY COMMAND

Mill locks `out/` for as long as a command runs, a `backend.run` and the IDE's build server included. A second `./mill` waits behind it without a word, for minutes.

- Run every Mill command as `./mill --no-wait-for-build-lock ...`, under a timeout: it fails at once when the lock is taken
- The lock is held by something you started: stop that one process, by its own PID, and retry
- The lock is held by anything else (the developer's `./mill backend.run`, the IDE): **stop and ask**. Never kill it, never wait for it. Meanwhile `MILL_OUTPUT_DIR=<another folder> ./mill ...` builds apart, with a lock of its own
- Never free a port or a lock with a blanket kill (`fuser -k`, `pkill`): find the process and check it is yours first
- Never start a test server with `./mill backend.run`, which keeps the lock: see "Running" below

## Commands

- Compile: `./mill __.compile`, or `./mill [module].compile`. Warnings are errors only under `CI=1`: use it before finishing
- After development is complete, always run `./mill __.fix + __.reformat` (scalafix import ordering, then scalafmt)

## Modules

- `backend`, `shared.jvm`, `shared.js`, `frontend`
- `runner` (`runner.test` on the processor, `runner.gpuTest` on the GPU when asked): `backend.run` builds it and hands the server its classpath (`RIPIENO_RUNNER_CLASSPATH`) to launch it; a server started by hand needs it too. See `runner.md`

## Dependencies

- `mvn"groupId:artifactId:version"` in the module's `mvnDeps`; Scala.js dependencies use `mvn"groupId::artifactId::version"`
- Versions shared by several dependencies are vals at the top of `build.mill`
- Always the latest stable release: check Maven Central or npm when adding or touching one
- JavaScript libraries are npm dependencies in the root `package.json`, bundled by Vite

## Running

- `./mill backend.run` links the frontend in fast mode, bundles it with Vite and serves everything on port 4322. This is the developer's way of running it, and it holds the lock
- For a test server of your own: `./mill frontend.fastLinkJS`, then `SCALAJS_MODE=fast npm run build -- --outDir <dir>`, then start `ripieno.backend.Main` with `java` on `<dir>` plus `backend.runClasspath`, with `RIPIENO_PORT` set to another port
- `npm run dev` serves the page with Vite on port 5174, proxying `/api` to the backend

## Realistic sound

- What the engines play with downloads on first use, pinned and checked (`Downloads`): VIOLET into the Hugging Face cache, MIDI-DDSP into `~/.cache/ripieno/models`, ROCm into `~/.cache/ripieno/runtimes`, the page's sound bank and the Salamander piano into `~/.cache/ripieno/soundfonts`. Nothing else is installed by hand but the system's FluidSynth, ffmpeg and libsoxr. A test needing a download gets it the same way first (`Downloaded`, in `backend.test`, which `runner.test` depends on): never skipped for want of it, nor reading a file kept by hand
- MIDI-DDSP runs in the runner on the processor (`CpuOps`), VIOLET on the GPU: a rendering uses both at once

## Data

- The player's data (the SQLite library, editions, recordings) is under `~/.local/share/ripieno`, what can be made again (rendered sounds, PDFs) under `~/.cache/ripieno`; `RIPIENO_DATA` and `RIPIENO_CACHE` move them. A test server gets its own of both
- The catalog's scores are the server's to download, into `~/.local/share/ripieno/catalog` when it first starts (`CatalogDownload`: music21's corpus, pinned); scores put there by hand are indexed alike and never committed. The library knows what `CatalogBuilder` makes of that folder, made again when its files change
