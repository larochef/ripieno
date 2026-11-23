package ripieno.backend.models

import java.nio.file.*
import java.util.concurrent.TimeUnit
import java.util.concurrent.locks.ReentrantLock
import java.util.zip.ZipFile
import scala.jdk.CollectionConverters.*
import scala.util.Using
import scala.util.control.NonFatal

import com.typesafe.scalalogging.Logger

/** What the sound engines need that Ripieno does not carry, downloaded from
  * where it is published, at a pinned revision, each file checked by its
  * SHA-256: the models' weights as their authors publish them, and the ROCm the
  * runner's GPU work runs on.
  */
enum Download(val title: String) {

  /** VIOLET's network and audio decoder, on Hugging Face. */
  case Violet extends Download("VIOLET")

  /** MIDI-DDSP's two generators, a zip on Magenta's GitHub (not on Hugging
    * Face).
    */
  case MidiDdsp extends Download("MIDI-DDSP")

  /** AMD's TheRock build of ROCm for gfx1151 (the Radeon of a Strix Halo),
    * whose HIP runtime and hipBLAS the runner loads and whose `hipcc` compiles
    * its kernels: a tarball of 2 GB, 9 GB unpacked.
    */
  case Rocm extends Download("ROCm")

  /** GeneralUser GS, the General MIDI sound bank the page plays with and
    * FluidSynth renders with, on its author's GitHub.
    */
  case SoundBank extends Download("the sound bank")

  /** The Salamander Grand Piano's samples (V3, FLAC), on the GitHub of the SFZ
    * instruments.
    */
  case Piano extends Download("the Salamander piano")
}

/** Where the downloads are, downloaded the first time they are needed: what is
  * on Hugging Face into its cache (`HuggingFaceCache`), shared with every tool
  * that reads it; the rest into Ripieno's `cache`: `models`, `runtimes`,
  * `soundfonts`.
  */
final class Downloads(
    huggingFace: HuggingFaceCache,
    cache: Path,
    downloader: Downloader
) {
  import Downloads.*

  private val logger = Logger(getClass)

  private val models = cache.resolve("models")
  private val runtimes = cache.resolve("runtimes")
  private val soundFonts = cache.resolve("soundfonts")

  // one download of each at a time in this server: the cache's file locks
  // only keep other processes out
  private val locks = Download.values.map(_ -> ReentrantLock()).toMap

  /** The folder of a download's files, once there: VIOLET's repository at its
    * commit, MIDI-DDSP's weights folder, the ROCm tree, the sound bank's
    * folder, the piano's (its `Samples` and its `.sfz`).
    */
  def folder(model: Download): Path = model match {
    case Download.Violet =>
      huggingFace.snapshot(VioletRepository, VioletCommit)
    case Download.MidiDdsp =>
      midiDdspFolder.resolve(MidiDdspWeights)
    case Download.Rocm =>
      runtimes.resolve("therock").resolve(s"gfx1151-$RocmVersion")
    case Download.SoundBank =>
      soundFonts.resolve("generaluser-gs").resolve(SoundBankCommit.take(12))
    case Download.Piano =>
      pianoFolder.resolve(s"SalamanderGrandPiano-$PianoCommit")
  }

  /** The sound bank's file, once downloaded. */
  def soundBank: Path = folder(Download.SoundBank).resolve(SoundBankFile)

  private def pianoFolder: Path =
    soundFonts.resolve("salamander").resolve(PianoCommit.take(12))

  private def midiDdspFolder: Path =
    models.resolve("midi-ddsp").resolve(MidiDdspCommit.take(12))

  def installed(model: Download): Boolean = model match {
    case Download.Violet =>
      VioletFiles.forall((path, _, _) =>
        Files.isRegularFile(folder(model).resolve(path))
      )
    case Download.SoundBank => Files.isRegularFile(soundBank)
    // moved there whole once complete
    case Download.MidiDdsp | Download.Rocm | Download.Piano =>
      Files.isDirectory(folder(model))
  }

  /** The download's folder, its files downloaded first when they are not there;
    * `onProgress` told how far that is, in percent.
    */
  def fetched(
      model: Download,
      isCancelled: () => Boolean,
      onProgress: Int => Unit
  ): DownloadOutcome = {
    val lock = locks(model)
    lock.lock()
    try
      if (installed(model)) DownloadOutcome.Completed(folder(model), 0)
      else {
        logger.info(s"Downloading ${model.title}")
        val outcome =
          try download(model, isCancelled, onProgress)
          catch {
            case NonFatal(error) =>
              DownloadOutcome.Failed(
                Option(error.getMessage).getOrElse(error.toString)
              )
            case _: InterruptedException => DownloadOutcome.Cancelled
          } finally {
            // a stalled transfer is interrupted on purpose: what follows is not
            Thread.interrupted()
          }
        logger.info(s"${model.title}: $outcome")
        outcome
      }
    finally lock.unlock()
  }

  private def download(
      model: Download,
      isCancelled: () => Boolean,
      onProgress: Int => Unit
  ): DownloadOutcome = model match {
    case Download.Violet =>
      val total = VioletFiles.map((_, _, size) => size).sum
      VioletFiles
        .foldLeft[(DownloadOutcome, Long)](
          (DownloadOutcome.Completed(folder(model), 0), 0L)
        ) {
          case ((_: DownloadOutcome.Completed, before), (path, sha256, size)) =>
            val outcome = huggingFace.fetched(
              VioletRepository,
              VioletCommit,
              path,
              sha256,
              isCancelled,
              (bytes, _) => onProgress((100 * (before + bytes) / total).toInt)
            )
            (outcome, before + size)
          case (failed, _) => failed
        }
        ._1 match {
        case _: DownloadOutcome.Completed =>
          DownloadOutcome.Completed(folder(model), total)
        case other => other
      }
    case Download.MidiDdsp =>
      zipped(
        s"https://github.com/magenta/midi-ddsp/raw/$MidiDdspCommit/$MidiDdspWeights.zip",
        MidiDdspSha256,
        midiDdspFolder,
        model,
        isCancelled,
        onProgress
      )
    case Download.Piano =>
      zipped(
        s"https://github.com/sfzinstruments/SalamanderGrandPiano/archive/$PianoCommit.zip",
        PianoSha256,
        pianoFolder,
        model,
        isCancelled,
        onProgress,
        // made by GitHub as it is sent: no length is told
        Some(PianoZipBytes)
      )
    case Download.SoundBank =>
      downloader.fetch(
        url =
          s"https://raw.githubusercontent.com/mrbumpy409/GeneralUser-GS/$SoundBankCommit/$SoundBankFile",
        partFile = soundBank.resolveSibling(s"$SoundBankFile.part"),
        finalFile = soundBank,
        expectedSha256 = Some(SoundBankSha256),
        isCancelled = isCancelled,
        onProgress = (bytes, total) =>
          total.foreach(all => onProgress((100 * bytes / all).toInt))
      ) match {
        case DownloadOutcome.Completed(_, bytes) =>
          DownloadOutcome.Completed(folder(model), bytes)
        case other => other
      }
    case Download.Rocm =>
      val archive = runtimes.resolve("downloads").resolve(RocmArchive)
      downloader.fetch(
        url = s"https://stable.repo.amd.com/rocm/core/tarball/$RocmArchive",
        partFile = archive.resolveSibling(s"$RocmArchive.part"),
        finalFile = archive,
        expectedSha256 = Some(RocmSha256),
        isCancelled = isCancelled,
        onProgress = (bytes, _) => onProgress((100 * bytes / RocmBytes).toInt)
      ) match {
        case DownloadOutcome.Completed(_, bytes) =>
          if (unpacked(archive, folder(model), isCancelled)) {
            Files.delete(archive)
            DownloadOutcome.Completed(folder(model), bytes)
          } else DownloadOutcome.Cancelled
        case other => other
      }
  }

  /** A zip downloaded beside `into`, checked, its files extracted there. */
  private def zipped(
      url: String,
      sha256: String,
      into: Path,
      model: Download,
      isCancelled: () => Boolean,
      onProgress: Int => Unit,
      bytes: Option[Long] = None
  ): DownloadOutcome = {
    val zip = into.resolveSibling(s"${into.getFileName}.zip")
    downloader.fetch(
      url = url,
      partFile = zip.resolveSibling(s"${zip.getFileName}.part"),
      finalFile = zip,
      expectedSha256 = Some(sha256),
      isCancelled = isCancelled,
      onProgress = (done, total) =>
        total.orElse(bytes).foreach(all => onProgress((100 * done / all).toInt))
    ) match {
      case DownloadOutcome.Completed(_, size) =>
        extracted(zip, into)
        Files.delete(zip)
        DownloadOutcome.Completed(folder(model), size)
      case other => other
    }
  }
}

object Downloads {

  // GeneralUser GS v2.0.3
  private val SoundBankCommit = "97049183643d5fc5a9322a69c5b09efb667c6c3a"
  private val SoundBankFile = "GeneralUser-GS.sf2"
  private val SoundBankSha256 =
    "9575028c7a1f589f5770fccc8cff2734566af40cd26ed836944e9a5152688cfe"

  // Salamander Grand Piano V3 as the SFZ instruments keep it (CC-BY 3.0)
  private val PianoCommit = "3382bf9496bba2486f5ab0de55a264d1dfc38404"
  private val PianoSha256 =
    "4f64bc2dadf2eb893e7fb824c2982c03962025e1c82ab9e593e2f7269c50a476"
  private val PianoZipBytes = 748522601L

  private val RocmVersion = "10.1.0"
  private val RocmArchive = s"therock-dist-linux-gfx1151-$RocmVersion.tar.gz"
  // AMD publishes no checksum: this one was taken from the tarball downloaded
  // on 2026-11-13, whose tree ran the runner's tests
  private val RocmSha256 =
    "50deb993035bc2f6a5aba5e3de8d476d8c512a2eec2cbfd5aa569d71e0b8b337"
  private val RocmBytes = 1993807074L

  /** The tarball's tree in `folder`: unpacked by `tar`, which keeps the
    * executables' bits and the links, beside it first, then moved there whole;
    * its single top folder is the tree. False when cancelled meanwhile.
    */
  def unpacked(
      archive: Path,
      folder: Path,
      isCancelled: () => Boolean
  ): Boolean = {
    Files.createDirectories(folder.getParent)
    val unpacking = folder.resolveSibling(s"${folder.getFileName}.unpacking")
    forget(unpacking)
    Files.createDirectories(unpacking)
    try {
      val tar = new ProcessBuilder(
        "tar",
        "-xzf",
        archive.toString,
        "-C",
        unpacking.toString
      ).redirectErrorStream(true).start()
      while (!tar.waitFor(1, TimeUnit.SECONDS) && !isCancelled()) {}
      if (tar.isAlive) {
        tar.destroyForcibly().waitFor()
        false
      } else {
        val said = new String(tar.getInputStream.readAllBytes()).take(300)
        if (tar.exitValue() != 0)
          throw IllegalStateException(s"tar failed on $archive: $said")
        val top =
          Using.resource(Files.list(unpacking))(_.iterator.asScala.toList)
        val tree = top match {
          case only :: Nil if Files.isDirectory(only) => only
          case _                                      => unpacking
        }
        Files.move(tree, folder, StandardCopyOption.ATOMIC_MOVE)
        true
      }
    } finally forget(unpacking)
  }

  private val VioletRepository = "User-tian/VIOLET"
  private val VioletCommit = "b5b316735adf9e01e067e250afd5c0c3abde467e"

  /** VIOLET's files the runner reads: path, SHA-256, size. */
  private val VioletFiles = Seq(
    (
      "pretrained_checkpoint/ema_snapshots/ema_prof_99515",
      "5e33833e68459115a96b2b696e709a117fcd7abf6556b18d003d39875e7a7262",
      581281166L
    ),
    (
      "dacvae_ft/weights.pth",
      "3d1e57c2dd75ae8c40ab6a26888e941ca542d9ed4acb955c002e3396e2a14663",
      430800929L
    )
  )

  // the commit of Magenta's `models` branch the zip is taken from
  private val MidiDdspCommit = "d466030e5a7dce7e71917775ae3e7d5088e840e5"
  private val MidiDdspWeights = "midi_ddsp_model_weights_urmp_9_10"
  private val MidiDdspSha256 =
    "46bae44c16c3399c24f8f0ba2557fc0432bc12aa23088606d465b22fec2cd05e"

  /** The zip's files in `folder`: extracted beside it first, then moved there
    * whole. An entry that would land outside it is refused.
    */
  def extracted(zip: Path, folder: Path): Unit = {
    val extracting = folder.resolveSibling(s"${folder.getFileName}.extracting")
    forget(extracting)
    Files.createDirectories(extracting)
    Using.resource(new ZipFile(zip.toFile)) { archive =>
      for (entry <- archive.entries().asScala) {
        val target = extracting.resolve(entry.getName).normalize
        if (!target.startsWith(extracting))
          throw IllegalArgumentException(
            s"$zip: ${entry.getName} would be extracted outside its folder"
          )
        if (entry.isDirectory) Files.createDirectories(target)
        else {
          Files.createDirectories(target.getParent)
          Using.resource(archive.getInputStream(entry))(in =>
            Files.copy(in, target, StandardCopyOption.REPLACE_EXISTING)
          )
        }
      }
    }
    Files.move(extracting, folder, StandardCopyOption.ATOMIC_MOVE)
  }

  private def forget(folder: Path): Unit =
    if (Files.exists(folder))
      Using
        .resource(Files.walk(folder))(_.iterator.asScala.toList)
        .reverse
        .foreach(Files.delete)
}
