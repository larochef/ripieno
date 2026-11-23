package ripieno.backend.models

import java.net.URI
import java.nio.channels.FileChannel
import java.nio.file.*

/** The Hugging Face hub's cache, shared with every tool that reads it
  * (`huggingface_hub`, drift), written in its own layout:
  *
  * {{{
  * <root>/.locks/models--{org}--{repo}/<sha256>.lock   held while downloading
  * <root>/models--{org}--{repo}/
  *   refs/main              = a commit, written when there is none
  *   blobs/<sha256>         = a file's content
  *   snapshots/<commit>/<path> -> a relative link into blobs/
  * }}}
  *
  * It only ever adds: blobs, snapshots, links. A file is asked for at a commit
  * and by its SHA-256, both pinned by the caller, so no listing is asked of
  * Hugging Face.
  */
final class HuggingFaceCache(
    val root: Path,
    downloader: Downloader,
    /** Where the files are asked for: Hugging Face, a local server in tests. */
    site: URI = URI.create("https://huggingface.co")
) {

  private def repositoryFolder(repository: String): Path =
    root.resolve("models--" + repository.replace("/", "--"))

  /** Where the repository's files are at `commit`, once downloaded. */
  def snapshot(repository: String, commit: String): Path =
    repositoryFolder(repository).resolve("snapshots").resolve(commit)

  /** The file `path` of the repository at `commit`, downloaded unless its blob
    * is there already, checked against `sha256`, linked into the snapshot.
    */
  def fetched(
      repository: String,
      commit: String,
      path: String,
      sha256: String,
      isCancelled: () => Boolean,
      onProgress: (Long, Option[Long]) => Unit
  ): DownloadOutcome = {
    val folder = repositoryFolder(repository)
    val blob = folder.resolve("blobs").resolve(sha256)
    val outcome = locked(repository, sha256) {
      if (Files.isRegularFile(blob))
        DownloadOutcome.Completed(blob, Files.size(blob))
      else
        downloader.fetch(
          // the path encoded segment by segment: file names may hold spaces
          url = URI(
            site.getScheme,
            site.getAuthority,
            s"/$repository/resolve/$commit/$path",
            null,
            null
          ).toASCIIString,
          // the cache's own name for a blob in flight
          partFile = folder.resolve("blobs").resolve(s"$sha256.incomplete"),
          finalFile = blob,
          expectedSha256 = Some(sha256),
          isCancelled = isCancelled,
          onProgress = onProgress
        )
    }
    outcome match {
      case done: DownloadOutcome.Completed =>
        val link = snapshot(repository, commit).resolve(path)
        Files.createDirectories(link.getParent)
        if (!Files.exists(link, LinkOption.NOFOLLOW_LINKS))
          Files.createSymbolicLink(link, link.getParent.relativize(blob))
        val main = folder.resolve("refs").resolve("main")
        if (!Files.exists(main)) {
          Files.createDirectories(main.getParent)
          Files.writeString(main, commit)
        }
        DownloadOutcome.Completed(link, done.bytes)
      case other => other
    }
  }

  /** `huggingface_hub` holds a lock per blob while downloading it: holding the
    * same keeps a `hf download` of the same file from racing this one.
    */
  private def locked[A](repository: String, sha256: String)(body: => A): A = {
    val folder = root
      .resolve(".locks")
      .resolve(
        repositoryFolder(repository).getFileName
      )
    Files.createDirectories(folder)
    val channel = FileChannel.open(
      folder.resolve(s"$sha256.lock"),
      StandardOpenOption.CREATE,
      StandardOpenOption.WRITE
    )
    try {
      val lock = channel.lock()
      try body
      finally lock.release()
    } finally channel.close()
  }
}

object HuggingFaceCache {

  /** The cache's folder as the hub finds it: `HF_HUB_CACHE`, else `HF_HOME`'s
    * `hub`, else `huggingface/hub` in the XDG cache.
    */
  def rootOf(environment: Map[String, String], home: Path): Path = {
    def variable(name: String): Option[Path] =
      environment.get(name).map(_.trim).filter(_.nonEmpty).map(Path.of(_))
    variable("HF_HUB_CACHE")
      .orElse(variable("HF_HOME").map(_.resolve("hub")))
      .getOrElse(
        variable("XDG_CACHE_HOME")
          .getOrElse(home.resolve(".cache"))
          .resolve("huggingface")
          .resolve("hub")
      )
  }
}
