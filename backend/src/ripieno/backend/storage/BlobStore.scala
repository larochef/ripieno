package ripieno.backend.storage

import java.nio.file.*
import java.security.MessageDigest
import java.time.Instant
import java.util.HexFormat

/** Files kept whole and never changed, named by the SHA-256 of their content:
  * scans, scores, recordings. The same content is kept once.
  *
  * A blob is either a copy under the data folder (`blobs/3f/a9c2…`) or a file
  * of the catalog left where it is (`catalog:vivaldi/op01/…`).
  */
final class BlobStore(places: Places, catalog: Path) {

  /** Keeps `bytes`; their hash. */
  def put(
      session: Database.Session,
      bytes: Array[Byte],
      mediaType: String
  ): String = {
    val hash = BlobStore.hashOf(bytes)
    if (location(session, hash).isEmpty) {
      val relative = s"blobs/${hash.take(2)}/${hash.drop(2)}"
      val target = places.data.resolve(relative)
      Files.createDirectories(target.getParent)
      val partial = target.resolveSibling(s"${target.getFileName}.part")
      Files.write(partial, bytes)
      Files.move(partial, target, StandardCopyOption.ATOMIC_MOVE)
      record(session, hash, mediaType, bytes.length.toLong, relative)
    }
    hash
  }

  /** Refers to a file of the catalog where it is; its hash. */
  def refer(
      session: Database.Session,
      file: Path,
      mediaType: String
  ): String = {
    val bytes = Files.readAllBytes(file)
    val hash = BlobStore.hashOf(bytes)
    if (location(session, hash).isEmpty) {
      val relative = catalog.toAbsolutePath.normalize
        .relativize(file.toAbsolutePath.normalize)
        .toString
      record(
        session,
        hash,
        mediaType,
        bytes.length.toLong,
        s"catalog:$relative"
      )
    }
    hash
  }

  /** Forgets those of `hashes` nothing refers to any more; the files of the
    * data folder they were, to delete once the transaction is kept (a file of
    * the catalog stays where it is).
    */
  def forgetUnused(
      session: Database.Session,
      hashes: List[String]
  ): List[Path] =
    hashes.distinct.filter(unused(session, _)).flatMap { hash =>
      val where = location(session, hash)
      session.update("DELETE FROM blob WHERE hash = ?", hash)
      where.filterNot(_.startsWith("catalog:")).map(places.data.resolve)
    }

  private def unused(session: Database.Session, hash: String): Boolean =
    session
      .queryOne(
        "SELECT (SELECT COUNT(*) FROM movement WHERE score = ?) + " +
          "(SELECT COUNT(*) FROM source WHERE blob = ?) + " +
          "(SELECT COUNT(*) FROM recording WHERE audio = ?) AS uses",
        hash,
        hash,
        hash
      )(_.int("uses"))
      .contains(0)

  /** Where a blob is on disk, if it is known. */
  def path(session: Database.Session, hash: String): Option[Path] =
    location(session, hash).map { where =>
      if (where.startsWith("catalog:"))
        catalog.resolve(where.stripPrefix("catalog:"))
      else places.data.resolve(where)
    }

  def bytes(session: Database.Session, hash: String): Option[Array[Byte]] =
    path(session, hash).filter(Files.isRegularFile(_)).map(Files.readAllBytes)

  private def location(
      session: Database.Session,
      hash: String
  ): Option[String] =
    session.queryOne("SELECT location FROM blob WHERE hash = ?", hash)(
      _.string("location")
    )

  private def record(
      session: Database.Session,
      hash: String,
      mediaType: String,
      size: Long,
      location: String
  ): Unit = {
    session.update(
      "INSERT INTO blob (hash, media_type, size, location, created_at) VALUES (?, ?, ?, ?, ?)",
      hash,
      mediaType,
      size,
      location,
      Instant.now().toString
    )
    ()
  }
}

object BlobStore {
  def hashOf(bytes: Array[Byte]): String =
    HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(bytes))
}
