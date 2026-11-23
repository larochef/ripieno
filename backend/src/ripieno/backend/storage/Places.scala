package ripieno.backend.storage

import java.nio.file.{Files, Path}

/** Where Ripieno keeps what it keeps, as the XDG base directories say: its data
  * (the library, the files it refers to, the catalog's scores) under
  * `~/.local/share/ripieno`, what it can make again under `~/.cache/ripieno`.
  * `RIPIENO_DATA` and `RIPIENO_CACHE` move them.
  */
final case class Places(data: Path, cache: Path) {
  def database: Path = data.resolve("library.db")
  def blobs: Path = data.resolve("blobs")

  /** The catalog's scores: those downloaded (`CatalogDownload`), and any the
    * player keeps beside them.
    */
  def catalog: Path = data.resolve("catalog")

  def created(): Places = {
    Files.createDirectories(data)
    Files.createDirectories(blobs)
    Files.createDirectories(catalog)
    Files.createDirectories(cache)
    this
  }
}

object Places {
  private val name = "ripieno"

  def fromEnvironment(environment: Map[String, String]): Places = {
    val home = Path.of(sys.props("user.home"))
    def xdg(variable: String, fallback: String): Path =
      environment
        .get(variable)
        .filter(_.nonEmpty)
        .map(Path.of(_))
        .getOrElse(home.resolve(fallback))
        .resolve(name)
    Places(
      data = environment
        .get("RIPIENO_DATA")
        .map(Path.of(_))
        .getOrElse(xdg("XDG_DATA_HOME", ".local/share")),
      cache = environment
        .get("RIPIENO_CACHE")
        .map(Path.of(_))
        .getOrElse(xdg("XDG_CACHE_HOME", ".cache"))
    )
  }
}
