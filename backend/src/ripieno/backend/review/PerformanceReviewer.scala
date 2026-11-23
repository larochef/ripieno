package ripieno.backend.review

import ripieno.backend.sound.Wav
import ripieno.shared.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.UUID
import java.util.concurrent.*
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.jdk.CollectionConverters.*
import scala.util.{Try, Using}
import scala.util.control.NonFatal

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.typesafe.scalalogging.Logger
import ox.*

/** Reviews recordings of a part against its score (`RecordingFeatures`, then
  * `PerformanceAnalysis`), and keeps them: each review is a folder of `reviews`
  * holding the part (`reference.json`), the recording and, once made, the
  * review. ffmpeg reads the recording, in whatever format the page recorded it.
  */
class PerformanceReviewer(reviews: Path)(using Ox) {
  private val logger = Logger(getClass)

  // A review takes a few cores for a few seconds: one at a time is enough.
  private val oneAtATime = Semaphore(1)
  private val longestDecoding: FiniteDuration = 2.minutes

  private val reviewing = ConcurrentHashMap.newKeySet[String]()
  private val failures = ConcurrentHashMap[String, String]()

  Files.createDirectories(reviews)

  def start(reference: PerformanceReference): Either[String, ReviewStatus] = {
    if (reference.notes.isEmpty) {
      Left("The part has no notes to review")
    } else {
      val id = UUID.randomUUID().toString.replace("-", "")
      Files.createDirectories(folder(id))
      Files.write(referenceFile(id), writeToArray(reference))
      Right(described(id))
    }
  }

  def record(
      id: String,
      recording: Array[Byte]
  ): Either[String, ReviewStatus] = {
    if (!isId(id) || !Files.isDirectory(folder(id))) {
      Left(s"No review $id")
    } else if (recording.isEmpty) {
      Left("The recording is empty")
    } else if (!reviewing.add(id)) {
      Left(s"Review $id is already being made")
    } else {
      Files.write(recordingFile(id), recording)
      failures.remove(id)
      fork {
        try {
          run(id)
        } catch {
          case NonFatal(error) =>
            logger.error(s"Review $id failed", error)
            failures.put(id, error.getMessage)
        } finally {
          reviewing.remove(id)
        }
      }.discard
      Right(described(id))
    }
  }

  def status(id: String): Either[String, ReviewStatus] =
    Either.cond(
      isId(id) && Files.isDirectory(folder(id)),
      described(id),
      s"No review $id"
    )

  /** The recordings reviewed of a score, the latest first; those whose review
    * cannot be read are left out.
    */
  def summaries(score: String): Either[String, ReviewSummaries] = {
    val folders =
      if (Files.isDirectory(reviews))
        Using.resource(Files.list(reviews))(_.iterator.asScala.toList)
      else Nil
    val found = folders
      .map(_.getFileName.toString)
      .filter(isId)
      .flatMap(summary)
      .filter(_.score.contains(score))
      .sortBy(_.recordedAt)
      .reverse
    Right(ReviewSummaries(found))
  }

  private def summary(id: String): Option[ReviewSummary] =
    Try {
      val reference = readFromArray[PerformanceReference](
        Files.readAllBytes(referenceFile(id))
      )
      val review =
        readFromArray[PerformanceReview](Files.readAllBytes(reviewFile(id)))
      def count(verdicts: String*) =
        review.notes.count(note => verdicts.contains(note.verdict))
      ReviewSummary(
        id,
        reference.score,
        reference.part,
        Files.getLastModifiedTime(recordingFile(id)).toInstant.toString,
        review.notes.headOption.fold("")(_.measure),
        review.notes.lastOption.fold("")(_.measure),
        review.notes.size,
        count("right"),
        count("wrong"),
        count("intonation"),
        review.notes.count(_.rhythm.isDefined),
        count("missed", "uncertain"),
        reference.beat,
        reference.writtenBeatsPerMinute
          .filter(_ => review.tempo.overallRatio > 0)
          .map(_ / review.tempo.overallRatio),
        review.tuningCents,
        review.clippedShare
      )
    }.toOption

  def delete(id: String): Either[String, Unit] = {
    if (!isId(id) || !Files.isDirectory(folder(id))) {
      Left(s"No review $id")
    } else if (reviewing.contains(id)) {
      Left(s"Review $id is being made")
    } else {
      Using
        .resource(Files.list(folder(id)))(_.iterator.asScala.toList)
        .foreach(Files.delete)
      Files.delete(folder(id))
      Right(())
    }
  }

  /** Deletes the reviews, recordings with them, of a score naming one of those
    * movements; one being made stays.
    */
  def forget(movements: Set[Long]): Unit =
    Using
      .resource(Files.list(reviews))(_.iterator.asScala.toList)
      .map(_.getFileName.toString)
      .filter(isId)
      .filter { id =>
        Try(
          readFromArray[PerformanceReference](
            Files.readAllBytes(referenceFile(id))
          )
        ).toOption
          .flatMap(_.score)
          .flatMap(Library.movementsOf)
          .exists(_.exists(movements.contains))
      }
      .foreach(delete)

  /** The recording of a review, to be played back. */
  def recording(id: String): Option[Path] =
    Option.when(isId(id))(recordingFile(id)).filter(Files.exists(_))

  private def described(id: String): ReviewStatus = {
    val review = Option
      .when(Files.exists(reviewFile(id)))(
        Try(
          readFromArray[PerformanceReview](Files.readAllBytes(reviewFile(id)))
        ).toEither
      )
    review match {
      case Some(Left(error)) =>
        ReviewStatus(
          id,
          ready = false,
          failure = Some(s"the review could not be read: ${error.getMessage}"),
          None
        )
      case Some(Right(made)) =>
        ReviewStatus(id, ready = true, failure = None, Some(made))
      case None =>
        ReviewStatus(
          id,
          ready = false,
          failure = Option(failures.get(id)),
          None
        )
    }
  }

  // An id is a folder name: nothing else than one of ours is looked for on disk.
  private def isId(id: String): Boolean = id.matches("[0-9a-f]{32}")

  private def folder(id: String): Path = reviews.resolve(id)
  private def referenceFile(id: String): Path =
    folder(id).resolve("reference.json")
  private def recordingFile(id: String): Path = folder(id).resolve("recording")
  private def reviewFile(id: String): Path = folder(id).resolve("review.json")
  private def logFile(id: String): Path = folder(id).resolve("review.log")

  private def run(id: String): Unit = {
    oneAtATime.acquire()
    try {
      logger.info(s"Reviewing $id")
      decoded(id).flatMap { audio =>
        val reference = readFromArray[PerformanceReference](
          Files.readAllBytes(referenceFile(id))
        )
        val notes = reference.notes.toVector.sortBy(_.onset)
        val heard = RecordingFeatures.of(audio, notes, reference.accompaniment)
        PerformanceAnalysis.review(heard, reference)
      } match {
        case Right(review) =>
          Files.write(reviewFile(id), writeToArray(review))
          logger.info(s"Reviewed $id")
        case Left(failure) =>
          Files.writeString(logFile(id), failure + "\n", StandardCharsets.UTF_8)
          failures.put(id, failure)
      }
    } finally {
      oneAtATime.release()
    }
  }

  // The recording as the analysis reads it: one channel at its rate, from the
  // Opus the browser records in WebM or Ogg.
  private def decoded(id: String): Either[String, Array[Float]] = {
    val wav = Files.createTempFile(folder(id), "recording", ".wav")
    try {
      val process = new ProcessBuilder(
        "nice",
        "-n",
        "10",
        "ffmpeg",
        "-loglevel",
        "error",
        "-y",
        "-i",
        recordingFile(id).toString,
        "-ac",
        "1",
        "-ar",
        ReviewFeatures.Rate.toString,
        wav.toString
      ).redirectErrorStream(true).redirectOutput(logFile(id).toFile).start()
      if (!process.waitFor(longestDecoding.toSeconds, TimeUnit.SECONDS)) {
        process.destroy()
        Left(s"reading the recording took more than $longestDecoding")
      } else if (process.exitValue() != 0) {
        Left(
          Files
            .readAllLines(logFile(id), StandardCharsets.UTF_8)
            .asScala
            .filter(_.trim.nonEmpty)
            .lastOption
            .getOrElse(
              s"ffmpeg could not read the recording (${process.exitValue()})"
            )
        )
      } else Right(Wav.read(Files.readAllBytes(wav)).mono)
    } finally {
      Files.deleteIfExists(wav)
    }
  }
}
