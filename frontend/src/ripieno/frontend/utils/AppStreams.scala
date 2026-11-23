package ripieno.frontend.utils

import ripieno.frontend.model.AppResult

import scala.scalajs.js
import scala.util.{Failure, Success}

import com.raquo.airstream.ownership.ManualOwner
import com.raquo.laminar.api.L.*

object AppStreams {

  // Listens to a stream until its first event, a request's answer for
  // instance, so that services don't keep a subscription per request.
  def runOnce[A](stream: EventStream[A])(onEvent: A => Unit): Unit = {
    val owner = ManualOwner()
    stream.foreach { event =>
      owner.killSubscriptions()
      onEvent(event)
    }(using owner)
  }

  // A promise as a stream whose failure travels as a value, not a stream error.
  def fromPromise[A](promise: => js.Promise[A]): EventStream[AppResult[A]] = {
    EventStream.fromJsPromise(promise).recoverToTry.map {
      case Success(value) => Right(value)
      case Failure(error) => Left(error.getMessage)
    }
  }
}
