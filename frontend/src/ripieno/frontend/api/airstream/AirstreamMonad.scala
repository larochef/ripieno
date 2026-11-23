package ripieno.frontend.api.airstream

import scala.util.*

import com.raquo.laminar.api.L.*
import sttp.monad.MonadError

// An effect is a stream emitting a single value, or a single error, once
// started: sttp only builds such streams. Unlike a Future, a stream keeps no
// result: restarting it runs it again.
object AirstreamMonad extends MonadError[EventStream] {

  override def unit[T](t: T): EventStream[T] = EventStream.fromValue(t)

  override def map[T, T2](fa: EventStream[T])(f: T => T2): EventStream[T2] =
    fa.map(f)

  override def flatMap[T, T2](fa: EventStream[T])(
      f: T => EventStream[T2]
  ): EventStream[T2] =
    fa.flatMapSwitch(f)

  override def error[T](t: Throwable): EventStream[T] =
    EventStream.fromTry(Failure(t))

  override protected def handleWrappedError[T](rt: EventStream[T])(
      h: PartialFunction[Throwable, EventStream[T]]
  ): EventStream[T] =
    rt.recoverToTry.flatMapSwitch {
      case Success(value) => EventStream.fromValue(value)
      case Failure(error) if h.isDefinedAt(error) => h(error)
      case Failure(error) => EventStream.fromTry(Failure(error))
    }

  override def ensure[T](
      f: EventStream[T],
      e: => EventStream[Unit]
  ): EventStream[T] =
    ensure2(f, e)

  override def ensure2[T](
      f: => EventStream[T],
      e: => EventStream[Unit]
  ): EventStream[T] =
    Try(f).fold(error, identity).recoverToTry.flatMapSwitch { result =>
      e.recoverToTry.flatMapSwitch(_ => EventStream.fromTry(result))
    }
}
