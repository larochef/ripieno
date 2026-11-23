package ripieno.frontend.model

import com.raquo.laminar.api.L.*

/** What a request comes back with: what was asked for, or why there is none. */
type AppResult[A] = Either[String, A]

// Components push commands and react to events; services handle the commands
// and publish the events. Components never reference a service.
class AppBus {
  val commands: EventBus[AppCommand] = EventBus()
  val events: EventBus[AppEvent] = EventBus()

  def send(command: AppCommand): Unit = commands.writer.onNext(command)

  def publish(event: AppEvent): Unit = events.writer.onNext(event)

  // Sends the command each time the returned stream starts, once it listens to
  // the events: Airstream delivers the answer in a later transaction, so it
  // can't be missed. Answers with the events `answer` picks, or with the
  // command's failure.
  def request[A](
      command: AppCommand
  )(answer: PartialFunction[AppEvent, A]): EventStream[AppResult[A]] = {
    EventStream.fromValue(command).flatMapSwitch { command =>
      send(command)
      events.events.collect {
        case AppEvent.RequestFailed(`command`, reason) => Left(reason)
        case event if answer.isDefinedAt(event)        => Right(answer(event))
      }
    }
  }
}
