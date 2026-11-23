# Frontend Architecture

Stack: Scala.js, Laminar and Airstream (`Var`, `Signal`, `EventStream`), Bulma, sttp and the Tapir endpoints of `shared`. Verovio (WebAssembly) engraves, spessasynth plays.

## Bus

- No `Future`: only Laminar and Airstream
- Components never reference a service. They push `AppCommand`s on the `AppBus` and react to its `AppEvent`s. `bus.request(command)(answer)` sends the command each time the returned stream starts, so binding it with `-->` or a `flatMapSwitch` replaces mount callbacks
- Events state facts. A business outcome has its own event (`ScoreUnreadable`); any other failure is `RequestFailed(command, reason)`
- Services (`appServices/`) subscribe to the commands and publish events. A new service takes the bus and the clients it needs, and runs each request with `AppStreams.runOnce`
- `ApiClient` returns `EventStream[AppResult[T]]`: failures travel as values, not stream errors. Don't alias the whole stream type: a one-parameter alias of a stream breaks the inference of `flatMapSwitch`
- State a component shows is derived from the bus's events (`events.collect { ... }.startWith(...)`), not kept in a `Var` that a service would have to update

## Components

- Every component is a class extending `ReactiveElement`, whose `lazy val node` is the Laminar element, built once. No factory objects
- A conditional modifier or element is `Option.when(condition)(...)`, never an `if` with an empty branch
- Every font is bundled with the page (fontsource packages imported in `frontend/main.js`), never left to the system's: text would align differently from one system to the next
- Every text shown is `Tr(english, french)` and every note name goes through `NoteNames` (do ré mi, middle C is do3 in French): no text left in one language. What the server stores (part names, edition names, values kept in the browser) stays in English, the same whatever the language
- `ScoreSheet` is the one place handling the DOM by hand: Verovio's pages are SVG text, lit and scrolled through their element ids

## Airstream sttp backend

`api/airstream/` provides `AjaxBackend`, an sttp backend whose effect is an `EventStream`:

- A stream keeps no result, unlike a `Future`: restarting it sends the request again, and a late subscriber gets nothing
- Requests don't abort when their stream stops, and streaming bodies, file responses and web sockets aren't supported
- It relies on sttp's `internal` `BodyFromResponseAs`: check it when upgrading sttp

## Engraving and sound: traps

- **One layout is the source of everything**: a score's pages, its MIDI and the times of its notes all come from the same `Engraving`. Never compute timings or MIDI elsewhere: what is heard and what is lit would drift apart
- Verovio answers questions of time (`getElementsAtTime`) only after `renderToTimemap` or `renderToMIDI` has run: `Engraving.load` does it
- What a score says (its editions played, its tempi, how each note is played) is read by the server (`backend/src/ripieno/backend/reading`, `POST /api/score/prepared`): the page engraves and plays what it is given, it does not analyse the MusicXML
- A note's id comes from the source (`ScoreEditing.identify`, on the server: part, measure, rank) and is the same in every engraving; edits name notes by it. Verovio gives every other element a new id at each load: such an id is only valid for the `Engraving` it came from
- Verovio has no option to show one part: `MusicXml.only` removes the others before engraving
- Never give Verovio anything but MusicXML: its ABC reader throws a WebAssembly exception on real tunes, which leaves the toolkit unusable
- The sequencer takes ownership of the MIDI buffer it is given: `Player.load` hands it a copy
- Whatever plays the score is a `ScorePlayer`, whose time is the score's own, in seconds at its written tempo. A new source of sound is another `ScorePlayer`, switched by `PlaybackService`; the sheet and the toolbar know nothing of it
- What is shown follows the audio clock, less the output delay the browser reports (`Player.position`), never wall-clock time
- The browser only starts audio after a click: `Player.open` is called from a command, never when the page loads
- JavaScript libraries are only typed as far as constructing them (`Libraries`); their calls are spelled out in the class wrapping them
