# Backend Architecture

## Routes

- Routes are defined in `shared/src/ripieno/shared/routes/`, one object per domain (`ScoreRoutes`), on `Routes.baseRoute`
- They are implemented in the domain's `*Api` class (`ScoreApi`), next to what it serves, and registered in `Main`
- **Keep route handlers thin**: delegate to a method of the domain
- Types exchanged with the page are in `shared/src/ripieno/shared/Model.scala`, each with its jsoniter codec and Tapir schema in its companion
- A failure the page shows to the user is an error output carrying the reason in words (`Routes.unreadable`)

## Server

- Netty Sync (`tapir-netty-server-sync`), set up in `Main.scala`
- Anything under `/api` that matches no route is a 404; any other path gets the page's shell (`FrontendApi`)

## Score library

- A path received from the page is resolved against the library's root and refused when it leaves it: go through `ScoreLibrary.resolved`, never `Path.of` on what came in
- The page's engraver is only ever given MusicXML in UTF-8. `ScoreLibrary` unzips `.mxl`, transcodes (some files are UTF-16) and converts every other format with SheetMusic4J: a new format is added there, not in the page

## Rendered sound

- A rendering is made from the MIDI the page sends, never from a MIDI the backend would produce itself: what is heard only lines up with the sheet if both come from the same bytes. `SoundRenderer` names a rendering after the digest of those bytes
- An engine (`RenderingEngine`) is built in: VIOLET and MIDI-DDSP run the runner as a process of their own, the backend splitting, bowing and mixing; `sampled` is FluidSynth with the page's sound bank; the Salamander piano runs in the backend, FluidSynth playing the other parts. Its name is its name on the wire and part of its renderings' digest: never rename one; `SoundEngine` on the page lists the ones offered. Rendering takes about as long as the music lasts: it is a job the page asks about again, never a request that waits, and a rendering nobody asks about any more is stopped
- A file an audio or video element plays is served with `ByteRanges`, as `SoundRenderingApi` does, not with tapir's static files: those answer `Range: bytes=0-` with a `Content-Range` browsers refuse

