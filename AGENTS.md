# Repository Instructions

Ripieno, a music practice application: scores are shown, played and followed note by note. Scala 3, built with Mill (`build.mill`).

- Web application: `backend` (Tapir on Netty, Ox) serves `frontend` (Scala.js, Laminar), with `shared` compiled for both
- Backend entry point: `backend/src/ripieno/backend/Main.scala`
- Frontend entry point: `frontend/src/ripieno/frontend/Application.scala`
- `runner`: runs the sound engines' models on the GPU (Scala and HIP kernels)

## Conventions: `.llm/`

Project conventions live in `.llm/`. Read only the files your task touches, before starting.

| File | Read it when |
|---|---|
| `build.md` | running any Mill command, compiling, formatting, running the application, adding dependencies |
| `scala-conventions.md` | writing any Scala: syntax, imports, visibility |
| `good-practices.md` | writing any code: naming, comments, unused code |
| `backend-architecture.md` | routes, their implementation, the score library |
| `frontend-architecture.md` | the page: bus, services, components, engraving and sound |
| `runner.md` | anything that runs on the GPU, or any model |

## Maintaining `.llm/`

These files are read on every task, so every line costs every later task. **Leave them alone by default.**

Add a line only when all three hold:

1. Someone working on a *different* task would get something wrong without it.
2. It cannot be learned from the code that task would open anyway.
3. It is a rule or a trap, not a description of what was built.

Never add a description of a feature, a list of classes or files, or a justification of an implementation. State a rule in one or two sentences, name one place in the code that shows it, and remove what it makes obsolete. Knowledge local to one file is a comment in that file.

Conventions change only when a human asks. If the code you are writing contradicts one, say so instead of editing the rule.
