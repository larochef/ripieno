# Good Practices

## Descriptive names over comments

When tempted to write a comment labelling a block, extract the block into a method or value named after **what the thing is**: `playButton`, `speedSlider`, `partButtons` rather than `leftSection`. Keep only the comments explaining *why* something non-obvious is done.

## Naming consistency within a family

A new file, class or object belonging to a family takes the **same full prefix** as its siblings: `AppBus`, `AppCommand`, `AppEvent`, `AppStreams`. Grep for the siblings before naming.

## Constructor argument order

1. `bus: AppBus`, when present
2. Services (`*Service`)
3. Everything else: API clients, signals, callbacks, primitive parameters

## Remove unused code

When editing a file, remove what is no longer used: constructor parameters never read, private vals and locals, imports, and the code that removing them makes dead in turn, up to the call sites, in the same edit.

- Unused lambda parameters are `_`
- Never silence the compiler with `@unused`: remove the variable

## Nothing personal in the repository

No file of the repository says who uses the application or what they play: no such comment, example, default or file name.

## Git and Git LFS

A file goes to Git LFS when it is binary storage that no diff between versions can be read from (sounds, MIDI, `.npy`, images), whatever its size; text stays in Git, even a large JSON. A new binary type gets its pattern in `.gitattributes` with the file.
