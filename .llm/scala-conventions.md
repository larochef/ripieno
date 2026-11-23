# Scala 3 Conventions

## Syntax

- Braces and parenthesised conditions, never significant indentation, `then` or `do`: `if (ready) { ... }`, `for (note <- notes) { ... }`. The compiler runs with `-old-syntax`
- Code, comments and every file of the repository are in English

## Idioms

- Conversions of domain models are methods or extension methods
- `collect { case Some(value) => value }` instead of `.filter(_.isDefined).map(_.get)`
- `_.isEmpty` instead of `_ == None`; `_.contains(value)` instead of `_ == Some(value)`

## Imports

- **Never write fully-qualified type names inline** in signatures, type annotations or expressions: import the type at the top of the file and use its simple name
- Bad: `def clicked(event: org.scalajs.dom.MouseEvent)`. Good: `import org.scalajs.dom.MouseEvent`, then `event: MouseEvent`

## Visibility

- No package-private members (`private[pkg]`): make them `private`, or public when other files need them
- No restricted constructors: keep constructors public, even for a type created in one place only
