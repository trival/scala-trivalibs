# Scala trivalibs

A collection of libraries for web and graphics programming in Scala.js.

Important: This is a very imperative style library targeting and using
JavaScript APIs directly. We avoid using functional programming patterns and the
Scala standard library as much as possible to minimize overhead and maximize
performance.

We use Scala 3 because of its string metaprogramming and contextual abstraction
features. We use its superb ability to create great tailored DSLs, while being
able to write low-level code with minimal overhead.

As data structures we use only JS objects, arrays, TypedArrays and js.UndefOr.
We use Promises directly for async code instead of Scala's Future.

But we provide type-safe APIs on top of them, so you can write code in a more
Scala-like way, using for comprehensions and concise syntax, without having to
deal with the low-level details of JavaScript interop.

## Usage

This library is meant to be included as source code via git submodule:

```bash
git submodule add <repo-url> trivalibs
```

In your project's `project.scala`:

```scala
//> using exclude trivalibs/test/**
//> using dep org.scala-js::scalajs-dom::2.8.1
```

Or if you prefer explicit inclusion:

```scala
//> using file trivalibs/src
//> using dep org.scala-js::scalajs-dom::2.8.1
```

## Contents

- **preact/** - Type-safe Preact bindings with signals and HTML DSL
- **utils/** - Utility libraries
  - `bufferdata.scala` - Zero-cost typed binary data structures
  - `numbers.scala` - Numeric extension methods
  - `js.scala` - JavaScript interop helpers

## Development

### Prerequisites

- [scala-cli](https://scala-cli.virtuslab.org/) installed

### Running Tests

```bash
cd test
scala-cli test .
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file
for details.
