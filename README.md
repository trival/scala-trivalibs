# Scala trivalibs

A collection of libraries for web and graphics programming in Scala.js.

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
