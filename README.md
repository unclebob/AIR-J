# AIR-J

AIR-J is an AI-first, JVM-targeting programming language whose primary artifact is a canonical, typed, effect-tracked intermediate representation.

It is meant to be written, transformed, checked, and lowered by software agents, not optimized for direct human authorship.

The mission, taken directly from the project design notes, is to:
- reduce ambiguity
- reduce representation variance
- reduce re-analysis cost
- make transformation legality mechanically checkable
- preserve direct access to the JVM ecosystem

Principle: `one meaning, one representation`.

## What AIR-J Is

AIR-J is not trying to be a nicer Java, Kotlin, or Clojure for humans. The persisted form is a canonical s-expression tree with:
- explicit imports and exports
- explicit types
- explicit effects
- explicit control flow
- explicit mutation
- explicit Java interop
- direct lowering to JVM bytecode

That makes AIR-J closer to a writable canonical IR than to a human-oriented surface language.

For the authoritative design and language contract, see:
- [language-reference.md](/language-reference.md)
- [notes.md](/notes.md)
- [formal-v0-spec.md](/formal-v0-spec.md)
- [AGENTS.md](/AGENTS.md)

## Current Status

The implementation is written in Clojure and targets the JVM directly.

Implemented areas include:
- module parsing and normalization
- explicit types and effects
- records, enums, and tagged unions
- contracts and invariants
- canonical primitive operators for `Int`, `Bool`, `Float`, and `Double`
- string and text-sequence primitives
- explicit stdin/stdout operations
- explicit Java interop
- host-backed modules for callback-oriented JVM frameworks such as Processing
- canonical standard modules for `airj/core`, `airj/bytes`, `airj/env`, `airj/file`, `airj/json`, and `airj/process`
- JVM lowering and bytecode emission

AIR-J is already far enough along to compile and run nontrivial programs, but it is still evolving toward a fuller AI-oriented language/runtime.

## CLI

The main entrypoint is `airj.cli`.

Examples:

```bash
clj -M -m airj.cli parse path/to/program.airj
clj -M -m airj.cli normalize path/to/program.airj
clj -M -m airj.cli check path/to/program.airj
clj -M -m airj.cli lower path/to/program.airj
clj -M -m airj.cli build path/to/program.airj target/classes
clj -M -m airj.cli run path/to/program.airj arg1 arg2
```

Two execution modes matter:
- `build` writes `.class` files
- `run` invokes the exported AIR-J `main`

The generated JVM `main` wrapper behaves like this:
- if AIR-J `main` returns `Int`, that becomes the JVM process exit code
- other return values are not printed automatically by the generated JVM wrapper
- program output should be done explicitly with `io/print` or `io/println`

For boundary-heavy programs, the canonical rule is:
- raw boundary ops may carry explicit host effects and `Foreign.Throw`
- `*-result` wrapper functions should convert recoverable host failures into `(Result _ Diagnostic)`
- `Diagnostic.message` should stay stable and machine-oriented
- `Diagnostic.detail` should identify the relevant input, path, or context

## Standard Modules

AIR-J keeps host interaction in a small canonical module surface:
- `airj/core`: canonical carriers such as `Diagnostic`, `Interchange`, `Option`, and `Result`
- `airj/bytes`: raw byte values and UTF-8 conversion
- `airj/env`: explicit environment reads
- `airj/file`: canonical filesystem boundary
- `airj/json`: canonical JSON interchange boundary
- `airj/process`: canonical subprocess boundary
- `airj/test`: canonical test outcomes and assertions
- `airj/test-runner`: AIR-J-native test summary and exit-code logic

These modules exist to reduce representation drift. They are the default machine-facing boundary, and raw Java interop should be reserved for intentional foreign integration.

## AIR-J Tests

The canonical AIR-J test shape is:
- reusable suite modules export uniquely named suite functions
- explicit runnable test-root modules export a zero-arg `tests` function returning `(Seq TestOutcome)`
- root `main` delegates to `airj/test-runner.run` or `airj/test-runner.run-json`
- the resulting root module can be built and run like any other AIR-J jar
- bootstrap CLI testing expects that exact root-module shape; individual exported test functions are not a second supported style
- `clj -M -m airj.cli test --json ...` returns one canonical summary artifact with:
  - `module`
  - `passed`
  - `failed`
  - `errored`
  - `outcomes`

See [`examples/HTW/README.md`](/examples/HTW/README.md) for a complete AIR-J-native test jar example.
See [`examples/ToolWorkflow/README.md`](/examples/ToolWorkflow/README.md) for a non-game example that emits and then consumes the canonical JSON test artifact.
See [`examples/Contracts/README.md`](/examples/Contracts/README.md) for a contract-heavy example that uses invariants plus `requires`/`ensures`.
See [`examples/Ledger/README.md`](/examples/Ledger/README.md) for a contract-heavy ledger example with explicit text and JSON test roots.
See [`examples/Thermostat/README.md`](/examples/Thermostat/README.md) for a non-financial safety-controller example with contract failure tests.
See [`examples/Wiki/README.md`](/examples/Wiki/README.md) for a pure AIR-J wiki domain example mapped from the non-HTTP wiki acceptance features.

## Development Workflow

The repo uses Speclj, coverage, CRAP, mutation testing, and dependency analysis.

Core commands:

```bash
clj -M:check-structure spec
clj -M:spec
clj -M:cov
clj -M:crap
clj -M:mutate src/.../file.clj --scan
clj -M:mutate src/.../file.clj --max-workers 3
clj -M:check-dependencies
```

The stricter project workflow and pinned toolchain are documented in [AGENTS.md](/AGENTS.md).

## Sample Program: Hello, World

This program prints its own output and returns `0`.

```lisp
(module example/hello
  (imports)
  (export main)

  (fn main
    (params (args StringSeq))
    (returns Int)
    (effects (Stdout.Write))
    (requires true)
    (ensures true)
    (seq
      (io/println "Hello, world!")
      0)))
```

Run it:

```bash
clj -M -m airj.cli run hello.airj
```

Build it:

```bash
clj -M -m airj.cli build hello.airj target/classes
```

If the program only uses pure computation and direct Java interop, the generated classes can usually be run directly from the output directory:

```bash
java -cp target/classes example.hello
```

## Sample Program: Prime Factors

This is a small AIR-J command-line program that reads its first argument, computes the prime factors, prints them, and returns `0`.

```lisp
(module example/prime-factors
  (imports)
  (export divides? append-factor main)

  (fn divides?
    (params (n Int) (divisor Int))
    (returns Bool)
    (effects ())
    (requires true)
    (ensures true)
    (int-eq
      (int-mod (local n) (local divisor))
      0))

  (fn append-factor
    (params (acc String) (first? Bool) (factor Int))
    (returns String)
    (effects ())
    (requires true)
    (ensures true)
    (if
      (local first?)
      (int->string (local factor))
      (string-concat
        (string-concat (local acc) " ")
        (int->string (local factor)))))

  (fn main
    (params (args StringSeq))
    (returns Int)
    (effects (Foreign.Throw Stdout.Write))
    (requires (int-gt (seq-length (local args)) 0))
    (ensures true)
    (let ((input
            (string->int
              (seq-get (local args) 0))))
      (loop ((n (local input))
             (divisor 2)
             (acc "")
             (first? true))
        (if
          (int-eq (local n) 1)
          (seq
            (io/println (local acc))
            0)
          (if
            (call (local divides?) (local n) (local divisor))
            (recur
              (int-div (local n) (local divisor))
              (local divisor)
              (call (local append-factor)
                    (local acc)
                    (local first?)
                    (local divisor))
              false)
            (recur
              (local n)
              (int-add (local divisor) 1)
              (local acc)
              (local first?))))))))
```

Run it:

```bash
clj -M -m airj.cli run prime_factors.airj 294
```

Expected program output:

```text
2 3 7 7
```

## Host-Backed Modules

AIR-J can optionally emit a class that extends a Java host superclass:

```lisp
(module example/hosted
  (host java.util.ArrayList)
  (imports
    (java java.util.ArrayList))
  (export snapshot)

  (fn snapshot
    (params (self (Java java.util.ArrayList)))
    (returns Int)
    (effects (Foreign.Throw))
    (requires true)
    (ensures true)
    (java/call
      (local self)
      size
      (signature () Int))))
```

This is the compatibility hook used for Processing-style callback frameworks and similar JVM libraries.

## Packaging And Launch

The canonical development path is still:

```bash
clj -M -m airj.cli run path/to/program.airj ...
```

For built classes, AIR-J has two runtime shapes:
- self-contained generated classes that only need the output directory on the classpath
- generated classes that rely on AIR-J runtime helpers such as JSON, file, process, or host support

For the second case, launch with the built output directory plus the current AIR-J runtime classpath:

```bash
java -cp "target/classes:$(clj -Spath)" example.tool input.json output.json
```

That keeps the persisted AIR-J program canonical while making the host/runtime dependency boundary explicit.

## Why Canonical Built-Ins Instead of “Just Use Java”

Java already has collections, numbers, strings, and framework APIs. AIR-J still needs its own canonical surface because the project goal is not raw capability. It is canonicality for agents.

If AIR-J exposed Java’s full representation freedom as the default, agents would have to choose between many equivalent encodings:
- `ArrayList` vs `LinkedList`
- `HashMap` vs `LinkedHashMap`
- `Optional` vs sentinel values
- exceptions vs explicit result values

That would directly work against the mission in [notes.md](/notes.md). AIR-J therefore prefers a smaller, explicit, canonical machine vocabulary and only uses raw Java APIs when interop is intentionally requested.

## Read Next

- [notes.md](/notes.md): design intent and rationale
- [formal-v0-spec.md](/formal-v0-spec.md): normative persisted-language contract
- [AGENTS.md](/AGENTS.md): implementation workflow, checks, and toolchain pins
