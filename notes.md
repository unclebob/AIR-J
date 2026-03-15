# AIR-J design

AIR-J is an AI-first, JVM-targeting programming language whose primary artifact is a canonical, typed, effect-tracked intermediate representation. It is intended to be written, transformed, checked, and lowered by software agents, not optimized for direct human authorship.

The point of the design is not "make programming nicer." The point is:
- reduce ambiguity
- reduce representation variance
- reduce re-analysis cost
- make transformation legality mechanically checkable
- preserve direct access to the JVM ecosystem

Principle: one meaning, one representation.

## Canonical Host Boundary

AIR-J should expose the minimum host-environment surface an agent needs through canonical modules and value carriers, not through arbitrary Java APIs by default.

That boundary now includes:
- `Bytes` as the canonical raw-data type
- `airj/env` for explicit environment reads
- `airj/process` for explicit subprocess execution with canonical `ProcessResult`
- `airj/file` and `airj/json` returning `Result` plus `Diagnostic` where failure transport should stay machine-readable

The intent is:
- one canonical representation for raw bytes
- one canonical representation for subprocess results
- one canonical failure carrier at the standard-module boundary
- no default drift into multiple Java process or environment APIs for the same semantic job

## Design target

AIR-J should behave like a writable canonical IR with a stable surface syntax.

That implies:
- canonical tree form
- explicit names and imports
- explicit types in persisted form
- explicit effects in function signatures
- explicit control-flow forms
- explicit mutation
- explicit foreign interop
- direct lowering to JVM bytecode

It does not try to be:
- pleasant for casual human editing
- highly syntactically flexible
- dependent on inference for correctness
- dependent on source-to-source translation through Java

## Non-goals

AIR-J is not intended to:
- compete with Kotlin, Java, or Clojure as a general human language
- preserve multiple equivalent surface syntaxes
- hide effects behind conventions or annotations
- model every JVM feature as first-class source syntax
- use macros as a core language feature

## Source model

The persisted representation is an s-expression tree with a small fixed vocabulary. Whitespace, comments, and field ordering must not change meaning.

Every source file contains exactly one `module` form.

Modules may optionally declare one host JVM superclass with a canonical host clause:

```lisp
(module app/sketch
  (host processing.core.PApplet)
  ...)
```

The meaning of `host` is narrow and explicit:
- the emitted module class extends that Java superclass
- exported AIR-J functions whose first parameter can accept the host instance become public instance bridge methods
- those bridge methods delegate into the ordinary static AIR-J implementation with `this` injected as the first argument

This keeps AIR-J internally static and canonical while making it compatible with callback-oriented JVM frameworks such as Processing.

Example:

```lisp
(module app/http
  (imports
    (java java.time.Instant)
    (airj core/string))

  (export
    Request
    Response
    route)

  (data Request
    (field path String)
    (field method Method))

  (enum Method Get Post)

  (data Response
    (field status Int)
    (field body String))

  (fn route
    (params (req Request))
    (returns Response)
    (effects ())
    (match (record-get req path)
      (case "/" (construct Response 200 "ok"))
      (case _   (construct Response 404 "not-found")))))
```

## Canonicality rules

Canonicality is a core semantic property, not a formatter preference.

Rules:
- each construct has exactly one valid tree shape
- optional sugar must be removed before persistence
- host-backed modules use exactly one host clause shape: `(host fully.qualified.JavaClass)`
- imports are explicit and sorted
- exported names are explicit and sorted
- records, enums, and function members appear in fixed field order
- local binders are unique within a scope
- equivalent type expressions normalize to the same form
- equivalent effect sets normalize to the same sorted set
- constants use normalized literal representations

Examples:
- there is one field-access form: `record-get`
- there is one constructor form: `construct`
- there is one static foreign call form: `java/static-call`
- there is no alternate spelling for `if`, `match`, `loop`, or `return`

## Module system

Modules are globally named by slash-delimited symbols such as `app/http`.

Rules:
- one module per file
- only explicitly imported names are visible
- no wildcard imports
- exports are explicit
- cyclic module references are allowed only if the compiler can validate them without unresolved types; otherwise rejected

Import forms:

```lisp
(imports
  (airj core/string join split)
  (airj billing/money Money add)
  (java java.time.Instant)
  (java java.util.ArrayList))
```

`airj` imports may name whole modules or specific exported symbols depending on compiler phase, but the persisted canonical form should resolve to explicit symbol imports.

## Names and symbols

Kinds of names:
- module names: `billing/invoice`
- local names: `invoice`
- type names: `Invoice`
- enum variants: `Pending`
- qualified foreign class names: `java.time.Instant`

Rules:
- names are case-sensitive
- locals may not shadow type names in the same scope
- imported names may be renamed only in a dedicated normalized alias form
- generated internal names must be marked as compiler-owned

## Type system

Persisted AIR-J is fully typed. An authoring layer may use inference, but canonical AIR-J does not depend on it.

Primitive types:
- `Bool`
- `Byte`
- `Short`
- `Int`
- `Long`
- `Float`
- `Double`
- `Char`
- `String`
- `Unit`

Constructed types:
- `(Tuple T1 T2 ...)`
- `(Array T)`
- `(List T)`
- `(Map K V)`
- `(Nullable T)`
- `(Result Ok Err)`
- `(Fn (T1 T2 ...) R (effects ...))`
- named record, enum, and union types
- foreign reference types via `(Java java.time.Instant)`

Parametric types are declared explicitly:

```lisp
(data Box
  (type-params T)
  (field value T))
```

Type rules:
- nullability is always explicit through `Nullable`
- function effects are part of function type identity
- primitive numeric widening is explicit in canonical form
- recursive types must be nominal, not anonymous
- foreign reference types are non-null by default unless wrapped in `Nullable`

## Data model

Records are nominal product types.

```lisp
(data User
  (field id UserId)
  (field name String)
  (field email (Nullable String)))
```

Enums are closed nominal sum types with nullary variants.

```lisp
(enum Method Get Post Put Delete)
```

Tagged unions support payload-bearing variants.

```lisp
(union PaymentResult
  (variant Approved (field authId String))
  (variant Declined (field reason String))
  (variant Timeout))
```

Canonical operations:
- `(construct Type arg1 arg2 ...)`
- `(variant Type Variant arg1 arg2 ...)`
- `(record-get value field-name)`
- `(match value (case Pattern expr) ...)`

## Function model

Functions are top-level module members. Nested anonymous functions may exist as expressions but must lower to canonical `lambda` form.

```lisp
(fn total
  (params (invoice Invoice))
  (returns Money)
  (effects ())
  (call add
    (record-get invoice subtotal)
    (record-get invoice tax)))
```

Function sections are mandatory and ordered:
- name
- params
- returns
- effects
- body expression

Rules:
- parameter types are mandatory
- return type is mandatory
- effects declaration is mandatory, even when empty
- the function body is exactly one expression tree
- implicit return is allowed only because the body is a single expression

Lambda form:

```lisp
(lambda
  (params (x Int))
  (returns Int)
  (effects ())
  (call add x 1))
```

## Effect system

Effects are explicit capabilities declared on function and lambda signatures.

Example:

```lisp
(fn load-config
  (params (path Path))
  (returns Config)
  (effects (Files.Read Clock.Read))
  ...)
```

Effect design rules:
- effects are an unordered set in semantics and a sorted list in syntax
- calling a function requires the caller's effect set to include the callee's effect set unless handled by a dedicated construct
- pure functions declare `(effects ())`
- mutation is represented as an effect
- foreign calls contribute explicit foreign effects
- exception-producing operations must declare either typed error results, effectful raise behavior, or both depending on the construct used

Initial standard effect namespaces:
- `State.Read`
- `State.Write`
- `Files.Read`
- `Files.Write`
- `Network`
- `Clock.Read`
- `Random`
- `Process`
- `Foreign.Throw`
- `Concurrency`

The exact initial effect catalog can remain small. The important part is that effects are namespaced, explicit, and checked.

## Expressions

The expression language should remain small and closed.

Core forms:
- `(literal value)`
- `(local name)`
- `(let ((name expr) ...) body)`
- `(if test then else)`
- `(match expr (case pattern body) ...)`
- `(call fn arg1 arg2 ...)`
- `(lambda ...)`
- `(construct Type arg1 arg2 ...)`
- `(variant Type Variant arg1 arg2 ...)`
- `(record-get expr field)`
- `(loop ((name init) ...) body)`
- `(recur expr1 expr2 ...)`
- `(try body (catch Type binder handler) ... (finally cleanup))`
- `(raise expr)`
- `(seq expr1 expr2 ... exprN)`
- `(var name Type init)`
- `(set name expr)`

Rules:
- there is no statement/expression split
- `seq` exists only where ordered evaluation is required
- `var` and `set` are explicit mutable operations
- `loop` and `recur` are the only canonical tail-loop forms
- short-circuit boolean operations lower to `if`

## Patterns

Pattern matching must be closed over a small pattern language.

Allowed patterns:
- `_`
- literals
- local binder
- enum variant names
- union variant destructuring
- record destructuring by explicit field set

Example:

```lisp
(match payment-result
  (case (Approved authId) (construct Receipt authId))
  (case (Declined reason) (raise (construct PaymentError reason)))
  (case Timeout (retry-payment)))
```

Canonical pattern forms should carry enough type information for exhaustiveness checking after normalization.

## Mutation

Mutation is allowed but explicit and easy to locate.

```lisp
(fn next-counter
  (params ())
  (returns Int)
  (effects (State.Write))
  (let ((counter (var counter Int 0)))
    (seq
      (set counter (call add (local counter) 1))
      (local counter))))
```

Rules:
- mutable bindings are introduced only with `var`
- rebinding immutable locals is not allowed
- mutation requires declared write effects
- shared mutable state should use explicit runtime abstractions, not implicit globals

## Errors and failure

AIR-J should support both typed errors and exceptional control flow, but both must be explicit.

Preferred recoverable failure model:
- `(Result Ok Err)`

Exceptional model:
- `(raise expr)`
- `(try body (catch Type name handler) ...)`

Rules:
- recoverable domain failures should prefer `Result`
- foreign exception behavior must not be hidden
- low-level fallible primitives may still expose `Foreign.Throw`
- a `try` with catches handles `Foreign.Throw` at the AIR-J effect boundary
- standard modules should wrap common recoverable failures into canonical `Result` values with `Diagnostic` payloads
- functions that may still raise must declare the relevant effects, such as `Foreign.Throw`

## Metadata

Metadata is structured, optional, and non-semantic unless a checker explicitly consumes it.

Example:

```lisp
(meta
  (intent "Parse config and validate required keys")
  (depends-on (Files.Read SchemaValidator))
  (invariants
    ("config.version present")
    ("timeout-ms >= 0"))
  (complexity O_n)
  (change-risk medium))
```

Metadata may attach to:
- modules
- types
- functions
- foreign bindings

Metadata should never alter runtime semantics in the base language.

## Java interop

Interop is direct, explicit, and mechanical.

Canonical foreign forms:
- `(java/type java.time.Instant)`
- `(java/new java.util.ArrayList (type-args String) arg1 arg2 ...)`
- `(java/call target method-id (signature (String) Bool) arg1 ...)`
- `(java/static-call java.time.Instant now (signature () java.time.Instant))`
- `(java/get-field target field-id FieldType)`
- `(java/set-field target field-id FieldType expr)`

Interop rules:
- all foreign interaction is syntactically marked with `java/...`
- overload resolution is explicit in canonical form via signatures or resolved member ids
- Java `null` becomes `Nullable[T]`
- checked and unchecked throws are surfaced to the effect system
- reflection is outside the core language and should live behind explicit library boundaries
- generic erasure on the JVM does not remove AIR-J source-level type requirements

## Runtime and lowering model

Compilation pipeline:

`source AIR-J -> parse -> normalize -> typed AIR-J -> validated AIR-J -> JVM IR -> classfiles`

Not:

`AIR-J -> Java source -> javac`

Lowering guidelines:
- modules lower to one or more JVM classes
- pure top-level functions lower to static methods where possible
- closures lower to generated classes or `invokedynamic`-backed lambdas
- records lower to final classes
- enums lower to JVM enums or optimized tagged constants
- tagged unions lower to sealed-style class families, tagged records, or another uniform runtime encoding
- metadata may be dropped from runtime artifacts unless requested for tooling

The language spec should define AIR-J semantics independently of the chosen JVM encoding, but the compiler should maintain a stable lowering contract.

## Verification

Verification is a first-class design requirement.

Mandatory checks:
- parse validation
- normalization validation
- name resolution
- type checking
- effect checking
- exhaustiveness checking
- arity checking
- foreign member resolution

Optional advanced checks:
- contract validation
- refinement or predicate checks
- SMT-assisted invariant checking
- rewrite equivalence checking

Verification outputs should be machine-oriented and structured, not human-prose only.

## Interface summaries

Each module should expose a compact summary artifact before full body loading.

Example:

```lisp
(interface billing/invoice
  (exports
    (fn create ((InvoiceDraft)) -> (Result Invoice InvoiceError) effects (Clock.Read IdGen))
    (fn total ((Invoice)) -> Money effects ())
    (data Invoice
      (field id InvoiceId)
      (field lines (List InvoiceLine)))
    (union InvoiceError
      (variant InvalidDraft)
      (variant MissingCurrency))))
```

The interface exists to reduce context cost for agents and to permit summary-first reasoning.

## Version 0 language boundary

The first useful AIR-J implementation should intentionally be small.

Version 0 should include:
- modules
- explicit imports and exports
- nominal records
- enums
- tagged unions
- first-order functions plus lambdas
- explicit types
- explicit effects
- `let`, `if`, `match`, `call`, `loop`, `recur`
- `Result`
- explicit Java interop
- direct JVM lowering

Version 0 should exclude:
- macros
- trait/typeclass systems
- dependent types
- user-defined effect handlers
- implicit inference in persisted artifacts
- reflection-heavy interop
- concurrency abstractions beyond basic foreign access

## Open design decisions

These decisions still need to be fixed:
- whether local variables should use `(local x)` in canonical form everywhere or allow bare symbols after parsing
- whether pattern syntax should be fully symbolic or always tagged
- whether unions lower to dedicated classes or a shared tagged-object runtime in version 0
- whether effects are purely symbolic sets or may carry typed parameters later
- whether function bodies should permit top-level metadata nodes for proofs/contracts

## Primitive operator policy

AIR-J should include primitive mathematical and logical operators when they reduce ambiguity, context cost, and foreign-interop churn for software agents.

These operators should not be added as human-oriented surface sugar. They should exist as canonical built-ins or ordinary library functions with one persisted representation only.

That means:
- operators are justified by machine reasoning value, not human ergonomics
- operators should be explicit, pure, and fully typed
- persisted AIR-J should keep one canonical representation for each primitive operation
- direct Java interop should remain available, but common agent work should not require it for basic arithmetic or boolean logic

Examples of the intended category:
- integer arithmetic
- integer comparison
- boolean logic
- string conversion and basic string operations
- explicit stdin/stdout boundary operations
- other primitive operations only when they materially reduce agent planning and transformation cost

Canonical persisted AIR-J should not add alternate branching spellings such as `when` or `unless`.

If an authoring layer wants them, it may lower them to canonical `if` before persisted AIR-J is produced.

Current primitive failure policy:
- fallible primitives such as `string->int` should remain explicit and require `Foreign.Throw`
- `io/read-line` should require `Stdin.Read` and `Foreign.Throw`
- `io/print` and `io/println` should require `Stdout.Write`
- pure string primitives such as concatenation and length should remain effect-free

Current standard module policy:
- `airj/core` should hold canonical data carriers such as `Option`, `Result`, and `Diagnostic`
- `airj/core` may provide wrapper functions such as `parse-int` that convert common recoverable failures into `Result`
- `airj/file` should expose the minimal canonical filesystem boundary first: existence, read, and write
- standard library sequence-producing operations should converge on `(Seq T)` rather than proliferating ad hoc sequence types
- canonical keyed structures should converge on `(Map String T)` rather than exposing Java collection choices by default
- line-oriented filesystem helpers are acceptable when they return canonical AIR-J data such as `(Seq String)` and `Result`

## Recommended next step

The next document should be a compact formal spec for version 0:
- grammar
- AST schema
- normalization rules
- typing rules
- effect checking rules
- JVM lowering contract

That is the point where AIR-J becomes implementable instead of merely well-motivated.

The formal v0 spec now lives in [formal-v0-spec.md](/formal-v0-spec.md).

## Current compiler usage

The current bootstrap compiler exposes these commands through `airj.cli`:
- `parse`
- `normalize`
- `check`
- `lower`
- `build`
- `run`

Current entrypoint contract:
- exported AIR-J `main` may be zero-arg
- exported AIR-J `main` may also declare `(params (args (Java "[Ljava.lang.String;")))`
- `run` invokes AIR-J `main` directly and forwards CLI arguments to that `String[]` form
- `run` returns the AIR-J `main` result to the host Clojure process
- `run` renders non-`Unit` AIR-J results as CLI output text
- `run` renders `Unit` as empty CLI output
- `build` emits a JVM `main(String[])` wrapper when the module satisfies that contract
- built JVM execution uses AIR-J `main` `Int` returns as process exit codes
- built JVM execution ignores `Unit` results and returns normally

Examples:

```bash
clj -M -m airj.cli run --stdin foo bar <<'EOF'
(module example/app
  (imports)
  (export main)
  (fn main
    (params (args (Java "[Ljava.lang.String;")))
    (returns Int)
    (effects ())
    (requires true)
    (ensures true)
    0))
EOF
```

```bash
clj -M -m airj.cli build --stdin target/classes <<'EOF'
(module example/app
  (imports)
  (export main)
  (fn main
    (params)
    (returns Unit)
    (effects ())
    (requires true)
    (ensures true)
    0))
EOF

java -cp target/classes example.app
```
## Standard Module Policy

- `airj/core` defines canonical language-level carriers and interchange values.
- `airj/file` defines canonical filesystem boundaries.
- `airj/json` defines the canonical JSON interchange boundary.
- Recoverable boundary failures should prefer `Result ... Diagnostic`.
- Raw boundary functions may still expose explicit host effects and `Foreign.Throw`, but standard `*-result` wrappers should carry recoverable failures as canonical AIR-J data.
- `Diagnostic.message` should remain stable and machine-oriented, while `Diagnostic.detail` should carry the relevant path, input, or boundary context.
- Standard modules should remain small and canonical: `airj/core`, `airj/bytes`, `airj/env`, `airj/file`, `airj/json`, and `airj/process`.
- Host-library details belong behind these module boundaries, not in ordinary AIR-J programs.
