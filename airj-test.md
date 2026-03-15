## AIR-J Test Framework Plan

Build an AI-first test system in two layers:

1. `airj/test`
   A small AIR-J library for declaring tests and assertions as canonical data.
   No macros, no alternate syntax.
   Tests should just be AIR-J functions returning structured `Result`/`Diagnostic`-style outcomes.

2. `airj.test-runner`
   A runner that loads test modules, executes exported test functions, and emits canonical result data.
   CLI can render human-readable summaries, but the primary artifact should be machine-readable.

### Goals

- One canonical representation for test definitions and results.
- Deterministic execution.
- Machine-readable failures.
- Easy for AIs to generate, transform, and repair.
- Strong pressure on AIR-J contracts, diagnostics, modules, jars, and project builds.

### Core Model

- A test is a named function with no params.
- It returns either:
  - pass result
  - fail result with `Diagnostic`
- Assertions are ordinary AIR-J functions, not syntax sugar.
- Runner result is a sequence of test case outcomes plus summary counts.

### Canonical Data Types

In `airj/core` or `airj/test`:

- `TestOutcome`
- `TestPass`
- `TestFail`
- `TestError`
- `TestSummary`
- `AssertionFailure`

Likely shapes:

- `TestPass {name String}`
- `TestFail {name String diagnostic Diagnostic}`
- `TestError {name String diagnostic Diagnostic}`
- `TestSummary {passed Int failed Int errored Int outcomes (Seq TestOutcome)}`

### Assertion API

Minimal first batch:

- `assert-true`
- `assert-false`
- `assert-eq`
- `assert-not-eq`
- `fail`

These should return canonical failure data, not rely on thrown exceptions for normal assertion failure.

### Execution Model

- A test module exports test functions explicitly.
- Runner accepts:
  - single source file
  - project root module
  - jar later
- Runner executes tests sequentially first.
- No parallelism until semantics and reporting are stable.

### Failure Policy

- Assertion mismatch: `TestFail`
- Unexpected exception / contract violation / runtime error: `TestError`
- All failures should include structured `Diagnostic`

### CLI

Add a dedicated command, likely:

- `clj -M -m airj.cli test ...`

Support:

- `test source.airj`
- `test --project-dir dir root/module`
- output modes:
  - default human summary
  - `--json` canonical result output

### Phased Implementation

1. Define canonical test result data types.
2. Add assertion functions in `airj/test`.
3. Add a simple runner for explicit exported tests.
4. Add CLI command and JSON result output.
5. Add project-level test discovery policy.
6. Add runnable jar support for test runners if still needed.

### What To Avoid

- Macros
- many assertion synonyms
- human-oriented DSLs
- implicit discovery magic
- stack-trace-only failure reporting

### Best First Proof

Write the framework in AIR-J itself with:

- one tiny self-test module
- one failing test
- one erroring test
- runner emits a canonical summary

That would prove AIR-J can support a real development loop before attempting larger self-hosting work.
