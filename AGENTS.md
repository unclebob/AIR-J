# AIR-J Agent Instructions

## Implementation language

- Implement AIR-J in Clojure.
- Keep the compiler and tooling JVM-native from the start.

## Development process

- Use TDD.
- Write unit specs first.
- Implement only enough production code to satisfy the next failing spec.
- Refactor only after specs are green.

## Test framework

- Use Speclj for unit tests.
- Put production code under `src`.
- Put specs under `spec`.
- Run specs with:

```bash
clj -M:spec
```

## Spec structure check

- Before running Speclj specs, run speclj-structure-check.
- Use it to catch invalid Speclj nesting before test execution.
- Run it with:

```bash
clj -M:check-structure spec
```

- If structure check fails, fix the spec structure before running specs.

## Mutation testing

- Use `clj-mutate` to keep modules covered and overloaded with tests.
- `clj-mutate --scan` is the fast structural prepass.
- Use `clj -M:mutate src/.../file.clj --scan` first on changed files to see total mutation count, changed mutation count, and any mutation-count warning before running coverage/spec-backed mutation work.
- Run mutation testing against the source file you changed.
- `clj-mutate` now defaults to differential mutation once a file already has an embedded footer manifest.
- That means `clj -M:mutate src/.../file.clj --max-workers 3` is usually a changed-top-level-forms run on previously mutated files.
- Differential mutation is the preferred default workflow.
- Use `--since-last-run` when you want to be explicit about that differential behavior.
- Do not run full-file mutation with `--mutate-all` unless the user explicitly requests it.
- Run mutation testing with `--max-workers 3`.
- Run mutation module-by-module sequentially. Do not run mutation jobs for multiple modules concurrently.
- If an unchanged file with a manifest is split, do not copy the parent manifest into the daughters.
- Run tests first.
- If green, update daughter manifests.
- Then continue with CRAP and differential mutation, which should be a no-op for a semantics-preserving split.
- Run it with:

```bash
clj -M:mutate src/.../file.clj --max-workers 3
```

- If survivors remain, add or improve specs until the important survivors are killed.

## CRAP control

- Use `crap4clj` to measure change risk.
- Keep CRAP below `8` for changed functions/modules.
- Run it with:

```bash
clj -M:crap
```

- If a changed function exceeds the threshold, reduce complexity or add tests until it is below `8`.

## Coverage

- Maintain a working coverage path through the `:cov` alias.
- `crap4clj` and `clj-mutate` depend on current coverage information.
- Run coverage with:

```bash
clj -M:cov
```

## Dependency structure

- Use `dependency-checker` to keep dependencies organized.
- As the system grows, break the code into components.
- Update `dependency-checker.edn` when component boundaries change.
- Check dependencies with:

```bash
clj -M:check-dependencies
```

- Treat dependency violations and cycles as design problems to fix, not warnings to ignore.

## Completion criteria

Do not consider a change complete until all applicable checks pass:

1. `clj -M:check-structure spec`
2. `clj -M:spec`
3. `clj -M:cov`
4. `clj -M:crap`
5. `clj -M:mutate src/.../changed_file.clj --scan`
6. `clj -M:mutate src/.../changed_file.clj`
7. `clj -M:check-dependencies`

## Tooling pinned in `deps.edn`

Current pinned SHAs:

- `clj-mutate`: `b63e1e31d8e35c5f567852c2b15126c1ae1ffeec`
- `dependency-checker`: `e8f35792434adb35fb6b99f5c3f9ab2ab24a0279`
- `speclj-structure-check`: `7cc804c4e99b3482ca1d8a26b735ed41751a03ab`
- `crap4clj`: `9f12da1b09bc1177f8f058108cf22a73bc9a97a0`

Repo name corrections:

- use `crap4clj`, not `crap4j`
- use `speclj-structure-check`, not `speclj-structure-checker`
