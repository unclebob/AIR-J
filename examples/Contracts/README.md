# AIR-J Contracts

This example pressures AIR-J's design-by-contract model directly.

It demonstrates:

- data invariants
- preconditions and postconditions on pure functions
- AIR-J unit tests with an explicit text test root

## Files

- `gauge.airj`: bounded gauge logic with invariants and contracts
- `gauge_test_suite.airj`: reusable AIR-J test suite
- `gauge_tests_root.airj`: canonical AIR-J text test root

## Build The Test Jar

```bash
clj -M -m airj.cli build --project-dir examples/Contracts --jar /tmp/gauge-tests.jar example/gauge_tests_root
```

## Run The Tests

```bash
java -jar /tmp/gauge-tests.jar
```

The tests focus on valid state transitions and contract-preserving behavior:

- opening the gauge at zero
- filling and draining within bounds
- clamping through explicit helpers instead of invalid construction
- formatting a stable label from valid state
