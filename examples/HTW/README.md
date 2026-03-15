# Hunt The Wumpus

This example is a complete AIR-J console game with AIR-J unit tests. It is split into:

- pure game logic in `wumpus_logic.airj`
- the console game in `hunt_the_wumpus.airj`
- an AIR-J-native test program in `hunt_the_wumpus_tests.airj`

## Files

- `wumpus_logic.airj`: pure game logic helpers
- `hunt_the_wumpus.airj`: the playable console game
- `hunt_the_wumpus_tests.airj`: AIR-J unit tests using the canonical `tests` suite function plus an AIR-J runner `main`

## Build A Runnable Jar

From the repo root:

```bash
clj -M -m airj.cli build --project-dir examples/HTW --jar /tmp/hunt-the-wumpus.jar example/hunt_the_wumpus
```

## Play

```bash
java -jar /tmp/hunt-the-wumpus.jar
```

Commands:

- `move <room>`
- `shoot <room>`
- `quit`

You can only move or shoot into an adjacent room.

## Scripted Smoke Test

This reproduces a known winning path in the example's default layout:

```bash
printf 'move 2\nmove 10\nshoot 11\n' | java -jar /tmp/hunt-the-wumpus.jar
```

It should end with:

```text
Your arrow strikes true. You win!
```

## Run The AIR-J Unit Tests

Build the AIR-J test jar:

```bash
clj -M -m airj.cli build --project-dir examples/HTW --jar /tmp/hunt-the-wumpus-tests.jar example/hunt_the_wumpus_tests
```

Run it:

```bash
java -jar /tmp/hunt-the-wumpus-tests.jar
```

The runner is AIR-J code, not Clojure. It prints one line per test plus a summary and exits with:

- `0` when all tests pass
- `1` when any test fails or errors
