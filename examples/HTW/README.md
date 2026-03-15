# Hunt The Wumpus

This example is a complete AIR-J console game. It uses canonical AIR-J input and output, builds as a runnable jar, and can be played directly from the terminal.

## Files

- `hunt_the_wumpus.airj`: the AIR-J source for the game

## Build A Runnable Jar

From the repo root:

```bash
clj -M -m airj.cli build --jar /tmp/hunt-the-wumpus.jar examples/HTW/hunt_the_wumpus.airj
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
