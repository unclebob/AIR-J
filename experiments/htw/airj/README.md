# AIR-J Hunt the Wumpus

This experiment implements a classic Hunt the Wumpus command-line game in AIR-J.

Rules modeled here:

- 20-room dodecahedron cave, each room connected to 3 others
- hazards: 1 Wumpus, 2 pits, 2 super-bat rooms
- warnings for adjacent Wumpus, pits, and bats
- `move <room>` moves only through adjacent tunnels
- `shoot <r1> <r2> ... <r5>` fires a crooked arrow through up to 5 rooms
- invalid arrow turns pick a random adjacent tunnel
- after a miss, the Wumpus may move

Build:

```bash
clj -M -m airj.cli build --project-dir experiments/htw/airj --jar /tmp/airj-htw.jar experiment/hunt_the_wumpus
```

Run:

```bash
java -jar /tmp/airj-htw.jar
```
