# Hunt the Wumpus Experiments

Two experiments were run against the same AIR-J, Java, and Clojure Wumpus implementations.

## Experiment 1

Change set:

- add `help`
- add `status`
- add optional deterministic startup seed

Result:

| Language | Main LOC | Extra scaffolding | Correction cycles | Outcome |
| --- | ---: | --- | ---: | --- |
| AIR-J | 581 | 16-line Java helper | 5 | completed |
| Java | 260 | none | 0 | completed |
| Clojure | 176 | none | 0 | completed |

This was boundary-heavy. AIR-J did badly because seeded randomness exposed weak Java interop ergonomics.

## Experiment 2

Change set:

- replace the fixed 20-room cave with a computed `10x10` torus
- room ids become `0..99`
- 4 wraparound neighbors per cell
- fixed hazards: 10 pits, 10 bats, 1 Wumpus
- keep crooked-arrow behavior the same

Verification:

```bash
javac experiments/htw/java/HuntTheWumpus.java
printf 'status\nmove 90\nquit\n' | java -cp experiments/htw/java HuntTheWumpus 7

printf 'status\nmove 90\nquit\n' | clj experiments/htw/clojure/hunt_the_wumpus.clj 7

javac src/experiment/htw/airj/SeededRandomFactory.java
clj -M -m airj.cli build --project-dir experiments/htw/airj --jar /tmp/airj-htw.jar experiment/hunt_the_wumpus
printf 'status\nmove 90\nquit\n' | java -jar /tmp/airj-htw.jar 7
```

Observed result:

| Language | Main LOC | Extra scaffolding | Correction cycles | Outcome |
| --- | ---: | --- | ---: | --- |
| AIR-J | 591 | same 16-line Java helper from experiment 1 | 1 | completed |
| Java | 252 | none | 1 | completed |
| Clojure | 167 | none | 1 | completed |

## Interpretation

The torus experiment is much more favorable to AIR-J than the seed/help/status experiment.

- The topology rewrite stayed mostly inside pure program structure.
- AIR-J no longer needed multiple rounds of parser-shape or interop debugging.
- The main AIR-J tax was explicit effect propagation after `neighbors` started parsing room ids.

Conclusion:

- AIR-J did better on the torus experiment.
- It still was not the most efficient overall.
- But this experiment supports the claim that AIR-J looks stronger on systematic structural rewrites than on boundary-heavy feature work.
- Ranking for experiment 2: `Clojure > Java > AIR-J`, with Java and AIR-J much closer than in experiment 1.

## Experiment 3

Change set:

- replace the single torus with two `10x10` tori: `O0..O99` and `I0..I99`
- each cell has 4 same-torus neighbors plus 1 bridge to the other torus
- the inner torus rotates by `+10` after each `move` or `shoot`
- hazards stay fixed to their own torus
- arrows may cross the bridge

Verification:

```bash
javac experiments/htw/java/HuntTheWumpus.java
printf 'status\nmove I0\nstatus\nquit\n' | java -cp experiments/htw/java HuntTheWumpus 7

printf 'status\nmove I0\nstatus\nquit\n' | clj experiments/htw/clojure/hunt_the_wumpus.clj 7

javac src/experiment/htw/airj/SeededRandomFactory.java src/experiment/htw/airj/RotatingDoubleTorus.java
clj -M -m airj.cli build --project-dir experiments/htw/airj --jar /tmp/airj-htw.jar experiment/hunt_the_wumpus
printf 'status\nmove I0\nstatus\nquit\n' | java -jar /tmp/airj-htw.jar 7
```

Observed result:

| Language | Main LOC | Extra scaffolding | Correction cycles | Outcome |
| --- | ---: | --- | ---: | --- |
| AIR-J | 601 | 129 lines of Java helpers under `src/experiment/htw/airj` | 6 | completed |
| Java | 291 | none | 1 | completed |
| Clojure | 197 | none | 2 | completed |

Interpretation:

- This experiment is structurally richer than experiment 2 because topology now depends on turn state.
- Clojure still handled the model change with the least friction.
- Java stayed straightforward.
- AIR-J completed the model change, but only after extra helper scaffolding and several rounds of structural/effect cleanup.

Ranking for experiment 3: `Clojure > Java > AIR-J`.

## Conclusion

AIR-J's mission appears coherent, but narrower than it first appears.

It does not look like a strong direct authoring language for humans, or even for
agents working directly in canonical form. It makes much more sense as a
canonical persisted target representation for agents: explicit, typed,
effect-aware, mechanically checkable, and JVM-native.

Across these experiments, AIR-J improved relative to Java and Clojure when the
change was a structural rewrite rather than a boundary-heavy feature. But the
current implementation still falls back to host helpers too often, which means
it pays the cost of a canonical IR without consistently delivering the full
transformation benefit its mission implies.

The most plausible long-term shape is:

- agents and humans work in a more ergonomic front-end
- that front-end lowers into canonical AIR-J
- AIR-J serves as the stable JVM-native machine substrate for checking,
  transformation, and lowering

Put plainly: AIR-J currently makes the most sense as a canonical JVM IR for
agents, not as the language they should prefer to author in directly.
