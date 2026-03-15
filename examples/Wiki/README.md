# AIR-J Wiki

This example ports the core non-HTTP wiki behavior from the Java `wiki`
example under:

- `/Users/unclebob/Documents/Pearson Courses/AgenticSoftwareDev/examples/wiki`

It intentionally focuses on the pure acceptance slices that AIR-J can model
cleanly today:

- page CRUD and validation
- alphabetical page discovery
- text search
- revision history
- wiki links
- markdown-like body rendering

It does not attempt to reproduce the Java example's Gherkin pipeline or HTTP
server in AIR-J.

## Files

- `wiki.airj`: pure in-memory wiki engine
- `wiki_app.airj`: runnable console wiki shell
- `wiki_test_suite.airj`: reusable AIR-J wiki acceptance-oriented tests
- `wiki_tests_root.airj`: canonical AIR-J text test root
- `wiki_tests_json.airj`: canonical AIR-J JSON test root

## Build The Console App Jar

```bash
clj -M -m airj.cli build --project-dir examples/Wiki --jar /tmp/wiki-app.jar example/wiki_app
```

## Run The Console App

```bash
java -jar /tmp/wiki-app.jar
```

Supported commands:

- `help`
- `list`
- `view <title>`
- `render <title>`
- `search <query>`
- `history <title>`
- `create <title>|<body>`
- `update <title>|<body>`
- `delete <title>`
- `quit`

Example session:

```bash
printf 'create Home|# Welcome\nrender Home\nlist\nquit\n' | java -jar /tmp/wiki-app.jar
```

## Build The Text Test Jar

```bash
clj -M -m airj.cli build --project-dir examples/Wiki --jar /tmp/wiki-tests.jar example/wiki_tests_root
```

## Run The Text Tests

```bash
java -jar /tmp/wiki-tests.jar
```

## Build The JSON Test Jar

```bash
clj -M -m airj.cli build --project-dir examples/Wiki --jar /tmp/wiki-tests-json.jar example/wiki_tests_json
```

## Run The JSON Tests

```bash
java -jar /tmp/wiki-tests-json.jar
```

The text and JSON roots both execute the same AIR-J suite. The JSON artifact
uses the canonical AIR-J test result shape:

- `module`
- `passed`
- `failed`
- `errored`
- `outcomes`

## Acceptance Scope Mapped From The Java Wiki

The AIR-J suite covers the non-HTTP feature slices from the Java example:

- `page_crud.feature`
- `discovery.feature`
- `revision_history.feature`
- `wiki_links.feature`
- `wiki_markup.feature`

The intent is not a browser clone. The intent is a faithful AIR-J domain
example that proves the same core behaviors as pure AIR-J logic and AIR-J
tests.
