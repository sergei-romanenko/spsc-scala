# Tasks for SPSC in Scala

First, build SPSC executable by running

```bash
sbt compile
```

The tasks for SPSC are in files with the extension `.task`.

For each task `name` in the directory `tasks`, run

```bash
sbt "run tasks/name"
```

Then SPSC will read the file `name.task` and produce the files
`name.tree` (containig the process tree) and `name.res`
(containing the residual task).
