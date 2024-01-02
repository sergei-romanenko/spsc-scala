# SPSC: a Simple Supercompiler in Scala

## Theory

* Ilya Klyuchnikov and Sergei Romanenko. [SPSC: a Simple Supercompiler in Scala](https://storage.googleapis.com/google-code-archive-downloads/v2/code.google.com/spsc/Klyuchnikov__Romanenko__SPSC_a_Simple_Supercompiler_in_Scala.pdf).
In: _International Workshop on Program Understanding_, Altai Mountains, Russia. 2009

## Practice

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
