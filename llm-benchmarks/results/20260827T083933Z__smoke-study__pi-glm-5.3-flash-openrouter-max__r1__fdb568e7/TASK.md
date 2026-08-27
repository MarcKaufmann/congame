# Task: implement a small Conscript study

Implement the study in `study.rkt`.

The study must:

1. Show an introduction page with a Continue button.
2. Ask the participant for a non-empty display name.
3. Show a final page that includes the submitted name.
4. Export the study as `benchmark-study`.
5. Compile and upload successfully through `raco congame upload`.

Exercise the implementation against the supplied Congame server. You may add
`exercise.rkt` if useful. Keep all changes within the paths allowed by the task.


## Benchmark environment

- Conscript source: `../reference/conscript/`
- Congame documentation: `../reference/congame-doc/`
- Curated examples: `../reference/examples/` (when present)
- Congame server: `http://host.docker.internal:45471`
- Study ID: `benchmark-study`
- Active instance slug: `bench-20260827t083933z-smoke--fdb568e7`
- Participant URL: `http://host.docker.internal:45471/_anon-login/bench-20260827t083933z-smoke--fdb568e7`
- Congame CLI authentication is already configured for this server.
- Run `raco congame upload` directly. Do not run `raco congame login`, visit `_cli-login`, or attempt an interactive/browser login.

Useful commands:

```bash
raco make study.rkt
raco congame upload benchmark-study study.rkt
racket exercise.rkt  # when an exercise is part of the task
```
