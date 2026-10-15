# Congame LLM Benchmarks

This package runs blinded Conscript implementation tasks against ordinary
command-line agent harnesses.  The benchmark coordinator is implemented in
Racket and is exposed as:

```sh
raco congame-llm-bench
```

The coordinator deliberately has no agent protocol. A harness is a shell
script that runs in a prepared working directory and exits when it is
finished. `TASK.md` is the portable entry point, but harnesses may use every
native facility they support, including skills, context files, extensions,
prompt templates, and custom tools.

## Installation

Install the package as a link from the root of the Congame checkout:

```sh
raco pkg install -D --auto --skip-installed llm-benchmarks/
```

The regular Congame packages, including `congame-cli`, `conscript`, and
`congame-web`, must already be installed from this checkout.

## Local prerequisites

The runner manages its test server and PostgreSQL databases directly on the
host. It expects:

- a PostgreSQL server on `127.0.0.1:5432`;
- an application role named `congame`, with password `congame`;
- a local PostgreSQL administrator capable of creating databases;
- the `createdb`, `dropdb`, and `psql` programs on `PATH`;
- a running Docker engine;
- a working local Congame installation; and
- any model server required by the chosen harness.

The PostgreSQL values can be overridden with `PGHOST`, `PGPORT`, `PGUSER`,
`PGPASSWORD`, `PG_ADMIN_USER`, and `PG_ADMIN_PASSWORD`.

Each phase gets a database whose name starts with
`congame_llm_bench_`. The runner refuses to drop names outside that prefix.
It also allocates two consecutive host ports because Congame runs its debugger
listener on the port immediately after its HTTP port.

Build the Pi harness images from the repository root. They extend the regular
local Congame image with `congame-cli`, Git, and a pinned Pi version. The local
profile retains 0.84.2, while the OpenRouter profiles use 0.84.3:

```sh
docker compose build congame
docker build \
  --file llm-benchmarks/harnesses/pi-qwen3.8-27b-mlx-medium/Dockerfile \
  --tag congame-llm-bench-pi:0.84.2 \
  .
docker build \
  --build-arg PI_VERSION=0.84.3 \
  --file llm-benchmarks/harnesses/pi-qwen3.8-27b-mlx-medium/Dockerfile \
  --tag congame-llm-bench-pi:0.84.3 \
  .
```

## Running benchmarks

List checked-in configurations:

```sh
raco congame-llm-bench list
```

Run the smoke suite:

```sh
raco congame-llm-bench run smoke
```

Run one task several times:

```sh
raco congame-llm-bench run-task \
  --harness pi-qwen3.8-27b-mlx-medium \
  --repetitions 3 \
  smoke-study
```

Run Kimi K3 through OpenRouter by supplying the credential from the host
environment:

```sh
OPENROUTER_API_KEY=... raco congame-llm-bench run-task \
  --harness pi-kimi-k3-openrouter-max \
  smoke-study

OPENROUTER_API_KEY=... raco congame-llm-bench run-task \
  --harness pi-glm-5.3-flash-openrouter-max \
  --stream-view \
  smoke-study
```

The key is forwarded directly to the container and is not copied into the
harness configuration or result manifest.

`--stream-view` replaces Pi's raw terminal JSONL with a compact live view of
thinking blocks, tool calls, execution status, and per-turn token and cost
usage. The complete JSONL is still written to the result's `stdout.log`.

Render any saved Pi log with the same view:

```sh
llm-benchmarks/scripts/view-pi-stream.rkt \
  llm-benchmarks/results/<run-id>/stdout.log
```

The viewer selects ANSI colors when writing to a terminal. Use `--color` or
`--no-color` to override detection. Add `--tail` (or `-f`) to keep following a
log while its benchmark is still running:

```sh
llm-benchmarks/scripts/view-pi-stream.rkt --tail \
  llm-benchmarks/results/<run-id>/stdout.log
```

The GLM profile caps a single model response at 16,384 tokens. It also stops a
response after 128 consecutive whitespace-only thinking deltas, which prevents
a malformed provider stream from consuming the entire run timeout. The stream
viewer reports the same condition at sparse intervals while following a log.
After a semantic stall, the harness resumes the same Pi session up to two times;
ordinary transient provider failures use Pi's built-in retry policy. A third
semantic stall fails the run with exit code 86.

Runs commit their result directory to Git by default. During harness
development, use `--no-commit`.

## Run lifecycle

For each run, the coordinator:

1. creates a unique PostgreSQL database;
2. starts `congame-web/dynamic.rkt --mode local` on an available port;
3. waits for the server health check;
4. grants the disposable local admin its CLI role and writes an isolated
   `raco congame` preference file;
5. uploads the task's valid starter study and creates an active instance;
6. builds a squashed task workspace and copies documentation and source
   references beside it;
7. executes the harness script in its configured Docker image under a hard
   wall-clock deadline;
8. captures a binary-safe Git patch, a human-friendly review index with final
   versions of changed files, and any new files;
9. tears down the agent server and database;
10. starts a second fresh server and database, replays the patch, uploads the
    submitted study, and runs visible and hidden checks; and
11. writes and commits the result record.

Cleanup is protected by `dynamic-wind`. Interrupting the coordinator removes
the harness container, stops the server process group, and drops the generated
database.

## Harnesses

Each harness lives under `harnesses/<id>/`:

```text
harnesses/<id>/
  config.json
  run.sh
  ... harness-native skills, context, and configuration ...
```

The required configuration fields are:

```json
{
  "id": "example",
  "script": "run.sh",
  "docker": {
    "image": "example-agent:1",
    "cpus": 4,
    "memory": "4g",
    "pids_limit": 256
  },
  "limits": {
    "wall_seconds": 2700,
    "termination_grace_seconds": 10
  }
}
```

The script runs as `/harness/<script>` with `/workspace` as its current
directory. It inherits the run-specific `HOME`, `PLTUSERHOME`, `CONGAME_URL`,
and CLI login. No output format is required; standard output and standard error
are saved verbatim.

Harness resources are pluggable. `config_directory`, when present, names a
directory copied into the run before the script starts; the harness can point
its native configuration home there. A harness may instead load checked-in
resources directly from its own directory. The coordinator does not interpret
or artificially restrict skills, context files, extensions, prompt templates,
custom tools, or other harness capabilities. The coordinator enforces
`wall_seconds`; a nonzero `max_cost_usd` is exposed to the script as
`LLM_BENCH_MAX_COST_USD` for the harness to enforce using its provider's
accounting. The local LM Studio profile has no usage charge and therefore uses
a zero cost limit.

`environment_from_host` may list credential variable names required by a
harness. The runner rejects missing or empty values and passes each variable to
Docker by name, keeping its value out of Docker's command-line arguments and
checked-in configuration.

Containers use Docker bridge networking. `host.docker.internal` is explicitly
mapped to the host gateway; the generated `CONGAME_URL` and Pi model
configuration use that hostname to reach the host-managed Congame server and
LM Studio. Congame binds to all host interfaces only for its random per-run
port while retaining a loopback public URL.

The container root filesystem and harness directory are read-only. The task
workspace, isolated home, copied harness configuration, and ephemeral `/tmp`
and `/var/tmp` filesystems are writable; documentation and source references
are mounted read-only. The container gets no repository, task-definition,
results, host-home, or Docker-socket mount. It runs as the invoking host UID/GID
with all capabilities dropped,
`no-new-privileges`, and configured CPU, memory, PID, and wall-clock limits.

The initial Pi profile copies a repository-owned `PI_CODING_AGENT_DIR` for
each run and connects to `qwen3.8-27b-mlx` at LM Studio's local OpenAI endpoint.
It trusts the isolated task workspace and leaves Pi's resource discovery and
tool set enabled. Skills, extensions, prompt templates, and other Pi resources
can be checked into its `pi-agent/` directory; task-specific `AGENTS.md` or
`CLAUDE.md` files can be included in a task fixture. Pi runs in streaming JSON
mode, so JSONL events—including thinking and tool-call deltas—are shown in the
invoking terminal and saved to `stdout.log` as they happen.

The `pi-kimi-k3-openrouter-max` profile uses Pi's pinned built-in OpenRouter
catalog entry for `moonshotai/kimi-k3` with Kimi K3's native `max` reasoning
effort. The `pi-glm-5.3-flash-openrouter-max` profile supplies an explicit model
entry for the newer `z-ai/glm-5.3-flash`, also at `max`, using the context,
capability, and pricing metadata available on 2026-08-27. Both require
`OPENROUTER_API_KEY`; unlike the local profile, their provider usage is billed
and the checked-in configurations do not impose a dollar cap.

## Tasks

Each task lives under `tasks/<id>/` and contains a `config.json`, prompt,
editable fixture, and valid starter study. The starter is uploaded before the
agent begins so the runner can create an instance. Repeated uploads with the
same study ID replace the uploaded study used by new participants in that
instance.

Task checks are arrays of argument strings and are executed directly, without
a shell. `${TASK_DIR}` and `${REPOSITORY_ROOT}` are expanded in check
arguments. Use an explicit `bash -lc` check only when shell behavior is
actually required.

The task workspace contains the complete fixture and generated `TASK.md`.
Fixtures may include task-specific context files, skills, tests, examples, or
any other resources intended for the agent.
References are copied beside it under `../reference/`. The original Git history,
other task fixtures, previous results, and task configuration are not copied
into the workspace.

The container substantially reduces accidental answer exposure and host
filesystem access. It is not a complete adversarial sandbox: bridge networking
still permits outbound connections and access to host services exposed on the
gateway. Stronger network isolation would require explicit proxies or host
firewall rules for Congame and LM Studio.

## Results and grading

Results are stored under `results/<run-id>/`. A result includes the resolved
configuration, task prompt, agent logs, patch, untracked files, server logs,
check logs, outcome, and a pending human grade. Start review at
`review/README.md`: it links to ordinary final file versions under
`review/files/` and identifies additions, modifications, and deletions. The
binary-safe `changes.patch` remains available for exact replay.

### How to review a run

Start with `TASK.md`, then `review/README.md` and the final files it links to.
Turn the task requirements into a checklist and trace each requirement through
the submitted code. `review/files/` contains added and modified files, not a
complete runnable workspace; use `changes.patch` for exact changes and deletions.

For the smoke task, check the introduction and Continue button, rejection of an
empty name, the final page displaying the submitted name, the exported
`benchmark-study`, and successful compilation and upload.

Next read `outcome.json` and `checks/summary.json`. Inspect each check's command,
result, and corresponding log under `checks/`. A `completed` outcome means the
runner finished; it is not a correctness grade. The current smoke checks verify
compilation and that the submission differs from the fixture, plus the runner's
upload check. They do **not** verify the full participant flow.

Render the agent transcript from the checkout root:

```sh
llm-benchmarks/scripts/view-pi-stream.rkt llm-benchmarks/results/<run-id>/stdout.log
```

Look for actual execution of the exercise script and its results, then read the
agent's final response. An `exercise.rkt` file shows intended verification;
the transcript shows whether it ran and what happened. Compare the final
response's claims against those results.

| Criterion                 | Main evidence                                                                                                             |
|---------------------------|---------------------------------------------------------------------------------------------------------------------------|
| Behavioral correctness    | Task checklist, final study code, participant-flow exercise results                                                       |
| Experimental validity     | Requested design and how assignment, incentives, and data are implemented; assess only requirements relevant to this task |
| Exercise and verification | Exercise script assertions and actual execution/results in the transcript                                                 |
| Robustness                | Input validation and state handling in code, plus relevant edge-case exercises                                            |
| Conscript quality         | Final code structure and use of Conscript APIs                                                                            |
| Scope discipline          | Task constraints, changed files/patch, and `outcome.json` scope violations                                                |
| Handoff                   | Final agent response compared with implementation and verification evidence                                               |

Use `stderr.log`, `server/`, and `setup-upload.log` when diagnosing failures.
`manifest.json` records the configuration and repository revision for provenance;
`grade.json` is your assessment, not evidence of correctness. `untracked-files/`
is a raw capture of new files, usually already represented in the review index.
Missing artifacts can indicate an early failure; do not infer a failed study
implementation solely from an infrastructure error.

The runner removes its temporary servers and databases, so URLs in saved logs
are no longer live. Interactive review requires reconstructing the submission
from the recorded fixture/revision and patch, then uploading it into a fresh
local instance. If the saved evidence cannot establish a criterion, defer the
grade and investigate; record specific evidence and uncertainty in your notes.

Machine results and human grades are separate Git commits:

```text
llm-bench: record <run-id>
llm-bench: grade <run-id>
```

List runs newest first, with their outcome and human grade status:

```sh
raco congame-llm-bench runs
raco congame-llm-bench runs --ungraded --task smoke-study
raco congame-llm-bench runs --harness pi-kimi-k3-openrouter-max --json
```

Open the native `racket/gui/easy` grader:

```sh
raco congame-llm-bench grade latest
raco congame-llm-bench grade 20261015T075152Z
```

The window starts with the task and a readable check summary. Each scoring step
shows the criterion, score examples, and suggested evidence side by side.
Use the score slider to rate it from 0 to 4 in tenths; jump between criteria
with the step selector.
Racket and Conscript source previews include syntax highlighting.
The evidence selector includes final source files, the patch, check logs, and
server logs. You can switch evidence at any step. The agent transcript is not
loaded or previewed in the grader; use the standalone log viewer if needed.
Large previews are limited to 2 MiB and marked when truncated.

Back and Next preserve your scores and notes. Existing grades are loaded for
revision. Reviewer identity defaults to your Git name or login; you can edit it
on the final review screen. Save requires every score to be in the rubric's
range (0–4 for `study-v1`, with fractional scores allowed). Closing without
saving leaves the previous grade intact and warns about unsaved edits.

Use a full run ID, a unique prefix, or `latest` (the newest recorded run).
Add `--no-commit` to save without committing. A commit failure keeps the window
open and reports where the grade was saved. For terminal-only sessions:

```sh
raco congame-llm-bench grade --cli latest
```

Alternatively, prepare a grade JSON file (see `rubrics/grade-example.json`)
and attach it with:

```sh
raco congame-llm-bench grade --from /path/to/grade.json <run-id>
```

Large exceptional media artifacts should use Git LFS; normal JSON, logs, and
patches remain ordinary Git objects.

## Tests

The unit tests do not require Docker, PostgreSQL, Congame, Pi, or LM Studio:

```sh
raco test llm-benchmarks/tests/all.rkt
```

The checked-in `smoke` suite is the host-service integration test and does
require all local prerequisites, including a running LM Studio model.

To exercise the full two-server and container lifecycle without contacting an
LLM (using the small `debian:bullseye-slim` image):

```sh
raco test llm-benchmarks/tests/integration.rkt
```
