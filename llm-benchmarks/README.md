# Congame LLM Benchmarks

Run Conscript implementation tasks against command-line agents, then grade the
results. The coordinator runs in Racket; each agent harness is a shell script
that works in a prepared directory and reads `TASK.md`.

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

Each run starts a fresh Congame server and PostgreSQL database, uploads the
starter study, and launches the harness in Docker with a prepared workspace.
After the agent finishes, the runner captures its changes and replays them
against a second fresh server to check compilation, upload, and task-specific
requirements. Results are committed to Git unless `--no-commit` is set.

The runner removes its containers, servers, and databases on completion or
interruption. Saved server URLs are no longer live after a run.

## Harnesses

Create a harness from scratch or use an existing one as a template:

```sh
raco congame-llm-bench new-harness
raco congame-llm-bench new-harness my-agent
raco congame-llm-bench new-harness --resume /path/to/draft.json
```

The wizard writes `config.json`, executable `run.sh`, and any copied resources.
Build the Docker image separately. Forward credentials by their host environment
variable names; do not put their values in the harness configuration.

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

Use `config_directory` to copy harness configuration into each run, or load
resources directly from the harness directory. Harnesses can use their native
skills, extensions, context files, and tools.

The coordinator enforces `wall_seconds`. A nonzero `max_cost_usd` is passed as
`LLM_BENCH_MAX_COST_USD`; the harness is responsible for enforcing it.

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

The local Pi profile connects to `qwen3.8-27b-mlx` through LM Studio. It copies
`pi-agent/` into each run as `PI_CODING_AGENT_DIR`, with Pi's resource discovery
and tools enabled. Pi events are streamed to the terminal and saved in
`stdout.log`.

The `pi-kimi-k3-openrouter-max` profile uses Pi's pinned built-in OpenRouter
catalog entry for `moonshotai/kimi-k3` with Kimi K3's native `max` reasoning
effort. The `pi-glm-5.3-flash-openrouter-max` profile supplies an explicit model
entry for the newer `z-ai/glm-5.3-flash`, also at `max`, using the context,
capability, and pricing metadata available on 2026-08-27. Both require
`OPENROUTER_API_KEY`; unlike the local profile, their provider usage is billed
and the checked-in configurations do not impose a dollar cap.

## Tasks

Create a task with the GUI wizard:

```sh
raco congame-llm-bench new-task
raco congame-llm-bench new-task my-study
```

Drafts are JSON files. To reopen one from the command line:

```sh
raco congame-llm-bench new-task --resume /path/to/draft.json
```

New tasks are saved under `tasks/`; add them to a suite and commit them when
ready. Blank source paths generate a minimal Conscript study. Custom sources
are copied without compilation, excluding `.git` and `compiled` directories;
symlinks are rejected.

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

List runs or open the grader:

```sh
raco congame-llm-bench runs
raco congame-llm-bench runs --ungraded --task smoke-study
raco congame-llm-bench runs --harness pi-kimi-k3-openrouter-max --json
raco congame-llm-bench grade latest
raco congame-llm-bench grade 20261015T075152Z
```

Run arguments accept a full ID, a unique prefix, or `latest`. Grading commits
by default; use `--no-commit` to save locally. For terminal sessions or a
prepared grade file:

```sh
raco congame-llm-bench grade --cli latest
raco congame-llm-bench grade --from /path/to/grade.json <run-id>
```

See `rubrics/grade-example.json` for the grade format. Machine results and
human grades are recorded in separate commits.

### Evidence

Results live under `results/<run-id>/`. The grader presents the task, check
summary, and submitted code. For review outside the grader, start with
`review/README.md`; it links to final versions of added and modified files.
`changes.patch` records the full diff, including deletions.

A `completed` outcome means the runner finished, not that the submission is
correct. Compare the implementation with the task requirements. The smoke
checks cover compilation, upload, and whether the fixture changed; they do not
test the full participant flow.

To inspect the agent's actions and verification results, use the transcript
viewer:

```sh
llm-benchmarks/scripts/view-pi-stream.rkt llm-benchmarks/results/<run-id>/stdout.log
```

Check logs are under `checks/`; `stderr.log`, `server/`, and `setup-upload.log`
help diagnose failures. `manifest.json` records the configuration and repository
revision. Infrastructure failures alone do not establish that a submission is
incorrect. If the recorded evidence is insufficient, reconstruct the workspace
from the fixture and patch and test it in a fresh local instance.

### Cost

`cost.json` stores the run's reported USD estimate, which the grader displays.
For Pi, it sums assistant message costs once, excluding duplicates in session
summaries. Interrupted or incomplete usage is marked partial; missing usage is
unavailable. These estimates may differ from provider bills.

Backfill or refresh costs from saved logs with:

```sh
raco congame-llm-bench cost latest
raco congame-llm-bench cost all
```

These commands write `cost.json` without committing.

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
