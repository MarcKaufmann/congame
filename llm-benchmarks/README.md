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

Build the initial Pi harness image from the repository root. It extends the
regular local Congame image with `congame-cli`, Git, and the pinned Pi version:

```sh
docker compose build congame
docker build \
  --file llm-benchmarks/harnesses/pi-qwen3.8-27b-mlx-medium/Dockerfile \
  --tag congame-llm-bench-pi:0.84.2 \
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
8. captures a binary-safe Git patch and any new files;
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

Containers use Docker bridge networking. `host.docker.internal` is explicitly
mapped to the host gateway; the generated `CONGAME_URL` and Pi model
configuration use that hostname to reach the host-managed Congame server and
LM Studio. Congame binds to all host interfaces only for its random per-run
port while retaining a loopback public URL.

The container root filesystem and harness directory are read-only. The task
workspace, isolated home, and copied harness configuration are writable;
documentation and source references are mounted read-only. The container gets
no repository, task-definition, results, host-home, or Docker-socket mount. It
runs as the invoking host UID/GID with all capabilities dropped,
`no-new-privileges`, and configured CPU, memory, PID, and wall-clock limits.

The initial Pi profile copies a repository-owned `PI_CODING_AGENT_DIR` for
each run and connects to `qwen3.8-27b-mlx` at LM Studio's local OpenAI endpoint.
It trusts the isolated task workspace and leaves Pi's resource discovery and
tool set enabled. Skills, extensions, prompt templates, and other Pi resources
can be checked into its `pi-agent/` directory; task-specific `AGENTS.md` or
`CLAUDE.md` files can be included in a task fixture. Pi runs in streaming JSON
mode, so JSONL events—including thinking and tool-call deltas—are shown in the
invoking terminal and saved to `stdout.log` as they happen.

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
check logs, outcome, and a pending human grade.

Machine results and human grades are separate Git commits:

```text
llm-bench: record <run-id>
llm-bench: grade <run-id>
```

Prepare a grade JSON file and attach it with:

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
