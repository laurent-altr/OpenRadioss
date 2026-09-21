# OpenRadioss PR reviewer

`review_open_prs.py` reviews eligible GitHub pull requests with Copilot CLI. It
uses complete local Git diffs, lets Copilot inspect the exact PR checkout, and
publishes only a validated review summary.

## Requirements

- Run from an OpenRadioss Git checkout. Review commits are fetched directly
  from `https://github.com/OpenRadioss/OpenRadioss.git`; no remote name is
  required.
- Authenticate `gh` for `OpenRadioss/OpenRadioss`.
- Authenticate Copilot CLI separately, or set `COPILOT_GITHUB_TOKEN`.
- Keep the tracked working tree clean. Changes to this reviewer and its test
  file are allowed while developing the tool.

Check the available options with any supported help spelling:

```bash
python scripts/review_open_prs.py -h
python scripts/review_open_prs.py -help
python scripts/review_open_prs.py --help
```

## Recommended first run

Review one PR without posting anything:

```bash
python scripts/review_open_prs.py --pr 5245 --dry-run
```

The final report is written to `review_reports/pr-<number>-<head>.md`. A dry run
still calls the configured models, but it does not publish a GitHub review.

After inspecting the report, omit `--dry-run` to publish:

```bash
python scripts/review_open_prs.py --pr 5245
```

Outside GitHub Actions, reviewing every eligible open PR must be explicit:

```bash
python scripts/review_open_prs.py --all
```

Running locally without either `--pr` or `--all` is rejected to prevent an
accidental batch publication.

## GitHub Actions handoff

The script is intentionally fixed to `OpenRadioss/OpenRadioss`. DevOps does not
need to pass a repository name, configure an `upstream` remote, or extract the
PR number for normal PR events.

When `GITHUB_ACTIONS=true`, the script reads `GITHUB_EVENT_PATH` and accepts one
PR from any of these payloads:

- `pull_request` or `pull_request_target`
- `workflow_run.pull_requests`
- `check_suite.pull_requests` or `check_run.pull_requests`

The normal Action command is therefore:

```bash
python scripts/review_open_prs.py
```

For `workflow_dispatch`, which has no PR in its event payload, pass the workflow
input as `--pr NUMBER`. The script rejects `--all` in GitHub Actions and rejects
an explicit PR number that differs from the event PR.

### Container contract

The workflow or container image must provide:

- Python 3.10 or newer, Git, GitHub CLI, and Copilot CLI
- a Git checkout of the trusted default-branch version of this script
- `GH_TOKEN` with `contents: read` and `pull-requests: write` when publishing
- a separate `COPILOT_GITHUB_TOKEN` for Copilot CLI
- network access to GitHub and the Copilot service
- a disposable workspace because the review temporarily checks out untrusted
  PR content

The script removes `GH_TOKEN` and `GITHUB_TOKEN` from the Copilot child process.
Container isolation is still required: the model currently has broad tool
access and reviews contributor-controlled content. Do not mount a Docker socket,
SSH agent, home directory, or other host credentials into the review container.

Recommended workflow responsibilities:

1. Trigger only after Developer CI succeeds.
2. Check out the trusted `main` version of the reviewer, never the PR version.
3. Use an ephemeral container or runner workspace with no persistent secrets.
4. Pin the container image and CLI versions.
5. Begin deployment with `--dry-run` and retain `review_reports/` as an artifact.
6. Enable publication only after security review of the model tool policy.

This is the minimum orchestration shape; DevOps should supply the organization
specific runner labels, container image, token source, and artifact retention:

```yaml
permissions:
  contents: read
  pull-requests: write

steps:
  - uses: actions/checkout@v6
    with:
      ref: main
      fetch-depth: 1
      persist-credentials: false
  - name: Review current PR
    env:
      GH_TOKEN: ${{ github.token }}
      COPILOT_GITHUB_TOKEN: ${{ secrets.COPILOT_GITHUB_TOKEN }}
    run: python scripts/review_open_prs.py --dry-run
```

For a `workflow_run` trigger, additionally require a successful conclusion in
the job condition. The script also queries the PR check rollup before review.

## Workflow

For each open PR, the script:

1. Loads GitHub metadata, checks, review history, and discussion.
2. Skips drafts, failed or incomplete checks, PRs above the total diff-size
   limit, and content already reviewed by the bot.
3. Fetches the recorded base commit and exact PR head from the canonical GitHub
  repository.
4. Checks out the PR head temporarily and builds complete local per-file diffs.
5. Selects a review path from the total diff size (not file count).
6. Extracts or recovers a marked publishable summary.
7. Writes a dry-run report or posts a review bound to the reviewed head commit.
8. Restores the original branch or detached revision, including after errors.

PRs are tiered by **total diff size in characters**, not file count:

- **Small** (`--small-pr-diff-size`, default 8,000 chars): a single call with
  `--small-pr-model`, full PR context.
- **Medium** (up to `--medium-pr-diff-size`, default 40,000 chars): a single
  call with `--medium-pr-model`, still full PR context.
- **Large** (above `--medium-pr-diff-size`): changed files are grouped into
  size-bounded chunks (`--scout-chunk-size`, default 15,000 chars; a single
  file larger than the budget still gets its own chunk — files are never
  split). Each chunk is scouted in parallel with `--large-file-model`
  (multiple files per call, plus a manifest of every changed filename for
  cross-file awareness), then all chunk findings are independently
  re-verified and combined against the full patches by
  `--synthesis-model`.

PRs whose total diff size exceeds `--max-review-diff-size` (default 400,000
chars) are skipped entirely; this replaces the old fixed 50-file cutoff so
that ordinary large PRs (many files, moderate total diff size) are reviewed
via more chunks instead of being skipped outright.

Findings are required to include a `Confidence: N/10` field. After synthesis,
findings with a parsed confidence below `--min-confidence` (default 6) are
dropped automatically before publishing, as a deterministic safety net against
false positives independent of model behavior; unparseable rows are kept
rather than silently dropped, and a response with no recognizable findings
table is left unchanged.

Large-PR scouting and synthesis default to `claude-opus-5` (previously
`claude-haiku-4.5` for scouts and `claude-sonnet-5` for synthesis). This
trades cost for accuracy: each large PR now issues one `claude-opus-5` call
per chunk plus one for synthesis, instead of one cheap call per file. Chunking
by diff size (rather than one file per scout) also gives each scout call
cross-file context, which is what curbed the false positives seen with the
old per-file/haiku scouting.

## Monitoring long reviews

Copilot calls can take several minutes while the model explores repository
context. The script prints a heartbeat every 60 seconds by default:

```text
Copilot: model=claude-opus-5 still running after 120s (pid=12345); waiting for response
```

This message is observational: it does not interrupt or restart Copilot. Change
the interval or disable it with:

```bash
python scripts/review_open_prs.py --heartbeat-seconds 30
python scripts/review_open_prs.py --heartbeat-seconds 0
```

For large PRs, chunk scout completions appear independently. In dry-run mode
their intermediate results are appended to the report as they finish.

## Safety controls

- `--force` reviews matching content again; it does not bypass draft, CI, or
  diff-size eligibility checks.
- Repeat `--deny-tool TOOL` to remove Copilot tools from the default tool set.
- Missing or malformed publication markers are recovered with
  `--polisher-model`. If recovery fails, nothing is posted.
- `--min-confidence` (default 6) drops low-confidence findings after synthesis;
  set to `0` to disable this filter.
- The GitHub tokens used by `gh` and GitHub Actions are always removed from the
  Copilot environment; use a separate `COPILOT_GITHUB_TOKEN`.

## Troubleshooting

`checks are not all successful`

: At least one reported GitHub check is absent, pending, skipped, cancelled, or
  unsuccessful. The PR is not reviewed.

`tracked working-tree changes would contaminate the review`

: Commit, stash, or otherwise resolve the listed tracked changes before retrying.

`fetching recorded base revision` or `fetching exact head revision`

: The script is reading immutable review endpoints directly from
  `OpenRadioss/OpenRadioss`. Check network access to GitHub and confirm that the
  PR still exists.

`still running after ...`

: Copilot remains active and has not returned its response. The PID in the
  message can be inspected externally without stopping the review.

## Exit codes

- `0`: all selected work completed or was skipped by eligibility rules
- `1`: GitHub, Git, Copilot, parsing, or publication failed for at least one PR
- `2`: command-line or GitHub Actions target configuration is invalid

## Tests

```bash
cd scripts
python3 -m unittest -v test_review_open_prs.py
```