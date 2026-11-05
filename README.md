# evidr: review R code changes with evidence

A local Shiny app for reviewing AI-drafted R code, in the spirit of `testthat::snapshot_review()`.

`snapshot_review()` shows what a snapshot changed and lets you accept or reject it. `evidr` does the same for an
AI-drafted analysis script, and adds what a regulated team also needs: an independent check, named reviewers with rules,
and a tamper-evident record of every step, written with [regulog](https://cran.r-project.org/package=regulog) and audited
with [reproducr](https://cran.r-project.org/package=reproducr).

```r
evidr::run_app(dir = "reviews")   # a permanent folder; the default is a temporary one
```

## What a reviewer sees

| Tab | What it shows |
|---|---|
| **Code diff** | The draft against the last *accepted* script for the task (the "snapshot"). |
| **Output diff** | Both scripts run on the current data in fresh R processes. The diff is what the change does to the result. |
| **Independent check** | A separately written implementation run on the same data. The draft agrees with it, or not, cell by cell. |
| **Gate and audit** | A static gate (blocks network, file-system and process calls) and the `reproducr` audit of the code. |

The reviewer then accepts, edits or rejects, with a reason for edits and rejections. Each step is written to a
hash-chained `.rlog` file and shown in the evidence log. **Verify chain** re-reads the file from disk.

## Features

- **Real execution.** Code runs in a fresh process (`callr`) with a time limit; fingerprints are of real results.
- **Real replay.** Replay reads the saved script from disk, re-runs it on the current data file, and compares code, data,
  environment and output fingerprints with the logged ones. A difference marks the review **stale** and withdraws the accepted baseline.
- **Snapshot semantics.** Accepting promotes the reviewed code to `accepted/<task>.R`; the next draft is diffed against it.
- **Executable evidence.** An edit is judged by running it against the independent implementation.
- **Blind drills.** The app can hand you a draft that may carry a planted error. At draft time the log commits to the
  answer (SHA-256 of `planted|nonce`); after your decision it reveals and scores it. Scores go to `drill-scores.csv`.
- **Rules in code.** The reviewer is not the scoper, reviewers differ from each other, a primary analysis needs two
  reviewers, reasons are required, and code blocked by the gate cannot be accepted.
- **Review packet.** *Download packet* gives a `.tar.gz` with a self-contained HTML record, the log, the audit trail CSV, the prompt, the draft and the final code.
- **Tamper test.** Changes one past entry in a copy of the log and verifies it.

## Use your own model

```r
options(evidr.model = function(prompt) {
  # call your model and return R code as one string
}, evidr.model_name = "my-model-v1")
evidr::run_app(dir = "reviews")
```
A fourth draft source appears. Its code goes through the same gate, packet and review. The package never calls a model
itself and never stores a key.

## Code layout

`R/mod_*.R` are Shiny modules, one per step (`scope`, `data`, `draft`, `review`, `replay`, `ledger`, `scores`), each with a
`*_ui()` and `*_server()`. They share one state object created in `app_server()`. The engine (`rv_*` functions in
`session.R`, `run.R`, `diff.R`, `packet.R`, `tasks.R`) has no Shiny code and is what the tests exercise. Tasks are plain
lists in `R/tasks.R`: synthetic data, a baseline, a correct draft, a draft with one realistic error, and an independent reference.

## Limits

- **Not a security sandbox.** The static gate is a pattern check; process isolation is for stability. Only review code you would be willing to run.
- **Names are not authenticated.** Reviewer names are typed.
- **Agreement is evidence, not proof.** The independent implementation can share a misunderstanding of the specification.
- **Hash chains reveal edits; they do not prevent them.** Keep the latest hash where the system cannot rewrite it.
- **Not validated software.** It makes no compliance claim.
- The environment fingerprint covers R and the packages the task uses, not the operating system.
- One review per browser tab; the app does not resume an earlier log.

## Tested how

21 testthat tests cover task consistency, diff, gate, runner limits, reviewer rules, stale and re-review, a changed script
file, snapshot behaviour, drill commitment and reveal, tamper test, export and the modules (`shiny::testServer`). Tests that
start R processes or the app are skipped on CRAN. The app was also driven end to end in headless Chromium at desktop and
phone width. These runs used a stand-in for `regulog` and `reproducr` with the same function names and arguments; run
the app once with the CRAN versions before release.
