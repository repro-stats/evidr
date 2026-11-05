# The review engine. Everything the app does goes through these functions, so they can be tested
# without Shiny. A session is an environment holding the state of one review.

rv_tiers <- list(explore = c(label = "Exploratory", need = 1L), support = c(label = "Supportive analysis", need = 1L),
                 primary = c(label = "Primary analysis", need = 2L))

# Work files go to a temporary folder unless the user chooses one. Nothing is written to the working directory by default.
rv_default_dir <- function() getOption("evidr.dir", file.path(tempdir(), "evidr"))

rv_session <- function(dir = rv_default_dir(), user = NULL) {
  s <- new.env(parent = emptyenv())
  s$root <- normalizePath(dir, mustWork = FALSE)
  for (d in c("accepted", "reviews")) dir.create(file.path(s$root, d), showWarnings = FALSE, recursive = TRUE)
  s$user <- user %||% getOption("evidr.user", Sys.info()[["user"]])
  rv_reset(s)
  s
}

rv_reset <- function(s) {
  keep <- c("root", "user")
  for (n in setdiff(ls(s, all.names = TRUE), keep)) rm(list = n, envir = s)
  s$step <- 0L                      # 0 new, 1 scoped, 2 data fixed, 3 drafted, 4 reviewed
  s$trail <- data.frame(seq = integer(), time = character(), actor = character(), event = character(), detail = character(), stringsAsFactors = FALSE)
  s$decisions <- list(); s$stale <- FALSE; s$pending <- NULL; s$round <- 0L; s$replays <- 0L; s$last_key <- ""
  s$id <- format(Sys.time(), "%Y%m%d-%H%M%S"); s$seed <- sample.int(1e6, 1)
  invisible(s)
}

rv_fail <- function(msg) list(ok = FALSE, message = msg)
rv_ok   <- function(msg, ...) c(list(ok = TRUE, message = msg), list(...))

# ---- ledger -----------------------------------------------------------------

# Write one event to the regulog chain and to the on-screen trail.
rv_emit <- function(s, type = c("action", "note", "change"), code, label, actor = "system", detail = "", object = "review", before = NULL, after = NULL) {
  type <- match.arg(type)
  switch(type,
    note   = regulog::log_note(s$log, paste0(label, " [", actor, "]: ", detail)),
    action = regulog::log_action(s$log, action = code, object = object, reason = detail, user = actor),
    change = regulog::log_change(s$log, object = object, field = code, before = before, after = after, reason = detail))
  s$trail[nrow(s$trail) + 1, ] <- list(nrow(s$trail) + 1L, rv_now(), actor, label, detail)
  invisible(NULL)
}

rv_trail <- function(s) {
  tr <- s$trail
  df <- tryCatch(as.data.frame(s$log), error = function(e) NULL)
  hcol <- if (!is.null(df)) intersect(c("entry_hash", "hash"), names(df)) else character()
  if (length(hcol) && nrow(df) == nrow(tr)) tr$hash <- vapply(df[[hcol[1]]], rv_short, "")
  tr
}

rv_verify <- function(s) {
  if (is.null(s$log_path) || !file.exists(s$log_path)) return(list(intact = NA, n = 0L, first_broken = NULL, message = "No log yet."))
  v <- regulog::verify_log(s$log_path, verbose = FALSE)
  n <- v$n_entries %||% NA
  list(intact = isTRUE(v$intact), n = n, first_broken = v$first_broken,
       message = if (isTRUE(v$intact)) sprintf("Chain intact: %s entries verify", n) else sprintf("Chain broken at entry %s", v$first_broken %||% "?"))
}

# Change one past entry in a COPY of the log and verify the copy. Shows that the chain notices.
rv_tamper_test <- function(s) {
  if (is.null(s$log_path) || !file.exists(s$log_path)) return(rv_fail("Nothing to test yet: record the scope first."))
  lines <- readLines(s$log_path)
  cand <- grep("\"reason\"", lines); target <- if (length(cand) >= 2) cand[2] else if (length(cand)) cand[1] else 1L
  new <- sub("\"reason\"\\s*:\\s*\"[^\"]*\"", "\"reason\": \"Edited after the fact\"", lines[target])
  if (identical(new, lines[target])) new <- sub("[a-z]", "Z", lines[target])
  lines[target] <- new
  copy <- tempfile(fileext = ".rlog"); writeLines(lines, copy)
  v <- regulog::verify_log(copy, verbose = FALSE)
  rv_ok(if (isTRUE(v$intact)) "The copy still verifies. Alter a character inside an entry and test again."
        else sprintf("Edited entry %d in a copy of the log: verification fails (first broken entry: %s). Your real log is untouched.", target, v$first_broken %||% "?"),
        intact = isTRUE(v$intact), first_broken = v$first_broken)
}

# ---- 1 scope ----------------------------------------------------------------

rv_scope <- function(s, task_id, tier = "support", scoper = s$user) {
  if (s$step >= 1L) return(rv_fail("The scope is already recorded. Reset to start again."))
  scoper <- rv_norm(scoper); if (!nzchar(scoper)) return(rv_fail("Enter the name of the person scoping the task."))
  t <- rv_task(task_id); tr <- rv_tiers[[tier]]
  s$task <- t; s$tier <- tier; s$needed <- as.integer(tr[["need"]]); s$scoper <- scoper
  s$dir <- file.path(s$root, "reviews", paste0(s$id, "-", t$id)); dir.create(s$dir, recursive = TRUE, showWarnings = FALSE)
  s$log_path <- file.path(s$dir, "review.rlog")
  s$log <- regulog::regulog_init(app = "evidr", version = as.character(utils::packageVersion("evidr")), user = scoper, path = s$log_path)
  rv_emit(s, "note", label = paste0("Task scoped: ", t$title), actor = scoper,
          detail = sprintf("context of use = %s; reviewers required = %d; AI may draft code only; every decision is logged", tr[["label"]], s$needed))
  s$step <- 1L
  rv_ok(sprintf("Scope recorded. %d reviewer(s) required for this context of use.", s$needed))
}

# ---- 2 data and environment --------------------------------------------------

rv_fix_data <- function(s, version = "v2") {
  if (s$step < 1L) return(rv_fail("Record the scope first."))
  if (s$step >= 2L) return(rv_fail("Data and environment are already fixed."))
  s$data_version <- version
  s$data_file <- file.path(s$dir, paste0(s$task$id, "-data.csv"))
  utils::write.csv(s$task$make_data(version), s$data_file, row.names = FALSE)
  d <- regulog::rl_read(s$log, utils::read.csv, s$data_file)
  s$trail[nrow(s$trail) + 1, ] <- list(nrow(s$trail) + 1L, rv_now(), "system", paste0("Input data read through the log: ", basename(s$data_file)), "read.csv via regulog::rl_read")
  s$n_rows <- nrow(d); s$data_sha <- rv_sha_file(s$data_file)
  s$baseline_code <- rv_baseline(s)
  s$env_text <- rv_env_text(rv_code(s$baseline_code), extra = s$task$pkgs); s$env_sha <- rv_sha(s$env_text)
  rv_emit(s, "action", "data_fingerprinted", sprintf("Input data fingerprinted: %s", basename(s$data_file)), actor = "system",
          detail = sprintf("version=%s; rows=%d; data_sha256=%s", version, s$n_rows, s$data_sha), object = basename(s$data_file))
  rv_emit(s, "action", "environment_recorded", sprintf("Environment recorded: %s", s$env_text), actor = "system",
          detail = sprintf("env_sha256=%s", s$env_sha), object = "environment")
  s$step <- 2L
  rv_ok(sprintf("Recorded %s (%d rows) and the environment.", basename(s$data_file), s$n_rows))
}

# The accepted snapshot for this task: the last accepted script, or the task's seeded baseline.
rv_baseline <- function(s) {
  f <- file.path(s$root, "accepted", paste0(s$task$id, ".R"))
  if (!file.exists(f)) writeLines(s$task$baseline, f)
  readLines(f, warn = FALSE)
}
rv_set_accepted <- function(s, code) writeLines(code, file.path(s$root, "accepted", paste0(s$task$id, ".R")))
rv_reset_snapshot <- function(s, task_id) { f <- file.path(s$root, "accepted", paste0(task_id, ".R")); if (file.exists(f)) unlink(f); invisible(TRUE) }

# ---- 3 draft and packet -----------------------------------------------------

rv_draft_sources <- function() {
  c("Correct draft" = "correct", "Draft with a planted error (shown)" = "planted", "Blind drill: correct or planted, not revealed" = "blind",
    if (is.function(getOption("evidr.model"))) c("Your model function" = "model"))
}

# Run the baseline, a piece of code and the independent reference on the current data and compare.
rv_build_packet <- function(s, code) {
  gate <- rv_static_gate(code)
  pk_new <- setdiff(rv_code_pkgs(rv_code(code)), c(rv_code_pkgs(rv_code(s$baseline_code)), s$task$pkgs))
  if (length(pk_new)) gate$warnings <- paste0("The code uses packages outside the recorded environment: ", paste(pk_new, collapse = ", "), ".")
  base <- rv_run(s$baseline_code, s$data_file); ref <- rv_run(s$task$reference, s$data_file)
  run  <- if (gate$pass) rv_run(code, s$data_file) else list(ok = FALSE, error = "Not run: the static gate failed.")
  agree <- if (run$ok && ref$ok) rv_compare_tables(run$result, ref$result) else list(equal = NA, detail = "The independent check could not be compared.")
  change <- if (run$ok && base$ok) rv_compare_tables(run$result, base$result) else list(equal = NA, detail = "No output comparison available.")
  list(code = code, gate = gate, base = base, ref = ref, run = run, agree = agree, change = change,
       code_diff = rv_diff_lines(rv_code(s$baseline_code), rv_code(code)),
       out_diff = if (run$ok && base$ok) rv_diff_lines(rv_df_text(base$result), rv_df_text(run$result)) else NULL,
       audit = if (gate$pass) rv_audit_text(code) else "Skipped: the static gate failed.")
}

rv_draft <- function(s, source = "correct") {
  if (s$step < 2L) return(rv_fail("Fix the data and environment first."))
  if (s$step >= 3L) return(rv_fail("A draft is already captured."))
  planted <- FALSE; model <- "stand-in (canned code, no model called)"
  if (source == "model") {
    f <- getOption("evidr.model"); if (!is.function(f)) return(rv_fail("No model function is set. Use options(evidr.model = function(prompt) ...)."))
    code <- strsplit(trimws(as.character(f(s$task$prompt))), "\n", fixed = TRUE)[[1]]; model <- getOption("evidr.model_name", "your model function")
  } else {
    planted <- switch(source, planted = TRUE, blind = rv_with_seed(s$seed, stats::runif(1)) < 0.5, FALSE)
    code <- if (planted) s$task$drafts$planted else s$task$drafts$correct
  }
  s$drill <- list(blind = identical(source, "blind"), planted = planted, nonce = rv_sha(c(s$id, s$seed)))
  s$draft_code <- code; s$draft_sha <- rv_sha(rv_code(code)); s$prompt <- s$task$prompt; s$baseline_at_draft <- s$baseline_code
  writeLines(s$prompt, file.path(s$dir, "prompt.txt")); writeLines(code, file.path(s$dir, "draft.R"))
  rv_emit(s, "action", "ai_draft_captured", "AI draft captured: prompt, model, output", actor = "assistant",
          detail = sprintf("model=%s; source=%s; prompt_sha256=%s; output_sha256=%s", model, if (s$drill$blind) "blind drill" else source, rv_sha(s$prompt), s$draft_sha), object = "draft.R")
  if (s$drill$blind)   # commit now, reveal after the decision: the log shows the truth was not changed in between
    rv_emit(s, "action", "drill_commitment", "Drill commitment recorded", actor = "system",
            detail = paste0("commitment_sha256=", rv_sha(paste(planted, s$drill$nonce, sep = "|"))), object = "draft.R")
  s$packet <- rv_build_packet(s, code)
  s$show_check <- !s$drill$blind
  p <- s$packet
  rv_emit(s, "action", "static_gate", paste0("Static gate: ", if (p$gate$pass) "passed" else "BLOCKED"), actor = "system",
          detail = if (p$gate$pass) "no blocked calls; code parses; result assigned" else paste(p$gate$findings, collapse = " "), object = "draft.R")
  rv_emit(s, "action", "packet_built", "Review packet built", actor = "system",
          detail = sprintf("baseline_output_sha256=%s; draft_output_sha256=%s; independent_output_sha256=%s",
                           p$base$sha %||% "none", p$run$sha %||% "none", p$ref$sha %||% "none"), object = "draft.R")
  s$step <- 3L
  rv_ok(if (p$gate$pass) "Draft captured. The review packet is ready." else "Draft captured, but the static gate blocked it. It can only be edited or rejected.")
}

# ---- 4 review ---------------------------------------------------------------

rv_check_reviewer <- function(s, reviewer) {
  r <- rv_norm(reviewer)
  if (!nzchar(r)) return("Enter the reviewer's name first.")
  low <- tolower(r)
  if (low == tolower(s$scoper)) return(sprintf("The reviewer must be a different person from %s, who scoped the task.", s$scoper))
  if (any(vapply(s$decisions, function(d) tolower(d$name) == low, NA))) return("Use a different reviewer for the second decision.")
  NULL
}

rv_decide <- function(s, reviewer, kind = c("accepted", "edited", "rejected"), reason = "", code = NULL) {
  kind <- match.arg(kind)
  if (s$step < 3L) return(rv_fail("Capture the AI draft first."))
  if (s$step >= 4L) return(rv_fail("The review is already complete."))
  if (isTRUE(s$rejected)) return(rv_fail("The draft was rejected. Reset to try again."))
  bad <- rv_check_reviewer(s, reviewer); if (!is.null(bad)) return(rv_fail(bad))
  name <- gsub("\\|", "/", rv_norm(reviewer)); why <- gsub("[|\n\r]", "/", rv_norm(reason))
  if (kind != "accepted" && !nzchar(why)) return(rv_fail("Give a reason for an edit or a rejection."))
  cur <- s$draft_code; pk <- s$packet; n <- length(s$decisions) + 1L
  stamp <- function() sprintf("reviewer %d of %d; data_sha256=%s; env_sha256=%s", n, s$needed, s$data_sha, s$env_sha)
  if (kind == "accepted" && !pk$gate$pass) return(rv_fail("The static gate blocked this code, so it cannot be accepted. Edit it or reject it."))
  if (kind == "rejected") {
    rv_emit(s, "action", "review_rejected", "Review decision: rejected", actor = name, detail = paste0(stamp(), "; reason=", why), object = "draft.R")
    s$rejected <- TRUE; s$decisions[[n]] <- list(name = name, kind = kind, reason = why)
    return(rv_finish_drill(s, rv_ok("Draft rejected. A rejected draft cannot be replayed. Reset to try again."), name, kind, NULL))
  }
  if (kind == "edited") {
    if (is.null(code)) return(rv_fail("Supply the edited code."))
    code <- strsplit(sub("\\s+$", "", paste(code, collapse = "\n")), "\n", fixed = TRUE)[[1]]
    if (!length(code) || !any(nzchar(trimws(code)))) return(rv_fail("The edited code cannot be empty."))
    if (identical(rv_code(code), rv_code(s$draft_code))) return(rv_fail("The code is unchanged. Change something, or use Accept."))
    ed <- rv_build_packet(s, code)
    if (!ed$gate$pass) return(rv_fail(paste("The edited code fails the static gate:", paste(ed$gate$findings, collapse = " "))))
    before <- rv_sha(rv_code(cur)); after <- rv_sha(rv_code(code))
    rv_emit(s, "change", "code_sha256", "Reviewer edit", actor = name, object = "draft.R", before = before, after = after,
            detail = paste0("Reviewer edit by ", name, ": ", why))
    rv_emit(s, "action", "review_edited", "Review decision: edited", actor = name,
            detail = paste0(stamp(), "; reason=", why, "; code_sha256_before=", before, "; code_sha256_after=", after), object = "draft.R")
    s$edit_packet <- ed; s$final_code <- code; s$packet_final <- ed
  } else {
    rv_emit(s, "action", "review_accepted", "Review decision: accepted", actor = name,
            detail = paste0(stamp(), "; code_sha256=", s$draft_sha), object = "draft.R")
    s$final_code <- cur; s$packet_final <- pk
  }
  s$decisions[[n]] <- list(name = name, kind = kind, reason = why)
  if (n < s$needed) {
    s$draft_code <- s$final_code; s$packet <- s$packet_final; s$draft_sha <- rv_sha(rv_code(s$draft_code))
    return(rv_ok(sprintf("Reviewer %d recorded. A second, different reviewer is needed.", n)))
  }
  rv_complete_review(s, name, kind)
}

rv_complete_review <- function(s, name, kind) {
  s$final_sha <- rv_sha(rv_code(s$final_code)); fr <- s$packet_final$run
  s$final_file <- file.path(s$dir, paste0("final-r", s$round, ".R")); writeLines(s$final_code, s$final_file)
  s$out_sha <- fr$sha; s$result <- fr$result
  s$cert_file <- file.path(s$dir, ".reproducr"); s$cert_tag <- paste0("review-", s$id, "-r", s$round)
  tryCatch(reproducr::certify(outputs = list(result_table = fr$result), tag = s$cert_tag, script = s$final_file, file = s$cert_file), error = function(e) NULL)
  rv_emit(s, "action", "analysis_run", "Reviewed code run in a clean process; output certified", actor = "system",
          detail = sprintf("final_sha256=%s; output_sha256=%s; certified as %s", s$final_sha, s$out_sha, s$cert_tag), object = basename(s$final_file))
  # Promote to the accepted snapshot, like accepting a snapshot in testthat. Drills and planted-error demos never promote.
  if (!isTRUE(s$drill$blind) && !isTRUE(s$drill$planted)) rv_set_accepted(s, s$final_code)
  s$step <- 4L; s$stale <- FALSE
  msg <- sprintf("Review complete: %d of %d reviewer decisions recorded.%s", length(s$decisions), s$needed, if (!isTRUE(s$drill$blind) && !isTRUE(s$drill$planted)) " The reviewed code is now the accepted baseline for this task." else " This was a drill, so the accepted baseline is unchanged.")
  rv_finish_drill(s, rv_ok(msg), name, kind, s$packet_final)
}

# Blind drill: reveal the truth, score the decision objectively, and log both.
rv_finish_drill <- function(s, res, name, kind, packet) {
  d <- s$drill; res$drill <- NULL
  if (is.null(d) || !isTRUE(d$blind) || isTRUE(s$drill_done)) return(res)
  fixed <- if (!is.null(packet)) isTRUE(packet$agree$equal) else FALSE
  outcome <- if (d$planted) switch(kind, accepted = "missed", rejected = "caught (rejected)", edited = if (fixed) "caught and fixed" else "caught, but the edit still disagrees with the independent check")
             else switch(kind, accepted = "correct accept", rejected = "false alarm (draft was correct)", edited = "unneeded edit (draft was correct)")
  s$drill_done <- TRUE
  rv_emit(s, "action", "drill_reveal", "Drill revealed", actor = "system", object = "draft.R",
          detail = sprintf("planted=%s; nonce=%s; outcome=%s", d$planted, d$nonce, outcome))
  f <- file.path(s$root, "drill-scores.csv")
  row <- data.frame(time = rv_now(), reviewer = name, task = s$task$id, planted = d$planted, decision = kind, outcome = outcome, stringsAsFactors = FALSE)
  utils::write.table(row, f, sep = ",", row.names = FALSE, col.names = !file.exists(f), append = file.exists(f), qmethod = "double")
  res$drill <- list(planted = d$planted, outcome = outcome, note = if (d$planted) s$task$drafts$planted_note else "This draft was correct.")
  res
}

rv_scores <- function(s) {
  f <- file.path(s$root, "drill-scores.csv"); if (!file.exists(f)) return(NULL)
  utils::read.csv(f, stringsAsFactors = FALSE)
}

# ---- 5 replay ---------------------------------------------------------------

# Real change to the data file on disk, as an outside edit would be. Used by the demo controls.
rv_simulate_data_edit <- function(s) {
  d <- utils::read.csv(s$data_file, stringsAsFactors = FALSE)
  col <- switch(s$task$id, cfb = "AVAL", km = "AVAL", ae = "ASTDT")
  if (s$task$id == "ae") d[[col]][4] <- if (identical(d[[col]][4], "2026-02-28")) "2026-02-27" else "2026-02-28" else d[[col]][4] <- d[[col]][4] + 1
  utils::write.csv(d, s$data_file, row.names = FALSE)
  invisible(TRUE)
}

rv_replay <- function(s, sim_data = FALSE, sim_env = FALSE) {
  if (s$step < 4L) return(rv_fail("Complete the review first."))
  if (sim_data && !isTRUE(s$data_edited)) { rv_simulate_data_edit(s); s$data_edited <- TRUE }
  code <- readLines(s$final_file, warn = FALSE)                  # replay from the saved file, not from memory
  code_sha <- rv_sha(rv_code(code)); data_sha <- rv_sha_file(s$data_file)
  env_text <- rv_env_text(rv_code(s$baseline_at_draft), extra = s$task$pkgs, simulate = sim_env); env_sha <- rv_sha(env_text)
  run <- rv_run(code, s$data_file); out_sha <- if (run$ok) run$sha else paste0("run-failed: ", run$error)
  diff <- c(if (code_sha != s$final_sha) "code", if (data_sha != s$data_sha) "data", if (env_sha != s$env_sha) "environment", if (out_sha != s$out_sha) "output")
  match <- !length(diff); key <- paste(code_sha, data_sha, env_sha, out_sha)
  if (identical(key, s$last_key)) return(rv_ok("Same fingerprints as the last replay, so nothing new was logged. Change an input, or run a clean pass.", logged = FALSE, match = match, diff = diff, dup = TRUE))
  s$last_key <- key; s$replays <- s$replays + 1L
  drift_txt <- if (run$ok) tryCatch(paste(utils::capture.output(print(reproducr::check_drift(list(result_table = run$result), against = s$cert_tag, file = s$cert_file))), collapse = "\n"), error = function(e) "") else ""
  rv_emit(s, "action", "replay", sprintf("Replay check %d: %s", s$replays, if (match) "fingerprints match" else paste0("difference detected (", paste(diff, collapse = ", "), ")")),
          actor = "system", object = basename(s$final_file),
          detail = sprintf("simulated=%s; code_sha256=%s; data_sha256=%s; env_sha256=%s; output_sha256=%s",
                           if (!sim_data && !sim_env) "nothing" else paste(c(if (sim_data) "data file edited", if (sim_env) "package update"), collapse = " + "), code_sha, data_sha, env_sha, out_sha))
  if (!match) {
    first <- !s$stale
    s$stale <- TRUE; s$pending <- list(diff = diff, data_sha = data_sha, env_text = env_text, env_sha = env_sha)
    if (first) {
      rv_emit(s, "action", "review_marked_stale", sprintf("Review marked stale: %s changed since the review", paste(diff, collapse = ", ")), actor = "system", object = basename(s$final_file),
              detail = sprintf("review covered code_sha256=%s; data_sha256=%s; env_sha256=%s; output_sha256=%s; new review required", s$final_sha, s$data_sha, s$env_sha, s$out_sha))
      rv_set_accepted(s, s$baseline_at_draft)   # a stale review is not an accepted snapshot any more
    }
  }
  rv_ok(if (match) (if (s$stale) "These match the logged fingerprints, but an earlier replay already marked the review stale. The log keeps that mark."
                    else "Code, data, environment and output all match the logged ones. The review still covers this run.")
        else paste0("The ", paste(diff, collapse = " and "), " fingerprint differs from the logged one, so the review no longer covers this run. It is marked stale."),
        logged = TRUE, match = match, diff = diff, drift = drift_txt)
}

# Adopt the changed state and send the reviewed code back for a new review.
rv_rereview <- function(s) {
  q <- s$pending; if (!isTRUE(s$stale) || is.null(q)) return(rv_fail("Nothing is stale."))
  s$data_sha <- q$data_sha; s$env_text <- q$env_text; s$env_sha <- q$env_sha
  rv_emit(s, "action", "fingerprints_re_recorded", sprintf("Fingerprints re-recorded after change: %s", paste(q$diff, collapse = ", ")), actor = "system", object = "review",
          detail = sprintf("data_sha256=%s; env_sha256=%s; the earlier review does not cover these", s$data_sha, s$env_sha))
  s$round <- s$round + 1L; s$decisions <- list(); s$rejected <- FALSE; s$stale <- FALSE; s$pending <- NULL; s$last_key <- ""; s$drill_done <- TRUE; s$data_edited <- FALSE
  s$draft_code <- s$final_code; s$draft_sha <- rv_sha(rv_code(s$draft_code))
  s$baseline_code <- s$baseline_at_draft
  s$packet <- rv_build_packet(s, s$draft_code); s$show_check <- TRUE
  rv_emit(s, "action", "packet_built", "Review packet rebuilt on the changed state", actor = "system", object = "draft.R",
          detail = sprintf("baseline_output_sha256=%s; code_output_sha256=%s; independent_output_sha256=%s", s$packet$base$sha %||% "none", s$packet$run$sha %||% "none", s$packet$ref$sha %||% "none"))
  s$step <- 3L
  rv_ok("Re-review needed: the earlier decision covered the earlier fingerprints. Record a new decision for the current ones.")
}
