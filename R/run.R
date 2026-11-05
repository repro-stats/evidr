# Static gate and sandboxed runner.
#
# The gate refuses code that touches the network, the file system or the process environment before
# anything runs. The runner executes code in a separate, fresh R process with a time limit.
# This is a guard for a reviewer's workstation, not a security sandbox: only review code you
# would be willing to run, and use a container for untrusted input.

rv_blocklist <- c(
  "system\\s*\\(", "system2\\s*\\(", "shell\\s*\\(", "pipe\\s*\\(", "unlink\\s*\\(", "file\\.remove\\s*\\(", "file\\.rename\\s*\\(",
  "file\\.copy\\s*\\(", "file\\.create\\s*\\(", "dir\\.create\\s*\\(", "download\\.file\\s*\\(", "\\burl\\s*\\(", "socketConnection\\s*\\(",
  "curl::", "httr2?::", "Sys\\.setenv\\s*\\(", "Sys\\.chmod\\s*\\(", "setwd\\s*\\(", "\\bsource\\s*\\(", "install\\.packages\\s*\\(",
  "\\bsink\\s*\\(", "write\\.csv\\s*\\(", "write\\.table\\s*\\(", "writeLines\\s*\\(", "saveRDS\\s*\\(", "\\bsave\\s*\\(", "readRDS\\s*\\(",
  "\\bload\\s*\\(", "\\.Call\\s*\\(", "\\beval\\s*\\(", "\\bparse\\s*\\(", "\\bget\\s*\\(", "\\bassign\\s*\\(", "\\bdo\\.call\\s*\\(",
  "\\bexec\\b", "reticulate::")

rv_static_gate <- function(code) {
  txt <- paste(code, collapse = "\n")
  # Ignore comments so that a note like "# no system() call" does not trip the gate.
  stripped <- paste(sub("#.*$", "", strsplit(txt, "\n", fixed = TRUE)[[1]]), collapse = "\n")
  hits <- character()
  for (p in rv_blocklist) if (grepl(p, stripped, perl = TRUE)) hits <- c(hits, gsub("\\\\s\\*\\\\\\(|\\\\\\(|\\\\b", "", p))
  has_result <- grepl("\\bresult\\s*(<-|=)", stripped)
  parses <- tryCatch({ parse(text = txt); TRUE }, error = function(e) FALSE)
  findings <- c(if (!parses) "The code does not parse.",
                if (!has_result) "The code never assigns `result`.",
                if (length(hits)) paste0("Blocked call: ", paste(unique(hits), collapse = ", ")))
  list(pass = length(findings) == 0, findings = findings, blocked = unique(hits))
}

rv_run <- function(code, data_file, timeout = 30) {
  t0 <- Sys.time()
  out <- tryCatch(
    callr::r(function(code, data_file) {
      env <- new.env(parent = globalenv())
      assign("data_file", data_file, envir = env)
      eval(parse(text = code), envir = env)
      if (!exists("result", envir = env, inherits = FALSE)) stop("The code did not create `result`.")
      get("result", envir = env)
    }, args = list(code = paste(code, collapse = "\n"), data_file = data_file), timeout = timeout, show = FALSE),
    error = function(e) structure(list(msg = conditionMessage(e)), class = "rv_run_error"))
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  if (inherits(out, "rv_run_error")) {
    msg <- sub("^.*Error[^:]*: ", "", gsub("\n.*$", "", out$msg))
    return(list(ok = FALSE, error = if (grepl("timed out|timeout", out$msg, ignore.case = TRUE)) paste0("Stopped after ", timeout, " seconds.") else trimws(msg), seconds = secs))
  }
  if (!is.data.frame(out)) return(list(ok = FALSE, error = "`result` must be a data frame.", seconds = secs))
  list(ok = TRUE, result = out, sha = rv_df_sha(out), seconds = secs)
}

# reproducr's audit of a piece of code, as text. Wrapped so that a failure never stops a review.
rv_audit_text <- function(code) {
  f <- tempfile(fileext = ".R"); on.exit(unlink(f)); writeLines(code, f)
  tryCatch({
    rep <- reproducr::audit_script(f, renv = FALSE, verbose = FALSE)
    rk  <- reproducr::risk_score(rep)
    paste(c(utils::capture.output(print(rep)), "", utils::capture.output(print(rk))), collapse = "\n")
  }, error = function(e) paste0("reproducr audit not available: ", conditionMessage(e)))
}
