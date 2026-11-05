# Small helpers shared by the engine and the app. No Shiny code here.

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

rv_sha      <- function(x) digest::digest(paste(x, collapse = "\n"), algo = "sha256", serialize = FALSE)
rv_sha_file <- function(f) digest::digest(file = f, algo = "sha256")
rv_short    <- function(h, n = 8) if (is.null(h) || !nzchar(h)) "" else paste0(substr(h, 1, n), "...", substr(h, nchar(h) - 3, nchar(h)))
rv_now      <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
rv_norm     <- function(x) trimws(gsub("[[:space:]]+", " ", x %||% ""))

# Run `expr` with a fixed RNG seed and put the caller's RNG state back afterwards.
rv_with_seed <- function(seed, expr) {
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = globalenv()) else NULL
  on.exit({
    if (had) assign(".Random.seed", old, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
  }, add = TRUE)
  set.seed(seed)
  expr
}

# A data frame as stable CSV text. This is what output fingerprints and output diffs use.
rv_df_text <- function(df) {
  stopifnot(is.data.frame(df))
  df <- as.data.frame(lapply(df, function(col) if (is.factor(col)) as.character(col) else col), stringsAsFactors = FALSE)
  paste(utils::capture.output(utils::write.csv(df, row.names = FALSE)), collapse = "\n")
}
rv_df_sha <- function(df) rv_sha(rv_df_text(df))

# Packages a piece of code visibly uses: library(x), require(x), x::y.
rv_code_pkgs <- function(code) {
  m1 <- regmatches(code, gregexpr("(?:library|require)\\(\\s*[\"']?([A-Za-z][A-Za-z0-9.]*)", code, perl = TRUE))[[1]]
  m1 <- sub(".*\\(\\s*[\"']?", "", m1)
  m2 <- regmatches(code, gregexpr("([A-Za-z][A-Za-z0-9.]*)::", code))[[1]]
  m2 <- sub("::$", "", m2)
  sort(unique(c(m1, m2)))
}

# Environment fingerprint text: R version plus the version of every package the code uses.
rv_env_text <- function(code, extra = character(), simulate = FALSE) {
  pk <- sort(unique(c(rv_code_pkgs(code), extra)))
  pk <- pk[nzchar(pk)]
  ver <- vapply(pk, function(p) tryCatch(as.character(utils::packageVersion(p)), error = function(e) "not-installed"), "")
  paste0("R ", getRversion(), if (length(pk)) paste0("; ", paste(pk, ver, collapse = ", ")) else "",
         if (simulate) "; SIMULATED package update" else "")
}
