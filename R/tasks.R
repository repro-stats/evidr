# The task bank. Each task has:
#   make_data(version)  synthetic data, the same every time for a given version
#   baseline            the last accepted script (the "snapshot" a draft is compared with)
#   drafts$correct      what a good AI draft looks like
#   drafts$planted      the same task with one realistic, subtle error
#   reference           an independently written implementation (the "challenger")
# All code reads the CSV at `data_file` and stores a data frame in `result`.

rv_pad <- function(n) formatC(n, width = 2, flag = "0")

rv_make_cfb <- function(version) {
  rv_with_seed(11, {
    ids <- paste0("S", rv_pad(1:12)); rows <- list()
    for (id in ids) {
      base <- 20 + sample(0:29, 1); wk <- base + sample(-4:7, 1)
      rows[[length(rows) + 1]] <- data.frame(USUBJID = id, PARAMCD = "ALT", AVISIT = "Baseline", ABLFL = "Y", AVAL = base)
      rows[[length(rows) + 1]] <- data.frame(USUBJID = id, PARAMCD = "ALT", AVISIT = "Week 12", ABLFL = "", AVAL = wk)
    }
    d <- do.call(rbind, rows)
  })
  if (version == "v3") {
    d$AVAL[6] <- d$AVAL[6] + 1                                   # one corrected value
    d <- rbind(d, data.frame(USUBJID = c("S99", "S99"), PARAMCD = "ALT", AVISIT = c("Baseline", "Week 12"), ABLFL = c("Y", ""), AVAL = c(31, 36)))
  }
  d
}

rv_make_km <- function(version) {
  d <- rv_with_seed(23, {
    n <- 24
    data.frame(USUBJID = paste0("S", rv_pad(1:n)), TRT01P = rep(c("Placebo", "Active"), length.out = n),
               AVAL = 20 + sample(0:179, n), CNSR = as.integer(stats::runif(n) < 0.35))
  })
  if (version == "v3") {
    d$AVAL[6] <- d$AVAL[6] + 1
    d <- rbind(d, data.frame(USUBJID = "S99", TRT01P = "Active", AVAL = 150L, CNSR = 0L))
  }
  d
}

rv_make_ae <- function(version) {
  d <- rv_with_seed(37, {
    n <- 24; terms <- c("Headache", "Nausea", "Fatigue", "Rash")
    day <- sample(1:27, n, replace = TRUE)
    data.frame(USUBJID = paste0("S", rv_pad(1 + (seq_len(n) %% 12))), AEDECOD = terms[sample(1:4, n, replace = TRUE)],
               ASTDT = format(as.Date("2026-02-01") + day - 1), TRTSDT = "2026-02-01", TRTEDT = "2026-03-15", stringsAsFactors = FALSE)
  })
  # Boundary cases that a careless rule gets wrong: first dose day, last dose day, the +30 day window edge, and a pre-treatment event.
  d$ASTDT[c(1, 2)]  <- "2026-02-01"
  d$ASTDT[3]        <- "2026-03-15"
  d$ASTDT[4]        <- "2026-04-14"
  d$ASTDT[5]        <- "2026-01-20"
  if (version == "v3") {
    d$ASTDT[8] <- "2026-04-15"
    d <- rbind(d, data.frame(USUBJID = "S99", AEDECOD = "Nausea", ASTDT = "2026-02-01", TRTSDT = "2026-02-01", TRTEDT = "2026-03-15"))
  }
  d
}

rv_tasks <- function() {
  tk <- rv_tasks_all()
  if (!requireNamespace("survival", quietly = TRUE)) tk$km <- NULL     # the survival task needs the survival package
  tk
}

rv_tasks_all <- function() list(
  cfb = list(
    id = "cfb", title = "Derive change from baseline (ADLB)", pkgs = character(), make_data = rv_make_cfb,
    prompt = "Write R code that reads the CSV at `data_file` (ALT results, one baseline row and one Week 12 row per subject). For each subject return USUBJID, BASE (baseline value), AVAL (Week 12 value) and CHG (AVAL - BASE). Store the data frame in `result`, ordered by USUBJID.",
    baseline = c('d <- read.csv(data_file)',
                 'base <- d[d$ABLFL == "Y", c("USUBJID", "AVAL")]',
                 'names(base)[2] <- "BASE"',
                 'post <- d[d$ABLFL != "Y", c("USUBJID", "AVAL")]',
                 'result <- merge(post, base, by = "USUBJID")',
                 'result$CHG <- result$AVAL - result$BASE',
                 'result <- result[order(result$USUBJID), c("USUBJID", "BASE", "AVAL", "CHG")]'),
    drafts = list(
      correct = c('d <- read.csv(data_file)',
                  'd$BASE <- ave(ifelse(d$ABLFL == "Y", d$AVAL, NA_real_), d$USUBJID,',
                  '              FUN = function(x) x[!is.na(x)][1])',
                  'result <- d[d$ABLFL != "Y", c("USUBJID", "BASE", "AVAL")]',
                  'result$CHG <- result$AVAL - result$BASE',
                  'result <- result[order(result$USUBJID), ]',
                  'rownames(result) <- NULL'),
      planted = c('d <- read.csv(data_file)',
                  'd$BASE <- ave(ifelse(d$ABLFL == "Y", d$AVAL, NA_real_), d$USUBJID,',
                  '              FUN = function(x) x[!is.na(x)][1])',
                  'result <- d[d$ABLFL != "Y", c("USUBJID", "BASE", "AVAL")]',
                  'result$CHG <- result$BASE - result$AVAL',
                  'result <- result[order(result$USUBJID), ]',
                  'rownames(result) <- NULL'),
      planted_note = "CHG is computed as BASE - AVAL, so the sign of every change is reversed."),
    reference = c('d <- read.csv(data_file)',
                  'b <- tapply(d$AVAL[d$ABLFL == "Y"], d$USUBJID[d$ABLFL == "Y"], function(x) x[1])',
                  'w <- d[d$ABLFL != "Y", ]',
                  'result <- data.frame(USUBJID = w$USUBJID, BASE = as.numeric(b[w$USUBJID]), AVAL = w$AVAL)',
                  'result$CHG <- result$AVAL - result$BASE',
                  'result <- result[order(result$USUBJID), ]; rownames(result) <- NULL')
  ),
  km = list(
    id = "km", title = "Median survival by arm (ADTTE)", pkgs = "survival", make_data = rv_make_km,
    prompt = "Write R code that reads the CSV at `data_file` (AVAL = time, CNSR = 1 if censored, TRT01P = arm). Estimate Kaplan-Meier median survival by arm with the survival package. Store a data frame with columns arm and median in `result`.",
    baseline = c('d <- read.csv(data_file)',
                 'fit <- survival::survfit(survival::Surv(AVAL, 1 - CNSR) ~ TRT01P, data = d)',
                 's <- summary(fit)$table',
                 'result <- data.frame(arm = sub("TRT01P=", "", rownames(s)), median = unname(s[, "median"]))'),
    drafts = list(
      correct = c('d <- read.csv(data_file)',
                  'd$event <- 1 - d$CNSR',
                  'fit <- survival::survfit(survival::Surv(AVAL, event) ~ TRT01P, data = d)',
                  's <- summary(fit)$table',
                  'result <- data.frame(arm = sub("TRT01P=", "", rownames(s)), median = unname(s[, "median"]))'),
      planted = c('d <- read.csv(data_file)',
                  'fit <- survival::survfit(survival::Surv(AVAL, CNSR) ~ TRT01P, data = d)',
                  's <- summary(fit)$table',
                  'result <- data.frame(arm = sub("TRT01P=", "", rownames(s)), median = unname(s[, "median"]))'),
      planted_note = "Surv(AVAL, CNSR) passes the censoring flag (1 = censored) as the event indicator, so censored subjects count as events. The correct call is Surv(AVAL, 1 - CNSR)."),
    reference = c('d <- read.csv(data_file)',
                  'med <- function(x) {',
                  '  x <- x[order(x$AVAL), ]; n <- nrow(x); atrisk <- n; surv <- 1; out <- NA_real_',
                  '  for (i in seq_len(n)) {',
                  '    if (x$CNSR[i] == 0) surv <- surv * (1 - 1 / atrisk)',
                  '    if (is.na(out) && surv <= 0.5) out <- x$AVAL[i]',
                  '    atrisk <- atrisk - 1',
                  '  }',
                  '  out',
                  '}',
                  'arms <- sort(unique(d$TRT01P))',
                  'result <- data.frame(arm = arms, median = vapply(arms, function(a) med(d[d$TRT01P == a, ]), 0))',
                  'rownames(result) <- NULL')
  ),
  ae = list(
    id = "ae", title = "Flag treatment-emergent adverse events (ADAE)", pkgs = character(), make_data = rv_make_ae,
    prompt = "Write R code that reads the CSV at `data_file` (ISO dates ASTDT, TRTSDT, TRTEDT). Flag an adverse event as treatment-emergent (TRTEMFL = \"Y\", otherwise \"N\") when it starts on or after the first dose date and no later than 30 days after the last dose date. Store USUBJID, AEDECOD, ASTDT, TRTEMFL in `result`.",
    baseline = c('d <- read.csv(data_file)',
                 'st <- as.Date(d$ASTDT); t0 <- as.Date(d$TRTSDT); t1 <- as.Date(d$TRTEDT)',
                 'd$TRTEMFL <- ifelse(st >= t0 & st <= t1 + 30, "Y", "N")',
                 'result <- d[, c("USUBJID", "AEDECOD", "ASTDT", "TRTEMFL")]'),
    drafts = list(
      correct = c('d <- read.csv(data_file)',
                  'for (v in c("ASTDT", "TRTSDT", "TRTEDT")) d[[v]] <- as.Date(d[[v]])',
                  'in_window <- !is.na(d$ASTDT) & d$ASTDT >= d$TRTSDT & d$ASTDT <= d$TRTEDT + 30',
                  'd$TRTEMFL <- ifelse(in_window, "Y", "N")',
                  'd$ASTDT <- format(d$ASTDT)',
                  'result <- d[, c("USUBJID", "AEDECOD", "ASTDT", "TRTEMFL")]'),
      planted = c('d <- read.csv(data_file)',
                  'for (v in c("ASTDT", "TRTSDT", "TRTEDT")) d[[v]] <- as.Date(d[[v]])',
                  'in_window <- !is.na(d$ASTDT) & d$ASTDT > d$TRTSDT & d$ASTDT <= d$TRTEDT + 30',
                  'd$TRTEMFL <- ifelse(in_window, "Y", "N")',
                  'd$ASTDT <- format(d$ASTDT)',
                  'result <- d[, c("USUBJID", "AEDECOD", "ASTDT", "TRTEMFL")]'),
      planted_note = "The start condition uses > instead of >=, so events on the first dose day are not flagged as treatment-emergent."),
    reference = c('d <- read.csv(data_file)',
                  'flag <- character(nrow(d))',
                  'for (i in seq_len(nrow(d))) {',
                  '  days_from_start <- as.numeric(as.Date(d$ASTDT[i]) - as.Date(d$TRTSDT[i]))',
                  '  days_after_end  <- as.numeric(as.Date(d$ASTDT[i]) - as.Date(d$TRTEDT[i]))',
                  '  flag[i] <- if (days_from_start >= 0 && days_after_end <= 30) "Y" else "N"',
                  '}',
                  'result <- data.frame(USUBJID = d$USUBJID, AEDECOD = d$AEDECOD, ASTDT = d$ASTDT, TRTEMFL = flag)')
  )
)

rv_task <- function(id) { t <- rv_tasks_all()[[id]]; if (is.null(t)) stop("Unknown task: ", id, call. = FALSE); t }
rv_code <- function(x) paste(x, collapse = "\n")
