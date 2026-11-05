# A small line diff in base R (longest common subsequence). No external dependency.

rv_diff_lines <- function(a, b) {
  a <- if (length(a) == 1 && grepl("\n", a)) strsplit(a, "\n", fixed = TRUE)[[1]] else as.character(a)
  b <- if (length(b) == 1 && grepl("\n", b)) strsplit(b, "\n", fixed = TRUE)[[1]] else as.character(b)
  n <- length(a); m <- length(b)
  L <- matrix(0L, n + 2, m + 2)
  for (i in n:1) for (j in m:1) L[i, j] <- if (identical(a[i], b[j])) L[i + 1, j + 1] + 1L else max(L[i + 1, j], L[i, j + 1])
  op <- character(); tx <- character(); i <- 1; j <- 1
  while (i <= n && j <= m) {
    if (identical(a[i], b[j])) { op <- c(op, "same"); tx <- c(tx, a[i]); i <- i + 1; j <- j + 1 }
    else if (L[i + 1, j] >= L[i, j + 1]) { op <- c(op, "del"); tx <- c(tx, a[i]); i <- i + 1 }
    else { op <- c(op, "add"); tx <- c(tx, b[j]); j <- j + 1 }
  }
  while (i <= n) { op <- c(op, "del"); tx <- c(tx, a[i]); i <- i + 1 }
  while (j <= m) { op <- c(op, "add"); tx <- c(tx, b[j]); j <- j + 1 }
  data.frame(op = op, text = tx, stringsAsFactors = FALSE)
}

rv_diff_summary <- function(d) c(added = sum(d$op == "add"), removed = sum(d$op == "del"), unchanged = sum(d$op == "same"))

rv_diff_html <- function(d, empty = "No differences.") {
  if (!nrow(d) || all(d$op == "same")) return(htmltools::tags$p(class = "rv-nodiff", empty))
  mark <- c(same = "  ", add = "+ ", del = "- ")
  lines <- lapply(seq_len(nrow(d)), function(i)
    htmltools::tags$span(class = paste0("rv-l rv-", d$op[i]), paste0(mark[[d$op[i]]], d$text[i])))
  htmltools::tags$pre(class = "rv-diff", lines)
}

# Cell-level comparison of two result tables, used for the independent check.
rv_compare_tables <- function(a, b) {
  if (!is.data.frame(a) || !is.data.frame(b)) return(list(equal = FALSE, detail = "One of the results is not a data frame."))
  ta <- rv_df_text(a); tb <- rv_df_text(b)
  if (identical(ta, tb)) return(list(equal = TRUE, detail = "The two results are identical."))
  if (!identical(names(a), names(b))) return(list(equal = FALSE, detail = paste0("Different columns: ", paste(names(a), collapse = ", "), " versus ", paste(names(b), collapse = ", "), ".")))
  if (nrow(a) != nrow(b)) return(list(equal = FALSE, detail = paste0("Different number of rows: ", nrow(a), " versus ", nrow(b), ".")))
  ch <- 0L; where <- character()
  for (nm in names(a)) {
    x <- as.character(a[[nm]]); y <- as.character(b[[nm]]); bad <- which(!(x == y | (is.na(x) & is.na(y))))
    ch <- ch + length(bad); if (length(bad)) where <- c(where, paste0(nm, " (", length(bad), ")"))
  }
  list(equal = FALSE, detail = paste0(ch, " of ", nrow(a) * ncol(a), " cells differ. Columns: ", paste(where, collapse = ", "), "."))
}
