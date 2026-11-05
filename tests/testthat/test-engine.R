test_that("the task bank is internally consistent", {
  skip_on_cran()
  for (id in names(rv_tasks())) for (v in c("v2", "v3")) {
    t <- rv_task(id); f <- tempfile(fileext = ".csv"); utils::write.csv(t$make_data(v), f, row.names = FALSE)
    run <- function(code) rv_run(code, f)
    base <- run(t$baseline); good <- run(t$drafts$correct); bad <- run(t$drafts$planted); ref <- run(t$reference)
    expect_true(all(base$ok, good$ok, bad$ok, ref$ok), info = paste(id, v))
    expect_identical(good$sha, base$sha, info = paste(id, v, "correct draft == baseline"))
    expect_identical(good$sha, ref$sha, info = paste(id, v, "correct draft == independent implementation"))
    expect_false(identical(bad$sha, good$sha), info = paste(id, v, "planted error changes the output"))
    expect_true(all(vapply(list(t$baseline, t$drafts$correct, t$drafts$planted, t$reference), function(x) rv_static_gate(x)$pass, NA)))
  }
})

test_that("the line diff finds additions, removals and no change", {
  d <- rv_diff_lines(c("a", "b", "c"), c("a", "x", "c", "d"))
  expect_equal(unname(rv_diff_summary(d)), c(2L, 1L, 2L))
  expect_equal(d$op, c("same", "del", "add", "same", "add"))
  expect_true(all(rv_diff_lines("x", "x")$op == "same"))
  expect_true(nrow(rv_diff_lines(character(), c("a"))) == 1)
})

test_that("the static gate blocks risky calls and ignores comments", {
  expect_false(rv_static_gate("result <- system('ls')")$pass)
  expect_false(rv_static_gate("download.file('http://x', 'y'); result <- data.frame(a = 1)")$pass)
  expect_false(rv_static_gate("x <- 1")$pass)                                   # never assigns result
  expect_false(rv_static_gate("result <- (")$pass)                              # does not parse
  expect_true(rv_static_gate("# no system() here\nresult <- data.frame(a = 1)")$pass)
})

test_that("the runner isolates errors and enforces a time limit", {
  skip_on_cran()
  f <- tempfile(fileext = ".csv"); utils::write.csv(data.frame(a = 1), f, row.names = FALSE)
  expect_false(rv_run("stop('boom'); result <- 1", f)$ok)
  expect_match(rv_run("result <- 1", f)$error, "data frame")
  r <- rv_run("Sys.sleep(5); result <- data.frame(a = 1)", f, timeout = 1)
  expect_false(r$ok); expect_match(r$error, "Stopped after")
  expect_true(rv_run("result <- read.csv(data_file)", f)$ok)
})

test_that("compare_tables explains differences", {
  a <- data.frame(x = 1:3, y = c("a", "b", "c")); b <- a; b$y[2] <- "z"
  expect_true(rv_compare_tables(a, a)$equal)
  expect_match(rv_compare_tables(a, b)$detail, "1 of 6 cells differ")
  expect_match(rv_compare_tables(a, a[1:2, ])$detail, "rows")
})
