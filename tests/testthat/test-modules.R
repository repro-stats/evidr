test_that("the scope module records a scope and shows the message", {
  skip_on_cran()
  state <- NULL
  server <- function(input, output, session) { state <<- new_state(tempfile("m"), "jsmith"); mod_scope_server("scope", state) }
  shiny::testServer(server, {
    session$setInputs(`scope-task` = "cfb", `scope-tier` = "primary", `scope-scoper` = "jsmith", `scope-scope` = 1)
    expect_equal(state$s$step, 1L)
    expect_equal(state$s$needed, 2L)
    expect_match(as.character(state$msgs$scope), "Scope recorded")
  })
})

test_that("the app server keeps one review per session", {
  skip_on_cran()
  a <- NULL; b <- NULL
  shiny::testServer(function(input, output, session) { a <<- new_state(tempfile("a"), "u1") }, {})
  shiny::testServer(function(input, output, session) { b <<- new_state(tempfile("b"), "u2") }, {})
  rv_scope(a$s, "cfb", "support", "u1")
  expect_equal(a$s$step, 1L); expect_equal(b$s$step, 0L)
})

test_that("messages are built from engine results", {
  expect_null(rv_msg(NULL))
  expect_match(as.character(rv_msg(list(ok = FALSE, message = "no"))), "rv-verdict bad")
  expect_match(as.character(rv_msg(list(ok = TRUE, message = "yes"))), "yes")
})

test_that("the default work folder is temporary and nothing is written to the working directory", {
  expect_match(normalizePath(rv_default_dir(), mustWork = FALSE), normalizePath(tempdir()), fixed = TRUE)
})
