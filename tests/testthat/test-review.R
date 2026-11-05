test_that("a full review is logged and the chain verifies", {
  skip_on_cran()
  s <- ready("km", "correct")
  expect_true(s$packet$agree$equal)
  expect_true(rv_decide(s, "asmith", "accepted")$ok)
  expect_equal(s$step, 4L)
  v <- rv_verify(s); expect_true(v$intact); expect_equal(v$n, nrow(s$trail))
})

test_that("the rules a reviewer must follow are enforced", {
  skip_on_cran()
  s <- ready("km", "planted")
  expect_match(rv_decide(s, "jsmith", "accepted")$message, "different person")
  expect_match(rv_decide(s, "JSmith  ", "accepted")$message, "different person")      # case and spacing do not help
  expect_match(rv_decide(s, "asmith", "edited", "", "x")$message, "reason")
  expect_match(rv_decide(s, "asmith", "rejected", "")$message, "reason")
  expect_match(rv_decide(s, "asmith", "edited", "why", rv_code(s$draft_code))$message, "unchanged")
  expect_match(rv_decide(s, "asmith", "edited", "why", "result <- system('ls')")$message, "static gate")
  expect_equal(s$step, 3L)                                                        # none of those logged a decision
})

test_that("primary analyses need two different reviewers", {
  skip_on_cran()
  s <- ready("cfb", "correct", tier = "primary")
  expect_match(rv_decide(s, "asmith", "accepted")$message, "second")
  expect_equal(s$step, 3L)
  expect_match(rv_decide(s, "ASMITH", "accepted")$message, "different reviewer")
  expect_true(rv_decide(s, "bjones", "accepted")$ok); expect_equal(s$step, 4L)
})

test_that("an edit that fixes a planted error agrees with the independent check", {
  skip_on_cran()
  s <- ready("km", "planted"); expect_false(s$packet$agree$equal)
  r <- rv_decide(s, "asmith", "edited", "censoring flag inverted", fix_km(s))
  expect_true(r$ok); expect_true(s$edit_packet$agree$equal)
  expect_match(paste(s$trail$event, collapse = " "), "Reviewer edit")
})

test_that("code blocked by the gate cannot be accepted", {
  skip_on_cran()
  s <- new_session(); rv_scope(s, "cfb", "support", "jsmith"); rv_fix_data(s, "v2")
  old <- getOption("evidr.model"); on.exit(options(evidr.model = old))
  options(evidr.model = function(prompt) "result <- system('ls')")
  rv_draft(s, "model")
  expect_false(s$packet$gate$pass)
  expect_match(rv_decide(s, "asmith", "accepted")$message, "static gate")
  expect_true(rv_decide(s, "asmith", "rejected", "blocked call")$ok)
})

test_that("a replay detects drift, marks the review stale and a re-review clears it", {
  skip_on_cran()
  s <- ready("ae", "correct"); rv_decide(s, "asmith", "accepted")
  expect_true(rv_replay(s)$match)
  expect_true(isTRUE(rv_replay(s)$dup))                                            # same inputs: not logged twice
  n <- nrow(s$trail)
  r <- rv_replay(s, sim_data = TRUE)
  expect_false(r$match); expect_true(all(c("data", "output") %in% r$diff)); expect_true(s$stale)
  expect_equal(nrow(s$trail), n + 2L)                                              # replay entry plus stale marker
  expect_true(rv_rereview(s)$ok); expect_equal(s$step, 3L); expect_false(s$stale)
  expect_true(rv_decide(s, "asmith", "accepted")$ok)
  expect_true(rv_replay(s)$match)
  r2 <- rv_replay(s, sim_env = TRUE); expect_true("environment" %in% r2$diff)
})

test_that("replay reads the saved script, so a changed file is caught", {
  skip_on_cran()
  s <- ready("cfb", "correct"); rv_decide(s, "asmith", "accepted")
  writeLines(c(readLines(s$final_file), "result$CHG <- -result$CHG"), s$final_file)
  r <- rv_replay(s); expect_false(r$match); expect_true("code" %in% r$diff)
})

test_that("the accepted snapshot moves on accept but not for drills or after a stale review", {
  skip_on_cran()
  s <- ready("cfb", "correct"); rv_decide(s, "asmith", "accepted")
  expect_identical(rv_baseline(s), s$final_code)
  s2 <- ready("cfb", "planted"); rv_decide(s2, "asmith", "rejected", "wrong sign")
  expect_false(any(grepl("BASE - result\\$AVAL|result\\$BASE - result\\$AVAL", rv_baseline(s2))))
  s3 <- ready("ae", "correct"); before <- rv_baseline(s3); rv_decide(s3, "asmith", "accepted"); rv_replay(s3, sim_data = TRUE)
  expect_identical(rv_baseline(s3), s3$baseline_at_draft)
})

test_that("blind drills commit first, reveal afterwards and score the decision", {
  skip_on_cran()
  s <- new_session(); rv_scope(s, "ae", "support", "jsmith"); rv_fix_data(s, "v3"); rv_draft(s, "blind")
  expect_false(s$show_check)
  expect_true(any(grepl("Drill commitment", s$trail$event)))
  planted <- s$drill$planted
  r <- rv_decide(s, "bjones", if (planted) "rejected" else "accepted", "looks wrong")
  expect_true(any(grepl("Drill revealed", s$trail$event)))
  expect_true(grepl("caught|correct accept", r$drill$outcome))
  sc <- rv_scores(s); expect_equal(nrow(sc), 1L); expect_identical(sc$planted, planted)
  expect_true(rv_verify(s)$intact)
})

test_that("the commitment matches the revealed answer", {
  skip_on_cran()
  s <- new_session(); rv_scope(s, "ae", "support", "jsmith"); rv_fix_data(s, "v3"); rv_draft(s, "blind")
  committed <- sub(".*commitment_sha256=", "", s$trail$detail[grepl("Drill commitment", s$trail$event)])
  rv_decide(s, "bjones", "accepted")
  rev <- s$trail$detail[grepl("Drill revealed", s$trail$event)]
  planted <- sub(";.*", "", sub("planted=", "", rev)); nonce <- sub(";.*", "", sub(".*nonce=", "", rev))
  expect_identical(rv_sha(paste(planted, nonce, sep = "|")), committed)
})

test_that("tampering with a copy of the log is detected and the real log stays intact", {
  skip_on_cran()
  s <- ready("km", "correct"); rv_decide(s, "asmith", "accepted")
  t <- rv_tamper_test(s); expect_false(t$intact)
  expect_true(rv_verify(s)$intact)
})

test_that("the packet exports to HTML and a tarball", {
  skip_on_cran()
  s <- ready("km", "planted"); rv_decide(s, "asmith", "edited", "censoring flag inverted", fix_km(s))
  r <- rv_export(s); expect_true(r$ok); expect_true(file.exists(r$path))
  html <- paste(readLines(file.path(s$dir, "review-packet.html")), collapse = "\n")
  expect_match(html, "Review packet"); expect_match(html, "Chain intact")
})
