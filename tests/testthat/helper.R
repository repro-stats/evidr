new_session <- function(user = "jsmith") rv_session(dir = tempfile("rvwork"), user = user)
ready <- function(task = "km", source = "planted", tier = "support", version = "v2") {
  s <- new_session(); rv_scope(s, task, tier, "jsmith"); rv_fix_data(s, version); rv_draft(s, source); s
}
fix_km <- function(s) sub("Surv(AVAL, CNSR)", "Surv(AVAL, 1 - CNSR)", rv_code(s$draft_code), fixed = TRUE)
