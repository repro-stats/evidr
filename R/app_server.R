# Shared state for one browser session, and the wiring of the modules.
# The engine (rv_* functions) holds the review; `tick` tells the screen to redraw after a change.

new_state <- function(dir, user) {
  tick <- shiny::reactiveVal(0)
  list(
    s = rv_session(dir = dir, user = user),
    tick = tick, bump = function() tick(tick() + 1),
    msgs = shiny::reactiveValues(scope = NULL, data = NULL, draft = NULL, review = NULL, replay = NULL, edit = NULL, ledger = NULL, drift = ""),
    task_id = shiny::reactiveVal(names(rv_tasks())[1]),
    editing = shiny::reactiveVal(FALSE),
    select_tab = shiny::reactiveVal(NULL),
    reset = shiny::reactiveVal(0)
  )
}

# A short coloured message box from an engine result.
rv_msg <- function(r, kind = NULL) {
  if (is.null(r) || is.null(r$message) || !nzchar(r$message)) return(NULL)
  k <- kind %||% if (isTRUE(r$ok)) "" else "bad"
  div(class = paste("rv-verdict", k), r$message)
}

app_server <- function(dir, user = NULL) {
  function(input, output, session) {
    state <- new_state(dir, user)               # one review per browser session, so two tabs never share state
    flag <- function(name, f) { output[[name]] <- shiny::reactive({ state$tick(); f() }); shiny::outputOptions(output, name, suspendWhenHidden = FALSE) }
    flag("step", function() state$s$step)
    flag("stale", function() isTRUE(state$s$stale))
    mod_scope_server("scope", state)
    mod_data_server("data", state)
    mod_draft_server("draft", state)
    mod_review_server("review", state)
    mod_replay_server("replay", state)
    mod_ledger_server("ledger", state)
    mod_scores_server("scores", state)
  }
}
