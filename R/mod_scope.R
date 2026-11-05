# Step 1: scope the task.

mod_scope_ui <- function(id, task_choices, tier_choices, default_user) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("1. Scope the task"),
    selectInput(ns("task"), "Task", task_choices, width = "100%", selectize = FALSE),
    bslib::layout_columns(col_widths = c(6, 6),
      selectInput(ns("tier"), "Context of use", tier_choices, selected = "support", selectize = FALSE, width = "100%"),
      textInput(ns("scoper"), "Scoped by", value = default_user, width = "100%")),
    uiOutput(ns("tier_note")),
    div(class = "rv-btn", actionButton(ns("scope"), "Record scope", class = "btn-primary btn-sm"),
        actionLink(ns("restore"), "Restore the seeded baseline for this task", class = "ms-3 small")),
    uiOutput(ns("msg")))
}

mod_scope_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    s <- state$s
    observe(state$task_id(input$task))
    output$tier_note <- renderUI({
      n <- rv_tiers[[input$tier]][["need"]]
      p(class = "small text-muted", sprintf("Illustrative rule: %s needed for this context of use.", if (n == 1) "one named reviewer is" else "two different reviewers are"))
    })
    output$msg <- renderUI(state$msgs$scope)
    observeEvent(input$scope, { r <- rv_scope(s, input$task, input$tier, input$scoper); state$msgs$scope <- rv_msg(r); state$bump() })
    observeEvent(input$restore, { rv_reset_snapshot(s, input$task); showNotification("Seeded baseline restored for this task.", type = "message") })
  })
}
