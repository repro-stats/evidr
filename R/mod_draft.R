# Step 3: capture the AI draft. This also builds the review packet.

mod_draft_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("3. Capture the AI draft"),
    p(class = "small text-muted", "The prompt, the model and the output are logged, then the static gate and the review packet are built automatically."),
    div(class = "rv-aud", textOutput(ns("prompt"), inline = TRUE)),
    radioButtons(ns("source"), "Draft source", rv_draft_sources(), width = "100%"),
    div(class = "rv-btn", actionButton(ns("capture"), "Capture draft and build the packet", class = "btn-primary btn-sm")),
    uiOutput(ns("msg")))
}

mod_draft_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    s <- state$s
    output$prompt <- renderText({ state$tick(); (s$task %||% rv_task(state$task_id()))$prompt })
    output$msg <- renderUI(state$msgs$draft)
    observeEvent(input$capture, {
      r <- rv_draft(s, input$source)
      state$msgs$draft <- rv_msg(r, if (r$ok && !s$packet$gate$pass) "warn")
      state$bump()
      if (isTRUE(r$ok)) state$select_tab("Code diff")
    })
  })
}
