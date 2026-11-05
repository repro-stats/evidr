# Step 5: replay check, stale reviews and re-review.

mod_replay_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("5. Replay check"),
    p(class = "small text-muted", "Re-run the saved script in a fresh R process and compare four fingerprints with the logged ones: code, data, environment and output. If anything differs, the earlier review is marked stale."),
    checkboxInput(ns("sim_data"), "Demo: edit one value in the data file on disk first", FALSE, width = "100%"),
    checkboxInput(ns("sim_env"), "Demo: pretend a package was updated (simulated)", FALSE, width = "100%"),
    div(class = "rv-btn", actionButton(ns("replay"), "Replay check", class = "btn-primary btn-sm")),
    p(class = "small text-muted mt-2", "Run it once with both boxes cleared for a clean pass, then with a box ticked to see a difference."),
    uiOutput(ns("msg")), uiOutput(ns("drift")),
    conditionalPanel("output.stale", div(class = "rv-btn", actionButton(ns("rereview"), "Re-review the changed state", class = "btn-primary btn-sm mt-2"))))
}

mod_replay_server <- function(id, state, review_id = "review") {
  moduleServer(id, function(input, output, session) {
    s <- state$s
    output$msg <- renderUI(state$msgs$replay)
    output$drift <- renderUI(if (nzchar(state$msgs$drift)) tags$details(class = "rv-ref", tags$summary("reproducr check_drift output"), tags$pre(class = "rv-aud", state$msgs$drift)))
    observeEvent(state$reset(), { updateCheckboxInput(session, "sim_data", value = FALSE); updateCheckboxInput(session, "sim_env", value = FALSE) }, ignoreInit = TRUE)
    observeEvent(input$replay, {
      r <- rv_replay(s, input$sim_data, input$sim_env)
      kind <- if (!isTRUE(r$ok)) "bad" else if (isTRUE(r$dup)) "info" else if (isTRUE(r$match)) "" else "bad"
      state$msgs$replay <- rv_msg(r, kind); state$msgs$drift <- r$drift %||% ""; state$bump()
    })
    observeEvent(input$rereview, {
      r <- rv_rereview(s)
      state$msgs$replay <- NULL; state$msgs$review <- rv_msg(r, "info"); state$msgs$drift <- ""
      updateCheckboxInput(session, "sim_data", value = FALSE); updateCheckboxInput(session, "sim_env", value = FALSE)
      state$bump(); session$sendCustomMessage("rv-scroll", paste0(review_id, "-packet_tabs"))
    })
  })
}
