# The evidence log: the on-screen trail, chain verification, tamper test, packet download and reset.

mod_ledger_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header(class = "d-flex justify-content-between align-items-center", "Evidence log", uiOutput(ns("status"), inline = TRUE)),
    p(class = "small text-muted", "Written to a regulog file as you go. Verify reads the file again from disk."),
    div(id = ns("trail_box"), class = "rv-scroll", uiOutput(ns("trail"))),
    div(class = "mt-2 d-flex gap-2 flex-wrap",
        actionButton(ns("verify"), "Verify chain", class = "btn-sm"), actionButton(ns("tamper"), "Tamper test (on a copy)", class = "btn-sm"),
        downloadButton(ns("export"), "Download packet", class = "btn-sm"), actionLink(ns("reset"), "Reset", class = "ms-auto align-self-center")),
    uiOutput(ns("msg")),
    p(class = "small text-muted mt-2", "Hash chains reveal edits; they do not prevent them. Keep the latest hash outside this system. Times are UTC."))
}

mod_ledger_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    s <- state$s
    output$msg <- renderUI(state$msgs$ledger)
    output$trail <- renderUI({ state$tick(); if (!nrow(s$trail)) div(class = "rv-nodiff", "No entries yet. Record the scope to start the chain.") else rv_trail_table(rv_trail(s)) })
    output$status <- renderUI({
      state$tick(); v <- rv_verify(s)
      if (is.na(v$intact)) span(class = "rv-badge rv-info", "Chain empty") else span(class = paste("rv-badge", if (v$intact) "rv-ok" else "rv-bad"), v$message)
    })
    observeEvent(state$tick(), session$sendCustomMessage("rv-bottom", session$ns("trail_box")), ignoreInit = TRUE)
    observeEvent(input$verify, {
      v <- rv_verify(s)
      state$msgs$ledger <- rv_msg(list(ok = TRUE, message = sprintf("Read the log file from disk: %s. The screen shows %d entries.", v$message, nrow(s$trail))), if (isTRUE(v$intact)) "" else "bad")
      state$bump()
    })
    observeEvent(input$tamper, { r <- rv_tamper_test(s); state$msgs$ledger <- rv_msg(r, if (isTRUE(r$ok) && isFALSE(r$intact)) "" else "info") })
    observeEvent(input$reset, {
      rv_reset(s)
      for (k in c("scope", "data", "draft", "review", "replay", "edit", "ledger")) state$msgs[[k]] <- NULL
      state$msgs$drift <- ""; state$editing(FALSE); state$reset(state$reset() + 1); state$bump()
    })
    output$export <- downloadHandler(
      filename = function() paste0("review-packet-", s$id, ".tar.gz"),
      content = function(file) { r <- rv_export(s); if (isTRUE(r$ok)) file.copy(r$path, file, overwrite = TRUE) else writeLines(r$message, file) })
  })
}
