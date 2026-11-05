# Step 2: fix the data and environment.

mod_data_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("2. Fix the data and environment"),
    p(class = "small text-muted", "Fingerprints tie the run to an exact data file and software environment, before any code is drafted or reviewed."),
    radioButtons(ns("version"), NULL, c("Dataset v2" = "v2", "Dataset v3 (one corrected value, one added subject)" = "v3"), inline = TRUE),
    div(class = "rv-btn", actionButton(ns("record"), "Record data and environment", class = "btn-primary btn-sm")),
    uiOutput(ns("msg")), uiOutput(ns("fingerprints")))
}

mod_data_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    s <- state$s
    output$msg <- renderUI(state$msgs$data)
    output$fingerprints <- renderUI({
      state$tick(); req(s$data_sha)
      tags$table(class = "small",
        tags$tr(tags$th("Data", style = "padding-right:1rem"), tags$td(class = "rv-fp", paste0(basename(s$data_file), " ", rv_short(s$data_sha, 12)))),
        tags$tr(tags$th("Environment"), tags$td(class = "rv-fp", paste0(s$env_text, " ", rv_short(s$env_sha, 12)))))
    })
    observeEvent(input$record, { r <- rv_fix_data(s, input$version); state$msgs$data <- rv_msg(r); state$bump() })
  })
}
