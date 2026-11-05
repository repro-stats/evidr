# Reviewer drill scores.

mod_scores_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Reviewer drill scores"),
    p(class = "small text-muted", "Blind drills hide whether a draft carries a planted error. The log commits to the answer before you decide and reveals it afterwards."),
    uiOutput(ns("table")))
}

mod_scores_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    s <- state$s
    output$table <- renderUI({
      state$tick(); sc <- rv_scores(s)
      if (is.null(sc)) return(p(class = "small", "No drills yet. Choose \"Blind drill\" as the draft source."))
      pl <- sc[sc$planted, , drop = FALSE]; idx <- rev(seq_len(nrow(sc)))[seq_len(min(nrow(sc), 6))]
      tagList(
        p(class = "small", sprintf("%d drill(s) recorded in %s. Planted errors caught: %d of %d.", nrow(sc), basename(s$root), sum(grepl("^caught", pl$outcome)), nrow(pl))),
        tags$table(class = "rv-trail",
          tags$thead(tags$tr(tags$th("Reviewer"), tags$th("Task"), tags$th("Planted"), tags$th("Decision"), tags$th("Outcome"))),
          tags$tbody(lapply(idx, function(j) tags$tr(tags$td(sc$reviewer[j]), tags$td(sc$task[j]), tags$td(sc$planted[j]), tags$td(sc$decision[j]), tags$td(sc$outcome[j]))))))
    })
  })
}
