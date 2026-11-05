# Top-level layout. Each numbered step is a module; later steps appear as earlier ones are completed.

app_ui <- function(dir, user = NULL) {
  default_user <- user %||% getOption("evidr.user", Sys.info()[["user"]])
  tk <- rv_tasks()
  task_choices <- stats::setNames(names(tk), vapply(tk, function(t) t$title, ""))
  tier_choices <- stats::setNames(names(rv_tiers), vapply(rv_tiers, function(t) t[["label"]], ""))
  bslib::page_fluid(
    title = "evidr",
    theme = bslib::bs_theme(version = 5, primary = "#d9480f", "font-size-base" = "0.95rem"),
    tags$head(tags$style(HTML(rv_css())),
              tags$script(HTML("Shiny.addCustomMessageHandler('rv-scroll',function(id){var e=document.getElementById(id);if(e)e.scrollIntoView({behavior:'smooth',block:'center'});});
                                Shiny.addCustomMessageHandler('rv-bottom',function(id){var e=document.getElementById(id);if(e)e.scrollTop=e.scrollHeight;});"))),
    div(class = "rv-head",
        h1("evidr"),
        p("Review AI-drafted R code the way testthat::snapshot_review() reviews a snapshot: see what changed in the code and in the output, check it against an independent implementation, then decide. Every step goes into a hash-chained regulog trail. Local app; no data leaves this machine."),
        p(class = "small text-muted", "Work folder: ", tags$code(dir))),
    bslib::layout_columns(
      col_widths = c(7, 5),
      div(
        mod_scope_ui("scope", task_choices, tier_choices, default_user),
        conditionalPanel("output.step >= 1", mod_data_ui("data")),
        conditionalPanel("output.step >= 2", mod_draft_ui("draft")),
        conditionalPanel("output.step >= 3", mod_review_ui("review")),
        conditionalPanel("output.step >= 4", mod_replay_ui("replay"))
      ),
      div(mod_ledger_ui("ledger"), mod_scores_ui("scores"))
    )
  )
}
