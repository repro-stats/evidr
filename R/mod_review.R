# Step 4: the review packet and the decision.

mod_review_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("4. Review the packet"),
    uiOutput(ns("status")),
    bslib::navset_tab(id = ns("packet_tabs"),
      bslib::nav_panel("Code diff", uiOutput(ns("code_diff"))),
      bslib::nav_panel("Output diff", uiOutput(ns("out_diff"))),
      bslib::nav_panel("Independent check", uiOutput(ns("check"))),
      bslib::nav_panel("Gate and audit", uiOutput(ns("gate"))),
      bslib::nav_panel("Code", uiOutput(ns("code")))),
    hr(),
    p(class = "small text-muted", "A named person accepts, edits or rejects the draft. Edits and rejections need a reason. Reviewers must differ from each other and from the person who scoped the task. Names are not authenticated."),
    bslib::layout_columns(col_widths = c(5, 7),
      textInput(ns("reviewer"), "Reviewer", placeholder = "not the scoper", width = "100%"),
      textInput(ns("reason"), "Reason (required for Edit and Reject)", placeholder = "For example: censoring flag inverted", width = "100%")),
    div(class = "btn-group rv-btn", role = "group", actionButton(ns("accept"), "Accept"), actionButton(ns("edit"), "Edit"), actionButton(ns("reject"), "Reject")),
    conditionalPanel("output.editing", ns = ns,
      div(id = ns("edit_wrap"), class = "mt-3",
          textAreaInput(ns("edit_area"), "Edited code", rows = 10, width = "100%"),
          actionButton(ns("submit_edit"), "Submit edit", class = "btn-primary btn-sm"), actionButton(ns("cancel_edit"), "Cancel", class = "btn-sm"),
          uiOutput(ns("msg_edit")))),
    uiOutput(ns("msg")))
}

mod_review_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    s <- state$s
    output$editing <- reactive(state$editing()); outputOptions(output, "editing", suspendWhenHidden = FALSE)
    output$msg <- renderUI(state$msgs$review); output$msg_edit <- renderUI(state$msgs$edit)

    output$status <- renderUI({
      state$tick()
      if (isTRUE(s$stale)) div(class = "rv-stale", "Review status: STALE. The ", paste(s$pending$diff, collapse = " and "), " changed after the earlier review, so it no longer covers the current run.")
      else if (s$step == 4L) div(class = "rv-verdict", "Review status: current. It covers data ", rv_short(s$data_sha), " and environment ", rv_short(s$env_sha), ".")
    })
    output$code_diff <- renderUI({
      state$tick(); req(s$packet); d <- rv_diff_summary(s$packet$code_diff)
      tagList(p(class = "small", sprintf("Against the accepted baseline for this task: %d added, %d removed, %d unchanged lines.", d[["added"]], d[["removed"]], d[["unchanged"]])),
              rv_diff_html(s$packet$code_diff, "The draft is identical to the accepted baseline."))
    })
    output$out_diff <- renderUI({
      state$tick(); req(s$packet); pk <- s$packet
      if (is.null(pk$out_diff)) return(p("The output could not be compared: ", pk$run$error %||% pk$base$error %||% "unknown reason."))
      tagList(p(class = "small", "Both the accepted baseline and this code were run on the current data. The diff shows what the code change does to the result: ", tags$b(pk$change$detail)),
              rv_diff_html(pk$out_diff, "The output is identical to the accepted baseline."))
    })
    output$check <- renderUI({
      state$tick(); req(s$packet)
      if (!isTRUE(s$show_check) && s$step < 4L) return(div(class = "rv-verdict info", "Hidden during a blind drill. The independent check is revealed after your decision."))
      show <- function(pk, who) {
        a <- pk$agree$equal
        tagList(tags$span(class = paste("rv-badge", if (isTRUE(a)) "rv-ok" else if (isFALSE(a)) "rv-bad" else "rv-warn"),
                          if (isTRUE(a)) "Matches the independent implementation" else if (isFALSE(a)) "Disagrees with the independent implementation" else "Not compared"),
                p(class = "small mt-1", who, pk$agree$detail))
      }
      tagList(show(s$packet, "The draft: "), if (!is.null(s$edit_packet)) show(s$edit_packet, "Your edited code: "),
              p(class = "small text-muted", "The independent implementation was written separately and run on the same data. Agreement is evidence, not proof: two implementations can share a misunderstanding of the specification."),
              tags$details(class = "rv-ref", tags$summary("Show the independent implementation"), tags$pre(class = "rv-aud", rv_code(s$task$reference))))
    })
    output$gate <- renderUI({
      state$tick(); req(s$packet); g <- s$packet$gate
      tagList(span(class = paste("rv-badge", if (g$pass) "rv-ok" else "rv-bad"), if (g$pass) "Static gate passed" else "Static gate blocked"),
              if (length(g$findings)) tags$ul(lapply(g$findings, tags$li)),
              if (length(g$warnings)) tags$ul(lapply(g$warnings, function(w) tags$li(class = "text-warning", w))),
              tags$details(class = "rv-ref", open = NA, tags$summary("reproducr audit"), tags$pre(class = "rv-aud", s$packet$audit)))
    })
    output$code <- renderUI({ state$tick(); req(s$packet); tags$pre(class = "rv-aud", rv_code(s$final_code %||% s$draft_code)) })

    observeEvent(state$select_tab(), { req(state$select_tab()); updateTabsetPanel(session, "packet_tabs", selected = state$select_tab()) })
    observeEvent(state$reset(), { updateTextInput(session, "reason", value = "") }, ignoreInit = TRUE)

    decide_msg <- function(r) {
      if (!isTRUE(r$ok)) return(rv_msg(r))
      kind <- if (!is.null(r$drill) && isTRUE(r$drill$planted) && !grepl("^caught", r$drill$outcome)) "warn" else ""
      extra <- if (!is.null(r$drill)) tagList(tags$br(), tags$b("Drill result: "), r$drill$outcome, ". ", r$drill$note)
      div(class = paste("rv-verdict", kind), r$message, extra)
    }
    observeEvent(input$accept, { r <- rv_decide(s, input$reviewer, "accepted"); state$msgs$review <- decide_msg(r); state$bump() })
    observeEvent(input$reject, { r <- rv_decide(s, input$reviewer, "rejected", input$reason); state$msgs$review <- decide_msg(r); state$bump() })
    observeEvent(input$edit, {
      bad <- if (s$step < 3L) "Capture the AI draft first." else rv_check_reviewer(s, input$reviewer)
      if (is.null(bad) && !nzchar(rv_norm(input$reason))) bad <- "Give a reason for an edit or a rejection."
      if (!is.null(bad)) { state$msgs$review <- rv_msg(rv_fail(bad)); return() }
      state$msgs$review <- NULL; state$msgs$edit <- NULL; state$editing(TRUE)
      updateTextAreaInput(session, "edit_area", value = rv_code(s$draft_code))
      session$sendCustomMessage("rv-scroll", session$ns("edit_wrap"))
    })
    observeEvent(input$cancel_edit, {
      state$editing(FALSE); state$msgs$edit <- NULL
      state$msgs$review <- rv_msg(list(ok = TRUE, message = "Edit cancelled. Nothing was logged."), "info")
    })
    observeEvent(input$submit_edit, {
      r <- rv_decide(s, input$reviewer, "edited", input$reason, input$edit_area)
      if (!isTRUE(r$ok)) { state$msgs$edit <- rv_msg(r); return() }
      state$editing(FALSE); state$msgs$edit <- NULL; state$msgs$review <- decide_msg(r); state$bump()
    })
  })
}
