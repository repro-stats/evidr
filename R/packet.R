# The review packet: a self-contained HTML record of one review, plus the files it refers to.

rv_css <- function() "
:root{--rv-ink:#14171f;--rv-soft:#4a5062;--rv-line:#d9dce5;--rv-ok:#0e7a4f;--rv-bad:#b3261e;--rv-warn:#a35a00;--rv-acc:#d9480f;--rv-bg:#f6f7fb}
.rv-head h1{font-size:1.5rem;margin:0}.rv-head p{color:var(--rv-soft);margin:.25rem 0 1rem;max-width:80ch}
.rv-diff{background:#0f1117;color:#e8eaf2;padding:.75rem;border-radius:6px;font-size:.82rem;line-height:1.5;overflow:auto;max-height:340px;margin:0}
.rv-l{display:block;white-space:pre}.rv-add{background:rgba(46,160,67,.28)}.rv-del{background:rgba(248,81,73,.28)}.rv-same{opacity:.75}
.rv-nodiff{color:var(--rv-soft);font-style:italic;margin:.5rem 0}
.rv-badge{display:inline-block;padding:.15rem .6rem;border-radius:4px;font-weight:600;font-size:.85rem}
.rv-ok{background:#dff3ea;color:var(--rv-ok)}.rv-bad{background:#fbe3e1;color:var(--rv-bad)}.rv-warn{background:#fdebd3;color:var(--rv-warn)}.rv-info{background:#e8ecf7;color:#33405e}
.rv-verdict{margin:.75rem 0 0;padding:.6rem .8rem;border-left:4px solid var(--rv-ok);background:#eef8f3;border-radius:4px}
.rv-verdict.bad{border-color:var(--rv-bad);background:#fdeceb}.rv-verdict.warn{border-color:var(--rv-acc);background:#fff1e6}.rv-verdict.info{border-color:#6b7bb0;background:#eef1fa}
.rv-trail{width:100%;font-size:.8rem;border-collapse:collapse}.rv-trail th{position:sticky;top:0;background:#fff;text-align:left;border-bottom:2px solid var(--rv-line);padding:.3rem .4rem}
.rv-trail td{border-bottom:1px solid var(--rv-line);padding:.3rem .4rem;vertical-align:top}.rv-trail .rv-mono{font-family:ui-monospace,Menlo,monospace;font-size:.75rem;color:var(--rv-soft)}
.card-body .rv-btn{align-self:flex-start}.rv-trail td:nth-child(4){min-width:150px}.rv-scroll{max-height:52vh;overflow:auto}.rv-fp{font-family:ui-monospace,Menlo,monospace;font-size:.78rem;word-break:break-all}
.rv-status{font-weight:600}.rv-status.ok{color:var(--rv-ok)}.rv-status.bad{color:var(--rv-bad)}
.rv-aud{white-space:pre-wrap;font-size:.8rem;background:#f1f2f7;padding:.6rem;border-radius:6px;max-height:260px;overflow:auto}
.rv-stale{border-left:4px solid var(--rv-bad);background:#fdeceb;padding:.5rem .75rem;margin-bottom:.75rem;font-weight:600}
details.rv-ref{margin-top:.5rem}details.rv-ref summary{cursor:pointer;color:var(--rv-soft)}
"

rv_trail_table <- function(tr, full = FALSE) {
  has_hash <- "hash" %in% names(tr)
  head <- htmltools::tags$tr(htmltools::tags$th("#"), htmltools::tags$th("Time (UTC)"), htmltools::tags$th("Who"), htmltools::tags$th("Event"), htmltools::tags$th("Detail"), if (has_hash) htmltools::tags$th("Hash"))
  rows <- lapply(seq_len(nrow(tr)), function(i) {
    det <- tr$detail[i]; shown <- if (!full && nchar(det) > 130) paste0(substr(det, 1, 127), "...") else det
    htmltools::tags$tr(htmltools::tags$td(tr$seq[i]), htmltools::tags$td(class = "rv-mono", substr(tr$time[i], 12, 19)),
                       htmltools::tags$td(tr$actor[i]), htmltools::tags$td(tr$event[i]),
                       htmltools::tags$td(class = "rv-mono", title = det, shown), if (has_hash) htmltools::tags$td(class = "rv-mono", tr$hash[i]))
  })
  htmltools::tags$table(class = "rv-trail", htmltools::tags$thead(head), htmltools::tags$tbody(rows))
}

rv_packet_ui <- function(s) {
  tg <- htmltools::tags; v <- rv_verify(s); p <- s$packet_final %||% s$packet
  fp <- function(k, v) tg$tr(tg$th(k), tg$td(class = "rv-fp", v))
  decisions <- if (length(s$decisions)) tg$ul(lapply(s$decisions, function(d) tg$li(tg$b(d$name), " ", d$kind, if (nzchar(d$reason)) paste0(" (reason: ", d$reason, ")")))) else tg$p("No decision recorded.")
  tg$div(
    tg$h2("Review packet"),
    tg$p(sprintf("Task: %s. Context of use: %s. Scoped by %s. Review ID %s.", s$task$title, rv_tiers[[s$tier]][["label"]], s$scoper, s$id)),
    tg$h3("Decisions"), decisions,
    tg$h3("Fingerprints"),
    tg$table(fp("Data file", paste0(basename(s$data_file), "  sha256 ", s$data_sha)), fp("Environment", paste0(s$env_text, "  sha256 ", s$env_sha)),
             fp("Reviewed code", if (!is.null(s$final_sha)) paste0("sha256 ", s$final_sha) else "not yet reviewed"),
             fp("Reviewed output", if (!is.null(s$out_sha)) paste0("sha256 ", s$out_sha) else "not yet run")),
    tg$h3("Code changes against the accepted baseline"), rv_diff_html(p$code_diff, "No differences."),
    tg$h3("Output changes (both scripts run on the current data)"), if (is.null(p$out_diff)) tg$p("Not available.") else rv_diff_html(p$out_diff, "The output is identical."),
    tg$h3("Independent check"), tg$p(p$agree$detail),
    tg$h3("Static gate"), tg$p(if (p$gate$pass) "Passed." else paste("Blocked:", paste(p$gate$findings, collapse = " "))),
    tg$h3("reproducr audit"), tg$div(class = "rv-aud", p$audit),
    tg$h3("Evidence log"), tg$p(class = paste("rv-status", if (isTRUE(v$intact)) "ok" else "bad"), v$message), rv_trail_table(rv_trail(s), full = TRUE),
    tg$p(class = "rv-nodiff", "Hash chains reveal edits; they do not prevent them. Keep the latest hash outside this system for a real deployment. Names are not authenticated by this app. The static gate is a guard, not a sandbox.")
  )
}

rv_packet_html <- function(s, file) {
  page <- htmltools::tags$html(htmltools::tags$head(htmltools::tags$meta(charset = "utf-8"), htmltools::tags$title(paste("Review packet", s$id)), htmltools::tags$style(htmltools::HTML(rv_css())),
                                                    htmltools::tags$style("body{font:15px/1.5 system-ui,sans-serif;max-width:980px;margin:2rem auto;padding:0 1rem;color:#14171f}table{border-collapse:collapse}th{text-align:left;padding-right:1rem;vertical-align:top}")),
                               htmltools::tags$body(rv_packet_ui(s)))
  htmltools::save_html(page, file); invisible(file)
}

# Write the packet and everything it refers to, and return a .tar.gz path.
rv_export <- function(s) {
  if (s$step < 3L) return(rv_fail("Nothing to export yet."))
  rv_packet_html(s, file.path(s$dir, "review-packet.html"))
  tryCatch(regulog::export_audit_trail(s$log, format = "csv", path = file.path(s$dir, "audit_trail.csv"), signed = TRUE), error = function(e) NULL)
  out <- file.path(tempdir(), paste0("review-packet-", s$id, ".tar.gz"))
  old <- setwd(dirname(s$dir)); on.exit(setwd(old), add = TRUE)
  utils::tar(out, files = basename(s$dir), compression = "gzip", tar = "internal")
  rv_ok("Packet written.", path = out)
}
