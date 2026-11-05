#' evidr: Review R Code Changes with Evidence and an Audit Trail
#'
#' A local 'shiny' app in the spirit of `testthat::snapshot_review()`. A reviewer
#' sees what an AI-drafted script changes in the code and in the output, compares it
#' with an independently written implementation, and accepts, edits or rejects it.
#' Every step is written to a hash-chained audit trail with the 'regulog' package.
#' Replays recompute fingerprints of code, data, environment and output, and mark an
#' earlier review stale when any of them changed.
#'
#' Start the app with [run_app()].
#'
#' @keywords internal
#' @import shiny
"_PACKAGE"
