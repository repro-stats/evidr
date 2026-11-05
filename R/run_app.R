#' Run the review app
#'
#' Starts a local 'shiny' app for reviewing AI-drafted R code. The app runs code in
#' separate R processes on your machine, so only review code you would be willing to
#' run. The static gate in the app is a guard, not a security sandbox.
#'
#' @param dir Folder for the work files: the accepted scripts, one sub-folder per review
#'   with its audit log, and the drill scores. Defaults to a temporary folder, so nothing
#'   is written to your working directory unless you choose a folder. Use a permanent
#'   folder to keep accepted scripts and logs between sessions.
#' @param user Name shown as the person scoping a task. Defaults to the system user.
#' @param launch.browser Passed to [shiny::runApp()].
#' @param ... Further arguments passed to [shiny::runApp()], for example `port`.
#'
#' @details
#' To use your own model, set `options(evidr.model = function(prompt) ...)` to a
#' function that returns R code as a single string. The app never calls a model by
#' itself and never stores keys.
#'
#' @return Called for its side effect of starting the app; returns `NULL` invisibly
#'   when the app stops.
#' @examples
#' if (interactive()) {
#'   run_app(dir = file.path(tempdir(), "reviews"))
#' }
#' @export
run_app <- function(dir = NULL, user = NULL, launch.browser = interactive(), ...) {
  if (is.null(dir)) {
    dir <- rv_default_dir()
    message("evidr: work files go to a temporary folder (", dir, "). Use `dir` to keep them.")
  }
  shiny::runApp(app_object(dir = dir, user = user), launch.browser = launch.browser, ...)
  invisible(NULL)
}

app_object <- function(dir, user = NULL) shiny::shinyApp(app_ui(dir = dir, user = user), app_server(dir = dir, user = user))
