# Run the app straight from the source folder, without installing the package.
#   Rscript dev-run.R
library(shiny)
for (f in list.files("R", full.names = TRUE)) source(f)
run_app(dir = file.path(getwd(), "evidr-work"))
