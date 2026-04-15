# This file controls rendering of documents with a bit more control than the knit button has.
# Output will appear in the "output" folder

rmarkdown::render(
  input = "./markdown/master.rmd",
  output_format = "papaja::apa6_pdf",
  output_dir = "markdown/output",
  output_file = paste0("apa7_document"),
  intermediates_dir = "markdown",
  knit_root_dir = file.path(rprojroot::find_rstudio_root_file()),
  clean = TRUE
  # params = params # can set author and date in a vector here
)

rmarkdown::render(
  input = "./markdown/master.rmd",
  output_format = "papaja::apa6_word",
  output_dir = "markdown/output",
  output_file = paste0("apa7_document"),
  intermediates_dir = "markdown",
  knit_root_dir = file.path(rprojroot::find_rstudio_root_file()),
  clean = TRUE
  # params = params # can set author and date in a vector here
)
