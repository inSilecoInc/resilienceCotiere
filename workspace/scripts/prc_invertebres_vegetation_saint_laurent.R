prc_invertebres_vegetation_saint_laurent <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/invertebres_vegetation_saint_laurent-1.0.0/processed/"
  # input_path <- "workspace/data/harvested/invertebres_vegetation_saint_laurent-1.0.0/raw/"
  # input_files <- file.path(
  #   input_path,
  #   c(
  #     "dictionnaire_donnees.csv",
  #     "evenements_cpd.csv",
  #     "occurrences_cpd.csv"
  #    )
  # )
  input_files <- unlist(input_files)

  # Events
  events <- input_files[grepl("evenements_cpd.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names()


  # Occurrences
  suppressWarnings({
    occurrences <- input_files[grepl("occurrences_cpd.csv", input_files)] |>
      vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
      janitor::clean_names()
  })

  # Dictionnaire
  suppressWarnings({
    dictionnaire <- input_files[grepl("dictionnaire_donnees.csv", input_files)] |>
      vroom::vroom(progress = FALSE, show_col_types = FALSE, skip = 1, delim = ";") |>
      janitor::clean_names()
  })


  # Export
  vroom::vroom_write(events, file.path(output_path, "evenements_cpd.csv"), delim = ",")
  vroom::vroom_write(occurrences, file.path(output_path, "occurrences_cpd.csv"), delim = ",")
  vroom::vroom_write(dictionnaire, file.path(output_path, "dictionnaire_donnees.csv"), delim = ",")
}
