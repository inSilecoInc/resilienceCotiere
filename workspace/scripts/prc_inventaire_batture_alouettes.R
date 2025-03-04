prc_inventaire_batture_alouettes <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/inventaire_batture_alouettes-1.0.0/processed/"
  # input_path <- "workspace/data/harvested/inventaire_batture_alouettes-1.0.0/raw/"
  # input_files <- file.path(
  #   input_path,
  #   c(
  #     "kelp_event-data_2018-2019.csv",
  #     "kelp_occurrence-data_2018-2019.csv"
  #    )
  # )
  input_files <- unlist(input_files)

  # Events
  suppressWarnings({
    events <- input_files[grepl("kelp_event-data_2018-2019.csv", input_files)] |>
      vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
      janitor::clean_names() |>
      dplyr::mutate(
        project_id = "inventaire_batture_alouettes",
        project_name = "Caractérisation du banc de laminaires de la Batture-aux-Alouettes",
        depth_value = as.numeric(depth_value),
        sample_size_value = as.numeric(sample_size_value)
      )
  })

  # Occurrences
  suppressWarnings({
    occurrences <- input_files[grepl("kelp_occurrence-data_2018-2019.csv", input_files)] |>
      vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
      janitor::clean_names()
  })

  # Export
  vroom::vroom_write(events, file.path(output_path, "kelp_events.csv"), delim = ",")
  vroom::vroom_write(occurrences, file.path(output_path, "kelp_occurrences.csv"), delim = ",")
}
