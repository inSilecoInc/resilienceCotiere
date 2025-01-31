prc_yanick_gendreau <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/yanick_gendreau-1.0.0/processed/"
  # input_path <- "workspace/data/harvested/yanick_gendreau-1.0.0/raw/"
  # input_files <- file.path(
  #   input_path,
  #   c(
  #     "Liste_Sp_CindyGrant.txt",
  #     "yanick_intertidal_data_abundance.csv"
  #    )
  # )
  input_files <- unlist(input_files)

  # Intertidal
  suppressWarnings({
    intertidal <- input_files[grepl("yanick_intertidal_data_abundance.csv", input_files)] |>
      vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
      janitor::clean_names()
  })

  # Intertidal - subtidal
  suppressWarnings({
    intertidal_subtidal <- input_files[grepl("Liste_Sp_CindyGrant.txt", input_files)] |>
      vroom::vroom(progress = FALSE, show_col_types = FALSE, delim = "\t") |>
      janitor::clean_names()
  })

  # Export
  vroom::vroom_write(
    intertidal,
    file.path(output_path, "yanick_gendreau_intertidal.csv"),
    delim = ","
  )
  vroom::vroom_write(
    intertidal_subtidal,
    file.path(output_path, "yanick_gendreau_intertidal_subtidal.csv"),
    delim = ","
  )
}
