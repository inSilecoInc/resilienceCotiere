fig_events_ecosystems <- function(input_files, output_path) {
  # output_path <- "workspace/data/analyzed/figures_events_ecosystems-1.0.0"
  # input_path <- "workspace/data/"
  # input_files <- file.path(
  #   input_path,
  #   c(
  #     "harvested/resilience_cotiere-1.0.0/processed/resilience_cotiere_ecosystemes_simple.gpkg",
  #     "analyzed/biodiversity_data-1.0.0/biodiversity_data.sqlite",
  #     "analyzed/indicateurs_ecosystemes-1.0.0/indicateurs_ecosystemes.csv"
  #    )
  # )

  input_files <- unlist(input_files)

  # Load ecosystem data
  ecosystem <- input_files[grep("ecosystemes_simple.gpkg", input_files)] |>
    sf::st_read(quiet = TRUE) |>
    sf::st_transform(crs = 4326) |>
    sf::st_make_valid()

  # Project
  meta <- get_tbl(input_files, "metadata") |>
    dplyr::collect() |>
    dplyr::select(project_id, project_name)

  # Load and join event data
  eveco <- get_tbl(input_files, "event_ecosystem") |>
    dplyr::collect() |>
    dplyr::left_join(sf::st_drop_geometry(ecosystem), by = "ecosystem_id")

  events <- get_tbl(input_files, "events") |>
    dplyr::collect() |>
    dplyr::left_join(eveco, by = "event_id", relationship = "many-to-many") |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

  events_ecosystems <- events |>
    dplyr::filter(!is.na(ecosystem))

  biodiversity <- get_tbl(input_files, "biodiversity") |>
    dplyr::select(event_id, measurement_type) |>
    dplyr::distinct() |>
    dplyr::mutate(value = 1) |> # Assign 1 to existing measurements
    tidyr::pivot_wider(
      names_from = measurement_type,
      values_from = value,
      values_fill = list(value = 0) # Fill missing values with 0
    ) |>
    dplyr::left_join(eveco, by = "event_id", relationship = "many-to-many") |>
    dplyr::left_join(
      get_tbl(input_files, "events") |>
        dplyr::select(project_id, event_id, longitude, latitude) |>
        dplyr::collect(),
      by = "event_id",
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(project_id = tolower(project_id)) |>
    dplyr::left_join(meta, by = "project_id") |>
    dplyr::relocate(project_id, project_name, event_id, ecosystem_id, ecosystem) |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

  # Indicators
  indicators <- input_files[grep("indicateurs_ecosystemes.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    dplyr::select(event_id, indicator, ecosystem_id, ecosystem) |>
    dplyr::mutate(value = 1) |> # Assign 1 to existing measurements
    tidyr::pivot_wider(
      names_from = indicator,
      values_from = value,
      values_fill = list(value = 0) # Fill missing values with 0
    ) |>
    dplyr::left_join(
      get_tbl(input_files, "events") |>
        dplyr::select(project_id, event_id, longitude, latitude) |>
        dplyr::collect(),
      by = "event_id",
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(project_id = tolower(project_id)) |>
    dplyr::left_join(meta, by = "project_id") |>
    dplyr::relocate(project_id, project_name, event_id, ecosystem_id, ecosystem) |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)



  # # Check and drop invalid geometries
  # ecosystem <- ecosystem[sf::st_is_valid(ecosystem), ]

  # # Define bounding boxes for regions
  # regions <- list(
  #   "Estuary_between_Ile_dOrleans_BaieComeau" = sf::st_bbox(
  #     c(xmin = -71.5, xmax = -68.5, ymin = 47.0, ymax = 49.0),
  #     crs = 4326
  #   ),
  #   "Gaspesie_Baie_des_Chaleurs" = sf::st_bbox(c(xmin = -66.5, xmax = -64.0, ymin = 47.5, ymax = 49.0), crs = 4326),
  #   "Cote_Nord_Sept_Iles_Kegaska" = sf::st_bbox(c(xmin = -67.5, xmax = -61.5, ymin = 49.0, ymax = 51.5), crs = 4326)
  # )

  # # Function to plot a region using tmap basemaps
  # plot_region_tmap <- function(region_name, bbox) {
  #   library(tmap)
  #   event_filtered <- event |> sf::st_crop(bbox)
  #   ecosystem_filtered <- ecosystem |> sf::st_crop(bbox)

  #   tm <- tm_shape(ecosystem_filtered) +
  #     tm_polygons(
  #       fill = "ecosystem",
  #       fill.scale = tm_scale(values = "brewer.set3"), # Fix palette placement
  #       fill_alpha = 1,
  #       lwd = 0.2,
  #       fill.legend = tm_legend(title = "Écosystèmes") # Fix legend title placement
  #     ) +
  #     tm_shape(event_filtered) +
  #     tm_dots(
  #       fill = "black", # Fix fill color
  #       lwd = 0.3,
  #       fill_alpha = 0.1,
  #       size = 0.2
  #     ) +
  #     tm_title(stringr::str_replace_all(region_name, "_", " ")) + # Fix title placement
  #     tm_basemap(server = "CartoDB.Positron") # Uses OSM basemap

  #   tmap_save(tm, filename = file.path(output_path, paste0(region_name, ".png")), width = 8, height = 6, dpi = 300)
  # }

  # # Generate maps for each region
  # for (name in names(regions)) {
  #   plot_region_tmap(name, regions[[name]])
  # }

  # ------------------------------------
  # Leaflet maps
  # ------------------------------------
  # Map 1 - events & ecosystems
  map <- mapview::mapview(ecosystem, zcol = "ecosystem", layer.name = "Écosystèmes") +
    mapview::mapview(events, zcol = "ecosystem", layer.name = "Évènements", legend = FALSE, hide = TRUE) +
    mapview::mapview(biodiversity, zcol = "ecosystem", layer.name = "Biodiversité", legend = FALSE, hide = TRUE) +
    mapview::mapview(indicators, zcol = "ecosystem", layer.name = "Indicateurs", legend = FALSE, hide = TRUE)


  # Define output path
  tmp <- file.path(output_path, "events_ecosystems")
  dir.create(tmp, showWarnings = FALSE, recursive = TRUE)

  # Save the map
  mapview::mapshot(map, url = file.path(tmp, "projet_indicateurs.html"))

  # Define zip file path
  zip_path <- file.path(output_path, "projet_indicateurs.zip")

  # Create ZIP file from the directory
  archive::archive_write_dir(zip_path, tmp)

  # Clean up temporary files
  fs::dir_delete(tmp)
}
