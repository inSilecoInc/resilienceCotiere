fig_events_ecosystems <- function(input_files, output_path) {
  input_files <- unlist(input_files)

  # Load ecosystem data
  ecosystem <- input_files[grep("ecosystemes_simple.gpkg", input_files)] |>
    sf::st_read(quiet = TRUE) |>
    sf::st_transform(crs = 4326) |>
    sf::st_make_valid()

  # Load and join event data
  eveco <- get_tbl(input_files, "event_ecosystem") |>
    dplyr::collect() |>
    dplyr::left_join(sf::st_drop_geometry(ecosystem), by = "ecosystem_id")

  event <- get_tbl(input_files, "events") |>
    dplyr::collect() |>
    dplyr::left_join(eveco, by = "event_id", relationship = "many-to-many") |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
    dplyr::select(ecosystem) |>
    na.omit()


  # Check and drop invalid geometries
  ecosystem <- ecosystem[sf::st_is_valid(ecosystem), ]

  # Define bounding boxes for regions
  regions <- list(
    "Estuary_between_Ile_dOrleans_BaieComeau" = sf::st_bbox(
      c(xmin = -71.5, xmax = -68.5, ymin = 47.0, ymax = 49.0),
      crs = 4326
    ),
    "Gaspesie_Baie_des_Chaleurs" = sf::st_bbox(c(xmin = -66.5, xmax = -64.0, ymin = 47.5, ymax = 49.0), crs = 4326),
    "Cote_Nord_Sept_Iles_Kegaska" = sf::st_bbox(c(xmin = -67.5, xmax = -61.5, ymin = 49.0, ymax = 51.5), crs = 4326)
  )

  # Function to plot a region using tmap basemaps
  plot_region_tmap <- function(region_name, bbox) {
    library(tmap)
    event_filtered <- event |> sf::st_crop(bbox)
    ecosystem_filtered <- ecosystem |> sf::st_crop(bbox)

    tm <- tm_shape(ecosystem_filtered) +
      tm_polygons(
        fill = "ecosystem",
        fill.scale = tm_scale(values = "brewer.set3"), # Fix palette placement
        fill_alpha = 1,
        lwd = 0.2,
        fill.legend = tm_legend(title = "Écosystèmes") # Fix legend title placement
      ) +
      tm_shape(event_filtered) +
      tm_dots(
        fill = "black", # Fix fill color
        lwd = 0.3,
        fill_alpha = 0.1,
        size = 0.2
      ) +
      tm_title(stringr::str_replace_all(region_name, "_", " ")) + # Fix title placement
      tm_basemap(server = "CartoDB.Positron") # Uses OSM basemap

    tmap_save(tm, filename = file.path(output_path, paste0(region_name, ".png")), width = 8, height = 6, dpi = 300)
  }

  # Generate maps for each region
  for (name in names(regions)) {
    plot_region_tmap(name, regions[[name]])
  }

  # ------------------------------------
  # Leaflet map
  # ------------------------------------
  # Map
  eco <- dplyr::select(ecosystem, ecosystem)
  map <- mapview::mapview(event, legend = FALSE) + eco

  # Define output path
  tmp <- file.path(output_path, "events_ecosystems")
  dir.create(tmp, showWarnings = FALSE, recursive = TRUE)

  # Save the map
  mapview::mapshot(map, url = file.path(tmp, "events_ecosystems.html"))

  # Define zip file path
  zip_path <- file.path(output_path, "events_ecosystems.zip")

  # Create ZIP file from the directory
  archive::archive_write_dir(zip_path, tmp)

  # Clean up temporary files
  fs::dir_delete(tmp)
}
