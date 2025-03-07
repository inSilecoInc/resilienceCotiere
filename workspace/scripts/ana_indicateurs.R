ana_abondance <- function(input_files, output_path) {
  # output_path <- "workspace/data/analyzed/indicateurs-1.0.0/" # nolint
  # input_path <- "workspace/data/analyzed/biodiversity_data-1.0.0"
  # input_files <- file.path(
  #   input_path,
  #   c(
  #     "biodiversity_data.sqlite"
  #    )
  # )
  input_files <- unlist(input_files)

  # Abundance
  ab <- get_biodiversity(input_files, "abundance", "n")

  # Density
  dn <- get_biodiversity(input_files, "abundance", "n/m2")

  # Assess indicator
  indicator <- dplyr::bind_rows(
    data.frame(
      event_id = ab$event_id,
      indicator = "Abondance",
      indicator_value = rowSums(dplyr::select(ab, -event_id)),
      unit = "n"
    ),
    data.frame(
      event_id = dn$event_id,
      indicator = "Abondance",
      indicator_value = rowSums(dplyr::select(dn, -event_id)),
      unit = "n/m2"
    )
  )

  vroom::vroom_write(indicator, file.path(output_path, "abondance.csv"), delim = ",")
}

ana_biomasse <- function(input_files, output_path) {
  # output_path <- "workspace/data/analyzed/indicateurs-1.0.0/"
  # input_path <- "workspace/data/analyzed/biodiversity_data-1.0.0"
  # input_files <- file.path(
  #   input_path,
  #   c(
  #     "biodiversity_data.sqlite"
  #    )
  # )
  input_files <- unlist(input_files)

  # Biomass
  g <- get_biodiversity(input_files, "biomass", "g")

  # Biomass / m2
  g_m2 <- get_biodiversity(input_files, "biomass", "g/m2")

  # Assess indicator
  indicator <- dplyr::bind_rows(
    data.frame(
      event_id = g$event_id,
      indicator = "Biomasse",
      indicator_value = rowSums(dplyr::select(g, -event_id)),
      unit = "g"
    ),
    data.frame(
      event_id = g_m2$event_id,
      indicator = "Biomasse",
      indicator_value = rowSums(dplyr::select(g_m2, -event_id)),
      unit = "g/m2"
    )
  )

  vroom::vroom_write(indicator, file.path(output_path, "biomasse.csv"), delim = ",")
}

ana_dominance <- function(input_files, output_path) {
  input_files <- unlist(input_files)

  # Abundance
  ab <- get_biodiversity(input_files, "abundance", "n")

  # Density
  dn <- get_biodiversity(input_files, "abundance", "n/m2")

  # Compute relative abundances
  ab[, -1] <- ab[, -1] / rowSums(ab[, -1], na.rm = TRUE)
  dn[, -1] <- dn[, -1] / rowSums(dn[, -1], na.rm = TRUE)

  # Compute Simpson's dominance index
  indicator <- dplyr::bind_rows(
    data.frame(
      event_id = ab$event_id,
      indicator = "Dominance",
      indicator_value = vegan::diversity(dplyr::select(ab, -event_id), index = "simpson"),
      unit = "n"
    ),
    data.frame(
      event_id = dn$event_id,
      indicator = "Dominance",
      indicator_value = vegan::diversity(dplyr::select(dn, -event_id), index = "simpson"),
      unit = "n/m2"
    )
  )

  # Save results
  vroom::vroom_write(indicator, file.path(output_path, "dominance.csv"), delim = ",")
}

ana_richesse_specifique <- function(input_files, output_path) {
  input_files <- unlist(input_files)

  # Abundance
  ab <- get_biodiversity(input_files, "abundance", "n")

  # Density
  dn <- get_biodiversity(input_files, "abundance", "n/m2")

  # Compute species richness per event
  indicator <- dplyr::bind_rows(
    data.frame(
      event_id = ab$event_id,
      indicator = "Richesse specifique",
      indicator_value = vegan::specnumber(dplyr::select(ab, -event_id)),
      unit = "n"
    ),
    data.frame(
      event_id = dn$event_id,
      indicator = "Richesse specifique",
      indicator_value = vegan::specnumber(dplyr::select(dn, -event_id)),
      unit = "n/m2"
    )
  )

  # Save results
  vroom::vroom_write(indicator, file.path(output_path, "richesse_specifique.csv"), delim = ",")
}

ana_diversite_shannon <- function(input_files, output_path) {
  input_files <- unlist(input_files)

  # Abundance
  ab <- get_biodiversity(input_files, "abundance", "n")

  # Density
  dn <- get_biodiversity(input_files, "abundance", "n/m2")

  # Compute species richness per event
  indicator <- dplyr::bind_rows(
    data.frame(
      event_id = ab$event_id,
      indicator = "Shannon-Wiener",
      indicator_value = vegan::diversity(dplyr::select(ab, -event_id), index = "shannon"),
      unit = "n"
    ),
    data.frame(
      event_id = dn$event_id,
      indicator = "Shannon-Wiener",
      indicator_value = vegan::diversity(dplyr::select(dn, -event_id), index = "shannon"),
      unit = "n/m2"
    )
  )

  # Export des résultats
  vroom::vroom_write(indicator, file.path(output_path, "diversite_shannon.csv"), delim = ",")
}


ana_equitabilite_pielou <- function(input_files, output_path) {
  input_files <- unlist(input_files)

  # Abundance
  ab <- get_biodiversity(input_files, "abundance", "n")

  # Density
  dn <- get_biodiversity(input_files, "abundance", "n/m2")

  # Calcul de l'indice de Shannon (H')
  H_ab <- vegan::diversity(dplyr::select(ab, -event_id), index = "shannon")
  H_dn <- vegan::diversity(dplyr::select(dn, -event_id), index = "shannon")

  # Calcul de la richesse spécifique (S)
  S_ab <- vegan::specnumber(dplyr::select(ab, -event_id))
  S_dn <- vegan::specnumber(dplyr::select(dn, -event_id))

  # Calcul de l’indice d’équitabilité de Piélou (J’)
  J_ab <- ifelse(S_ab > 1, H_ab / log(S_ab), NA) # Évite division par zéro si S = 1
  J_dn <- ifelse(S_dn > 1, H_dn / log(S_dn), NA) # Évite division par zéro si S = 1

  # Créer le tableau des résultats
  indicator <- dplyr::bind_rows(
    data.frame(
      event_id = ab$event_id,
      indicator = "Equitabilite de Pielou",
      indicator_value = J_ab,
      unit = "n"
    ),
    data.frame(
      event_id = dn$event_id,
      indicator = "Equitabilite de Pielou",
      indicator_value = J_dn,
      unit = "n/m2"
    )
  )

  # Export des résultats
  vroom::vroom_write(indicator, file.path(output_path, "equitabilite_pielou.csv"), delim = ",")
}

ana_diversite_simpson <- function(input_files, output_path) {
  input_files <- unlist(input_files)

  # Abundance
  ab <- get_biodiversity(input_files, "abundance", "n")

  # Density
  dn <- get_biodiversity(input_files, "abundance", "n/m2")

  # Compute species richness per event
  indicator <- dplyr::bind_rows(
    data.frame(
      event_id = ab$event_id,
      indicator = "Simpson",
      indicator_value = vegan::diversity(dplyr::select(ab, -event_id), index = "simpson"),
      unit = "n"
    ),
    data.frame(
      event_id = dn$event_id,
      indicator = "Simpson",
      indicator_value = vegan::diversity(dplyr::select(dn, -event_id), index = "simpson"),
      unit = "n/m2"
    )
  )

  # Export des résultats
  vroom::vroom_write(indicator, file.path(output_path, "diversite_simpson.csv"), delim = ",")
}


ana_indices_hill <- function(input_files, output_path) {
  input_files <- unlist(input_files)

  # Abundance
  ab <- get_biodiversity(input_files, "abundance", "n")

  # Density
  dn <- get_biodiversity(input_files, "abundance", "n/m2")

  # Calcul des abondances relatives
  rel_abundance_ab <- ab[, -1] / rowSums(ab[, -1], na.rm = TRUE)
  rel_abundance_dn <- dn[, -1] / rowSums(dn[, -1], na.rm = TRUE)

  # Calcul des indices de Hill
  D0_ab <- vegan::specnumber(rel_abundance_ab) # Richesse spécifique (q = 0)
  D1_ab <- exp(vegan::diversity(rel_abundance_ab, index = "shannon")) # Shannon exponentiel (q = 1)
  D2_ab <- 1 / vegan::diversity(rel_abundance_ab, index = "invsimpson") # Inverse Simpson (q = 2)
  D0_dn <- vegan::specnumber(rel_abundance_dn) # Richesse spécifique (q = 0)
  D1_dn <- exp(vegan::diversity(rel_abundance_dn, index = "shannon")) # Shannon exponentiel (q = 1)
  D2_dn <- 1 / vegan::diversity(rel_abundance_dn, index = "invsimpson") # Inverse Simpson (q = 2)


  # Créer le tableau des résultats
  indicator <- dplyr::bind_rows(
    data.frame(
      event_id = ab$event_id,
      Hill_q0 = D0_ab,
      Hill_q1 = D1_ab,
      Hill_q2 = D2_ab
    ) |>
      tidyr::pivot_longer(
        cols = -event_id,
        names_to = "indicator",
        values_to = "indicator_value"
      ) |>
      dplyr::mutate(unit = "n"),
    data.frame(
      event_id = dn$event_id,
      Hill_q0 = D0_dn,
      Hill_q1 = D1_dn,
      Hill_q2 = D2_dn
    ) |>
      tidyr::pivot_longer(
        cols = -event_id,
        names_to = "indicator",
        values_to = "indicator_value"
      ) |>
      dplyr::mutate(unit = "n/m2")
  )

  # Export des résultats
  vroom::vroom_write(indicator, file.path(output_path, "indices_hill.csv"), delim = ",")
}


ana_rarefaction_hurlbert <- function(input_files, output_path) {
  input_files <- unlist(input_files)

  # Abundance
  ab <- get_biodiversity(input_files, "abundance", "n")

  # Density
  dn <- get_biodiversity(input_files, "abundance", "n/m2")

  # Vérifier que l'échantillon contient plus d'individus que `sample_size`
  sample_size <- 10
  total_individuals_ab <- rowSums(ab[, -1], na.rm = TRUE)
  total_individuals_dn <- rowSums(dn[, -1], na.rm = TRUE)
  valid_rows_ab <- which(total_individuals_ab >= sample_size)
  valid_rows_dn <- which(total_individuals_dn >= sample_size)

  # Appliquer la raréfaction de Hurlbert uniquement aux échantillons valides
  rarefied_species_ab <- rep(NA, nrow(ab))
  rarefied_species_ab[valid_rows_ab] <- vegan::rarefy(round(ab[valid_rows_ab, -1], 0), sample = sample_size)
  rarefied_species_dn <- rep(NA, nrow(dn))
  rarefied_species_dn[valid_rows_dn] <- vegan::rarefy(round(dn[valid_rows_dn, -1], 0), sample = sample_size)

  # Créer le tableau des résultats
  indicator <- dplyr::bind_rows(
    data.frame(
      event_id = ab$event_id,
      indicator = "Hurlbert",
      indicator_value = rarefied_species_ab,
      unit = "n"
    ),
    data.frame(
      event_id = dn$event_id,
      indicator = "Hurlbert",
      indicator_value = rarefied_species_dn,
      unit = "n/m2"
    )
  )

  # Export des résultats
  vroom::vroom_write(indicator, file.path(output_path, "rarefaction_hurlbert.csv"), delim = ",")
}


ana_diversite_taxonomique <- function(input_files, output_path) {
  input_files <- unlist(input_files)

  # Connect to db
  con <- connect_sqlite(input_files)
  withr::defer(DBI::dbDisconnect(con))

  # Référencer la table biodiversité et taxonomie
  taxo <- dplyr::tbl(con, "taxonomy") |>
    dplyr::collect()

  # Assess taxonomic diversity and extract indicators
  indicator <- dplyr::bind_rows(
    txdiv(input_files, "abundance", "n", taxo),
    txdiv(input_files, "abundance", "n/m2", taxo)
  )

  # Export des résultats
  vroom::vroom_write(indicator, file.path(output_path, "diversite_taxonomique.csv"), delim = ",")
}

# Fonction pour diversité taxonomique
txdiv <- function(input_files, measurement, unit, tx) {
  # Data
  bio_wide <- get_biodiversity(input_files, measurement, unit, TRUE)
  bio <- get_biodiversity(input_files, measurement, unit, FALSE)

  # Taxonomic table
  tx_tbl <- dplyr::left_join(bio, tx, by = "species_id") |>
    dplyr::select(species_id, phylum, class, order, family, genus) |>
    dplyr::distinct() |>
    tibble::column_to_rownames("species_id")

  # Calcul de la distance taxonomique entre espèces
  taxo_dist <- vegan::taxa2dist(tx_tbl, varstep = TRUE)
  taxo_dist[is.na(taxo_dist)] <- 100 # Replace missing distances with 100 (not ideal but prevents errors)
  taxo_dist <- taxo_dist / 100 # Normalize between 0 and 1

  # Table de biodiversité
  event_ids <- bio_wide$event_id
  bio_wide <- dplyr::select(bio_wide, -event_id)

  # Calcul de distance taxonomique
  taxo_div <- vegan::taxondive(bio_wide, taxo_dist)

  # Indicators
  suppressWarnings({
    ind <- data.frame(
      event_id = rep(event_ids, times = 6), # Repeat each event for each metric
      indicator = rep(c(
        "Total taxonomic distance (D)",
        "Adjusted taxonomic distance (D*)",
        "Total variation in taxonomic distinctness(Δ)",
        "Average taxonomic distinctness (Δ+)",
        "Standard deviation of Δ+",
        "Variation in taxonomic distinctness (Λ+)"
      ), each = length(event_ids)),
      indicator_value = c(
        taxo_div$D, # Total taxonomic distance (D)
        taxo_div$Dstar, # Adjusted taxonomic distance (D*)
        taxo_div$Lambda, # Total variation in taxonomic distinctness (Δ)
        taxo_div$Dplus, # Average taxonomic distinctness (Δ+)
        taxo_div$sd.Dplus, # Standard deviation of Δ+
        taxo_div$SDplus # Variation in taxonomic distinctness (Λ+)
      ),
      unit = unit
    )
  })

  return(ind)
}


ana_dissimilarite_bray_curtis <- function(input_files, output_path) {
  input_files <- unlist(input_files)

  # Wrapper
  wrap_bc <- function(input_files, measurement, unit) {
    get_biodiversity(input_files, measurement, unit) |>
      dplyr::select(-event_id) |>
      calculate_dissimilarity(method = "bray") |>
      dplyr::mutate(unit = unit)
  }

  # Calcul de la matrice de dissimilarité de Bray-Curtis
  indicator <- dplyr::bind_rows(
    wrap_bc(input_files, "biomass", "g"),
    wrap_bc(input_files, "biomass", "g/m2"),
    wrap_bc(input_files, "abundance", "n"),
    wrap_bc(input_files, "abundance", "n/m2"),
    wrap_bc(input_files, "occurrence", NA)
  ) |>
    dplyr::mutate(unit = dplyr::if_else(is.na(unit), "count", unit))


  # Export des résultats
  vroom::vroom_write(indicator, file.path(output_path, "dissimilarite_bray_curtis.csv"), delim = ",")
}


# Function to calculate similarity matrix
calculate_dissimilarity <- function(data, method = "bray") {
  # Prepare data
  rwnm <- data[, 1, drop = TRUE]
  data <- data[, -1]
  data <- as.matrix(data)
  rownames(data) <- rwnm
  data <- t(data)

  # Identify empty rows (all zeros)
  empty_rows <- rowSums(data) == 0

  # Compute similarity matrix
  suppressWarnings({
    dissimilarity_matrix <- vegan::vegdist(data, method = method) |>
      as.matrix()
  })

  # Dissimilarity  of empty rows == 0 for all empty stations, and 1 with all non-empty stations
  dissimilarity_matrix[is.na(dissimilarity_matrix)] <- 1
  dissimilarity_matrix[empty_rows, empty_rows] <- 0


  # Set upper triangle and diagonal to NA
  dissimilarity_matrix[upper.tri(dissimilarity_matrix, diag = TRUE)] <- NA
  dissimilarity_matrix[diag(dissimilarity_matrix)] <- NA

  # Transform to long format
  dissimilarity_matrix <- dissimilarity_matrix |>
    as.matrix() |>
    as.table() |>
    data.frame() |>
    na.omit() |>
    dplyr::mutate(indicator = "Dissimilarite Bray-Curtis") |>
    dplyr::select(event_id_1 = Var1, event_id_2 = Var2, indicator, indicator_value = Freq)

  return(dissimilarity_matrix)
}
