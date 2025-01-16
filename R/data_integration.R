#' Script to integrate all biodiversity data together into a relational-type database
data_integration <- function() {
  data_input <- file.path("workspace/data/harvested/donnees-indicateurs_2024-11-26_1701")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # batture_aux_alouettes
  input <- file.path(data_input, "batture_aux_alouettes")
  event <- vroom::vroom(file.path(input, "kelp_event-data_2018-2019.csv"))
  occ <- vroom::vroom(file.path(input, "kelp_occurrence-data_2018-2019.csv"))

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # cabin
  input <- file.path(data_input, "cabin")
  dir(input)


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # DL_Assemblage
  input <- file.path(data_input, "DL_Assemblage")
  dir(input)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # DL_CPD_PPO
  input <- file.path(data_input, "DL_CPD_PPO")
  dir(input)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # donnees_indicateurs.csv
  input <- file.path(data_input, "donnees_indicateurs.csv")
  dir(input)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # io_herbier_benthos
  input <- file.path(data_input, "io_herbier_benthos")
  dir(input)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # lagrave
  input <- file.path(data_input, "lagrave")
  dir(input)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # Liste_Sp_CindyGrant.txt
  input <- file.path(data_input, "Liste_Sp_CindyGrant.txt")
  dir(input)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # macroalgae_macroinvertebrates
  input <- file.path(data_input, "macroalgae_macroinvertebrates")
  dir(input)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # maria
  input <- file.path(data_input, "maria")
  dir(input)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # piei_go_pro_iml_2017-2021
  input <- file.path(data_input, "piei_go_pro_iml_2017-2021")
  dir(input)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # pointe-aux-loups
  input <- file.path(data_input, "pointe-aux-loups")
  dir(input)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # pointe-aux-outardes
  input <- file.path(data_input, "pointe-aux-outardes")
  dir(input)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # sainte-flavie
  input <- file.path(data_input, "sainte-flavie")
  dir(input)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # sainte-luce
  input <- file.path(data_input, "sainte-luce")
  dir(input)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # yanick_intertidal_data_abundance.csv
  input <- file.path(data_input, "yanick_intertidal_data_abundance.csv")
  dir(input)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # zip-rne_marais_hickey_2022
  input <- file.path(data_input, "zip-rne_marais_hickey_2022")
  dir(input)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # zip-rne_marais-bergeronnes_2021
  input <- file.path(data_input, "zip-rne_marais-bergeronnes_2021")
  dir(input)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # zip-rne_marais-bmv_2019-2021
  input <- file.path(data_input, "zip-rne_marais-bmv_2019-2021")
  dir(input)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # zip-rne_marais-pao_2020-2021
  input <- file.path(data_input, "zip-rne_marais-pao_2020-2021")
  dir(input)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # zip-rne_marais-pf_2019-2021
  input <- file.path(data_input, "zip-rne_marais-pf_2019-2021")
  dir(input)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # zip-rne_marais-psm_2018
  input <- file.path(data_input, "zip-rne_marais-psm_2018")
  dir(input)
}
