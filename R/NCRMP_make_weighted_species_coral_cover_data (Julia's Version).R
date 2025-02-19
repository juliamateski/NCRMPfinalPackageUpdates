
## Function to calculate weighted coral cover by species

# Purpose:
# creates csv files with weighted mean density & CVs.


## Tag: data analysis


# outputs created in this file --------------
# region_means
# strata_means
#


# CallS:
# analysis ready data

# output gets called by:
# NCRMP_colony_percent_cover
#

# NCRMP Caribbean Benthic analytics team: Groves, Viehman, Williams
# Last update: Nov 2023


##############################################################################################################################

#' Calculates weighted mean density & CVs of coral cover, by species
#'
#' Calculates weighted benthic cover data for individual coral species. NCRMP utilizes a stratified random
#' sampling design. Regional estimates of coral cover are weighted by the number of grid cells of a stratum
#' in the sample frame. Function calculates strata means, weighted strata means,
#' and weighted regional estimates for for coral cover from benthic cover data.
#' Support function called by [NCRMP_colony_percent cover()].
#'
#'
#'
#'
#' @param region A string indicating the region. Options are: "SEFCRI", "FLK", "Tortugas", "STX", "STTSTJ", "PRICO", and "GOM".
#' @param sppcvr A dataframe of site and species level percent cover data.
#' @param project A string indicating the project: "NCRMP" or "MIR".
#' @return A list of dataframes, including a dataframe of strata mean cover by coral species
#' and a dataframe of regional weighted mean cover by coral species
#' @importFrom magrittr "%>%"
#' @importFrom dplyr "mutate"
#' @importFrom dplyr "n"
#' @export
#'
#'

# function to Calculate weights based on the most recent sampling grid
NCRMP_make_weighted_species_coral_cover_data <- function(region, sppcvr, project = "NULL") {

  ntot <- load_NTOT(region = region, inputdata = sppcvr, project = project)

  # Preprocess data function
  prep_data <- function(data) {
    excluded_species <- c("Solenastrea spp", "Siderastrea spp", "Scolymia spp",
                          "Agaricia spp", "Diploria spp", "Orbicella spp", "Madracis spp",
                          "Other coral", "Isophyllia spp", "Porites spp", "Meandrina spp",
                          "Pseudodiploria spp", "Orbicella annularis species complex",
                          "Tubastraea coccinea")

    data %>%
      dplyr::filter(cover_group == "HARD CORALS", !COVER_CAT_NAME %in% excluded_species) %>%
      dplyr::mutate(SPECIES_NAME = COVER_CAT_NAME, cvr = Percent_Cvr)
  }

  # Calculate strata means
  strata_means <- sppcvr %>%
    prep_data() %>%
    dplyr::group_by(REGION, YEAR, SPECIES_NAME, ANALYSIS_STRATUM) %>%
    dplyr::summarize(
      mean = mean(cvr),
      svar = var(cvr, na.rm = TRUE),
      N_LPI_CELLS_SAMPLED = n(),
      .groups = "keep"
    ) %>%
    dplyr::mutate(
      svar = ifelse(svar == 0, 1e-8, svar),  # Replace zeros with a small number
      Var = svar / N_LPI_CELLS_SAMPLED,
      std = sqrt(svar),
      SE = sqrt(Var),
      CV_perc = (SE / mean) * 100,
      CV = SE / mean
    )

  # Calculate region/population means
  region_means <- strata_means %>%
    dplyr::left_join(ntot, by = c("REGION", "ANALYSIS_STRATUM")) %>%
    dplyr::mutate(
      wh_mean = wh * mean,
      wh_var = wh^2 * Var
    ) %>%
    dplyr::group_by(REGION, YEAR, SPECIES_NAME) %>%
    dplyr::summarize(
      avCvr = sum(wh_mean, na.rm = TRUE),
      Var = sum(wh_var, na.rm = TRUE),
      SE = sqrt(Var),
      CV_perc = (SE / avCvr) * 100,
      CV = SE / avCvr,
      n_sites = sum(N_LPI_CELLS_SAMPLED, na.rm = TRUE),
      .groups = "keep"
    ) %>%
    dplyr::mutate(
      STRAT_ANALYSIS = "ALL_STRAT",
      DEPTH_STRAT = "ALL_DEPTHS",
      HABITAT_CD = "ALL_HABS"
    ) %>%
    dplyr::select(REGION, YEAR, STRAT_ANALYSIS, SPECIES_NAME, avCvr, Var, SE, CV_perc, CV, n_sites, HABITAT_CD, DEPTH_STRAT)

  # Calculate n_sites where species is present
  strata_presence <- sppcvr %>%
    prep_data() %>%
    dplyr::filter(cvr > 0) %>%
    dplyr::group_by(REGION, YEAR, SPECIES_NAME, ANALYSIS_STRATUM) %>%
    dplyr::summarize(n_sites = n(), .groups = "keep") %>%
    dplyr::ungroup()

  region_presence <- strata_presence %>%
    dplyr::group_by(REGION, YEAR, SPECIES_NAME) %>%
    dplyr::summarize(n_sites_present = sum(n_sites), .groups = "keep") %>%
    dplyr::ungroup()

  # Merge presence data with region means
  region_means <- region_means %>%
    dplyr::left_join(region_presence, by = c("REGION", "YEAR", "SPECIES_NAME")) %>%
    dplyr::select(REGION, YEAR, STRAT_ANALYSIS, SPECIES_NAME, avCvr, Var, SE, CV_perc, CV,
                  n_sites_present, n_sites, HABITAT_CD, DEPTH_STRAT) %>%
    tidyr::drop_na(SPECIES_NAME) %>%
    dplyr::filter(!grepl('spp', SPECIES_NAME, ignore.case = TRUE)) %>%
    dplyr::mutate(n_sites_present = tidyr::replace_na(n_sites_present, 0))

  # Return results
  list("region_means" = region_means, "strata_means" = strata_means)
}
