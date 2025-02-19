## Function to calculate colony density for combined NCRMP and DRM  data

# Purpose:
# creates csv files with colony density.


## Tag: data analysis


# outputs created in this file --------------
# density_site
# density_strata,
# Domain_est


# CallS:
# analysis ready data

# output gets called by:
# Analysis Rmarkdown, etc.
#

# NCRMP Caribbean Benthic analytics team: Groves, Viehman, Williams
# Last update: Dec 2023


##############################################################################################################################

#' Creates colony density summary dataframes
#'
#' Calculates coral density from NCRMP coral demographic data at the site level,
#' strata mean level, and regional weighted mean level (all across species), as well
#' site at the species level.  NCRMP utilizes a stratified random sampling design.
#' Regional estimates of coral density are weighted by the number of
#' grid cells of a stratum in the sample frame.
#'
#'
#'
#'
#'
#' @param project A string indicating the project, NCRMP or NCRMP and DRM combined ("NCRMP_DRM").
#' @param region A string indicating the region.  Options are: "SEFCRI", "FLK", "Tortugas", "STX", "STTSTJ", "PRICO", and "GOM".
#' @param species_filter A string indicating whether to filter to a subset of species.
#' @return A list of dataframes includeind 1) coral density by species at each site,
#' 2) total coral density at each site, 3) mean total coral density for each strata,
#' and 4) weighted mean regional total coral density.
#' @importFrom magrittr "%>%"
#' @export
#'
#'

NCRMP_DRM_calculate_colony_density <- function(project = "NULL", region, species_filter = "NULL") {

  ####Load and preprocess data ####
  tmp <- load_NCRMP_DRM_demo_data(project = project, region = region, species_filter = species_filter)
  list2env(tmp, envir = environment())

  ####Helper function to clean data####
  clean_data <- function(data) {
    data %>%
      dplyr::ungroup() %>%
      dplyr::filter(!SUB_REGION_NAME %in% c("Marquesas", "Marquesas-Tortugas Trans"), JUV == 0) %>%
      dplyr::mutate(
        PROT = as.factor(PROT),
        PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT),
        LAT_DEGREES = sprintf("%0.4f", LAT_DEGREES),
        LON_DEGREES = sprintf("%0.4f", LON_DEGREES)
      )
  }

  #Apply clean_data helper function to datasets
  dat_1stage <- clean_data(dat_1stage)
  dat_2stage <- clean_data(dat_2stage)

  ####calculate density####
  calculate_density <- function(data, group_vars) {
    data %>%
      dplyr::group_by(across(all_of(group_vars))) %>%
      dplyr::summarise(ABUNDANCE = sum(N), .groups = "keep") %>%
      dplyr::mutate(DENSITY = ABUNDANCE / METERS_COMPLETED) %>%
      dplyr::ungroup() %>%
      dplyr::select(-ABUNDANCE, -METERS_COMPLETED)
  }

  ####common group vars####
  common_groups <- c("REGION", "SURVEY", "YEAR", "SUB_REGION_NAME", "ADMIN", "PRIMARY_SAMPLE_UNIT",
                     "LAT_DEGREES", "LON_DEGREES", "STRAT", "HABITAT_CD", "PROT")

  ####Calculate densities####
  density_site <- calculate_density(dat_1stage, c(common_groups, "METERS_COMPLETED"))
  density_species <- calculate_density(dat_1stage, c(common_groups, "METERS_COMPLETED", "SPECIES_CD", "SPECIES_NAME"))

  if (project %in% c("NCRMP_DRM", "NCRMP") && region %in% c("SEFCRI", "Tortugas")) {
    density_site <- dplyr::bind_rows(density_site, calculate_density(dat_2stage, c(common_groups, "METERS_COMPLETED")))
    density_species <- dplyr::bind_rows(density_species, calculate_density(dat_2stage, c(common_groups, "METERS_COMPLETED", "SPECIES_CD", "SPECIES_NAME")))
  }

  #make sure every species is represented
  spp <- density_species %>% dplyr::select(SPECIES_CD, SPECIES_NAME) %>% dplyr::distinct()
  density_species <- density_species %>%
    tidyr::pivot_wider(names_from = SPECIES_CD, values_from = DENSITY, values_fill = 0) %>%
    tidyr::pivot_longer(cols = spp$SPECIES_CD, names_to = "SPECIES_CD", values_to = "DENSITY") %>%
    dplyr::left_join(spp, by = "SPECIES_CD")

  #### Run weighting function ####
  tmp <- NCRMP_make_weighted_demo_data(project, inputdata = density_site, region, datatype = "density", species_filter = species_filter)
  list2env(tmp, envir = environment())

  ####Export####
  output <- list(
    "density_species" = density_species,
    "density_site" = density_site,
    "density_strata" = density_strata,
    "Domain_est" = Domain_est
  )

  if (project == "MIR") {
    output$Domain_est_PROT <- Domain_est_PROT
  }

  return(output)
}
