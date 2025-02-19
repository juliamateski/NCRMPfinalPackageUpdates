## Function to calculate disease prevalence & bleaching prevalence for NCRMP, MIR, and NCRMP + DRM data (FL only) by calculating species then colony prevalence (%) at the site level,
## taking the mean of all sites within each strata, strata area weighting each strata and summing all strata means to reach the domain estimate.

# Purpose:
# creates csv files with disease/bleaching prevalence by region


## Tag: data analysis


# outputs created in this file --------------
# disease_prev_site
# disease_prev_strata
# Domain estimates

# CallS:
# analysis ready data

# output gets called by:
# NCRMP_DRM_calculate_dis_ble_prevalence_species_domain.R
#

# NCRMP Caribbean Benthic analytics team: Groves, Viehman, Williams
# Last update: Feb 2023


##############################################################################################################################

#' Calculate disease prevalence & bleaching prevalence at the species/site, site, strata and domain levels
#'
#' Calculates disease and bleaching prevalence for each species at each site,
#' at each site across all species, at each strata across all species, and
#' regional estimates for each year of a given region.
#' NCRMP utilizes a stratified random sampling design.
#' Regional estimates of disease and bleaching prevalence are weighted by the number of
#' grid cells of a stratum in the sample frame. Species-level outputs from
#' this function are utilized by [NCRMP_DRM_calculate_dis_ble_prevalence_species_domain()].
#'
#'
#'
#'
#' @param project A string indicating the project, NCRMP, MIR, or NCRMP and DRM combined ("NCRMP_DRM").
#' @param region A string indicating the region. Options are: "SEFCRI", "FLK", "Tortugas", "STX", "STTSTJ", "PRICO", and "GOM".
#' @param species_filter An optional concatenated string indicating whether to filter to a subset of species
#' @return A list of dataframes including 1) bleaching and disease prevalence by species
#' and site, 2) bleaching and disease prevalence by site, 3) disease prevalence by
#' strata, 4) bleaching prevalence by strata, and 5) regional estimates for disease
#' and bleaching prevalence.
#' @importFrom magrittr "%>%"
#' @export
#'
#'

### Main Function ###

NCRMP_DRM_calculate_disease_prevalence_colonies <- function(project, region, species_filter = "NULL") {

  ### Helper Functions ###

  # Filter common rows
  filter_data <- function(data, filter_disease = TRUE) {
    data <- data %>%
      dplyr::filter(
        N == 1,
        JUV == 0,
        SUB_REGION_NAME != "Marquesas",
        SUB_REGION_NAME != "Marquesas-Tortugas Trans"
      )
    if (filter_disease) {
      data <- data %>%
        dplyr::filter(DISEASE != "N/A", !is.na(DISEASE))
    }
    return(data)
  }

  # Convert variables and format coordinates
  prepare_data <- function(data) {
    data %>%
      dplyr::mutate(
        PROT = as.factor(PROT),
        LAT_DEGREES = sprintf("%0.4f", as.numeric(LAT_DEGREES)),
        LON_DEGREES = sprintf("%0.4f", as.numeric(LON_DEGREES)),
        PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT)
      )
  }

  # Convert DISEASE to numeric indicator
  mutate_disease <- function(data) {
    data %>%
      dplyr::mutate(
        DISEASE = dplyr::case_when(
          DISEASE == "A" ~ 0,
          DISEASE %in% c("P", "F", "S") ~ 1,
          TRUE ~ 0
        )
      )
  }

  # Convert BLEACH_CONDITION to numeric indicator.
  # For stage1 (or species in stage2) we use one set of conditions,
  # for stage2 (colonies from dat_2stage, excluding "PL") we include "PL" as diseased.
  mutate_bleach <- function(data, stage = "1") {
    if (stage == "2") {
      data %>%
        dplyr::mutate(
          BLEACH = dplyr::case_when(
            BLEACH_CONDITION == "N" ~ 0,
            BLEACH_CONDITION %in% c("P", "T", "B", "PB", "PL") ~ 1,
            TRUE ~ 0
          )
        )
    } else {
      data %>%
        dplyr::mutate(
          BLEACH = dplyr::case_when(
            BLEACH_CONDITION == "N" ~ 0,
            BLEACH_CONDITION %in% c("P", "B", "T", "PB") ~ 1,
            TRUE ~ 0
          )
        )
    }
  }

  # Summarize prevalence by summing within groups and then format percentages
  summarise_prevalence <- function(data, group_vars) {
    data %>%
      dplyr::group_by(across(all_of(group_vars))) %>%
      dplyr::summarise(
        Total_dis = sum(DISEASE),
        Total_ble = sum(BLEACH),
        Total_col = sum(N),
        DIS_PREV = (Total_dis / Total_col) * 100,
        BLE_PREV = (Total_ble / Total_col) * 100,
        .groups = "keep"
      ) %>%
      dplyr::mutate(
        DIS_PREV = as.numeric(sprintf("%0.1f", DIS_PREV)),
        BLE_PREV = as.numeric(sprintf("%0.1f", BLE_PREV))
      )
  }

  # When data from stage2 have been summarized at a finer level,
  # we need to take the mean over duplicate groups.
  summarise_mean_prevalence <- function(data, group_vars) {
    data %>%
      dplyr::group_by(across(all_of(group_vars))) %>%
      dplyr::summarise(
        Total_dis = mean(Total_dis),
        Total_ble = mean(Total_ble),
        Total_col = mean(Total_col),
        DIS_PREV = mean(DIS_PREV),
        BLE_PREV = mean(BLE_PREV),
        .groups = "keep"
      ) %>%
      dplyr::mutate(
        DIS_PREV = as.numeric(sprintf("%0.1f", DIS_PREV)),
        BLE_PREV = as.numeric(sprintf("%0.1f", BLE_PREV))
      )
  }


  # Load data
  tmp <- load_NCRMP_DRM_demo_data(project = project, region = region, species_filter = species_filter)
  for (k in 1:length(tmp)) assign(names(tmp)[k], tmp[[k]])

  # Define grouping variables
  grp_site <- c("SURVEY", "REGION", "YEAR", "SUB_REGION_NAME", "PRIMARY_SAMPLE_UNIT", "LAT_DEGREES", "LON_DEGREES", "STRAT", "HABITAT_CD", "PROT")
  grp_species <- c(grp_site, "SPECIES_CD")


  if (project == "NCRMP_DRM" || (project == "NCRMP" && region %in% c("SEFCRI", "Tortugas"))) {

    ## Process dat_1stage
    dat1_1stage <- dat_1stage %>%
      filter_data(filter_disease = TRUE) %>%
      prepare_data() %>%
      mutate_disease() %>%
      mutate_bleach(stage = "1") %>%
      summarise_prevalence(grp_site)

    ## Process disease species dat_1stage
    dis_species_1stage <- dat_1stage %>%
      filter_data(filter_disease = TRUE) %>%
      prepare_data() %>%
      mutate_disease() %>%
      mutate_bleach(stage = "1") %>%
      summarise_prevalence(grp_species)

    ## Process dat_2stage
    dat1_2stage <- dat_2stage %>%
      filter_data(filter_disease = TRUE) %>%
      prepare_data() %>%
      mutate_disease() %>%
      mutate_bleach(stage = "2") %>%
      summarise_prevalence(grp_site) %>%
      summarise_mean_prevalence(grp_site)

    ## Process disease species dat_2stage
    dis_species_2stage <- dat_2stage %>%
      filter_data(filter_disease = TRUE) %>%
      prepare_data() %>%
      mutate_disease() %>%
      mutate_bleach(stage = "1") %>%# For species-level in stage2 we use the stage1 bleach coding.
      summarise_prevalence(grp_species) %>%
      summarise_mean_prevalence(grp_species)

    disease_prev_species <- dplyr::bind_rows(dis_species_1stage, dis_species_2stage)
    disease_prev_site <- dplyr::bind_rows(dat1_1stage, dat1_2stage)

  } else {

    ## For other projects/regions, do not filter out DISEASE N/A values.
    disease_prev_site <- dat_1stage %>%
      filter_data(filter_disease = FALSE) %>%
      prepare_data() %>%
      mutate_disease() %>%
      mutate_bleach(stage = "1") %>%
      summarise_prevalence(grp_site)

    disease_prev_species <- dat_1stage %>%
      filter_data(filter_disease = FALSE) %>%
      prepare_data() %>%
      mutate_disease() %>%
      mutate_bleach(stage = "1") %>%
      summarise_prevalence(grp_species)
  }

  # Run the weighting function on the site-level data
  tmp <- NCRMP_make_weighted_demo_data(project, inputdata = disease_prev_site, region, datatype = "disease")
  for (k in 1:length(tmp)) assign(names(tmp)[k], tmp[[k]])

  # Create output list
  output <- list(
    dis_ble_prev_species = disease_prev_species,
    dis_ble_prev_site    = disease_prev_site,
    dis_prev_strata      = dis_prev_strata,
    ble_prev_strata      = ble_prev_strata,
    Domain_est           = Domain_est
  )

  return(output)
}



