
## Function to randomly subset 2 stage data for length frequency data

# Purpose:
# creates data objects of subsetted 2 stage data


## Tag: data analysis


# output gets called by:
# NCRMP_make_size_bins
#

# NCRMP Caribbean Benthic analytics team: Davis, Groves, Viehman, Williams
# Last update: Sep 2023



###############################################################################


#' Subset 2 stage data
#'
#' Subsets 2 stage data to single stage to provide data frames that can be used
#' for the size frequency data summaries. This function should NOT be called and run
#' but serves as base code for randomly subsetting 2 stage data into single stage
#' and saving it to the package.
#'
#' @param region A string indicating the region
#' @param project A string indicating the project, NCRMP or NCRMP and DRM combined
#' @param year A numeric indicating the year of interest
#'
#' @return # A subsetted dataframe
#'

subset_2stage_data <- function(region, project, year) {
  if (project != "NCRMP") return(NULL)

  #### Lookup table for datasets ####
  dataset_lookup <- list(
    "SEFCRI_2014" = list(data = SEFCRI_2014_2stage_coral_demographics,
                         output_name = "SEFCRI_2014_1stage_coral_demographics"),
    "Tortugas_2018" = list(data = Tortugas_2018_coral_demographics,
                           output_name = "Tortugas_2018_1stage_coral_demographics")
  )

  ####Generate key for lookup####
  key <- paste(region, year, sep = "_")
  if (!key %in% names(dataset_lookup)) return(NULL)

  dat <- dataset_lookup[[key]]$data
  output_name <- dataset_lookup[[key]]$output_name

  ####Select unique PSU-station combinations####
  tmp <- dat %>%
    dplyr::select(YEAR, PRIMARY_SAMPLE_UNIT, STATION_NR) %>%
    dplyr::distinct() %>%
    dplyr::group_by(PRIMARY_SAMPLE_UNIT) %>%
    dplyr::slice_sample(n = 1, set.seed(3)) %>%
    dplyr::ungroup()

  tmp <- tmp %>% dplyr::mutate(PSU_station = paste(PRIMARY_SAMPLE_UNIT, STATION_NR, sep = "-"))

  final_data <- dat %>%
    dplyr::mutate(PSU_station = paste(PRIMARY_SAMPLE_UNIT, STATION_NR, sep = "-")) %>%
    dplyr::filter(PSU_station %in% tmp$PSU_station) %>%
    dplyr::select(-PSU_station)

  assign(output_name, final_data, envir = .GlobalEnv)
}
