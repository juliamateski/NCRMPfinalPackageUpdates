## Function to calculate weighted percent cover by strata and protected area and then at the regional level

# Purpose:
# support function to calculate weighted percent cover data


## Tag: data analysis


# outputs created in this file --------------
# unwh_cover_strata
# domain estimates

# Current weighting scheme:
# STRAT + PROT


# CallS:
# analysis ready data

# output gets called by:
# NCRMP_calculate_cover.R
#

# NCRMP Caribbean Benthic analytics team: Groves, viehman, Williams
# Last update: Nov 2023


##############################################################################################################################

#' Creates weighted benthic cover data
#'
#' Calcualtes weighted benthic cover data. NCRMP utilizes a stratified random
#' sampling design. Regional estimates of benthic cover are weighted by the
#' number of grid cells of a stratum in the sample frame.
#' Function produces strata means, weighted strata means,
#' and weighted regional estimates for benthic cover data.
#' Support function called by [NCRMP_calculate_cover()].
#'
#'
#'
#'
#'
#' @param inputdata A dataframe of benthic cover data summarized by cover group at each site in a single region.
#' @param region A string indicating the region. Options are: "SEFCRI", "FLK", "Tortugas", "STX", "STTSTJ", "PRICO", and "GOM".
#' @param project A string indicating the project. "NCRMP" is the only option.
#' @return A list of dataframes, including a dataframe of strata means of cover groups
#' and a dataframe of weighted regional estimates of cover groups, for specified region.
#' @importFrom magrittr "%>%"
#' @export
#'
NCRMP_make_weighted_LPI_data <- function(inputdata, region, project = "NULL") {

  # Define regional groups
  FL <- c("SEFCRI", "FLK", "Tortugas")
  GOM <- "GOM"
  Carib <- c("STTSTJ", "STX", "PRICO")

  ntot <- load_NTOT(region = region, inputdata = inputdata, project = project)

  # Helper function for calculating cover estimates
  calculate_cover_estimates <- function(data, group_vars, extra_mutate = list()) {
    data %>%
      dplyr::group_by(!!!syms(group_vars)) %>%
      dplyr::summarise(
        avcvr = mean(Percent_Cvr),
        svar = var(Percent_Cvr),
        n = length(unique(PRIMARY_SAMPLE_UNIT)),
        MIN_DEPTH = mean(MIN_DEPTH, na.rm = TRUE),
        MAX_DEPTH = mean(MAX_DEPTH, na.rm = TRUE),
        DEPTH_M = (MIN_DEPTH + MAX_DEPTH) / 2
      ) %>%
      dplyr::mutate(
        svar = ifelse(svar == 0, 1e-8, svar),
        Var = svar / n,
        std = sqrt(svar),
        SE = sqrt(Var),
        CV_perc = (SE / avcvr) * 100,
        !!!extra_mutate
      )
  }

  group_vars <- if (region %in% FL) {
    c("YEAR", "ANALYSIS_STRATUM", "STRAT", "PROT", "cover_group")
  } else {
    c("YEAR", "ANALYSIS_STRATUM", "STRAT", "cover_group")
  }

  cover_est <- calculate_cover_estimates(inputdata, group_vars, list(PROT = ifelse(region %in% FL, NA, NULL)))

  cover_est <- cover_est %>%
    dplyr::full_join(ntot) %>%
    dplyr::mutate(
      whavcvr = wh * avcvr,
      whsvar = wh^2 * Var,
      n = tidyr::replace_na(n, 0)
    ) %>%
    dplyr::filter(!is.na(cover_group))

  # Reformat output
  cover_strata <- cover_est %>%
    dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, PROT, DEPTH_M, cover_group, n, avcvr, Var, SE, CV_perc) %>%
    dplyr::mutate(n = tidyr::replace_na(n, 0), CV_perc = ifelse(CV_perc == Inf, NA_real_, CV_perc))

  Domain_est <- cover_est %>%
    dplyr::mutate(CV_perc = ifelse(CV_perc == Inf, NA_real_, CV_perc)) %>%
    dplyr::group_by(REGION, YEAR, cover_group) %>%
    dplyr::summarise(
      avCvr = sum(whavcvr, na.rm = TRUE),
      Var = sum(whsvar, na.rm = TRUE),
      SE = sqrt(Var),
      CV_perc = (SE / avCvr) * 100,
      n_sites = sum(n),
      n_strat = length(unique(ANALYSIS_STRATUM)),
      ngrtot = sum(NTOT)
    ) %>%
    dplyr::ungroup()

  list("cover_strata" = cover_strata, "Domain_est" = Domain_est)
}

