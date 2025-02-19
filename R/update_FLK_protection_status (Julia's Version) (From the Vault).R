update_protection_status <- function(data, grid_df) {

  new_prots <- grid_df %>%
    dplyr::select(MAPGRID_NR, PROT) %>%
    dplyr::rename("PROT_og" = PROT) %>%
    dplyr::mutate(
      MAPGRID_NR = as.numeric(MAPGRID_NR),
      PROT_og = as.numeric(PROT_og)
    )

  data <- data %>%
    dplyr::left_join(.,new_prots, by = "MAPGRID_NR") %>%
    dplyr::mutate(
      PROT_og = case_when(
        PRIMARY_SAMPLE_UNIT == 1006 ~ 0,
        PRIMARY_SAMPLE_UNIT == 1382 ~ 1,
        TRUE ~ PROT_og
      )
    ) %>%
    dplyr::select(-PROT) %>%
    dplyr::rename("PROT" = PROT_og)
}


