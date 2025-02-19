recode_and_clean_species <- function(data) {

  # Recode SPECIES_CD and SPECIES_NAME if SPECIES_CD exists
  if ("SPECIES_CD" %in% colnames(data)) {
    data <- data %>%
      dplyr::mutate(
        SPECIES_CD = dplyr::case_when(
          SPECIES_CD == "DIP CLIV" ~ "PSE CLIV",
          SPECIES_CD == "DIP STRI" ~ "PSE STRI",
          SPECIES_CD == "CLA ARBU" ~ "CLA ABRU",
          SPECIES_CD == "MEAN JACK" ~ "MEA JACK",
          TRUE ~ SPECIES_CD
        ),
        SPECIES_NAME = dplyr::case_when(
          SPECIES_CD == "PSE CLIV" ~ "Pseudodiploria clivosa",
          SPECIES_CD == "PSE STRI" ~ "Pseudodiploria strigosa",
          SPECIES_CD == "CLA ABRU" ~ "Cladocora arbuscula",
          SPECIES_CD == "MEA JACK" ~ "Meandrina jacksoni",
          TRUE ~ SPECIES_NAME
        )
      )
  }

  # Recode COVER_CAT_NAME and COVER_CAT_CD if COVER_CAT_NAME exists
  if ("COVER_CAT_NAME" %in% colnames(data)) {
    data <- data %>%
      dplyr::mutate(
        COVER_CAT_NAME = dplyr::case_when(
          COVER_CAT_NAME == "Erythropodium caribaeorum" ~ "Encrusting gorgonian",
          COVER_CAT_NAME == "Diploria spp." ~ "Diploria spp",
          COVER_CAT_NAME == "Cladocora abruscula" ~ "Cladocora arbuscula",
          TRUE ~ COVER_CAT_NAME
        ),
        COVER_CAT_CD = dplyr::case_when(
          COVER_CAT_NAME == "Cladocora arbuscula" ~ "CLA ABRU",
          COVER_CAT_CD == "POF SPE." ~ "SPO OTHE",
          COVER_CAT_CD == "ERY CARI" ~ "ERY CARY",
          COVER_CAT_CD == "MAD MIRA" ~ "MAD AURE",
          COVER_CAT_CD == "ENCR GORG" ~ "GOR ENCR",
          TRUE ~ COVER_CAT_CD
        )
      )
  }
  return(data)
}

