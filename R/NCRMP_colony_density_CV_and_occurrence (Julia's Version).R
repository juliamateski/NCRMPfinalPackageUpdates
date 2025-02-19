## Function to calculate colony density cv by species, site level occurrence, identify species with CV less than 20%, and plot back to back figure

# Purpose:
# creates csv files with colony density, occurrence and a plot of both.


## Tag: data analysis


# outputs created in this file --------------
#
#
#


# CallS:
# analysis ready data

# output gets called by:
# Analysis Rmarkdown, etc.
#

# NCRMP Caribbean Benthic analytics team: Groves, Viehman
# Last update: Jan 2023


##############################################################################################################################

#' Creates colony density summary dataframes
#'
#'
#'
#'
#' @param region A string indicating the region
#' @param ptitle A string indicating the plot title
#' @param year A numeric indicating the year of interest
#' @param path A string indicating the filepath for the figure
#' @return A dataframe
#' @importFrom magrittr "%>%"
#' @importFrom ggplot2 "ggplot"
#' @export
#'
#'


NCRMP_colony_density_CV_and_occurrence <- function(region, ptitle, year, file_path = "NULL", species_filter = "NULL", project = "NULL"){


    ####Coral species used in allocation####

    coral_species <- switch(region,
                            "STX" = c("Orbicella annularis", "Orbicella faveolata", "Orbicella franksi", "Acropora cervicornis", "Acropora palmata", "Dendrogyra cylindrus", "Mycetophyllia ferox", "Colpophyllia natans","Dichocoenia stokesii", "Diploria labyrinthiformis", "Eusmilia fastigiata", "Meandrina meandrites", "Pseudodiploria strigosa", "Pseudodiploria clivosa"),
                            "STTSTJ" = c("Orbicella annularis", "Orbicella faveolata", "Orbicella franksi", "Acropora cervicornis", "Acropora palmata", "Dendrogyra cylindrus", "Mycetophyllia ferox", "Colpophyllia natans","Dichocoenia stokesii", "Diploria labyrinthiformis", "Eusmilia fastigiata", "Meandrina meandrites", "Pseudodiploria strigosa", "Pseudodiploria clivosa"),
                            "PRICO" = c("Orbicella annularis", "Orbicella faveolata", "Orbicella franksi", "Acropora cervicornis", "Acropora palmata", "Dendrogyra cylindrus", "Mycetophyllia ferox", "Colpophyllia natans","Dichocoenia stokesii", "Diploria labyrinthiformis", "Eusmilia fastigiata", "Meandrina meandrites", "Pseudodiploria strigosa", "Pseudodiploria clivosa"),
                            "FLK" = c("Colpophyllia natans", "Montastraea cavernosa", "Orbicella faveolata", "Porites astreoides", "Siderastrea siderea", "Solenastrea bournoni"),
                            "Tortugas" = c("Colpophyllia natans", "Montastraea cavernosa", "Orbicella faveolata", "Porites astreoides", "Orbicella franksi", "Stephanocoenia intersepta"),
                            "SEFCRI" = c("Acropora cervicornis", "Dichocoenia stokesii", "Montastraea cavernosa", "Porites astreoide", "Pseudodiploria strigosa", "Siderastrea siderea"),
                            stop("Unknown region")  #Region NOT Found
    )


    #Get sppdens dataset
    sppdens <- switch(region,
                      "FLK" = switch(project,
                                     "NCRMP" = NCRMP_FLK_2014_22_density_species,
                                     "MIR" = MIR_2022_density_species_DUMMY,
                                     "NCRMP_DRM" = NCRMP_DRM_FLK_2014_22_density_species,
                                     stop("Unknown project for FLK")
                      ),
                      "Tortugas" = NCRMP_Tort_2014_22_density_species,
                      "SEFCRI" = switch(project,
                                        "NCRMP" = NCRMP_SEFCRI_2014_22_density_species,
                                        stop("Unknown project for SEFCRI")
                      ),
                      "PRICO" = NCRMP_PRICO_2014_23_density_species,
                      "STTSTJ" = NCRMP_STTSTJ_2013_23_density_species,
                      "STX" = NCRMP_STX_2015_23_density_species,
                      "GOM" = NCRMP_FGBNMS_2013_22_density_species,
                      stop("Unknown region")
    )


    region_means <- NCRMP_make_weighted_density_CV_data(region = region, sppdens = sppdens, project = project)



    if (species_filter == TRUE) {

      region_means <- region_means %>% dplyr::filter(SPECIES_CD %in% coral_species)
    }

    plot <- function(region_means, set_occurrence, year, ptitle) {
      g.mid <- region_means %>%
        dplyr::filter(YEAR == year) %>%
        dplyr::filter(occurrence > set_occurrence) %>%
        dplyr::filter(CV < 1) %>%
        ggplot(aes(x = 1, y = reorder(SPECIES_CD, occurrence))) +
        geom_text(aes(label = SPECIES_CD), size = 3, fontface = "italic") +
        geom_segment(aes(x = 0.94, xend = 0.96, yend = SPECIES_CD)) +
        geom_segment(aes(x = 1.04, xend = 1.065, yend = SPECIES_CD)) +
        ggtitle(ptitle) +
        ylab(NULL) +
        scale_x_continuous(expand = c(0, 0), limits = c(0.94, 1.065)) +
        theme(axis.title = element_blank(),
              panel.grid = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.background = element_blank(),
              axis.text.x = element_text(color = NA),
              axis.ticks.x = element_line(color = NA),
              plot.margin = unit(c(t = 1, r = -1, b = 1, l = -1), "mm"),
              plot.title = element_text(hjust = 0.5))

      g1 <- region_means %>%
        dplyr::filter(YEAR == year) %>%
        dplyr::filter(occurrence > 0.01) %>%
        dplyr::filter(CV < 1) %>%
        ggplot(aes(x = reorder(SPECIES_CD, occurrence), y = occurrence)) +
        geom_bar(stat = "identity", fill = "deepskyblue4") +
        ggtitle("Species occurrence") +
        theme_light() +
        scale_y_continuous(expand = c(0, 0)) +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              plot.margin = unit(c(t = 1, r = -0.5, b = 1, l = 1), "mm"),
              plot.title = element_text(hjust = 0.5, size = 10, face = "bold")) +
        coord_flip() +
        scale_y_reverse(expand = c(NA, 0)) +
        guides(fill = "none")

      g2 <- region_means %>%
        dplyr::filter(YEAR == year) %>%
        dplyr::filter(occurrence > set_occurrence) %>%
        dplyr::filter(CV < 1) %>%
        ggplot(aes(x = reorder(SPECIES_CD, occurrence), y = CV * 100)) +
        xlab(NULL) +
        geom_bar(stat = "identity", fill = "deepskyblue4") +
        ggtitle("Coefficient of Variation (CV) of density") +
        theme_light() +
        scale_y_continuous(expand = c(0, 0)) +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              plot.margin = unit(c(t = 1, r = 1, b = 1, l = -2), "mm"),
              plot.title = element_text(hjust = 0.5, size = 10, face = "bold")) +
        coord_flip() +
        guides(fill = "none") +
        geom_hline(yintercept = 20, linetype = "dashed", color = "black")

      # Return a list containing all three plots
      return(list(g.mid = g.mid, g1 = g1, g2 = g2))
    }

    #plot region_means
    if (region %in% c("STX", "STTSTJ", "PRICO", "GOM")) {
      resulting_plot_parts <- plot(region_means, set_occurrence = 0.02, year, ptitle)
    } else {
      resulting_plot_parts <- plot(region_means, set_occurrence = 0.01, year, ptitle)
    }

    #use cowplot to plot grid
    cowplot::plot_grid(resulting_plot_parts$g1, resulting_plot_parts$g.mid, resulting_plot_parts$g2, ncol = 3,
                       rel_widths = c(3.5 / 9, 2 / 9, 3.5 / 9))

    #use ggsave to save plots
    ggsave(filename = paste(region_means$REGION[1], "Occ_v_CV.jpeg", sep = "_"),
           path = file_path,
           plot = gridExtra::grid.arrange(resulting_plot_parts$g1, resulting_plot_parts$g.mid, resulting_plot_parts$g2,
                                          ncol = 3, widths = c(3.5 / 9, 2 / 9, 3.5 / 9)),
           width = 9.8,
           height = 6.5,
           dpi = 300,
           units = "in",
           device = "jpg")

    #Prep data for export
    region_means_cv20 <- region_means %>%
      dplyr::filter(CV <= 0.20)

    #Export data
    output <- list(
      "region_means" = region_means,
      "region_means_cv20" = region_means_cv20
    )

    return(output)
  }
