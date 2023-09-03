# set working directory
#setwd('path_to_r_code')
getwd()
# Load necessary libraries
library(gtsummary)
library(ggplot2)
library(patchwork)
library(tictoc)
suppressMessages(library(tidytable))

# Load custom functions from the functions directory
invisible(lapply(list.files('PATH_TO_r_code/functions', pattern = '\\.r$', full.names = TRUE), source))

# Define the function
wf <- function(x) {
    r <- (-7E-11 * x^3 + 2E-06 * x^2 + 0.973 * x + 51.611) - x
    return(r)
}

# Define color palettes for plots
{
    pal1 <- c(
        '#1F77B4FF',  # covid nat 2020
        '#FF7F0EFF',  # ncari nat 2020
        '#2CA02CFF',  # covid kz 2020
        '#D62728FF',  # covid kz 2022
        '#9467BDFF',  # ncari kz 2022
        '#8C564BFF'   # hc
    )

    pal2 <- c(
        '#5B487D',   # covid 2020+2022
        '#E57F76FF',  # ncari 2020+2022
        '#8C564BFF'   # hc
    )

    pal3 <- c(
        '#258B70FF',  # covid 2020
        '#FF7F0EFF',  # ncari 2020
        '#8C564BFF'   # hc
    )

    pal4 <- c(
        '#D62728FF',  # covid kz 2022
        '#9467BDFF',  # ncari kz 2022
        '#8C564BFF'   # hc
    )
}

# Start processing NAT model
{
    tic()

    # Perform spectra processing
    nat_iM <- spectra_processing(
        object_or_path = c(
            "PATH_TO_raw_spectra/nature_spectra/1_CovidTalca_Lab2_Pos",
            "PATH_TO_raw_spectra/nature_spectra/2_CovidArgentina_Lab1_Pos",
            "PATH_TO_raw_spectra/nature_spectra/3_CovidPeru_Lab3_Pos",
            "PATH_TO_raw_spectra/nature_spectra/4_ControlTalca_Lab2_Neg",
            "PATH_TO_raw_spectra/nature_spectra/5_ControlArgentina_Lab1_Neg",
            "PATH_TO_raw_spectra/nature_spectra/6_ControlPeru_Lab3_Neg"
        ),
        wf = wf,
        warp_gr = c(1, 4),
        peak_filt_cutoff = 0.8,
        bin_tol = 0.003,
        spectra_table = TRUE,
        spectra_list = TRUE
    )

    toc()
}

# Export results to Excel
rio::export(nat_iM$iM_peak, "FIG1/peak_of_nat_data.xlsx")

# Generate summary table and save as HTML
nat_iM$iM_peak |>
    select(-sample) |>
    mutate(class = case_when(
        grepl('[Cc]ovid', class) ~ 'Covid',
        TRUE ~ 'Control')
    ) |>
    rename_with(~as.character(round(as.numeric(.x), 0)), starts_with(as.character(1:9))) |>
    tbl_summary(
        by = class,
        missing = "no",
        type = everything() ~ "continuous",
        statistic = all_continuous() ~ c("{median} [{p25}; {p75}]"),
        digits = all_continuous() ~ list(
            \(x) format(x, digits = 2, scientific = TRUE)
        )
    ) |>
    add_p() |>
    add_significance_stars(hide_p = FALSE, pattern = "{p.value}{stars}") |>
    modify_header(label = '**Peak**', all_stat_cols() ~ '**{level}**,  \nN = {n}') |>
    as_gt() |>
    gt::fmt_markdown(columns = everything()) |>
    gt::gtsave('FIG1/peak_comparison_nat_data.html')

# Process peaks on NAT ML
{
    tic()
    peaks_on_nat_ml <- spectra_processing(
        object_or_path = c(
            "PATH_TO_raw_spectra/covid_2020",
            "PATH_TO_raw_spectra/covid_2022",
            "PATH_TO_raw_spectra/control_healthy",
            "PATH_TO_raw_spectra/control_arvi"
        ),
        reference = TRUE,
        ref_im = nat_iM$iM_peak,
        ref_im_aver = "median",
        bin_tol = 0.003,
        spectra_table = TRUE,
        sample_averaging = TRUE,
        sample_averfun = 'median'
    )
    toc()
}

# Export peaks_on_nat_ml$iM_peak data to an Excel file
rio::export(peaks_on_nat_ml$iM_peak, "FIG1/kz_peak_on_nat_data.xlsx")

# Generate a summary table and save it as an HTML file
peaks_on_nat_ml$iM_peak |>
    select(-sample) |>
    rename_with(~as.character(round(as.numeric(.x), 0)), starts_with(as.character(1:9))) |>
    tbl_summary(
        by = class,
        missing = "no",
        type = everything() ~ "continuous",
        statistic = all_continuous() ~ c("{median} [{p25}; {p75}]"),
        digits = all_continuous() ~ list(
            \(x) format(x, digits = 2, scientific = TRUE)
        )
    ) |>
    add_p() |>
    add_significance_stars(hide_p = FALSE, pattern = "{p.value}{stars}") |>
    modify_header(label = '**Peak**', all_stat_cols() ~ '**{level}**,  \nN = {n}') |>
    as_gt() |>
    gt::fmt_markdown(columns = everything()) |>
    gt::gtsave('FIG1/peak_comparison_kz_data_on_nat.html')

################################################
# FIGS
################################################
# FIG1
{
    # Combine NAT and KZ spectra data
    spectra_figs <- bind_rows(
        nat_iM$iM_spectra |>
            mutate(class = case_when(
                grepl('[Cc]ovid', class) ~ 'SARS-CoV-2+, South America',
                TRUE ~ 'NCARI, South America')
            ),
        peaks_on_nat_ml$iM_spectra |>
            mutate(class = case_when(
                class == 'covid_2020' ~ 'SARS-CoV-2+, Kazakhstan, 2020',
                class == 'covid_2022' ~ 'SARS-CoV-2+, Kazakhstan, 2022',
                class == 'control_arvi' ~ 'NCARI, Kazakhstan',
                class == 'control_healthy' ~ 'AC'
            ))
    ) |>
    mutate(class = factor(class, levels = c(
        'SARS-CoV-2+, South America',
        'NCARI, South America',
        'SARS-CoV-2+, Kazakhstan, 2020',
        'SARS-CoV-2+, Kazakhstan, 2022',
        'NCARI, Kazakhstan',
        'AC'
    ))) |>
    reframe(across(where(is.numeric), quantile, prob = c(0.5, 0.25, 0.75), na.rm = TRUE), .by = class) |>
    mutate(stat = rep(c('me', 'q1', 'q3'), times = 6)) |>
    pivot_longer(-c(class, stat), names_to = 'x_pos', values_to = 'values') |>
    mutate(x_pos = as.numeric(x_pos)) |>
    pivot_wider(names_from = 'stat', values_from = 'values') |>
    nest(.by = class) |>
    mutate(
        fig = map2(.x = data, .y = class, \(data, class) data |>
        ggplot() +
        geom_line(aes(x = x_pos, y = me), color = ifelse(grepl('^SARS', class), 'maroon4', 'deepskyblue4')) + 
        geom_ribbon(aes(x = x_pos, ymin = q1, ymax = q3), fill = ifelse(grepl('^SARS', class), 'maroon1', 'deepskyblue'), alpha = 0.35) +
        labs(
            title = class,
            x = expression(paste("Peaks ", italic("(m/z)"))),
            y = ifelse(grepl('Nature', class), 'Relative intensity', '')
        ) +
        theme(
            plot.title = element_text(size = 16),
            plot.margin = margin(0, 0, 0, 0, "mm"),
            axis.title = element_text(face = "italic", size = 11),
            axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5),
            axis.text.y = element_text(size = 13)
        ) +
        scale_x_continuous(breaks = seq(3000, 15500, 500), limits = c(3000, 15500), expand = c(0, 0)) +
        ggforce::facet_zoom(xlim = c(3000, 5500), zoom.size = 1)
        ))

    # Create plots and save them as SVG and PNG
    spectra_plots <- wrap_plots(spectra_figs$fig[c(1, 3, 4, 2, 5, 6)], ncol = 3) +
        plot_annotation(
            title = 'MALDI MS spectra',
            caption = expression(italic('Median and Q1-Q3 IQR')),
            theme = theme(
                plot.title = element_text(size = 22, hjust = 0.5),
                plot.caption = element_text(size = 14)
            )
        )

    ggsave(spectra_plots, filename = 'FIG1/fig_spectra.svg', width = 22, height = 14)
    ggsave(spectra_plots, filename = 'FIG1/fig_spectra.png', width = 22, height = 14)
}

# Process data for KZ region only
{
    spectra_figs1 <- peaks_on_nat_ml$iM_spectra |>
        filter(class %in% c('covid_2020', 'control_arvi', 'control_healthy')) |>
        mutate(
            class = case_when(
                class == 'covid_2020' ~ 'SARS-CoV-2+, Kazakhstan, 2020',
                class == 'control_arvi' ~ 'NCARI, Kazakhstan',
                class == 'control_healthy' ~ 'AC'
            ),
            class = factor(class, levels = c(
                'SARS-CoV-2+, Kazakhstan, 2020',
                'NCARI, Kazakhstan',
                'AC'
            ))
        ) |>
        reframe(across(where(is.numeric), quantile, prob = c(0.5, 0.25, 0.75), na.rm = TRUE), .by = class) |>
        mutate(stat = rep(c('me', 'q1', 'q3'), times = 3)) |>
        pivot_longer(-c(class, stat), names_to = 'x_pos', values_to = 'values') |>
        mutate(x_pos = as.numeric(x_pos)) |>
        pivot_wider(names_from = 'stat', values_from = 'values') |>
        nest(.by = class) |>
        mutate(
            fig = map2(.x = data, .y = class, \(data, class) data |>
            ggplot() +
            geom_line(aes(x = x_pos, y = me), color = ifelse(grepl('^SARS', class), 'maroon4', 'deepskyblue4')) +
            geom_ribbon(aes(x = x_pos, ymin = q1, ymax = q3), fill = ifelse(grepl('^SARS', class), 'maroon1', 'deepskyblue'), alpha = 0.35) +
            labs(
                title = class,
                x = expression(paste("Peaks ", italic("(m/z)"))),
                y = ''
            ) +
            theme(
                plot.title = element_text(size = 16),
                plot.margin = margin(0, 0, 0, 0, "mm"),
                axis.title = element_text(face = "italic", size = 11),
                axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5),
                axis.text.y = element_text(size = 13)
            ) +
            scale_x_continuous(breaks = seq(3000, 15500, 500), limits = c(3000, 15600), expand = c(0, 0)) +
            ggforce::facet_zoom(xlim = c(3000, 5550), zoom.size = 1)
            ))

    # Create plots and save them as SVG and PNG
    spectra_plots1 <- wrap_plots(spectra_figs1$fig, ncol = 3)

    ggsave(spectra_plots1, filename = 'FIG1/fig_spectra1.svg', width = 22, height = 7.5)
    ggsave(spectra_plots1, filename = 'FIG1/fig_spectra1.png', width = 22, height = 7.5)
}

# Process all peaks data
all_peaks <- bind_rows(
    nat_iM$iM_peak,
    peaks_on_nat_ml$iM_peak
) |>
    mutate(class_u = case_when(
        grepl('Lab', class) & grepl('Pos', class) ~ 'SARS-CoV-2+, South America',
        grepl('Lab', class) & grepl('Neg', class) ~ 'NCARI, South America',
        class == 'control_healthy' ~ 'AC',
        class == 'control_arvi' ~ 'NCARI, Kazakhstan',
        class == 'covid_2020' ~ 'SARS-CoV-2+, Kazakhstan, 2020',
        class == 'covid_2022' ~ 'SARS-CoV-2+, Kazakhstan, 2022',
        TRUE ~ class
    ),
    class_u = factor(class_u, levels = c(
        'SARS-CoV-2+, South America',
        'NCARI, South America',
        'SARS-CoV-2+, Kazakhstan, 2020',
        'SARS-CoV-2+, Kazakhstan, 2022',
        'NCARI, Kazakhstan',
        'AC'
    )),
    class_ori = class,
    class = case_when(
        grepl('[Cc]ovid', class) ~ 'Covid',
        TRUE ~ 'Control'
    ),
    class = factor(class, levels = c('Covid', 'Control'))
)



{
perform_pca_and_plot <- function(data, palette, nrow_legend) {
    # load packages
    library(FactoMineR)
    library(factoextra)
    library(ggthemes)
  # Extract only the peaks data
  only_peaks <- data |>
    select(-c(sample, class, class_u, class_ori))

  # Perform PCA
  pca_tlb <- only_peaks |>
    FactoMineR::PCA(graph = FALSE, scale.unit = TRUE)

  # Create PCA plot
  pca_plot <- factoextra::fviz_pca_ind(
    pca_tlb,
    col.ind = data$class_u,
    geom.ind = "point",
    pointshape = 16,
    pointsize = 2.2,
    alpha.ind = 0.8,
    addEllipses = TRUE,
    ellipse.type = "convex",
    palette = palette
  ) +
    ggthemes::theme_tufte() +
    theme(
      plot.title = element_blank(),
      legend.title = element_blank(),
      legend.position = 'top',
      axis.ticks = element_blank(),
      axis.title = element_text(
        size = 12,
        hjust = 0
        ),
      axis.text = element_text(size = 10),
      legend.text = element_text(size = 13.5)
    ) +
    guides(col = guide_legend(nrow = nrow_legend))

  return(pca_plot)
}

pca_plot1 <- all_peaks |>
    perform_pca_and_plot(
        pal1,
        2
    )

ggsave(pca_plot1, file = 'FIG1/pca1.svg', width = 8, height = 8)


pca_plot2 <- all_peaks |>
    mutate(
        class_u = case_when(
            class_u %in% c('SARS-CoV-2+, South America','SARS-CoV-2+, Kazakhstan, 2020', 'SARS-CoV-2+, Kazakhstan, 2022') ~ 'SARS-CoV-2+ [2020+2022]',
            class_u %in% c('NCARI, South America', 'NCARI, Kazakhstan') ~ 'NCARI [2020+2022]',
            class_u == 'AC' ~ 'AC'
        ) |> factor(levels=c('SARS-CoV-2+ [2020+2022]', 'NCARI [2020+2022]', 'AC'))
    ) |>
    perform_pca_and_plot(
        pal2,
        1
    )


pca_plot3 <- all_peaks |>
    filter(
        class_u %in% c(
            'SARS-CoV-2+, South America',
            'SARS-CoV-2+, Kazakhstan, 2020',
            'NCARI, South America',
            'AC')
        ) |>
    mutate(
        class_u = case_when(
            class_u %in% c('SARS-CoV-2+, South America', 'SARS-CoV-2+, Kazakhstan, 2020')  ~ 'SARS-CoV-2+ [2020]',
            class_u %in% c('NCARI, South America') ~ 'NCARI [2020]',
            class_u == 'AC' ~ 'AC'
      ) |>
      factor(levels = c('SARS-CoV-2+ [2020]', 'NCARI [2020]', 'AC'))
    ) |>
    perform_pca_and_plot(
        pal3,
        1
    )


pca_plot4 <- all_peaks |>
    filter(
        class_u %in% c(
            'SARS-CoV-2+, Kazakhstan, 2022',
            'NCARI, Kazakhstan',
            'AC')
        ) |>
    mutate(
        class_u = case_when(
            class_u %in% c('SARS-CoV-2+, Kazakhstan, 2022') ~ 'SARS-CoV-2+ [2022]',
            class_u %in% c('NCARI, Kazakhstan') ~ 'NCARI [2022]',
            class_u == 'AC' ~ 'AC'
      ) |> factor(levels = c('SARS-CoV-2+ [2022]', 'NCARI [2022]', 'AC'))
    ) |>
    perform_pca_and_plot(
        pal4,
        1
    )

all_pca = (pca_plot1 + pca_plot2) / (pca_plot3 + pca_plot4) +
  plot_annotation(tag_levels = 'A')

ggsave(all_pca, file = 'FIG1/all_pca.svg', width = 16, height = 13)
ggsave(all_pca, file = 'FIG1/all_pca.png', width = 16, height = 13, bg = 'white')

}

### Venn diagramm Nat ###
{
pca_tlb <- all_peaks |>
    select(-c(sample, class, class_u, class_ori)) |>
    FactoMineR::PCA(graph = FALSE, scale.unit = TRUE)

# Venn1
venn_tbl1 <- pca_tlb$ind$coord[, 1:2]

v1_tmp1 <- venn_tbl1[which(all_peaks$class_u %in% c('SARS-CoV-2+, South America')), ]
v1_tmp2 <- venn_tbl1[which(all_peaks$class_u %in% c('NCARI, South America')), ]
v1_tmp3 <- venn_tbl1[which(all_peaks$class_u %in% c('SARS-CoV-2+, Kazakhstan, 2020')), ]
v1_tmp4 <- venn_tbl1[which(all_peaks$class_u %in% c('SARS-CoV-2+, Kazakhstan, 2022')), ]
v1_tmp5 <- venn_tbl1[which(all_peaks$class_u %in% c('NCARI, Kazakhstan')), ]
v1_tmp6 <- venn_tbl1[which(all_peaks$class_u %in% c('AC')), ]

v1_h1 = grDevices::chull(v1_tmp1)
v1_h2 = grDevices::chull(v1_tmp2)
v1_h3 = grDevices::chull(v1_tmp3)
v1_h4 = grDevices::chull(v1_tmp4)
v1_h5 = grDevices::chull(v1_tmp5)
v1_h6 = grDevices::chull(v1_tmp6)

v1_pp1 <- v1_tmp1[v1_h1, ]
v1_pp2 <- v1_tmp2[v1_h2, ]
v1_pp3 <- v1_tmp3[v1_h3, ]
v1_pp4 <- v1_tmp4[v1_h4, ]
v1_pp5 <- v1_tmp5[v1_h5, ]
v1_pp6 <- v1_tmp6[v1_h6, ]

v1_pols <- raster::spPolygons(
  v1_pp1,
  v1_pp2,
  v1_pp3,
  v1_pp4,
  v1_pp5,
  v1_pp6
)

v1_e <- raster::extract(v1_pols, venn_tbl1)

v1_list <- list(
  `SARS-CoV-2+\n[Nature 2020]` = v1_e[v1_e[, 2] == 1, 1],
  `NCARI\n[Nature 2020]` = v1_e[v1_e[, 2] == 2, 1],
  `SARS-CoV-2+\n[Kz 2020]` = v1_e[v1_e[, 2] == 3, 1],
  `SARS-CoV-2+\n[Kz 2022]` = v1_e[v1_e[, 2] == 4, 1],
  `NCARI\n[Kz 2022]` = v1_e[v1_e[, 2] == 5, 1],
  `AC` = v1_e[v1_e[, 2] == 6, 1]
)

v1_pl <- ggVennDiagram::ggVennDiagram(
  v1_list,
  set_size = 6,
  label = "percent",
  label_alpha = 20,
  set_color = pal1
) +
scale_x_continuous(expand = expansion(mult = .2)) +
scale_color_manual(values = pal1)

# Venn2
venn_tbl2 <- pca_tlb$ind$coord[, 1:2]

v2_tmp1 <- venn_tbl2[which(all_peaks$class_u %in% c('SARS-CoV-2+, South America','SARS-CoV-2+, Kazakhstan, 2020', 'SARS-CoV-2+, Kazakhstan, 2022')), ]
v2_tmp2 <- venn_tbl2[which(all_peaks$class_u %in% c('NCARI, South America', 'NCARI, Kazakhstan')), ]
v2_tmp3 <- venn_tbl2[which(all_peaks$class_u %in% c('AC')), ]

v2_h1 = grDevices::chull(v2_tmp1)
v2_h2 = grDevices::chull(v2_tmp2)
v2_h3 = grDevices::chull(v2_tmp3)

v2_pp1 <- v2_tmp1[v2_h1, ]
v2_pp2 <- v2_tmp2[v2_h2, ]
v2_pp3 <- v2_tmp3[v2_h3, ]

v2_pols <- raster::spPolygons(
  v2_pp1,
  v2_pp2,
  v2_pp3
)

v2_e <- raster::extract(v2_pols, venn_tbl2)

v2_list <- list(
  `SARS-CoV-2+\n[2020+2022]` = v2_e[v2_e[, 2] == 1, 1],
  `NCARI\n[2020+2022]` = v2_e[v2_e[, 2] == 2, 1],
  `AC` = v2_e[v2_e[, 2] == 3, 1]
)

v2_pl <- ggVennDiagram::ggVennDiagram(
  v2_list,
  set_size = 6,
  label = "percent",
  label_alpha = 20,
  set_color = pal2
) +
scale_x_continuous(expand = expansion(mult = .2)) +
scale_color_manual(values = pal2)

# Venn3
tmp_peaks3 <- all_peaks |>
    filter(class_u %in% c(
      'SARS-CoV-2+, South America',
      'SARS-CoV-2+, Kazakhstan, 2020',
      'NCARI, South America',
      'AC')
      )

only_peaks3 <- tmp_peaks3 |>
    select(-c(sample, class, class_u, class_ori))

pca_tlb3 <- only_peaks3 |>
    FactoMineR::PCA(graph = F, scale.unit = TRUE)


venn_tbl3 <- pca_tlb3$ind$coord[, 1:2]

v3_tmp1 <- venn_tbl3[which(tmp_peaks3$class_u %in% c('SARS-CoV-2+, South America','SARS-CoV-2+, Kazakhstan, 2020')), ]
v3_tmp2 <- venn_tbl3[which(tmp_peaks3$class_u %in% c('NCARI, South America')), ]
v3_tmp3 <- venn_tbl3[which(tmp_peaks3$class_u %in% c('AC')), ]

v3_h1 = grDevices::chull(v3_tmp1)
v3_h2 = grDevices::chull(v3_tmp2)
v3_h3 = grDevices::chull(v3_tmp3)

v3_pp1 <- v3_tmp1[v3_h1, ]
v3_pp2 <- v3_tmp2[v3_h2, ]
v3_pp3 <- v3_tmp3[v3_h3, ]

v3_pols <- raster::spPolygons(
  v3_pp1,
  v3_pp2,
  v3_pp3
)

v3_e <- raster::extract(v3_pols, venn_tbl3)

v3_list <- list(
  `SARS-CoV-2+\n[2020]` = v3_e[v3_e[, 2] == 1, 1],
  `NCARI\n[2020]` = v3_e[v3_e[, 2] == 2, 1],
  `AC` = v3_e[v3_e[, 2] == 3, 1]
)

v3_pl <- ggVennDiagram::ggVennDiagram(
  v3_list,
  set_size = 6,
  label = "percent",
  label_alpha = 20,
  set_color = pal3
) +
scale_x_continuous(expand = expansion(mult = .2)) +
scale_color_manual(values = pal3)

# Venn4
tmp_peaks4 <- all_peaks |>
    filter(class_u %in% c(
      'SARS-CoV-2+, Kazakhstan, 2022',
      'NCARI, Kazakhstan',
      'AC')
      )

only_peaks4 <- tmp_peaks4 |>
    select(-c(sample, class, class_u, class_ori))

pca_tlb4 <- only_peaks4 |>
    FactoMineR::PCA(graph = FALSE, scale.unit = TRUE)

venn_tbl4 <- pca_tlb4$ind$coord[, 1:2]

v4_tmp1 <- venn_tbl4[which(tmp_peaks4$class_u %in% c('SARS-CoV-2+, Kazakhstan, 2022')), ]
v4_tmp2 <- venn_tbl4[which(tmp_peaks4$class_u %in% c('NCARI, Kazakhstan')), ]
v4_tmp3 <- venn_tbl4[which(tmp_peaks4$class_u %in% c('AC')), ]

v4_h1 = grDevices::chull(v4_tmp1)
v4_h2 = grDevices::chull(v4_tmp2)
v4_h3 = grDevices::chull(v4_tmp3)

v4_pp1 <- v4_tmp1[v4_h1, ]
v4_pp2 <- v4_tmp2[v4_h2, ]
v4_pp3 <- v4_tmp3[v4_h3, ]

v4_pols <- raster::spPolygons(
  v4_pp1,
  v4_pp2,
  v4_pp3
)

v4_e <- raster::extract(
  v4_pols,
  venn_tbl4
)

v4_list <- list(
  `SARS-CoV-2+\n[2022]` = v4_e[v4_e[, 2] == 1, 1],
  `NCARI\n[2022]` = v4_e[v4_e[, 2] == 2, 1],
  `AC` = v4_e[v4_e[, 2] == 3, 1]
)

v4_pl <- ggVennDiagram::ggVennDiagram(
  v4_list,
  set_size = 6,
  label = "percent",
  label_alpha = 20,
  set_color = pal4
) +
scale_x_continuous(expand = expansion(mult = .2)) +
scale_color_manual(values = pal4)

all_venn = (v1_pl + v2_pl) / (v3_pl + v4_pl) +
plot_annotation(tag_levels = 'A')

# Save the combined Venn diagram as SVG and PNG
ggsave(all_venn, file = 'FIG1/all_venn.svg', width = 16, height = 16)
ggsave(all_venn, file = 'FIG1/all_venn.png', width = 16, height = 16, bg = 'white')
}

{
### GGTREE_1 ###

library(ggtree)
library(ggtreeExtra)

# Function to create ggtree plot for a given set of peaks and palette
create_ggtree_plot <- function(peaks, palette, filename_prefix) {
  
  # Compute hierarchical clustering
  hc <- peaks |>
    select(-c(sample, class_u, class_ori, class)) |>
    dist(method = 'euclidean') |>
    hclust(method = 'average')
  
  hc$labels <- peaks$sample
  
  # Create the ggtree plot
  ggtree_p <- ggtree::ggtree(
    hc,
    layout = "fan",
    open.angle = 10
  )
  
  ggtree_p$data$x <- ggtree_p$data$x + abs(min(ggtree_p$data$x))
  ggtree_p <- rotate_tree(ggtree_p, -90)
  
  d <- peaks |>
    select(sample, class_u, class_ori, class) |>
    rename(id = sample)

  ggtree_p <- ggtree_p %<+% d
  
  ggtree_p1 <- ggtree_p +
    geom_tippoint(
      mapping = aes(
        colour = class_u
      ),
      size = 3,
      alpha = 0.7
    ) +
    scale_colour_manual(
      name = NA,
      values = palette,
      guide = guide_legend(
        keywidth = 0.8,
        keyheight = 0.8,
        override.aes = list(
          size = 7,
          alpha = 1
        )
      ),
      na.translate = FALSE
    )
  
  nds_c = ggtree_p$data |>
    filter(isTip != TRUE) |>
    pull(node)
  nds_c_info = tidytable()
  
  while(length(nds_c) != 0){
    tmp <- tidytree::offspring(ggtree_p$data, nds_c[1])
    if(tmp |>
       pull(class_u) |>
       na.omit() |>
       unique() |>
       length() == 1){
      nds_c_info <- bind_rows(
        nds_c_info,
        tidytable(
          node = nds_c[1],
          class = tmp |>
            pull(class_u) |>
            na.omit() |>
            unique()
        ) |>
          mutate(pal = palette[as.numeric(class)])
      )
      nds_c <- nds_c[!nds_c %in% tmp$node]
    }
    nds_c <- nds_c[-1]
  }
  
  for(i in 1:nrow(nds_c_info)){
    ggtree_p1 <- ggtree_p1  |>
      collapse(
        node = nds_c_info$node[i],
        mode = 'max',
        clade_name = nds_c_info$class[i],
        alpha = 0.6,
        color = nds_c_info$pal[i],
        fill = nds_c_info$pal[i]
      )
  }
  
  ggtree_pl1 <- ggtree_p1 +
    theme(
      legend.title = element_blank(),
      legend.justification = c(0.5, 0.5),
      legend.position = c(0.46, 0.54),
      legend.background = element_blank(),
      legend.text = element_text(size = 15)
    )
  
  ggsave(ggtree_pl1, file = paste0('FIG1/', filename_prefix, '.svg'), width = 12, height = 12)
  ggsave(ggtree_pl1, file = paste0('FIG1/', filename_prefix, '.png'), width = 12, height = 12)
}

# GGTREE1
all_peaks |>
    create_ggtree_plot(
    pal1,
    'ggtree_A'
    )

# GGTREE2

all_peaks |>
    mutate(
        class_u = case_when(
            grepl('^SARS', class_u) ~ 'SARS-CoV-2+, 2020 & 2022',
            grepl('^NCARI', class_u) ~ 'NCARI, 2020 & 2022',
            class_u == 'AC' ~ 'AC'),
        class_u = factor(
            class_u,
            levels = c(
                'SARS-CoV-2+, 2020 & 2022',
                'NCARI, 2020 & 2022',
                'AC'))
        ) |>
    create_ggtree_plot(
        pal2,
        'ggtree_B'
        )

# GGTREE3
all_peaks |>
    filter(
        class_u %in% c(
            "SARS-CoV-2+, South America",
            "SARS-CoV-2+, Kazakhstan, 2020",
            "NCARI, South America",
            'AC')
    ) |>
    mutate(
      class_u = case_when(
        class_u %in% c("SARS-CoV-2+, South America", "SARS-CoV-2+, Kazakhstan, 2020") ~ 'SARS-CoV-2+, 2020',
        class_u == "NCARI, South America" ~ 'NCARI, South America',
        class_u == 'AC' ~ 'AC'
      ),
      class_u = factor(class_u, levels = c('SARS-CoV-2+, 2020', 'NCARI, South America', 'AC'))
    ) |>
    create_ggtree_plot(
        pal3,
        'ggtree_C'
        )


# GGTREE4
all_peaks |>
    filter(
        class_u %in% c(
            'SARS-CoV-2+, Kazakhstan, 2022',
            'NCARI, Kazakhstan',
            'AC')
    ) |>
    mutate(
      class_u = case_when(
        class_u == 'SARS-CoV-2+, Kazakhstan, 2022' ~ 'SARS-CoV-2+, 2022',
        class_u == 'NCARI, Kazakhstan' ~ 'NCARI, Kazakhstan',
        class_u == 'AC' ~'AC'
      ),
      class_u = factor(class_u, levels = c('SARS-CoV-2+, 2022', 'NCARI, Kazakhstan', 'AC'))
    ) |>
    create_ggtree_plot(
        pal4,
        'ggtree_D'
        )
}

# NAT models
{
#nat_mlModels <- MLmodel(
#  iM = nat_iM$iM_peak |>
#        mutate(
#          fold_gr = case_when(
#            class %in% c('1_CovidTalca_Lab2_Pos')         ~ 1,
#            class %in% c('2_CovidArgentina_Lab1_Pos')     ~ 2,
#            class %in% c('3_CovidPeru_Lab3_Pos')          ~ 3,
#            class %in% c('4_ControlTalca_Lab2_Neg')       ~ 4,
#            class %in% c('5_ControlArgentina_Lab1_Neg')   ~ 5,
#            class %in% c('6_ControlPeru_Lab3_Neg')        ~ 6                                                                           
#            ),
#          class_ori = class,
#          class = tidytable::case_when(
#            grepl('[Cc]ovid', class) ~ 'Covid',
#            .default = 'Control'
#            ),
#          class = factor(class, levels = c('Covid', 'Control'))
#        ),
#  sFunction = caret::twoClassSummary,
#  ncores = 10,
#  nfolds = 5
#  )
#
#save(nat_mlModels, file = "nat_mlModels_5.RData")
load("nat_mlModels_5.RData")
}

# TAB NAT_ML estimation

# Perform Model Estimation
nat_ml_res <- ModelEstimation(
    iM = nat_mlModels$testData,
    mlModels = nat_mlModels$model_for_prediction,
    f_meas = TRUE,
    recall = TRUE,
    accuracy = TRUE,
    spec = TRUE,
    sens = TRUE,
    ppv = TRUE,
    npv = TRUE,
    roc_auc = TRUE
)

# Rename model names for better readability
nat_ml_res <- nat_ml_res |>
  tidytable::mutate(
    model_name = map_chr(model, \(x) x$modelInfo$label),
    model_name = case_when(
      model_name == 'C5.0' ~ 'DT',
      model_name == 'k-Nearest Neighbors' ~ 'KNN',
      model_name == 'Naive Bayes' ~ 'NB',
      model_name == 'Random Forest' ~ 'RF',
      model_name == 'Support Vector Machines with Linear Kernel' ~ 'SVM-L',
      model_name == 'Support Vector Machines with Radial Basis Function Kernel' ~ 'SVM-R',
      model_name == 'eXtreme Gradient Boosting' ~ 'XGBoost'
    )
  )

# Select and format the relevant columns and save the formatted table
nat_ml_res <- nat_ml_res |>
  tidytable::select(model_name, f_meas, recall, accuracy, spec, sens, ppv, npv, roc_auc) |>
  tbl_summary(
    by = model_name,
    missing = "no",
    type = everything() ~ "continuous",
    statistic = all_continuous() ~ c("{median} [{p25}; {p75}]"),
    label = list(
      f_meas ~ "F meas",
      sens ~ "Sensitivity",
      spec ~ "Specificity",
      recall ~ "Recall",
      accuracy ~ "Accuracy",
      ppv ~ "PPV",
      npv ~ "NPV",
      roc_auc ~ "ROC AUC"
    )
  ) |>
    modify_header(label = '**Classification metric**', all_stat_cols() ~ '**{level}**,  \nN = {n}') |>
    as_gt() |>
    gt::fmt_markdown(columns = everything()) |>
    gt::gtsave('FIG1/tab1_nat_ml_comparison.html')

###################################################################
# Dot plot of accuracy of different ML models on different datasets

{
# Load the 'scales' library
library(scales)

# Create a table by binding rows of ModelEstimation results
nat_kz_est_on_natml_tbl <- bind_rows(
  # ModelEstimation for 'SARS-CoV-2+, South America'
  ModelEstimation(
    iM = nat_mlModels$testData |> 
      map(~filter(.x, class == 'Covid') |> 
        mutate(class = factor('Covid', levels = c('Covid', 'Control')))
      ),
    mlModels = nat_mlModels$model_for_prediction,
    accuracy = TRUE
  ) |>
  mutate(
    model_name = map_chr(model, \(x) x$modelInfo$label),
    id = 'SARS-CoV-2+, South America'
  ),
  
  # ModelEstimation for 'NCARI, South America'
  ModelEstimation(
    iM = nat_mlModels$testData |> 
      map(~filter(.x, class == 'Control') |> 
        mutate(class = factor('Control', levels = c('Covid', 'Control')))
      ),
    mlModels = nat_mlModels$model_for_prediction,
    accuracy = TRUE
  ) |>
  mutate(
    model_name = map_chr(model, \(x) x$modelInfo$label),
    id = 'NCARI, South America'
  ),
  
  # ModelEstimation for 'SARS-CoV-2+, Kazakhstan, 2020'
  ModelEstimation(
    iM = peaks_on_nat_ml$iM_peak |> 
      filter(class == 'covid_2020') |> 
      mutate(class = factor('Covid', levels = c('Covid', 'Control'))),
    mlModels = nat_mlModels$model_for_prediction,
    accuracy = TRUE
  ) |>
  mutate(
    model_name = map_chr(model, \(x) x$modelInfo$label),
    id = 'SARS-CoV-2+, Kazakhstan, 2020'
  ),
  
  # ModelEstimation for 'SARS-CoV-2+, Kazakhstan, 2022'
  ModelEstimation(
    iM = peaks_on_nat_ml$iM_peak |> 
      filter(class == 'covid_2022') |> 
      mutate(class = factor('Covid', levels = c('Covid', 'Control'))),
    mlModels = nat_mlModels$model_for_prediction,
    accuracy = TRUE
  ) |>
  mutate(
    model_name = map_chr(model, \(x) x$modelInfo$label),
    id = 'SARS-CoV-2+, Kazakhstan, 2022'
  ),
  
  # ModelEstimation for 'NCARI, Kazakhstan'
  ModelEstimation(
    iM = peaks_on_nat_ml$iM_peak |> 
      filter(class == 'control_arvi') |> 
      mutate(class = factor('Covid', levels = c('Covid', 'Control'))),
    mlModels = nat_mlModels$model_for_prediction,
    accuracy = TRUE
  ) |>
  mutate(
    model_name = map_chr(model, \(x) x$modelInfo$label),
    id = 'NCARI, Kazakhstan'
  ),
  
  # ModelEstimation for 'AC'
  ModelEstimation(
    iM = peaks_on_nat_ml$iM_peak |> 
      filter(class == 'control_healthy') |> 
      mutate(class = factor('Covid', levels = c('Covid', 'Control'))),
    mlModels = nat_mlModels$model_for_prediction,
    accuracy = TRUE
  ) |>
  mutate(
    model_name = map_chr(model, \(x) x$modelInfo$label),
    id = 'AC'
  )
) |>
mutate(
  id = factor(id, levels = c(
    'SARS-CoV-2+, South America',
    'NCARI, South America',
    'SARS-CoV-2+, Kazakhstan, 2020',
    'SARS-CoV-2+, Kazakhstan, 2022',
    'NCARI, Kazakhstan',
    'AC'
  )), 
  model_name = case_when(
    model_name == 'C5.0' ~ 'DT',
    model_name == 'k-Nearest Neighbors' ~ 'KNN',
    model_name == 'Naive Bayes' ~ 'NB',
    model_name == 'Random Forest' ~ 'RF',
    model_name == 'Support Vector Machines with Linear Kernel' ~ 'SVM-L',
    model_name == 'Support Vector Machines with Radial Basis Function Kernel' ~ 'SVM-R',
    model_name == 'eXtreme Gradient Boosting' ~ 'XGBoost'
  ),
  model_name = factor(model_name, levels = c('DT','KNN','NB','RF','SVM-L','SVM-R','XGBoost'))
)

# Calculate pairwise Wilcoxon test results
df_p_val <- nat_kz_est_on_natml_tbl |> 
  rstatix::group_by(model_name) |> 
  rstatix::pairwise_wilcox_test(accuracy ~ id, p.adjust.method = "none") |>
  dplyr::filter(p <= 0.05) |>
  rstatix::add_xy_position(scale = 'free')

# Create a plot using ggplot2
nat_kz_est_on_natml_tbl_plot <- nat_kz_est_on_natml_tbl |>
  ggplot(aes(x = id, y = accuracy)) +
  geom_point(
    aes(fill = id),
    colour = '#0b1164',
    stroke = 0.2,
    size = 4.5,
    shape = 21,
    alpha = 0.6,
    position = position_jitterdodge(
      jitter.width = 0.5,
      dodge.width = 0.7
    )
  ) +
  stat_summary(
    fun = "median",
    geom = "crossbar",
    colour = "#ff0000",
    width = 0.5,
    linewidth = 0.1,
    alpha = 0.75,
    show.legend = FALSE
  ) +
  stat_summary(
    aes(label = round(..y.., 2), fontface = "bold"),
    fun = median,
    geom = "text",
    size = 4,
    alpha = 1,
    label.size = NA,
    vjust = -0.125,
    hjust = 1.15,
    color = "#000000",
    show.legend = FALSE
  ) +
  facet_wrap(~model_name, nrow = 2) +
  labs(
    x = "",
    y = expression(italic("Accuracy"))
  ) +
  theme(
    # Theme settings...
    plot.background = element_rect(colour = 'white', fill = 'white'),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.2),
    panel.background = element_blank(),
    axis.line = element_line(linewidth = 1, colour = "black"),
    strip.background = element_rect(color = "white", fill = "white"),
    strip.text = element_text(size = 19),
    #plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 19),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 9.5, angle = 35, hjust = 0.7, vjust = 0.85),
    legend.spacing.x = unit(0, "pt"),
    legend.position = c(1, 0),
    legend.justification = c(1, 0),
    legend.text = element_text(size = 12), #, margin = margin(r = 10)),
    legend.title = element_blank(),
    panel.grid.major.y = element_line(color = "grey80", size = 0.5, linetype = 2),
    legend.background = element_blank(),
    legend.key = element_rect(fill = NA),
    legend.key.width = unit(0.5, 'cm'),
    legend.key.height = unit(0.5, 'cm')
  ) +
  guides(fill = guide_legend(override.aes = list(alpha = 1, size = 10))) +
  scale_fill_manual(values = pal1) +
  scale_x_discrete(labels = \(x) stringr::str_replace(x, ',', '\n\\'))

# Save the plot as an SVG and PNG file
ggsave(nat_kz_est_on_natml_tbl_plot, file = 'FIG1/nat_kz_est_on_natml_plot1.svg', width = 13, height = 10)
ggsave(nat_kz_est_on_natml_tbl_plot, file = 'FIG1/nat_kz_est_on_natml_plot1.png', width = 13, height = 10, bg = 'white')
}


# ROC curves of 2 ML model - SVR-L and RF
{
roc_auc_tbl_1analysis <- bind_rows(
  # SARS-CoV-2+, Kazakhstan, 2020 vs NCARI, Kazakhstan
  ModelEstimation(
    iM = peaks_on_nat_ml$iM_peak |>
      filter(class %in% c('covid_2020', 'control_arvi')) |>
      mutate(
        class = case_when(
          class == 'covid_2020' ~ 'Covid',
          class == 'control_arvi' ~ 'Control'
        ),
        class = factor(class, levels = c('Covid', 'Control'))
      ),
    mlModels = nat_mlModels$model_for_prediction,
    roc_auc = TRUE
  ) |>
  mutate(
    model_name = map_chr(model, \(x) x$modelInfo$label),
    model_name = case_when(
      model_name == 'C5.0' ~ 'DT',
      model_name == 'k-Nearest Neighbors' ~ 'KNN',
      model_name == 'Naive Bayes' ~ 'NB',
      model_name == 'Random Forest' ~ 'RF',
      model_name == 'Support Vector Machines with Linear Kernel' ~ 'SVM-L',
      model_name == 'Support Vector Machines with Radial Basis Function Kernel' ~ 'SVM-R',
      model_name == 'eXtreme Gradient Boosting' ~ 'XGBoost'
    ),
    model_name = factor(model_name, levels = c('DT', 'KNN', 'NB', 'RF', 'SVM-L', 'SVM-R', 'XGBoost'))
  ) |>
  mutate(id = 'SARS-CoV-2+, Kazakhstan, 2020 vs NCARI, Kazakhstan'),

  # SARS-CoV-2+, Kazakhstan, 2020 vs NCARI, Kazakhstan + HC
  ModelEstimation(
    iM = peaks_on_nat_ml$iM_peak |>
      filter(class %in% c('covid_2020', 'control_arvi', 'control_healthy')) |>
      mutate(
        class = case_when(
          class == 'covid_2020' ~ 'Covid',
          class %in% c('control_arvi', 'control_healthy') ~ 'Control'
        ),
        class = factor(class, levels = c('Covid', 'Control'))
      ),
    mlModels = nat_mlModels$model_for_prediction,
    roc_auc = TRUE
  ) |>
  mutate(
    model_name = map_chr(model, \(x) x$modelInfo$label),
    model_name = case_when(
      model_name == 'C5.0' ~ 'DT',
      model_name == 'k-Nearest Neighbors' ~ 'KNN',
      model_name == 'Naive Bayes' ~ 'NB',
      model_name == 'Random Forest' ~ 'RF',
      model_name == 'Support Vector Machines with Linear Kernel' ~ 'SVM-L',
      model_name == 'Support Vector Machines with Radial Basis Function Kernel' ~ 'SVM-R',
      model_name == 'eXtreme Gradient Boosting' ~ 'XGBoost'
    ),
    model_name = factor(model_name, levels = c('DT', 'KNN', 'NB', 'RF', 'SVM-L', 'SVM-R', 'XGBoost'))
  ) |>
  mutate(id = 'SARS-CoV-2+, Kazakhstan, 2020 vs NCARI, Kazakhstan + AC'),

  # SARS-CoV-2+, Kazakhstan, 2022 vs NCARI, Kazakhstan
  ModelEstimation(
    iM = peaks_on_nat_ml$iM_peak |>
      filter(class %in% c('covid_2022', 'control_arvi')) |>
      mutate(
        class = case_when(
          class == 'covid_2022' ~ 'Covid',
          class == 'control_arvi' ~ 'Control'
        ),
        class = factor(class, levels = c('Covid', 'Control'))
      ),
    mlModels = nat_mlModels$model_for_prediction,
    roc_auc = TRUE
  ) |>
  mutate(
    model_name = map_chr(model, \(x) x$modelInfo$label),
    model_name = case_when(
      model_name == 'C5.0' ~ 'DT',
      model_name == 'k-Nearest Neighbors' ~ 'KNN',
      model_name == 'Naive Bayes' ~ 'NB',
      model_name == 'Random Forest' ~ 'RF',
      model_name == 'Support Vector Machines with Linear Kernel' ~ 'SVM-L',
      model_name == 'Support Vector Machines with Radial Basis Function Kernel' ~ 'SVM-R',
      model_name == 'eXtreme Gradient Boosting' ~ 'XGBoost'
    ),
    model_name = factor(model_name, levels = c('DT', 'KNN', 'NB', 'RF', 'SVM-L', 'SVM-R', 'XGBoost'))
  ) |>
  mutate(id = 'SARS-CoV-2+, Kazakhstan, 2022 vs NCARI, Kazakhstan'),

  # SARS-CoV-2+, Kazakhstan, 2022 vs NCARI, Kazakhstan + HC
  ModelEstimation(
    iM = peaks_on_nat_ml$iM_peak |>
      filter(class %in% c('covid_2022', 'control_arvi', 'control_healthy')) |>
      mutate(
        class = case_when(
          class == 'covid_2022' ~ 'Covid',
          class %in% c('control_arvi', 'control_healthy') ~ 'Control'
        ),
        class = factor(class, levels = c('Covid', 'Control'))
      ),
    mlModels = nat_mlModels$model_for_prediction,
    roc_auc = TRUE
  ) |>
  mutate(
    model_name = map_chr(model, \(x) x$modelInfo$label),
    model_name = case_when(
      model_name == 'C5.0' ~ 'DT',
      model_name == 'k-Nearest Neighbors' ~ 'KNN',
      model_name == 'Naive Bayes' ~ 'NB',
      model_name == 'Random Forest' ~ 'RF',
      model_name == 'Support Vector Machines with Linear Kernel' ~ 'SVM-L',
      model_name == 'Support Vector Machines with Radial Basis Function Kernel' ~ 'SVM-R',
      model_name == 'eXtreme Gradient Boosting' ~ 'XGBoost'
    ),
    model_name = factor(model_name, levels = c('DT', 'KNN', 'NB', 'RF', 'SVM-L', 'SVM-R', 'XGBoost'))
  ) |>
  mutate(id = 'SARS-CoV-2+, Kazakhstan, 2022 vs NCARI, Kazakhstan + AC'),

  # SARS-CoV-2+ [Kz 2020+2023] vs NCARI, Kazakhstan
  ModelEstimation(
    iM = peaks_on_nat_ml$iM_peak |>
      filter(class %in% c('covid_2020', 'covid_2022', 'control_arvi')) |>
      mutate(
        class = case_when(
          class %in% c('covid_2020', 'covid_2022') ~ 'Covid',
          class == 'control_arvi' ~ 'Control'
        ),
        class = factor(class, levels = c('Covid', 'Control'))
      ),
    mlModels = nat_mlModels$model_for_prediction,
    roc_auc = TRUE
  ) |>
  mutate(
    model_name = map_chr(model, \(x) x$modelInfo$label),
    model_name = case_when(
      model_name == 'C5.0' ~ 'DT',
      model_name == 'k-Nearest Neighbors' ~ 'KNN',
      model_name == 'Naive Bayes' ~ 'NB',
      model_name == 'Random Forest' ~ 'RF',
      model_name == 'Support Vector Machines with Linear Kernel' ~ 'SVM-L',
      model_name == 'Support Vector Machines with Radial Basis Function Kernel' ~ 'SVM-R',
      model_name == 'eXtreme Gradient Boosting' ~ 'XGBoost'
    ),
    model_name = factor(model_name, levels = c('DT', 'KNN', 'NB', 'RF', 'SVM-L', 'SVM-R', 'XGBoost'))
  ) |>
  mutate(id = 'SARS-CoV-2+ [Kz 2020 + 2022] vs NCARI, Kazakhstan'),

  # SARS-CoV-2+ [Kz 2020+2022] vs NCARI, Kazakhstan + HC
  ModelEstimation(
    iM = peaks_on_nat_ml$iM_peak |>
      filter(class %in% c('covid_2020', 'covid_2022', 'control_arvi', 'control_healthy')) |>
      mutate(
        class = case_when(
          class %in% c('covid_2020', 'covid_2022') ~ 'Covid',
          class %in% c('control_arvi', 'control_healthy') ~ 'Control'
        ),
        class = factor(class, levels = c('Covid', 'Control'))
      ),
    mlModels = nat_mlModels$model_for_prediction,
    roc_auc = TRUE
  ) |>
  mutate(
    model_name = map_chr(model, \(x) x$modelInfo$label),
    model_name = case_when(
      model_name == 'C5.0' ~ 'DT',
      model_name == 'k-Nearest Neighbors' ~ 'KNN',
      model_name == 'Naive Bayes' ~ 'NB',
      model_name == 'Random Forest' ~ 'RF',
      model_name == 'Support Vector Machines with Linear Kernel' ~ 'SVM-L',
      model_name == 'Support Vector Machines with Radial Basis Function Kernel' ~ 'SVM-R',
      model_name == 'eXtreme Gradient Boosting' ~ 'XGBoost'
    ),
    model_name = factor(model_name, levels = c('DT', 'KNN', 'NB', 'RF', 'SVM-L', 'SVM-R', 'XGBoost'))
  ) |>
  mutate(id = 'SARS-CoV-2+ [Kz 2020 + 2022] vs NCARI, Kazakhstan + AC')
) |>
mutate(
  id = factor(id, levels = c(
        'SARS-CoV-2+, Kazakhstan, 2020 vs NCARI, Kazakhstan',
        'SARS-CoV-2+, Kazakhstan, 2020 vs NCARI, Kazakhstan + AC',
        'SARS-CoV-2+, Kazakhstan, 2022 vs NCARI, Kazakhstan',
        'SARS-CoV-2+, Kazakhstan, 2022 vs NCARI, Kazakhstan + AC',
        'SARS-CoV-2+ [Kz 2020 + 2022] vs NCARI, Kazakhstan',
        'SARS-CoV-2+ [Kz 2020 + 2022] vs NCARI, Kazakhstan + AC'
  ))
)
}

# Table
{
roc_auc_tbl_1analysis |>
select(id, model_name, roc_auc) |>
rename(`AUC value` = roc_auc) |>
tbl_strata(
  strata = model_name,
  ~ .x |>
  tbl_summary(
    by = id,
    type = everything() ~ "continuous",
    statistic = all_continuous() ~ "{median} [{p25}; {p75}]",
    missing = "no"
  ) |>
  modify_header(
    label = '**ROC AUC**',
    all_stat_cols() ~ '**{level}**,  \nN = {n}'
  ),
  .combine_with = "tbl_stack"
) |>
as_gt() |>
gt::tab_style(
  style = gt::cell_text(weight = "bold", align = 'center'),
  locations = gt::cells_row_groups(groups = everything())
) |>
gt::fmt_markdown(columns = everything()) |>
gt::gtsave('FIG1/tab2_auc_kz_on_nat_ml_1analysis.html')
}

# Plot
{
ROC_plot_2020_1analysis <- roc_auc_tbl_1analysis |>
  filter(id == 'SARS-CoV-2+, Kazakhstan, 2020 vs NCARI, Kazakhstan') |>
  filter(
    (model_name == 'RF' & dplyr::near(roc_auc, 0.69, 0.01)) |
    (model_name == 'SVM-L' & dplyr::near(roc_auc, 0.76, 0.005))
  ) |>
  mutate(
    roc = map2(inputed_class, predicted_prob, \(x, y) data.frame(x, y) |>
      yardstick::roc_curve(truth = x, y)),
    model_name = paste0(model_name, ' [AUC = ', round(roc_auc, 3), ']')
  ) |>
  select(roc, model_name) |>
  unnest() |>
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_path(
    aes(color = model_name),
    size = 1.5,
    alpha = 0.5
  ) +
  geom_segment(
    aes(x = 0, y = 0, xend = 1, yend = 1),
    colour = 'grey',
    linetype = 'dotdash'
  ) +
  labs(
    title = "ML model ROC curve",
    x = expression(italic("1 - Specificity")), y = expression(italic('Sensitivity'))
  ) +
  theme(
    plot.background = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line = element_blank(),
    strip.background = element_rect(color = "white", fill = "white"),
    plot.title = element_blank(),
    legend.position = c(1, 0),
    legend.justification = c(1, 0),
    axis.title = element_text(size = 19),
    axis.text = element_text(size = 15),
    axis.ticks = element_blank(),
    legend.spacing = unit(0, "pt"),
    legend.text = element_text(size = 16),
    legend.title = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.background = element_blank(),
    legend.key = element_rect(fill = NA),
    legend.key.width = unit(2, 'cm'),
    legend.key.height = unit(1.5, 'cm')
  ) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 8))) +
  scale_color_manual(values = c('#b80000', '#020292'))

ggsave(ROC_plot_2020_1analysis, file = 'FIG1/ROC_plot_2020_1analysis.svg', width = 10, height = 10)
ggsave(ROC_plot_2020_1analysis, file = 'FIG1/ROC_plot_2020_1analysis.png', width = 10, height = 10, bg = 'white')

ROC_plot_2020_2022_1analysis <- roc_auc_tbl_1analysis |>
  filter(id == 'SARS-CoV-2+ [Kz 2020 + 2022] vs NCARI, Kazakhstan + AC') |>
  filter(
    (model_name == 'RF' & dplyr::near(roc_auc, 0.70, 0.0099)) |
    (model_name == 'SVM-L' & dplyr::near(roc_auc, 0.73, 0.012))
  ) |>
  mutate(
    roc = map2(inputed_class, predicted_prob, \(x, y) data.frame(x, y) |>
      yardstick::roc_curve(truth = x, y)),
    model_name = paste0(model_name, ' [AUC = ', round(roc_auc, 3), ']')
  ) |>
  select(roc, model_name) |>
  unnest() |>
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_path(
    aes(color = model_name),
    size = 1.5,
    alpha = 0.5
  ) +
  geom_segment(
    aes(x = 0, y = 0, xend = 1, yend = 1),
    colour = 'grey',
    linetype = 'dotdash'
  ) +
  labs(
    title = "ML model ROC curve",
    x = expression(italic("1 - Specificity")), y = expression(italic('Sensitivity'))
  ) +
  theme(
    plot.background = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line = element_blank(),
    strip.background = element_rect(color = "white", fill = "white"),
    plot.title = element_blank(),
    legend.position = c(1, 0),
    legend.justification = c(1, 0),
    axis.title = element_text(size = 19),
    axis.text = element_text(size = 15),
    axis.ticks = element_blank(),
    legend.spacing = unit(0, "pt"),
    legend.text = element_text(size = 16),
    legend.title = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.background = element_blank(),
    legend.key = element_rect(fill = NA),
    legend.key.width = unit(2, 'cm'),
    legend.key.height = unit(1.5, 'cm')
  ) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 8))) +
  scale_color_manual(values = c('#b80000', '#020292'))

ggsave(ROC_plot_2020_2022_1analysis, file = 'FIG1/ROC_plot_2020_2022_1analysis.svg', width = 10, height = 10)
ggsave(ROC_plot_2020_2022_1analysis, file = 'FIG1/ROC_plot_2020_2022_1analysis.png', width = 10, height = 10, bg = 'white')
}

{
tic()
    nat_kz_3l_iM <- spectra_processing(
        object_or_path = c(
            "PATH_TO_raw_spectra/nature_spectra/1_CovidTalca_Lab2_Pos",
            "PATH_TO_raw_spectra/nature_spectra/2_CovidArgentina_Lab1_Pos",
            "PATH_TO_raw_spectra/nature_spectra/3_CovidPeru_Lab3_Pos",
            "PATH_TO_raw_spectra/nature_spectra/4_ControlTalca_Lab2_Neg",
            "PATH_TO_raw_spectra/nature_spectra/5_ControlArgentina_Lab1_Neg",
            "PATH_TO_raw_spectra/nature_spectra/6_ControlPeru_Lab3_Neg",
            "PATH_TO_raw_spectra/covid_2020",
            "PATH_TO_raw_spectra/covid_2022",
            "PATH_TO_raw_spectra/control_arvi",
            "PATH_TO_raw_spectra/control_healthy"
        ),
    wf = wf,
    warp_gr = c(1, 4),
    peak_filt_cutoff = 0.8,
    bin_tol = 0.003,
    spectra_table = TRUE,
    spectra_list = TRUE,
    sample_averaging = TRUE,
    sample_averfun = 'median')
toc()
}

# Export results to Excel
rio::export(nat_kz_3l_iM$iM_peak, file = "FIG1/nat_kz_3l_peak.xlsx")

# How many peaks are similar/identical in both analysis

## when round to integers
(nat_kz_3l_iM$iM_peak |> select(-c(sample, class)) |> names() |> as.numeric() |> round(digits = 0))  %in% 
(nat_iM$iM_peak |> select(-c(sample, class)) |> names() |> as.numeric() |> round(digits = 0)) |> table() |> prop.table() |> round(digits = 2)

## with tolerance 

vec1 = nat_kz_3l_iM$iM_peak |> select(-c(sample, class)) |> names() |> as.numeric() 
vec2 = nat_iM$iM_peak       |> select(-c(sample, class)) |> names() |> as.numeric() 

m1 <- which(abs(outer(vec1, vec2, `-`)) < 1 , arr.ind = TRUE)

cbind(g = vec1[m1[,1]], h = vec2[m1[,2]]) |>
data.frame() |>
mutate(d = abs(g - h)) |>
#filter(d <= 0.5) |>
nrow()


########
{
# Generate summary table and save as HTML
nat_kz_3l_iM$iM_peak |>
  select(-sample) |>
  rename_with(~ as.character(round(as.numeric(.x), 0)), starts_with(as.character(1:9))) |>
  tbl_summary(
    by = class,
    missing = "no",
    type = everything() ~ "continuous",
    statistic = ~ "{median} [{p25}; {p75}]",
    digits = all_continuous() ~ list(
      \(x) format(x, digits = 2, scientific = TRUE)
    )
  ) |>
  add_p() |>
  add_significance_stars(hide_p = FALSE, pattern = "{p.value}{stars}") |>
  modify_header(label = '**Peak**', all_stat_cols() ~ '**{level}**,  \nN = {n}') |>
  as_gt() |>
  gt::fmt_markdown(columns = everything()) |>
  gt::gtsave('FIG1/peak_comparison_nat_kz_3l_iM.html')


# Generate Spectra Figures
spectra_figs_3l <- nat_kz_3l_iM$iM_spectra |>
  mutate(
    class = case_when(
      class %in% c('1_CovidTalca_Lab2_Pos', '2_CovidArgentina_Lab1_Pos', '3_CovidPeru_Lab3_Pos') ~ 'SARS-CoV-2+, South America',
      class %in% c('4_ControlTalca_Lab2_Neg', '5_ControlArgentina_Lab1_Neg', '6_ControlPeru_Lab3_Neg') ~ 'NCARI, South America',
      class == 'covid_2020' ~ 'SARS-CoV-2+, Kazakhstan, 2020',
      class == 'covid_2022' ~ 'SARS-CoV-2+, Kazakhstan, 2022',
      class == 'control_arvi' ~ 'NCARI, Kazakhstan',
      class == 'control_healthy' ~ 'AC'
    ),
    class = factor(class, levels = c(
      'SARS-CoV-2+, South America',
      'NCARI, South America',
      'SARS-CoV-2+, Kazakhstan, 2020',
      'SARS-CoV-2+, Kazakhstan, 2022',
      'NCARI, Kazakhstan',
      'AC'
    ))
  ) |>
  reframe(across(where(is.numeric), quantile, prob = c(0.5, 0.25, 0.75), na.rm = TRUE), .by = class) |>
  mutate(stat = rep(c('me', 'q1', 'q3'), times = 6)) |>
  pivot_longer(-c(class, stat), names_to = 'x_pos', values_to = 'values') |>
  mutate(x_pos = as.numeric(x_pos)) |>
  pivot_wider(names_from = 'stat', values_from = 'values') |>
  nest(.by = class) |>
  mutate(
    fig = map2(.x = data, .y = class, \(data, class) data |>
      ggplot() +
      geom_line(aes(x = x_pos, y = me), color = ifelse(grepl('^SARS', class), 'maroon4', 'deepskyblue4')) +
      geom_ribbon(aes(x = x_pos, ymin = q1, ymax = q3), fill = ifelse(grepl('^SARS', class), 'maroon1', 'deepskyblue'), alpha = 0.35) +
      labs(
        title = class,
        x = expression(paste("Peaks ", italic("(m/z)"))),
        y = ifelse(grepl('Nature', class), 'Relative intensity', '')
      ) +
      theme(
        plot.title = element_text(size = 16),
        plot.margin = margin(0, 0, 0, 0, "mm"),
        axis.title = element_text(face = "italic", size = 11),
        axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5),
        axis.text.y = element_text(size = 13)
      ) +
      scale_x_continuous(breaks = seq(3000, 15500, 500), limits = c(3000, 15500), expand = c(0, 0)) +
      ggforce::facet_zoom(xlim = c(3000, 5500), zoom.size = 1)
    ))

# Combine Spectra Plots and Annotate
spectra_plots_3l <- wrap_plots(spectra_figs_3l$fig[c(1, 3, 4, 2, 5, 6)], ncol = 3) +
  plot_annotation(
    title = 'MALDI MS spectra',
    caption = expression(italic('Median and Q1-Q3 IQR')),
    theme = theme(
      plot.title = element_text(size = 22, hjust = 0.5),
      plot.caption = element_text(size = 14)
    )
  )

# Save Spectra Plots as SVG and PNG
ggsave(spectra_plots_3l, filename = 'FIG1/spectra_plots_3l.svg', width = 22, height = 14)
ggsave(spectra_plots_3l, filename = 'FIG1/spectra_plots_3l.png', width = 22, height = 14)

}


# Clean and categorize class column in the all_peaks3l dataframe
all_peaks3l <- nat_kz_3l_iM$iM_peak |>
  mutate(
    # Create a new column class_u to categorize classes
    class_u = case_when(
      grepl('Lab', class) & grepl('Pos', class) ~ 'SARS-CoV-2+, South America',
      grepl('Lab', class) & grepl('Neg', class) ~ 'NCARI, South America',
      grepl('control_healthy', class) ~ 'AC',
      grepl('control_arvi', class) ~ 'NCARI, Kazakhstan',
      grepl('covid_2020', class) ~ 'SARS-CoV-2+, Kazakhstan, 2020',
      grepl('covid_2022', class) ~ 'SARS-CoV-2+, Kazakhstan, 2022',
      .default = class
    ),
    # Convert class_u to a factor with specified levels
    class_u = factor(class_u, levels = c(
      'SARS-CoV-2+, South America',
      'NCARI, South America',
      'SARS-CoV-2+, Kazakhstan, 2020',
      'SARS-CoV-2+, Kazakhstan, 2022',
      'NCARI, Kazakhstan',
      'AC'
    )),
    # Create a new column class_ori to store the original class values
    class_ori = class,
    # Create a new column class to categorize classes into 'Covid' and 'Control'
    class = case_when(
      grepl('[Cc]ovid', class) ~ 'Covid',
      .default = 'Control'
    ),
    # Convert class to a factor with levels 'Covid' and 'Control'
    class = factor(class, levels = c('Covid', 'Control'))
  )

# PCA plots
{
pca_plot3l_1 <- all_peaks3l |>
    perform_pca_and_plot(
        pal1,
        2
    )

pca_plot3l_2 <- all_peaks3l |>
    mutate(
        class_u = case_when(
            class_u %in% c('SARS-CoV-2+, South America','SARS-CoV-2+, Kazakhstan, 2020', 'SARS-CoV-2+, Kazakhstan, 2022') ~ 'SARS-CoV-2+ [2020+2022]',
            class_u %in% c('NCARI, South America', 'NCARI, Kazakhstan') ~ 'NCARI [2020+2022]',
            class_u == 'AC' ~ 'AC'
        ) |> factor(levels=c('SARS-CoV-2+ [2020+2022]', 'NCARI [2020+2022]', 'AC'))
    ) |>
    perform_pca_and_plot(
        pal2,
        1
    )

pca_plot3l_3 <- all_peaks3l |>
    filter(
        class_u %in% c(
            'SARS-CoV-2+, South America',
            'SARS-CoV-2+, Kazakhstan, 2020',
            'NCARI, South America',
            'AC')
        ) |>
    mutate(
        class_u = case_when(
            class_u %in% c('SARS-CoV-2+, South America', 'SARS-CoV-2+, Kazakhstan, 2020')  ~ 'SARS-CoV-2+ [2020]',
            class_u %in% c('NCARI, South America') ~ 'NCARI [2020]',
            class_u == 'AC' ~ 'AC'
      ) |>
      factor(levels = c('SARS-CoV-2+ [2020]', 'NCARI [2020]', 'AC'))
    ) |>
    perform_pca_and_plot(
        pal3,
        1
    )


pca_plot3l_4 <- all_peaks3l |>
    filter(
        class_u %in% c(
            'SARS-CoV-2+, Kazakhstan, 2022',
            'NCARI, Kazakhstan',
            'AC')
        ) |>
    mutate(
        class_u = case_when(
            class_u %in% c('SARS-CoV-2+, Kazakhstan, 2022') ~ 'SARS-CoV-2+ [2022]',
            class_u %in% c('NCARI, Kazakhstan') ~ 'NCARI [2022]',
            class_u == 'AC' ~ 'AC'
      ) |> factor(levels = c('SARS-CoV-2+ [2022]', 'NCARI [2022]', 'AC'))
    ) |>
    perform_pca_and_plot(
        pal4,
        1
    )

all_pca3l = (pca_plot3l_1 + pca_plot3l_2) / (pca_plot3l_3 + pca_plot3l_4) +
  plot_annotation(tag_levels = 'A')

ggsave(all_pca3l, file = 'FIG1/all_pca_3l.svg', width = 16, height = 13)
ggsave(all_pca3l, file = 'FIG1/all_pca_3l.png', width = 16, height = 13, bg = 'white')

}

## GGTREES
{
# GGTREE1
all_peaks3l |>
    create_ggtree_plot(
    pal1,
    'ggtree_A_3l'
    )

# GGTREE2

all_peaks3l |>
    mutate(
        class_u = case_when(
            grepl('^SARS', class_u) ~ 'SARS-CoV-2+, 2020 & 2022',
            grepl('^NCARI', class_u) ~ 'NCARI, 2020 & 2022',
            class_u == 'AC' ~ 'AC'),
        class_u = factor(
            class_u,
            levels = c(
                'SARS-CoV-2+, 2020 & 2022',
                'NCARI, 2020 & 2022',
                'AC'))
        ) |>
    create_ggtree_plot(
        pal2,
        'ggtree_B_3l'
        )

# GGTREE3
all_peaks3l |>
    filter(
        class_u %in% c(
            "SARS-CoV-2+, South America",
            "SARS-CoV-2+, Kazakhstan, 2020",
            "NCARI, South America",
            'AC')
    ) |>
    mutate(
      class_u = case_when(
        class_u %in% c("SARS-CoV-2+, South America", "SARS-CoV-2+, Kazakhstan, 2020") ~ 'SARS-CoV-2+, 2020',
        class_u == "NCARI, South America" ~ 'NCARI, South America',
        class_u == 'AC' ~ 'AC'
      ),
      class_u = factor(class_u, levels = c('SARS-CoV-2+, 2020', 'NCARI, South America', 'AC'))
    ) |>
    create_ggtree_plot(
        pal3,
        'ggtree_C_3l'
        )


# GGTREE4
all_peaks3l |>
    filter(
        class_u %in% c(
            'SARS-CoV-2+, Kazakhstan, 2022',
            'NCARI, Kazakhstan',
            'AC')
    ) |>
    mutate(
      class_u = case_when(
        class_u == 'SARS-CoV-2+, Kazakhstan, 2022' ~ 'SARS-CoV-2+, 2022',
        class_u == 'NCARI, Kazakhstan' ~ 'NCARI, Kazakhstan',
        class_u == 'AC' ~'AC'
      ),
      class_u = factor(class_u, levels = c('SARS-CoV-2+, 2022', 'NCARI, Kazakhstan', 'AC'))
    ) |>
    create_ggtree_plot(
        pal4,
        'ggtree_D_3l'
        )
}


#################
# Nat + Kz models with 3 labels
{
#nat_kz_mlModels_3l <- MLmodel(
#  iM = nat_kz_3l_iM$iM_peak |>
#        mutate(
#      fold_gr = case_when(
#        class %in% c('1_CovidTalca_Lab2_Pos')         ~ 1,
#        class %in% c('2_CovidArgentina_Lab1_Pos')     ~ 2,
#        class %in% c('3_CovidPeru_Lab3_Pos')          ~ 3,
#        class %in% c('covid_2020')                    ~ 4,
#        class %in% c('covid_2022')                    ~ 5,
#        class %in% c('4_ControlTalca_Lab2_Neg')       ~ 6,
#        class %in% c('5_ControlArgentina_Lab1_Neg')   ~ 7,
#        class %in% c('6_ControlPeru_Lab3_Neg')        ~ 8,
#        class %in% c('control_arvi')                  ~ 9,
#        class %in% c('control_healthy')               ~ 10                                                                           
#        ),
#      class_ori = class, 
#      class = case_when(
#        class %in% c('1_CovidTalca_Lab2_Pos','2_CovidArgentina_Lab1_Pos', '3_CovidPeru_Lab3_Pos', 'covid_2020', 'covid_2022') ~ factor('SARS.CoV.2', levels = c('Healthy.Controls', 'non.SARS.CoV.2.ARI', 'SARS.CoV.2')),
#        class %in% c('4_ControlTalca_Lab2_Neg','5_ControlArgentina_Lab1_Neg','6_ControlPeru_Lab3_Neg', 'control_arvi')        ~ factor('non.SARS.CoV.2.ARI', levels = c('Healthy.Controls', 'non.SARS.CoV.2.ARI', 'SARS.CoV.2')),
#        class == 'control_healthy'                                                                                            ~ factor('Healthy.Controls', levels = c('Healthy.Controls', 'non.SARS.CoV.2.ARI', 'SARS.CoV.2'))
#      )),
#  algorithmList = c("C5.0", "knn", "naive_bayes", "rf", "svmLinear", "svmRadial",  "xgbTree"),
#  sFunction = caret::multiClassSummary,
#  ncores = 10,
#  nfolds = 5
#  )
#
#save(nat_kz_mlModels_3l, file = "nat_kz_mlModels_3l_5.RData")
load("nat_kz_mlModels_3l_5.RData")
}

# Update the model table nat_kz_mlModels_3l
nat_kz_mlModels_3l <- nat_kz_mlModels_3l |>
  mutate(
    # Extract the model names from model_for_prediction
    model_name = map_chr(model_for_prediction, \(x) x$modelInfo$label),
    # Rename model names to shorter labels
    model_name = case_when(
      model_name == 'C5.0' ~ 'DT',
      model_name == 'k-Nearest Neighbors' ~ 'KNN',
      model_name == 'Naive Bayes' ~ 'NB',
      model_name == 'Random Forest' ~ 'RF',
      model_name == 'Support Vector Machines with Linear Kernel' ~ 'SVML',
      model_name == 'Support Vector Machines with Radial Basis Function Kernel' ~ 'SVMR',
      model_name == 'eXtreme Gradient Boosting' ~ 'XGBoost'
    )
  ) |>
  arrange(model_name) # Arrange the table by the updated model names

# Dot plot
{
  library(scales)
  
  # Combine ModelEstimation results for different groups
  nat_kz_mlModels_3l_tbl <- bind_rows(
    ModelEstimation(
      iM = nat_kz_mlModels_3l$testData |> map(~ filter(.x, grepl('1_CovidTalca_Lab2_Pos|2_CovidArgentina_Lab1_Pos|3_CovidPeru_Lab3_Pos', class_ori))),
      mlModels = nat_kz_mlModels_3l$model_for_prediction,
      accuracy = TRUE
    ) |>
    mutate(
      model_name = map_chr(model, \(x) x$modelInfo$label),
      id = 'SARS-CoV-2+, South America'
    ), 
    
    ModelEstimation(
      iM = nat_kz_mlModels_3l$testData |> map(~ filter(.x, grepl('^4_ControlTalca_Lab2_Neg|^5_ControlArgentina_Lab1_Neg|^6_ControlPeru_Lab3_Neg', class_ori))),
      mlModels = nat_kz_mlModels_3l$model_for_prediction,
      accuracy = TRUE
    ) |>
    mutate(
      model_name = map_chr(model, \(x) x$modelInfo$label),
      id = 'NCARI, South America'
    ),
    
    ModelEstimation(
      iM = nat_kz_mlModels_3l$testData |> map(~ filter(.x, grepl('^covid_2020|^covid_2022', class_ori))),
      mlModels = nat_kz_mlModels_3l$model_for_prediction,
      accuracy = TRUE
    ) |>
    mutate(
      model_name = map_chr(model, \(x) x$modelInfo$label),
      id = 'SARS-CoV-2+, Kazakhstan, 2020 & 2022'
    ),
    
    ModelEstimation(
      iM = nat_kz_mlModels_3l$testData |> map(~ filter(.x, grepl('^control_arvi', class_ori))),
      mlModels = nat_kz_mlModels_3l$model_for_prediction,
      accuracy = TRUE
    ) |>
    mutate(
      model_name = map_chr(model, \(x) x$modelInfo$label),
      id = 'NCARI, Kazakhstan'
    ),
    
    ModelEstimation(
      iM = nat_kz_mlModels_3l$testData |> map(~ filter(.x, grepl('^control_healthy', class_ori))),
      mlModels = nat_kz_mlModels_3l$model_for_prediction,
      accuracy = TRUE
    ) |>
    mutate(
      model_name = map_chr(model, \(x) x$modelInfo$label),
      id = 'AC'
    )
  ) |>
  mutate(
    id = factor(id, , levels = c(
      'SARS-CoV-2+, South America',
      'NCARI, South America',
      'SARS-CoV-2+, Kazakhstan, 2020 & 2022',
      'NCARI, Kazakhstan',
      'AC'
    )), 
    model_name = case_when(
      model_name == 'C5.0' ~ 'DT',
      model_name == 'k-Nearest Neighbors' ~ 'KNN',
      model_name == 'Naive Bayes' ~ 'NB',
      model_name == 'Random Forest' ~ 'RF',
      model_name == 'Support Vector Machines with Linear Kernel' ~ 'SVM-L',
      model_name == 'Support Vector Machines with Radial Basis Function Kernel' ~ 'SVM-R',
      model_name == 'eXtreme Gradient Boosting' ~ 'XGBoost'
    ),
    model_name = factor(model_name, levels = c('DT','KNN','NB','RF','SVM-L','SVM-R','XGBoost'))
  )
  
  # Create the dot plot
  nat_kz_mlModels_3l_plot <- nat_kz_mlModels_3l_tbl |>
    ggplot(aes(x = id, y = accuracy)) +
    geom_point(
      aes(fill = id),
      colour = '#0b1164',
      stroke = 0.2,
      size = 4.5,
      shape = 21,
      alpha = 0.6,
      position = position_jitterdodge(
        jitter.width = 0.5,
        dodge.width = 0.7
      )
    ) +
    stat_summary(
      fun = "median",
      geom = "crossbar",
      colour = "#ff0000",
      width = 0.5,
      linewidth = 0.1,
      alpha = 0.75,
      show.legend = FALSE
    ) +
    stat_summary(
      aes(label = round(..y.., 2), fontface = "bold"),
      fun = median,
      geom = "text",
      size = 4,
      alpha = 1,
      label.size = NA,
      vjust = -0.125,
      hjust = 1.15,
      color = "#000000",
      show.legend = FALSE
    ) +
    facet_wrap(~model_name, nrow = 2) +
    labs(
      x = "",
      y = expression(italic("Accuracy"))
    ) +
    theme(
      plot.background = element_rect(colour = 'white', fill = 'white'),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.2),
      panel.background = element_blank(),
      axis.line = element_line(linewidth = 1, colour = "black"),
      strip.background = element_rect(color = "white", fill = "white"),
      strip.text = element_text(size = 19),
      axis.title.y = element_text(size = 19),
      axis.text.y = element_text(size = 15),
      axis.title.x = element_blank(),
      axis.text.x = element_text(size = 9.5, angle = 35, hjust = 0.7, vjust = 0.85),
      legend.spacing.x = unit(0, "pt"),
      legend.position = c(1, 0),
      legend.justification = c(1, 0),
      legend.text = element_text(size = 10),
      legend.title = element_blank(),
      panel.grid.major.y = element_line(color = "grey80", size = 0.5, linetype = 2),
      legend.background = element_blank(),
      legend.key = element_rect(fill = NA),
      legend.key.width = unit(0.5, 'cm'),
      legend.key.height = unit(0.5, 'cm')
    ) +
    guides(fill = guide_legend(override.aes = list(alpha = 1, size = 10))) +
    scale_fill_manual(values = pal1) +
    scale_x_discrete(labels = \(x) stringr::str_replace(x, ',', '\n\\'))
    
  # Save the plot as SVG and PNG
  ggsave(nat_kz_mlModels_3l_plot, file = 'FIG1/nat_kz_mlModels_3l_plot1.svg', width = 13, height = 10)
  ggsave(nat_kz_mlModels_3l_plot, file = 'FIG1/nat_kz_mlModels_3l_plot1.png', width = 13, height = 10, bg = 'white')
}


{
# AUC and ROC table
AUC_ROC_2 <- nat_kz_mlModels_3l |>
  mutate(
    # Rename model names
    model_name = case_when(
      model_name == 'SVML' ~ 'SVM-L',
      model_name == 'SVMR' ~ 'SVM-R', 
      TRUE ~ model_name
    ),
    model_name = factor(model_name, levels = c('DT', 'KNN', 'NB', 'RF', 'SVM-L', 'SVM-R', 'XGBoost')),
    # AUC 2020
    auc_val_2020 = pmap(
      .l = list(
        input = testData |>
          map(~ filter(.x, grepl('^covid_2020|^control_arvi|^control_healthy', class_ori))),
        model = model_for_prediction,
        mname = model_name
      ),
      \(input, model, mname) {
        pred_ml = predict(
          model,
          input |>
            select(-c(class, sample, class_ori)),
          type = 'prob'
        ) |>
        data.frame() |>
        rename_with(~ paste(c('Healthy_Controls', 'non_SARS_CoV2_ARI', 'SARS_CoV2'), 'pred_', sep = "_"))

        true_l = data.frame(true_l = input |>
            pull(class)) |>
          mutate(rn = row_number(), val = 1) |>
          pivot_wider(names_from = true_l, values_from = val, values_fill = 0) |>
          select(-rn) |>
          rename_with(~ paste(c('Healthy_Controls', 'non_SARS_CoV2_ARI', 'SARS_CoV2'), 'true', sep = "_"))

        final_tbl = cbind(true_l, pred_ml) 

        roc_res = multiROC::multi_roc(final_tbl, force_diag = TRUE)

        roc_res$AUC |>
          unlist()
      }
    ), 
    # AUC 2020+2022 
    auc_val_2020_2022 = pmap(
      .l = list(
        input = testData |>
          map(~ filter(.x, grepl('^covid_2020|^covid_2022|^control_arvi|^control_healthy', class_ori))),
        model = model_for_prediction,
        mname = model_name
      ),
      \(input, model, mname) {
        pred_ml = predict(
          model,
          input |>
            select(-c(class, sample, class_ori)),
          type = 'prob'
        ) |>
        data.frame() |>
        rename_with(~ paste(c('Healthy_Controls', 'non_SARS_CoV2_ARI', 'SARS_CoV2'), 'pred_', sep = "_"))

        true_l = data.frame(true_l = input |>
            pull(class)) |>
          mutate(rn = row_number(), val = 1) |>
          pivot_wider(names_from = true_l, values_from = val, values_fill = 0) |>
          select(-rn) |>
          rename_with(~ paste(c('Healthy_Controls', 'non_SARS_CoV2_ARI', 'SARS_CoV2'), 'true', sep = "_"))

        final_tbl <- cbind(true_l, pred_ml)

        roc_res <- multiROC::multi_roc(final_tbl, force_diag = TRUE)

        roc_res$AUC |>
          unlist()
      }
    ),
    # ROC plot data 2020
    roc_plot_data_2020 = map2(
      .x = testData |>
        map(~ filter(.x, grepl('^covid_2020|^control_arvi|^control_healthy', class_ori))),
      .y = model_for_prediction,
      \(input, model) {
        pred_ml = predict(
          model,
          input |>
            select(-c(class, sample, class_ori)),
          type = 'prob'
        ) |>
        data.frame() |>
        rename_with(~ paste(c('Healthy_Controls', 'non_SARS_CoV2_ARI', 'SARS_CoV2'), 'pred_', sep = "_"))

        true_l <- data.frame(true_l = input |>
            pull(class)) |>
          mutate(rn = row_number(), val = 1) |>
          pivot_wider(names_from = true_l, values_from = val, values_fill = 0) |>
          select(-rn) |>
          rename_with(~ paste(c('Healthy_Controls', 'non_SARS_CoV2_ARI', 'SARS_CoV2'), 'true', sep = "_"))

        final_tbl <- cbind(true_l, pred_ml)

        roc_res <- multiROC::multi_roc(final_tbl, force_diag = TRUE)

        plot_roc_df <- multiROC::plot_roc_data(roc_res)
      }
    ),
    # ROC plot data 2020 + 2023
    roc_plot_data_2020_2022 = map2(
      .x = testData |>
        map(~ filter(.x, grepl('^covid_2020|^covid_2022|^control_arvi|^control_healthy', class_ori))),
      .y = model_for_prediction,
      \(input, model) {
        pred_ml = predict(
          model,
          input |>
            select(-c(class, sample, class_ori)),
          type = 'prob'
        ) |>
        data.frame() |>
        rename_with(~ paste(c('Healthy_Controls', 'non_SARS_CoV2_ARI', 'SARS_CoV2'), 'pred_', sep = "_"))

        true_l <- data.frame(true_l = input |>
            pull(class)) |>
          mutate(rn = row_number(), val = 1) |>
          pivot_wider(names_from = true_l, values_from = val, values_fill = 0) |>
          select(-rn) |>
          rename_with(~ paste(c('Healthy_Controls', 'non_SARS_CoV2_ARI', 'SARS_CoV2'), 'true', sep = "_"))

        final_tbl <- cbind(true_l, pred_ml)

        roc_res <- multiROC::multi_roc(final_tbl, force_diag = TRUE)

        plot_roc_df <- multiROC::plot_roc_data(roc_res)
      }
    )
  )


{
# Table AUC val comparison
AUC_ROC_2 |> 
  # Select relevant columns
  select(model_name, auc_val_2020, auc_val_2020_2022) |> 

  # Create a column for AUC names
  mutate(
    auc_name = map(auc_val_2020, \(x) names(x))
  ) |> 

  # Unnest the auc_name column
  unnest() |> 

  # Rename AUC names and set factor levels
  mutate(
    auc_name = factor(
      auc_name,
      levels = c('SARS_CoV2', 'non_SARS_CoV2_ARI', 'Healthy_Controls', 'micro', 'macro'),
      labels = c('SARS-CoV-2+ vs Rest', 'NCARI vs Rest', 'HC vs Rest', 'Micro-averaged', 'Macro-averaged')
    )
  ) |> 

  # Pivot the data to long format
  pivot_longer(cols = c('auc_val_2020', 'auc_val_2020_2022'), names_to = 'id', values_to = 'auc_val') |> 

  # Rename id column
  mutate(
    id = case_when(
      id == 'auc_val_2020' ~ 'SARS-CoV-2+, Kazakhstan, 2020  \nNCARI, Kazakhstan  \nAC',
      id == 'auc_val_2020_2022' ~ 'SARS-CoV-2+, Kazakhstan, 2020 & 2022  \nNCARI, Kazakhstan  \nAC'
    )
  ) |> 

  # Pivot the data to wide format
  pivot_wider(names_from = auc_name, values_from = auc_val, values_fn = list) |> 

  # Unnest the data
  unnest() |> 

  # Summarize the data by model_name and id
  tbl_strata(
    strata = model_name, 
    ~ .x |> 
      tbl_summary(
        by = id,
        type = everything() ~ "continuous",
        statistic = all_continuous() ~ "{median}  \n[{p25}; {p75}]",
        missing = "no",
        include = where(is.numeric)
      ) |> 
      modify_header(
        label = '**Classification metric**',
        all_stat_cols() ~ '**{level}**,  \nN = {n}'
      ),
    .combine_with = "tbl_merge"
  ) |> 

  # Convert to gt object
  as_gt() |> 

  # Apply styling to the table
  gt::tab_style(
    style = gt::cell_text(weight = "bold", align = 'center'),
    locations = gt::cells_row_groups(groups = everything())
  ) |> 

  # Format table as Markdown
  gt::fmt_markdown(columns = everything()) |> 

  # Save the table as an HTML file
  gt::gtsave('FIG1/tab3_nat_kz_kz_auc_3l_merge.html')
}

# Plot for AUC comparison in 2020
ROC_plot_2020_2analysis <- AUC_ROC_2 |>
  # Extract AUC names
  mutate(auc_name = map(auc_val_2020, \(x) names(x))) |>
  # Filter models of interest
  filter(model_name %in% c('DT', 'SVM-R')) |>
  # select representative models
  slice(c(3,7)) |>
  # Select relevant columns
  select(roc_plot_data_2020, model_name) |>
  unnest() |>
  # Filter data for 'SARS_CoV2' group
  filter(Group == 'SARS_CoV2') |>
  # Modify model names with AUC values
  mutate(model_name = paste0(model_name, ' [AUC = ', round(AUC, 3), '] ')) |>
  ggplot2::ggplot(aes(x = 1 - Specificity, y = Sensitivity)) +
  geom_path(
    aes(color = model_name),
    size = 1.5,
    alpha = 0.5
  ) +
  geom_segment(
    aes(x = 0, y = 0, xend = 1, yend = 1),
    colour = 'grey',
    linetype = 'dotdash'
  ) +
  labs(
    title = "ML model ROC curve",
    x = expression(italic("1 - Specificity")),
    y = expression(italic('Sensitivity'))
  ) +
  theme(
    plot.background = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line = element_blank(),
    strip.background = element_rect(color = "white", fill = "white"),
    plot.title = element_blank(),
    legend.position = c(1, 0),
    legend.justification = c(1, 0),
    axis.title = element_text(size = 19),
    axis.text = element_text(size = 15),
    axis.ticks = element_blank(),
    legend.spacing = unit(0, "pt"),
    legend.text = element_text(size = 16),
    legend.title = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.background = element_blank(),
    legend.key = element_rect(fill = NA),
    legend.key.width = unit(2, 'cm'),
    legend.key.height = unit(1.5, 'cm')
  ) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 8))) +
  scale_color_manual(values = c('#b80000', '#020292'))

# Save the 2020 plot as SVG and PNG
ggsave(ROC_plot_2020_2analysis, file = 'FIG1/ROC_plot_2020_2analysis.svg', width = 10, height = 10)
ggsave(ROC_plot_2020_2analysis, file = 'FIG1/ROC_plot_2020_2analysis.png', width = 10, height = 10, bg = 'white')

# Plot for AUC comparison in 2020 + 2022
ROC_plot_2020_2022_2analysis <- AUC_ROC_2 |>
  # Extract AUC names
  mutate(auc_name = map(auc_val_2020_2022, \(x) names(x))) |>
  # Filter models of interest
  filter(model_name %in% c('DT', 'SVM-R')) |>
  # select representative models
  slice(c(1,8)) |> #select(auc_val_2020_2022, auc_name, model_name) |> unnest() |> filter(auc_name == 'micro')# representative based on median micro
  # Select relevant columns
  select(roc_plot_data_2020_2022, model_name) |>
  unnest() |>
  # Filter data for 'SARS_CoV2' group
  filter(Group == 'SARS_CoV2') |>
  # Modify model names with AUC values
  mutate(model_name = paste0(model_name, ' [AUC = ', round(AUC, 3), '] ')) |>
  ggplot(aes(x = 1 - Specificity, y = Sensitivity)) +
  geom_path(
    aes(color = model_name),
    size = 1.5,
    alpha = 0.5
  ) +
  geom_segment(
    aes(x = 0, y = 0, xend = 1, yend = 1),
    colour = 'grey',
    linetype = 'dotdash'
  ) +
  labs(
    title = "ML model ROC curve",
    x = expression(italic("1 - Specificity")),
    y = expression(italic('Sensitivity'))
  ) +
  theme(
    plot.background = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line = element_blank(),
    strip.background = element_rect(color = "white", fill = "white"),
    plot.title = element_blank(),
    legend.position = c(1, 0),
    legend.justification = c(1, 0),
    axis.title = element_text(size = 19),
    axis.text = element_text(size = 15),
    axis.ticks = element_blank(),
    legend.spacing = unit(0, "pt"),
    legend.text = element_text(size = 16),
    legend.title = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.background = element_blank(),
    legend.key = element_rect(fill = NA),
    legend.key.width = unit(2, 'cm'),
    legend.key.height = unit(1.5, 'cm')
  ) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 8))) +
  scale_color_manual(values = c('#b80000', '#020292'))

# Save the 2020 + 2022 plot as SVG and PNG
ggsave(ROC_plot_2020_2022_2analysis, file = 'FIG1/ROC_plot_2020_2022_2analysis.svg', width = 10, height = 10)
ggsave(ROC_plot_2020_2022_2analysis, file = 'FIG1/ROC_plot_2020_2022_2analysis.png', width = 10, height = 10, bg = 'white')
}