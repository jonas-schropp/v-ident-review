#' Figure 1: Modality over Time
#'
#' @param general_path string(1) path to the coded general-study RDS file.
#' @param signal_path string(1) path to the coded signal-acquisition RDS file.
#' @param output_path string(1) path where the TIFF figure should be saved.
#' @param width,height numeric(1) figure dimensions in inches.
#' @param dpi numeric(1) figure resolution in dots per inch.
#'
#' @returns an arranged plot containing the sensors used per year and study and
#' per experiment. The plot is also saved to `output_path` as a TIFF file.
#'
plot_modality_over_time <- function(
    general_path = here::here("data", "general.rds"),
    signal_path = here::here("data", "signal.rds"),
    output_path = here::here("results", "figures", "modality_over_time.tiff"),
    width = 7,
    height = 4,
    dpi = 300
  ) {

  library(dplyr)
  library(ggplot2)
  library(tidyr)

  general <- readr::read_rds(general_path)
  signal <- readr::read_rds(signal_path)

  pdat <- left_join(
    general,
    signal, by = c("id", "experience")
  ) %>%
    mutate(
      modality = coalesce(as.character(.data$modality), ""),
      ECG = coalesce(as.logical(.data$ECG), stringr::str_detect(.data$modality, "ECG"), FALSE),
      PPG = coalesce(as.logical(.data$PPG), stringr::str_detect(.data$modality, "PPG"), FALSE),
      microphone = coalesce(as.logical(.data$microphone), stringr::str_detect(.data$modality, "microphone|voice"), FALSE),
      EDA = coalesce(as.logical(.data$EDA), stringr::str_detect(.data$modality, "EDA"), FALSE),
      GSR = coalesce(as.logical(.data$GSR), stringr::str_detect(.data$modality, "GSR"), FALSE),
      VHF = coalesce(as.logical(.data$VHF), stringr::str_detect(.data$modality, "VHF"), FALSE),
      SCG = coalesce(as.logical(.data$SCG), stringr::str_detect(.data$modality, "SCG"), FALSE),
      `Other (EDA, GSR, VHF, SCG, microphone)` = EDA | GSR | VHF | SCG | microphone
    )

  modality_levels <- c("ECG", "PPG", "Other (EDA, GSR, VHF, SCG, microphone)")

  p1 <- pdat %>%
    group_by(year) %>%
    summarize(
      ECG = sum(ECG, na.rm = TRUE),
      PPG = sum(PPG, na.rm = TRUE),
      `Other (EDA, GSR, VHF, SCG, microphone)` = sum(
        `Other (EDA, GSR, VHF, SCG, microphone)`, na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = all_of(modality_levels)
    ) %>%
    mutate(
      name = factor(name, levels = modality_levels)
    ) %>%
    ggplot(
      aes(x = year, y = value, color = name, group = name)
    ) +
    geom_line(linewidth = 1.2, alpha = 0.6) +
    geom_point(size = 2) +
    scale_x_continuous(
      limits = c(2015, 2025),
      breaks = seq(2015, 2025, 2)
    ) +
    scale_y_continuous(
      limits = c(0, 300),
      breaks = c(0, 100, 200, 300)
    ) +
    ylab("Number of Experiments") +
    xlab("Year of Publication") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.background = element_rect(fill = "white", color = NA),
      legend.box.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )

  p2 <- pdat %>%
    group_by(id, year) %>%
    summarize(
      ECG = sum(ECG, na.rm = TRUE) > 0,
      PPG = sum(PPG, na.rm = TRUE) > 0,
      `Other (EDA, GSR, VHF, SCG, microphone)` = sum(
        `Other (EDA, GSR, VHF, SCG, microphone)`, na.rm = TRUE
      ) > 0,
      .groups = "drop"
    ) %>%
    group_by(year) %>%
    summarize(
      ECG = sum(ECG, na.rm = TRUE),
      PPG = sum(PPG, na.rm = TRUE),
      `Other (EDA, GSR, VHF, SCG, microphone)` = sum(
        `Other (EDA, GSR, VHF, SCG, microphone)`, na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = all_of(modality_levels)
    ) %>%
    mutate(
      name = factor(name, levels = modality_levels)
    ) %>%
    ggplot(
      aes(x = year, y = value, color = name, group = name)
    ) +
    geom_line(linewidth = 1.2, alpha = 0.6) +
    geom_point(size = 2) +
    scale_x_continuous(
      limits = c(2015, 2025),
      breaks = seq(2015, 2025, 2)
    ) +
    ylab("Number of Papers") +
    xlab("Year of Publication") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.background = element_rect(fill = "white", color = NA),
      legend.box.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )

  plot <- ggpubr::ggarrange(
    p1, p2, common.legend = TRUE
  )

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(
    filename = output_path,
    plot = plot,
    device = "tiff",
    width = width,
    height = height,
    units = "in",
    dpi = dpi,
    compression = "lzw",
    bg = "white"
  )

  plot
}
