#' Ridgeline for Algorithm (factor-coded)
plot_algo <- function(midat, fit = fit_algo2) {
  library(ggplot2)
  library(ggridges)
  library(dplyr)
  library(tidyr)
  library(scales)
  
  # Use the factor levels present in your data (order preserved)
  levs <- levels(midat[[1]]$algo)
  
  # New-study predictions for each algorithm level
  nd <- data.frame(
    algo    = factor(levs, levels = levs),
    multimodal = c(0, 1),
    logn = log(1),
    activity = 'one condition',
    duration = '1 day',
    se_logit = 0
  )
  
  eta <- posterior_linpred(fit, newdata = nd, re_formula = NA)
  p   <- plogis(eta)
  p   <- as.data.frame(p)
  names(p) <- levs
  
  p_long <- pivot_longer(p, cols = everything(),
                         names_to = "algo", values_to = "prob") %>%
    transmute(algo = factor(algo, levels = levs), prob = prob)
  
  # Summaries for intervals/medians
  summ <- p_long %>%
    group_by(algo) %>%
    summarise(med = median(prob),
              lo  = quantile(prob, 0.025),
              hi  = quantile(prob, 0.975),
              .groups = "drop")
  
  ggplot(p_long, aes(x = prob, y = algo, fill = algo)) +
    geom_density_ridges(scale = 2.2, alpha = 0.7, color = "white", linewidth = 0.2) +
    geom_segment(data = summ,
                 aes(x = lo, xend = hi, y = algo, yend = algo),
                 inherit.aes = FALSE, color = "black", linewidth = 0.6) +
    geom_point(data = summ,
               aes(x = med, y = algo),
               inherit.aes = FALSE, color = "black", size = 1.6) +
    scale_x_continuous(
      labels = percent_format(accuracy = 1), limits = c(0, 1),
      breaks = seq(0,1,by=0.2)
      ) +
    guides(fill = "none") +
    labs(
      title = "Posterior predicted accuracy by algorithm",
      subtitle = "Population-level predictions",
      x = "Predicted accuracy"
    ) +
    theme_ridges(font_size = 12, grid = TRUE)
}


#' Ridgeline for Multimodal (within/between-coded)
plot_multimodal <- function(midat, fit) {
  library(ggplot2)
  library(ggridges)
  library(dplyr)
  library(tidyr)
  library(scales)
  
  # Two regimes: Unimodal (0) vs Multimodal (1)
  levs <- c("Unimodal", "Multimodal")
  
  # Study-average predictions for a new study:
  # within term = 0 ; between term toggles 0/1
  nd <- data.frame(
    label = factor(levs, levels = levs),
    multimodal_within  = 0,
    multimodal_between = c(0, 1),
    multimodal = c(0, 1),
    logn = log(1),
    activity = 'one condition',
    duration = '1 day',
    se_logit = 0
  )
  
  # Population-level predictions (new-study target)
  eta <- posterior_linpred(fit, newdata = nd, re_formula = NA)
  p   <- plogis(eta)
  p   <- as.data.frame(p)
  names(p) <- levs
  
  p_long <- pivot_longer(p, cols = everything(), names_to = "multimodal", values_to = "prob") %>%
    transmute(multimodal = factor(multimodal, levels = levs),
              prob = prob)
  
  # Summaries for intervals/medians
  summ <- p_long %>%
    group_by(multimodal) %>%
    summarise(med = median(prob),
              lo  = quantile(prob, 0.025),
              hi  = quantile(prob, 0.975),
              .groups = "drop")
  
  ggplot(p_long, aes(x = prob, y = multimodal, fill = multimodal)) +
    geom_density_ridges(scale = 2.2, alpha = 0.7, color = "white", linewidth = 0.2) +
    geom_segment(data = summ,
                 aes(x = lo, xend = hi, y = multimodal, yend = multimodal),
                 inherit.aes = FALSE, color = "black", linewidth = 0.6) +
    geom_point(data = summ,
               aes(x = med, y = multimodal),
               inherit.aes = FALSE, color = "black", size = 1.6) +
    scale_x_continuous(
      labels = percent_format(accuracy = 1), 
      limits = c(0, 1),
      breaks = seq(0,1,0.2)
      ) +
    guides(fill = "none") +
    labs(
      title = "Posterior predicted accuracy: Unimodal vs Multimodal",
      subtitle = "Population-level predictions",
      x = "Predicted accuracy"
    ) +
    theme_ridges(font_size = 12, grid = TRUE)
}

#' Ridgeline for ECG / PPG / Other
plot_modalities <- function(midat, fit = fit_mod) {
  library(ggplot2)
  library(ggridges)
  library(dplyr)
  library(tidyr)
  library(scales)
  
  # Levels (baseline "Other" = neither ECG nor PPG)
  levs <- c("Other", "ECG", "PPG")
  
  nd <- data.frame(
    modality = factor(levs, levels = levs),
    ECG = c(0, 1, 0),
    PPG = c(0, 0, 1),
    logn = log(1),
    multimodal = 0,
    activity = 'one condition',
    duration = '1 day',
    se_logit = 0    
  )
  
  # Population-level predictions (new-study target)
  eta <- posterior_linpred(fit, newdata = nd, re_formula = NA)
  p   <- plogis(eta)
  p   <- as.data.frame(p)
  names(p) <- levs
  
  p_long <- pivot_longer(p, cols = names(p)) %>%
    transmute(modality = factor(name, levels = levs),
              prob = value)
  
  # Summaries for intervals/medians
  summ <- p_long %>%
    group_by(modality) %>%
    summarise(med = median(prob),
              lo  = quantile(prob, 0.025),
              hi  = quantile(prob, 0.975),
              .groups = "drop")
  
  ggplot(p_long, aes(x = prob, y = modality, fill = modality)) +
    geom_density_ridges(scale = 2.2, alpha = 0.7, color = "white", linewidth = 0.2) +
    geom_segment(data = summ,
                 aes(x = lo, xend = hi, y = modality, yend = modality),
                 inherit.aes = FALSE, color = "black", linewidth = 0.6) +
    geom_point(data = summ,
               aes(x = med, y = modality),
               inherit.aes = FALSE, color = "black", size = 1.6) +
    scale_x_continuous(
      labels = percent_format(accuracy = 1), 
      limits = c(0, 1),
      breaks = seq(0,1,0.2)
    ) +
    guides(fill = "none") +
    labs(
      title = "Posterior predicted accuracy by modality",
      subtitle = "Population-level predictions",
      x = "Predicted accuracy"
    ) +
    theme_ridges(font_size = 12, grid = TRUE)
}


#' Ridgeline for Activity (within/between-coded)
plot_activity <- function(midat, fit = fit_activity) {
  library(ggplot2)
  library(ggridges)
  library(dplyr)
  library(tidyr)
  library(scales)
  
  # Levels in desired order
  levs <- levels(midat[[1]]$activity)  # c("one condition","multiple conditions","everyday conditions")
  
  # Newdata rows for each level:
  # - within terms set to 0 (study-average prediction)
  # - between terms: 1 for the target level, 0 otherwise
  nd <- data.frame(
    activity = factor(levs, levels = levs),
    duration = "1 day",
    se_logit = 0,
    logn = log(1),
    multimodal = 0,
    multiC_within  = 0,
    everyC_within  = 0,
    multiC_between = as.integer(levs == "multiple conditions"),
    everyC_between = as.integer(levs == "everyday conditions"),
    multiC = levs == "multiple conditions",
    everyC = levs == "everyday conditions"
  )
  
  # Population-level predictions (no study REs)
  eta <- posterior_linpred(fit, newdata = nd, re_formula = NA)
  p   <- plogis(eta)
  p   <- as.data.frame(p)
  names(p) <- levs
  
  p_long <- pivot_longer(p, cols = names(p)) %>%
    transmute(activity = factor(name, levels = levs),
              prob = value)
  
  # Summaries for intervals/medians
  summ <- p_long %>%
    group_by(activity) %>%
    summarise(med = median(prob),
              lo  = quantile(prob, 0.025),
              hi  = quantile(prob, 0.975),
              .groups = "drop")
  
  ggplot(p_long, aes(x = prob, y = activity, fill = activity)) +
    geom_density_ridges(scale = 2.2, alpha = 0.7, color = "white", linewidth = 0.2) +
    geom_segment(data = summ,
                 aes(x = lo, xend = hi, y = activity, yend = activity),
                 inherit.aes = FALSE, color = "black", linewidth = 0.6) +
    geom_point(data = summ,
               aes(x = med, y = activity),
               inherit.aes = FALSE, color = "black", size = 1.6) +
    scale_x_continuous(
      labels = percent_format(accuracy = 1), 
      limits = c(0, 1),
      breaks = seq(0,1,0.2)
    ) +
    guides(fill = "none") +
    labs(
      title = "Posterior predicted accuracy by activity",
      subtitle = "Population-level predictions",
      x = "Predicted accuracy"
    ) +
    theme_ridges(font_size = 12, grid = TRUE)
}

#' Ridgeline for Duration
plot_duration <- function(midat) {
  
  library(ggplot2)
  library(ggridges)
  library(dplyr)
  library(tidyr)
  library(scales)
  
  levs <- levels(midat[[1]]$duration)  
  nd <- data.frame(
    duration = factor(levs, levels = levs, ordered = TRUE),
    se_logit = 0, 
    logn = log(1), 
    activity = 'one condition', 
    multimodal = 0
  )
  eta <- posterior_linpred(fit_duration, newdata = nd, re_formula = NA)  
  p   <- plogis(eta)
  p <- as.data.frame(p)
  names(p) <- levs
  p_long <- pivot_longer(p, cols = names(p)) %>%
    transmute(duration = factor(name, levels = levs, ordered = TRUE),
              prob = value)
  
  # Summaries for intervals/medians
  summ <- p_long %>%
    group_by(duration) %>%
    summarise(med = median(prob),
              lo  = quantile(prob, 0.025),
              hi  = quantile(prob, 0.975),
              .groups = "drop")
  
  ggplot(p_long, aes(x = prob, y = duration, fill = duration)) +
    geom_density_ridges(scale = 2.2, alpha = 0.7, color = "white", linewidth = 0.2) +
    geom_segment(data = summ,
                 aes(x = lo, xend = hi, y = duration, yend = duration),
                 inherit.aes = FALSE, color = "black", size = 0.6) +
    geom_point(data = summ,
               aes(x = med, y = duration),
               inherit.aes = FALSE, color = "black", size = 1.6) +
    scale_x_continuous(
      labels = percent_format(accuracy = 1), 
      limits = c(0, 1),
      breaks = seq(0,1,0.2)
    ) +
    guides(fill = "none") +
    labs(
      title = "Posterior predicted accuracy by duration",
      subtitle = "Population-level predictions",
      x = "Predicted accuracy"
    ) +
    theme_ridges(font_size = 12, grid = TRUE)
  
}