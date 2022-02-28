#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
library(dplyr)
library(ggplot2)
library(purrr)
library(forcats)
library(stringr)
library(tibble)
library(scales)
library(ggdist)
library(ggtext)
library(ragg)
library(showtext)
library(thematic)
library(bslib)
library(Cairo)
library(viridis)
library(patchwork)
library(mvtnorm)
library(condMVNorm)

# Options
options(shiny.usecairo = T, # use Cairo device for better antialiasing
        scipen = 999 # Do not use scientific notation
        )

# Helper functions ----

# Remove leading zero for display of proportions
remove_leading_zero <- function(x, digits = 2) {
    x[x < 0.0000001] <- 0.0000001
    dig <- digits - ceiling(log10(abs(x)))
    x1 <- map2(x, dig, formatC, format = "f")

    x1 <- sub("^-0", "-", sub("^0", "", x1))
    x1[digits - ceiling(log10(abs(x))) > 6] <- ".000000"
    x1
}

# Round to significant digits for near 0 and near 1
proportion_round <- function(p, digits = 2) {
    p1 <- round(p, digits)
    lower_limit <- 0.95 * 10 ^ (-1 * digits)
    upper_limit <- 1 - lower_limit
    p1[p > upper_limit &
           p <= 1] <- 1 - signif(1 - p[p > upper_limit & p <= 1],
                                 digits - 1)
    p1[p < lower_limit &
           p >= 0] <- signif(p[p < lower_limit & p >= 0],
                             digits - 1)
    p1
}

prob_label <- function(p,
                       accuracy = 0.01,
                       digits = NULL,
                       max_digits = NULL,
                       remove_leading_zero = TRUE,
                       round_zero_one = TRUE) {
  if (is.null(digits)) {
    l <- scales::number(p, accuracy = accuracy)
  } else {
    sig_digits <- abs(ceiling(log10(p + p / 1000000000)) - digits)
    sig_digits[p > 0.99] <- abs(ceiling(log10(1 - p[p > 0.99])) - digits + 1)
    sig_digits[ceiling(log10(p)) == log10(p)] <- sig_digits[ceiling(log10(p)) == log10(p)] - 1
    sig_digits[is.infinite(sig_digits)] <- 0
    l <- purrr::map2_chr(p,
                         sig_digits,
                         formatC,
                         format = "f",
                         flag = "#")

  }
  if (remove_leading_zero) l <- sub("^-0","-", sub("^0","", l))

  if (round_zero_one) {
    l[p == 0] <- "0"
    l[p == 1] <- "1"
    l[p == -1] <- "-1"
  }

  if (!is.null(max_digits)) {
    if (round_zero_one) {
      l[round(p, digits = max_digits) == 0] <- "0"
      l[round(p, digits = max_digits) == 1] <- "1"
      l[round(p, digits = max_digits) == -1] <- "-1"
    } else {
      l[round(p, digits = max_digits) == 0] <- paste0(".", paste0(rep("0", max_digits), collapse = ""))
      l[round(p, digits = max_digits) == 1] <- paste0("1.", paste0(rep("0", max_digits), collapse = ""))
      l[round(p, digits = max_digits) == -1] <- paste0("-1.", paste0(rep("0", max_digits), collapse = ""))
    }
  }

  dim(l) <- dim(p)
  l
}

# Makes geom_text size the same as the base size
# (scaled to the specified ratio)
ggtext_size <- function(base_size, ratio = 0.8) {
    ratio * base_size / 2.845276
}

# Constants ----

# SLD colors
mycolors <- c(SLD = "#482576",
              Buffer = "#35608D",
              NotSLD = "#43BF71")

# Not sure why this is needed, but it is.
mycolours <- mycolors

lightenedcolors <- tinter::lighten(mycolors, .15)

viridis_start = .20
viridis_end = .75
viridis_alpha = .40

# layout widths
split_width <- c("50%", "50%")


# Main plot ----
make_conditional_ppv_plot <- function(
  General = 100,
  Specific = 85,
  Academic = 70,
  # Reliability
  r_gg = 0.97,
  r_ss = 0.92,
  r_aa = 0.96,
  # Thresholds
  threshold_s = 85,
  threshold_a = 85,
  threshold_g = 90,
  buffer = 5,
  meaningful_difference = 10,
  gscor = .6, # Corelation of G and S
  gacor = .6, # Corelation of G and A
  sacor = .6, # Corelation of S and A
  myfont = "Roboto Condensed",
  sigma = 15,
  mu = 100) {

  # Base font size
  b_size <- 11

  mycols <- viridis::viridis(3, begin = viridis_start, end = viridis_end, direction = -1) %>%
    `names<-`(c("SLD-Likely", "Buffer", "SLD-Unlikely"))

  # Observed Scores
  x_SS <- c(G = General, S = Specific, A = Academic)

  # Variable Names
  v_true <- c("g", "s", "a")
  v_observed <- c("G", "S", "A")
  v_names <- c(v_true, v_observed)
  decision_labels <- c("SLD-Unlikely",
                       "SLD-Possible",
                       "SLD-Likely")

  # Reliability
  r_xx <- c(r_gg, r_ss, r_aa)


  m_observed_cov <- matrix(c(1, gscor, gacor,
                             gscor, 1, sacor,
                             gacor, sacor, 1), nrow = 3)

  m_true_cov <- m_observed_cov

  diag(m_true_cov) <- r_xx
  m_cov <- rbind(cbind(m_true_cov, m_true_cov),
                 cbind(m_true_cov, m_observed_cov))

    colnames(m_cov) <- v_names
  rownames(m_cov) <- v_names


  if (det(m_cov) <= 0) stop(
    "This combination of reliability and correlation coefficents is mathematically impossible. Try altering one or more coefficient."
    )

  cov_all <- m_cov * sigma ^ 2
  # True score covariance
  cov_true <- cov_all[v_true, v_true]
  # Observed score covariance
  cov_observed <- cov_all[v_observed, v_observed]
  # Cross covariances
  cov_true_observed <-  cov_all[v_true, v_observed]

  b_s.g <- (solve(m_true_cov[1,1,drop = F]) %*% m_true_cov[1,2,drop = F])[1,1]


  plot_warnings <- ""

  b_s.g_neg <- b_s.g < 0

  if (b_s.g_neg) plot_warnings <- paste0(plot_warnings,
      "<li>The direct path between the general ability true score and the specific  ability true score is negative: ",
      round(b_s.g, 4),
      "<br> This is an unlikely value for ability data.</li>")


  b_a <- (solve(m_true_cov[c(1,2),c(1,2),drop = F]) %*% m_true_cov[c(1,2),3,drop = F])[,1]
  b_a.g <- b_a[1]
  b_a.s <- b_a[2]

  b_a.g_neg <- b_a.g < 0

  if (b_a.g_neg) plot_warnings <- paste(plot_warnings,
      "<li>The direct path between the general ability true score and the academic ability true score is negative: ",
      round(b_a.g, 4),
      "<br> This is an unlikely value for academic ability data.</li>")

  b_a.s_neg <- b_a.s < 0

  if (b_a.s_neg) plot_warnings <- paste0(plot_warnings,
   paste0("<li>The direct path between the specific ability true score and the academic ability true score is negative: ",
    round(b_a.s, 4),
    "<br> This is an unlikely value for academic ability data.</li>"))

  if (plot_warnings != "") {
    plot_warnings <- paste('<div class = "alert alert-danger"><h4>Warning:</h4><p>As seen in the <strong>Model Calculations</strong> tab, the specified reliability and correlation coefficients result in an unlikely model:</p><ul>', plot_warnings , "</ul></div>")
  }

  # Explained variance
  s_by_g <- r_gg * (b_s.g ^ 2)
  a_by_g <- r_gg * (b_a.g + b_s.g * b_a.s) ^ 2
  a_by_s <- (r_ss - s_by_g) * (b_a.s ^ 2)

  # Residual variances
  v_s <- r_ss - s_by_g
  v_a <- r_aa - a_by_g - a_by_s

  # Conditional means
  mu_conditional <-
    mu +
    cov_true_observed %*%
    solve(cov_observed) %*%
    (x_SS - mu) %>%
    as.numeric()

  # Conditional covariance
  cov_conditional <-
    cov_true -
    cov_true_observed %*%
    solve(cov_observed) %*%
    t(cov_true_observed)

  # Difference weights
  w_difference <- matrix(
    c(1, -1, rep(0,4),
      1, 0, -1, rep(0,3)),
    ncol = 2
  ) %>%
    `colnames<-`(c("gs", "ga")) %>%
    `rownames<-`(v_names)

  # Full weight matrix
  w <- cbind(diag(6), w_difference) %>%
    `colnames<-`(c(rownames(w_difference),
                   colnames(w_difference)))

  # Covariance matrix of all scores and true difference scores
  big_sigma <-  t(w) %*% cov_all %*% w

  # Conditional covariance of all true scores and difference scores
  cond_mu_sigma <-
    condMVNorm::condMVN(
      mean = c(rep(100, 6), rep(0, 2)) %>% `names<-`(rownames(big_sigma)),
      sigma = big_sigma,
      dependent.ind = c("g", "s", "a", "gs", "ga"),
      given.ind = toupper(c("g", "s", "a")),
      X.given = x_SS,
      check.sigma = F
    )

  # Multivariate conditional means and standard errors
  multivariate_mu <- cond_mu_sigma$condMean[1:3]
  multivariate_see <- sqrt(diag(cond_mu_sigma$condVar)[1:3])

  # Percent meeting strict criteria
  p_strict <- mvtnorm::pmvnorm(
    lower = c(
      threshold_g,-Inf,-Inf,
      meaningful_difference,
      meaningful_difference
    ),
    upper = c(Inf,
              threshold_s,
              threshold_a,
              Inf,
              Inf),
    mean = cond_mu_sigma$condMean,
    sigma = cond_mu_sigma$condVar,
    keepAttr = F
  )

  # Percent meeting relaxed criteria
  p_relaxed <- mvtnorm::pmvnorm(
    lower = c(
      threshold_g - buffer,-Inf,-Inf,
      meaningful_difference - buffer,
      meaningful_difference - buffer
    ),
    upper = c(Inf,
              threshold_s + buffer,
              threshold_a + buffer,
              Inf,
              Inf),
    mean = cond_mu_sigma$condMean,
    sigma = cond_mu_sigma$condVar,
    keepAttr = F
  )

  p_ga_strict <- mvtnorm::pmvnorm(
    lower = c(
      -Inf,
      -Inf,
      -Inf,
      -Inf,
      meaningful_difference
    ),
    upper = c(Inf,
              Inf,
              Inf,
              Inf,
              Inf),
    mean = cond_mu_sigma$condMean,
    sigma = cond_mu_sigma$condVar,
    keepAttr = F
  )


  threshold = threshold_a


  # Shaded regions
  d_rect <- tibble(
    x = c(
      40,
      40,
      threshold + buffer,
      threshold + buffer,
      threshold,
      40,
      threshold + buffer
    ),
    y = c(1, 3, 3, 1, 1, 4, 4) - 0.2,
    xmax = c(
      threshold,
      threshold,
      160,
      160,
      threshold + buffer,
      threshold,
      160
    ),
    ymax = c(3, 4, 4, 3, 5, 5, 5) - 0.2,
    fill = scales::alpha(c(mycols[c("SLD-Likely",
                                    "SLD-Unlikely",
                                    "SLD-Likely",
                                    "SLD-Unlikely",
                                    "Buffer")],
                           "gray30",
                           "gray70"),
                         0.15)
  )



  # distributions and probabilties
  d <- tibble(
    dist = rep("norm", 4),
    args = list(list(100, 15),
                list(multivariate_mu[1], multivariate_see[1]),
                list(multivariate_mu[2], multivariate_see[2]),
                list(multivariate_mu[3], multivariate_see[3])),
    Ability = c(
      "Population",
      "General\nAbility",
      "Specific\nAbility",
      "Academic\nAbility"
    ),
    symbol = c("X", "g", "s", "a")
    # p_x_strict = c(p_g_strict, p_s_strict, p_a_strict),
    # p_x_relaxed = c(p_g_relaxed, p_s_relaxed, p_a_relaxed)
  ) %>%
    mutate(rxx_display = if_else(
      Ability == "Population",
      "",
      paste0("italic(r[xx]) == '",
             c("", remove_leading_zero(r_xx)),
             "'"))) %>%
    mutate(
      Score = c(100, General, Specific, Academic),
      mu = c(100, multivariate_mu),
      sd = c(15, multivariate_see),
      p = paste0(
        "P(*",symbol,"* < ",
        threshold,
        ") = ",
        prob_label(pnorm(threshold, mu, sd),
                            digits = 2,
                            max_digits = 4,
                            round_zero_one = F)
      ),
      ScoreDisplay = ifelse(
        Ability == "Population",
        0,
        Score),
      p_more = paste0(
        "P(*",symbol,"* > ",
        threshold + buffer,
        ") = ",
        prob_label(1 - pnorm(threshold + buffer, mu, sd),
                            digits = 2,
                            max_digits = 4,
                            round_zero_one = F)
      ),
      Ability = factor(Ability) %>%
        fct_inorder() %>%
        fct_rev(),
      p_less_color = case_when(
        Ability == "Population" ~ "gray30",
        Ability == "General\nAbility" ~ mycols["SLD-Unlikely"],
        TRUE ~ mycols["SLD-Likely"]
      ) %>% tinter::darken(.5),
      p_more_color = case_when(
        Ability == "Population" ~ "gray70",
        Ability == "General\nAbility" ~ mycols["SLD-Likely"],
        TRUE ~ mycols["SLD-Unlikely"]
      ) %>% tinter::darken(.5),
      p_between_color = case_when(
        Ability == "Population" ~ "gray50",
        TRUE ~ mycols["Buffer"])
    )


  # Upper plot ----
  gp1 <- ggplot(d, aes(y = Ability,
                       dist = dist,
                       args = args)) +
    geom_richtext(
      aes(
        label = p,
        x = threshold - 1,
        y = as.numeric(Ability),
        color = p_less_color
      ),
      vjust = 1.3,
      hjust = 1,
      size = ggtext_size(16),
      label.padding = unit(0, "mm"),
      show.legend = F,
      family = myfont,
      label.colour = NA
    ) +
    geom_richtext(
      aes(
        label = p_more,
        x = threshold + buffer + 1,
        y = as.numeric(Ability),
        color = p_more_color,
      ),
      vjust = 1.3,
      hjust = 0,
      size = ggtext_size(16),
      label.padding = unit(0, "mm"),
      show.legend = F,
      label.colour = NA,
      family = myfont
    ) +
    geom_hline(yintercept = 1:4 - 0.2,
               size = 0.2,
               color = "gray30") +
    annotate(
      x = c(rep(42, 3),
            rep(158, 3)),
      y = rep(c(1.7, 2.7, 3.7), 2),
      label = c("SLD-Likely", "SLD-Unlikely")[c(1, 1, 2, 2, 2, 1)],
      geom = "label",
      family = myfont,
      hjust = c(rep(0,3), rep(1,3)),
      color = mycols[c(1, 1, 3, 3, 3, 1)] %>% tinter::darken(.5),
      size = ggtext_size(16),
      label.padding = unit(0, "lines"),
      label.size = 0) +
    geom_rect(
      data = d_rect,
      aes(
        xmin = x,
        ymin = y,
        xmax = xmax,
        ymax = ymax,
        fill = fill
      ),
      inherit.aes = F
    ) +
    stat_dist_halfeye(
      normalize = "groups",
      limits = c(40, 160),
      aes(fill = stat(
        case_when(
          y == 4 & x < threshold ~ "gray30",
          y == 4 &
            x < threshold + buffer ~ mycols["Buffer"],
          y == 4 ~ "gray70",
          (x < threshold & y != 3) ~ mycols["SLD-Likely"],
          ((x >= threshold + buffer)  &
             y == 3) ~ mycols["SLD-Likely"],
          (x > threshold + buffer) |
            (x < threshold) ~ mycols["SLD-Unlikely"],
          TRUE ~ mycols["Buffer"]
        )
      )),
      height = 0.8,
      show_interval = F,
      alpha = 0.55, color = NA
    ) +
    geom_point(aes(x = ScoreDisplay),
               color = "gray20") +
    geom_text(
      aes(label = ScoreDisplay,
          x = ScoreDisplay),
      vjust = -.5,
      size = 5,
      color = "gray20",
      family = myfont
    ) +
    geom_vline(xintercept = threshold + buffer,
               color = "gray30",
               size = 0.25) +
    geom_vline(xintercept = threshold) +
    scale_x_continuous(
      "Standard Scores",
      breaks = seq(40, 160, 10),
      minor_breaks = seq(40, 160, 5),
      expand = expansion(add = 2)
    ) +
    scale_y_discrete(NULL, expand = expansion(mult = .05)) +
    scale_color_identity(NULL) +
    scale_fill_identity(NULL, guide = "none") +
    coord_cartesian(xlim = c(40, 160), clip = "off") +
    theme_minimal(base_size = 18, base_family = myfont, base_line_size = .5) +
    theme(
      plot.title.position = "panel",
      plot.background = element_rect(fill = "white", color = NA),
      plot.title = element_markdown(size = 12,
                                    color = "gray30",
                                    padding = margin(l = 2, unit = "mm")),
      plot.subtitle = element_markdown(size = 12,
                                       color = "gray30",
                                       padding = margin(l = 2, unit = "mm")),
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(vjust = 0,lineheight = 1.2, size = 12,
                                 hjust = 0.5)
    ) +
    annotate(
      x = mean(c(threshold,
                 threshold + buffer)),
      y = 4,
      vjust = 0.5,
      hjust = -0.16,
      label = paste0("Buffer"),
      geom = "text",
      angle = 90,
      size = ggtext_size(16),
      family = myfont
    ) +
    # annotate(
    #   x = mean(c(threshold,
    #              threshold + buffer)),
    #   y = c(1,2,3),
    #   vjust = 0.5,
    #   hjust = -0.06,
    #   label = paste0("SLD-Possible"),
    #   geom = "text",
    #   color = tinter::darken(mycols["Buffer"], amount = .5),
    #   angle = 90,
    #   size = ggtext_size(14),
    #   family = myfont
    # ) +
    annotate(
      x = threshold + buffer + 1,
      y = 4,
      label = "Not Low",
      geom = "text",
      angle = 0,
      hjust = 0,
      vjust = -0.5,
      size = ggtext_size(16),
      family = myfont
    ) +
    annotate(
      x = threshold - 1,
      y = 4,
      label = "Low",
      geom = "text",
      angle = 0,
      hjust = 1,
      vjust = -0.5,
      size = ggtext_size(16),
      family = myfont
    ) +
    # geom_label(
    #   aes(label = rxx_display, x = 27.25, y = as.numeric(Ability) +  -0.1),
    #   hjust = 0.5,
    #   vjust = 0.5,
    #   parse = T,
    #   family = myfont,
    #   label.padding = unit(0, "lines"),
    #   label.size = 0,
    #   color = "gray30"
    # ) +
    ggtitle(
      paste0(
        'Probability all true scores meet **<span style="color:',
        mycols["SLD-Likely"],
        '">SLD-Likely</span>** criteria',
        " = ",
        prob_label(p_strict,
                            digits = 2,
                            max_digits = 4,
                            round_zero_one = F)
      ),
      subtitle = paste0(
        'Probability all true scores meet **<span style="color:',
        mycols["SLD-Likely"],
        '">SLD-Likely</span>** criteria or in **<span style="color:',
        mycols["Buffer"],
        '">Buffer</span>**',
        " = ",
        prob_label(p_relaxed,
                            digits = 2,
                            max_digits = 4,
                            round_zero_one = F)
      ))
  # Lower plot ----
  gp2 <- tibble(
    dist = rep("norm", 2),
    args = list(list(cond_mu_sigma$condMean[4],
                     sqrt(cond_mu_sigma$condVar[4,4])),
                list(cond_mu_sigma$condMean[5],
                     sqrt(cond_mu_sigma$condVar[5,5]))),
    Ability = c(
      "General -\nSpecific\nDifference",
      "General -\nAcademic\nDifference"
    ),
    symbol = c("g - s", "g - a")
  ) %>%
    mutate(
      Score = c(General - Specific, General - Academic),
      mu = cond_mu_sigma$condMean[4:5],
      sd = sqrt(diag(cond_mu_sigma$condVar)[4:5]),
      p = paste0(
        "P(*",symbol,"* < ",
        meaningful_difference - buffer,
        ") = ",
        prob_label(pnorm(meaningful_difference - buffer, mu, sd),
                            digits = 2,
                            max_digits = 4,
                            round_zero_one = F)
      ),
      ScoreDisplay = Score,
      p_more = paste0(
        "P(*",symbol,"* > ",
        meaningful_difference,
        ") = ",
        prob_label(1 - pnorm(meaningful_difference, mu, sd),
                            digits = 2,
                            max_digits = 4,
                            round_zero_one = F)
      ),
      Ability = factor(Ability) %>%
        fct_inorder() %>%
        fct_rev(),
      p_less_color = mycols["SLD-Unlikely"] %>% tinter::darken(.5),
      p_more_color = mycols["SLD-Likely"] %>% tinter::darken(.5),
      p_between_color = mycols["Buffer"]
    ) %>%
    ggplot(aes(y = Ability)) +
    geom_blank(aes(x = 1)) +
    annotate(
      x = c(rep(-58, 2),
            rep(58, 2)),
      y = rep(c(1.7, 2.7), 2),
      label = c("SLD-Likely", "SLD-Unlikely")[c(2, 2, 1, 1)],
      geom = "label",
      family = myfont,
      hjust = c(rep(0,2), rep(1,2)),
      color = mycols[c(3, 3, 1, 1)] %>% tinter::darken(.5),
      size = ggtext_size(16),
      label.padding = unit(0, "lines"),
      label.size = 0) +
    geom_richtext(
      aes(
        label = p,
        x = meaningful_difference - buffer - 1,
        y = as.numeric(Ability),
        color = p_less_color
      ),
      vjust = 1.3,
      hjust = 1,
      size = ggtext_size(16),
      label.padding = unit(0, "mm"),
      show.legend = F,
      family = myfont,
      label.colour = NA
    ) +
    geom_richtext(
      aes(
        label = p_more,
        x = meaningful_difference + 1,
        y = as.numeric(Ability),
        color = p_more_color,
      ),
      vjust = 1.3,
      hjust = 0,
      size = ggtext_size(16),
      label.padding = unit(0, "mm"),
      show.legend = F,
      label.colour = NA,
      family = myfont
    ) +
    geom_rect(
      data = tibble(x = c(-60,
                          meaningful_difference - buffer,
                          meaningful_difference),
                    xmax = c(meaningful_difference - buffer,
                             meaningful_difference,
                             60),
                    ymax = 3 - 0.2,
                    y = 1 - 0.2,
                    fill = rev(mycols)),
      aes(
        xmin = x,
        ymin = y,
        xmax = xmax,
        ymax = ymax,
        fill = fill
      ),
      inherit.aes = F,
      alpha = .2
    ) +
    stat_dist_halfeye(
      normalize = "groups",
      limits = c(-60, 60),
      aes(fill = stat(
        case_when(
          (x >= meaningful_difference) ~ mycols["SLD-Likely"],
          (x >= meaningful_difference - buffer) ~ mycols["Buffer"],
          TRUE ~ mycols["SLD-Unlikely"]
        )
      ),
      dist = dist,
      args = args),
      height = 0.8,
      show_interval = F,
      alpha = 0.55, color = NA
    ) +
    geom_vline(xintercept = meaningful_difference - buffer,
               color = "gray30",
               size = 0.25) +
    geom_hline(yintercept = 1:2 - 0.2,
               size = 0.2,
               color = "gray30") +
    geom_vline(xintercept = meaningful_difference) +
    geom_point(aes(x = Score), color = "gray20") +
    geom_text(
      aes(label = ScoreDisplay,
          x = ScoreDisplay),
      vjust = -.5,
      size = 5,
      color = "gray20",
      family = myfont
    ) +
    scale_x_continuous(
      "Difference Scores",
      breaks = seq(-60, 60, 10),
      minor_breaks = seq(-60, 60, 5),
      expand = expansion(add = 2)
    ) +
    scale_y_discrete(NULL, expand = expansion(mult = .05)) +
    scale_color_identity(NULL) +
    scale_fill_identity(NULL, guide = "none") +
    coord_cartesian(xlim = c(-60, 60), clip = "off") +
    theme_minimal(base_size = 18, base_family = myfont, base_line_size = .5) +
    theme(
      plot.title.position = "panel",
      plot.background = element_rect(fill = "white", color = NA),
      plot.title = element_markdown(size = 15,
                                    color = "gray30",
                                    padding = margin(l = 2, unit = "mm")),
      plot.subtitle = element_markdown(size = 15,
                                       color = "gray30",
                                       padding = margin(l = 2, unit = "mm")),
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(vjust = 0,lineheight = 1.2, size = 12,
                                 hjust = 0.5)
    )



  # Join plots
  gp <- (gp1 / gp2 + patchwork::plot_layout(heights = c(2.1, 1)))

  # Return list ----
  list(
    plot = gp,
    plot_warnings = plot_warnings,
    model = paste0('
<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" width="100%" height="100%" viewBox="0 0 376.93 237" version="1.1">
	<style>
		.latent {fill:white; font-size:20pt; font-style:italic; }
		.observed {fill:white; font-size:20pt; font-style:normal; }
		.blabel {fill:rgb(40%,40%,40%); font-size:8pt; color: rgb(40%,40%,40%)}
	</style>
	<g id="surface1">
		<g id="latent">
			<path style=" stroke:none;fill-rule:nonzero;fill:rgb(34.707642%,51.998901%,69.291687%);fill-opacity:1;" d="M 163.664063 56.367188 C 163.664063 40.710938 150.972656 28.019531 135.316406 28.019531 C 119.660156 28.019531 106.96875 40.710938 106.96875 56.367188 C 106.96875 72.023438 119.660156 84.714844 135.316406 84.714844 C 150.972656 84.714844 163.664063 72.023438 163.664063 56.367188 Z M 163.664063 56.367188 "/>
			<path style=" stroke:none;fill-rule:nonzero;fill:rgb(36.999512%,55.332947%,37.333679%);fill-opacity:1;" d="M 163.664063 179.113281 C 163.664063 163.457031 150.972656 150.765625 135.316406 150.765625 C 119.660156 150.765625 106.96875 163.457031 106.96875 179.113281 C 106.96875 194.769531 119.660156 207.460938 135.316406 207.460938 C 150.972656 207.460938 163.664063 194.769531 163.664063 179.113281 Z M 163.664063 179.113281 "/>
			<path style=" stroke:none;fill-rule:nonzero;fill:rgb(47.3526%,37.353516%,63.233948%);fill-opacity:1;" d="M 269.964844 117.742188 C 269.964844 102.085938 257.273438 89.394531 241.617188 89.394531 C 225.960938 89.394531 213.269531 102.085938 213.269531 117.742188 C 213.269531 133.398438 225.960938 146.085938 241.617188 146.085938 C 257.273438 146.085938 269.964844 133.398438 269.964844 117.742188 Z M 269.964844 117.742188 "/>
			<g class="latent">
				<text text-anchor="middle" x="136" y="61">g</text>
				<text text-anchor="middle" x="136" y="187">s</text>
				<text text-anchor="middle" x="241" y="125">a</text>
			</g>
			<g id="latentvar">
				<path style="fill:none;stroke-width:0.79701;stroke-linecap:butt;stroke-linejoin:miter;stroke:rgb(59.999084%,59.999084%,59.999084%);stroke-opacity:1;stroke-miterlimit:10;" d="M -25.789063 93.951937 C -15.597656 115.8035 -55.269531 115.8035 -45.078125 93.951937 " transform="matrix(1,0,0,-1,170.75,117.741)"/>
				<path style=" stroke:none;fill-rule:nonzero;fill:rgb(59.999084%,59.999084%,59.999084%);fill-opacity:1;" d="M 143.648438 26.605469 C 144.429688 25.539063 145.886719 23.644531 147.597656 22.730469 C 145.761719 23.300781 144.820313 22.863281 144.078125 21.089844 C 144.476563 22.988281 143.960938 25.320313 143.648438 26.605469 Z M 143.648438 26.605469 "/>
				<path style=" stroke:none;fill-rule:nonzero;fill:rgb(59.999084%,59.999084%,59.999084%);fill-opacity:1;" d="M 126.984375 26.605469 C 126.671875 25.320313 126.15625 22.988281 126.554688 21.089844 C 125.8125 22.863281 124.871094 23.300781 123.035156 22.730469 C 124.746094 23.644531 126.203125 25.539063 126.984375 26.605469 Z M 126.984375 26.605469 "/>
				<path style=" stroke:none;fill-rule:nonzero;fill:rgb(100%,100%,100%);fill-opacity:1;" d="M 143.871094 8.554688 C 143.871094 3.832031 140.039063 0 135.316406 0 C 130.59375 0 126.761719 3.832031 126.761719 8.554688 C 126.761719 13.277344 130.59375 17.109375 135.316406 17.109375 C 140.039063 17.109375 143.871094 13.277344 143.871094 8.554688 Z M 143.871094 8.554688 "/>
				<g transform="translate(135,13)">
					<text id="varg" class="blabel" text-anchor="middle" transform="rotate(0)">1.0</text>
				</g>
				<path style="fill:none;stroke-width:0.79701;stroke-linecap:butt;stroke-linejoin:miter;stroke:rgb(59.999084%,59.999084%,59.999084%);stroke-opacity:1;stroke-miterlimit:10;" d="M 80.511719 32.580844 C 90.703125 54.432406 51.03125 54.432406 61.222656 32.580844 " transform="matrix(1,0,0,-1,170.75,117.741)"/>
				<path style=" stroke:none;fill-rule:nonzero;fill:rgb(59.999084%,59.999084%,59.999084%);fill-opacity:1;" d="M 249.949219 87.976563 C 250.730469 86.914063 252.1875 85.019531 253.898438 84.101563 C 252.0625 84.675781 251.121094 84.238281 250.378906 82.460938 C 250.777344 84.363281 250.261719 86.695313 249.949219 87.976563 Z M 249.949219 87.976563 "/>
				<path style=" stroke:none;fill-rule:nonzero;fill:rgb(59.999084%,59.999084%,59.999084%);fill-opacity:1;" d="M 233.285156 87.976563 C 232.972656 86.695313 232.457031 84.363281 232.855469 82.460938 C 232.113281 84.238281 231.171875 84.675781 229.335938 84.101563 C 231.046875 85.019531 232.503906 86.914063 233.285156 87.976563 Z M 233.285156 87.976563 "/>
				<path style=" stroke:none;fill-rule:nonzero;fill:rgb(100%,100%,100%);fill-opacity:1;" d="M 250.09375 69.925781 C 250.09375 65.246094 246.300781 61.449219 241.617188 61.449219 C 236.933594 61.449219 233.140625 65.246094 233.140625 69.925781 C 233.140625 74.609375 236.933594 78.40625 241.617188 78.40625 C 246.300781 78.40625 250.09375 74.609375 250.09375 69.925781 Z M 250.09375 69.925781 "/>
				<g transform="translate(241.5,74)">
					<text id="vara" class="blabel" text-anchor="middle" transform="rotate(0)">',
					str_remove(formatC(v_a, digits = 2, format = "f"), pattern = "^0"),
					'</text>
				</g>
				<path style="fill:none;stroke-width:0.79701;stroke-linecap:butt;stroke-linejoin:miter;stroke:rgb(59.999084%,59.999084%,59.999084%);stroke-opacity:1;stroke-miterlimit:10;" d="M -45.078125 -93.954313 C -55.269531 -115.805875 -15.597656 -115.805875 -25.789063 -93.954313 " transform="matrix(1,0,0,-1,170.75,117.741)"/>
				<path style=" stroke:none;fill-rule:nonzero;fill:rgb(59.999084%,59.999084%,59.999084%);fill-opacity:1;" d="M 126.984375 208.878906 C 126.203125 209.941406 124.746094 211.835938 123.035156 212.75 C 124.871094 212.179688 125.8125 212.617188 126.554688 214.394531 C 126.15625 212.492188 126.671875 210.160156 126.984375 208.878906 Z M 126.984375 208.878906 "/>
				<path style=" stroke:none;fill-rule:nonzero;fill:rgb(59.999084%,59.999084%,59.999084%);fill-opacity:1;" d="M 143.648438 208.878906 C 143.960938 210.160156 144.476563 212.492188 144.078125 214.394531 C 144.820313 212.617188 145.761719 212.179688 147.597656 212.75 C 145.886719 211.835938 144.429688 209.941406 143.648438 208.878906 Z M 143.648438 208.878906 "/>
				<path style=" stroke:none;fill-rule:nonzero;fill:rgb(100%,100%,100%);fill-opacity:1;" d="M 143.832031 226.925781 C 143.832031 222.222656 140.019531 218.414063 135.316406 218.414063 C 130.613281 218.414063 126.800781 222.222656 126.800781 226.925781 C 126.800781 231.628906 130.613281 235.441406 135.316406 235.441406 C 140.019531 235.441406 143.832031 231.628906 143.832031 226.925781 Z M 143.832031 226.925781 "/>
				<g transform="translate(134.5,230)">
					<text id="bvars" class="blabel" text-anchor="middle" transform="rotate(0)">',
str_remove(formatC(v_s, digits = 2, format = "f"), pattern = "^0"),
'</text>
				</g>
			</g>
			<g id="ga">
				<path style="fill:none;stroke-width:1.59404;stroke-linecap:butt;stroke-linejoin:miter;stroke:rgb(59.999084%,59.999084%,59.999084%);stroke-opacity:1;stroke-miterlimit:10;" d="M -6.914063 44.908969 L 38.414063 18.737094 " transform="matrix(1,0,0,-1,170.75,117.741)"/>
				<path style=" stroke:none;fill-rule:nonzero;fill:rgb(59.999084%,59.999084%,59.999084%);fill-opacity:1;" d="M 213.097656 101.277344 C 211.648438 100.003906 209.078125 97.644531 207.960938 95.03125 C 208.558594 97.78125 207.804688 99.09375 205.121094 99.949219 C 207.941406 99.609375 211.269531 100.65625 213.097656 101.277344 Z M 213.097656 101.277344 "/>
				<path style=" stroke:none;fill-rule:nonzero;fill:rgb(100%,100%,100%);fill-opacity:1;" d="M 195.578125 86.246094 C 195.578125 81.542969 191.765625 77.730469 187.0625 77.730469 C 182.359375 77.730469 178.546875 81.542969 178.546875 86.246094 C 178.546875 90.945313 182.359375 94.757813 187.0625 94.757813 C 191.765625 94.757813 195.578125 90.945313 195.578125 86.246094 Z M 195.578125 86.246094 "/>
				<g transform="translate(185,89.5)">
					<text id="bga" class="blabel" text-anchor="middle" transform="rotate(30)" ',
ifelse(b_a.g_neg, 'style="fill: red; font-size: 7pt;"', ''),
'>',
prob_label(b_a.g),
'</text>
				</g>
			</g>
			<g id="sa">
				<path style="fill:none;stroke-width:1.59404;stroke-linecap:butt;stroke-linejoin:miter;stroke:rgb(59.999084%,59.999084%,59.999084%);stroke-opacity:1;stroke-miterlimit:10;" d="M -6.914063 -44.907438 L 38.414063 -18.735563 " transform="matrix(1,0,0,-1,170.75,117.741)"/>
				<path style=" stroke:none;fill-rule:nonzero;fill:rgb(59.999084%,59.999084%,59.999084%);fill-opacity:1;" d="M 213.097656 134.207031 C 211.269531 134.824219 207.941406 135.871094 205.121094 135.53125 C 207.804688 136.390625 208.558594 137.699219 207.960938 140.449219 C 209.078125 137.839844 211.648438 135.480469 213.097656 134.207031 Z M 213.097656 134.207031 "/>
				<path style=" stroke:none;fill-rule:nonzero;fill:rgb(100%,100%,100%);fill-opacity:1;" d="M 195.578125 149.238281 C 195.578125 144.535156 191.765625 140.722656 187.0625 140.722656 C 182.359375 140.722656 178.546875 144.535156 178.546875 149.238281 C 178.546875 153.941406 182.359375 157.753906 187.0625 157.753906 C 191.765625 157.753906 195.578125 153.941406 195.578125 149.238281 Z M 195.578125 149.238281 "/>
				<g transform="translate(188,152)">
					<text id="bsa" class="blabel" text-anchor="middle" transform="rotate(-30)" ',
ifelse(b_a.s_neg, 'style="fill: red; font-size: 7pt;"', ''),
'>',
          prob_label(b_a.s),
          '</text>
				</g>
			</g>
			<g id="ga">
				<path style="fill:none;stroke-width:1.59404;stroke-linecap:butt;stroke-linejoin:miter;stroke:rgb(59.999084%,59.999084%,59.999084%);stroke-opacity:1;stroke-miterlimit:10;" d="M -35.433594 28.444125 L -35.433594 -23.899625 " transform="matrix(1,0,0,-1,170.75,117.741)"/>
				<path style=" stroke:none;fill-rule:nonzero;fill:rgb(59.999084%,59.999084%,59.999084%);fill-opacity:1;" d="M 135.316406 146.183594 C 135.695313 144.292969 136.453125 140.882813 138.15625 138.613281 C 136.074219 140.503906 134.558594 140.503906 132.476563 138.613281 C 134.179688 140.882813 134.9375 144.292969 135.316406 146.183594 Z M 135.316406 146.183594 "/>
				<path style=" stroke:none;fill-rule:nonzero;fill:rgb(100%,100%,100%);fill-opacity:1;" d="M 143.832031 116.121094 C 143.832031 111.417969 140.019531 107.605469 135.316406 107.605469 C 130.613281 107.605469 126.800781 111.417969 126.800781 116.121094 C 126.800781 120.824219 130.613281 124.636719 135.316406 124.636719 C 140.019531 124.636719 143.832031 120.824219 143.832031 116.121094 Z M 143.832031 116.121094 "/>
				<g style="fill:rgb(39.99939%,39.99939%,39.99939%);fill-opacity:1;" transform="translate(134,119)">
					<text id="bgs" class="blabel" text-anchor="middle" transform="rotate(0)" ',
ifelse(b_s.g_neg, 'style="fill: red; font-size: 7pt;"', ''),
'>',
prob_label(b_s.g),
'</text>
				</g>
			</g>
		</g>
		<g id="observed">
			<path style=" stroke:none;fill-rule:nonzero;fill:rgb(34.707642%,51.998901%,69.291687%);fill-opacity:1;" d="M 73.839844 32.273438 L 33.621094 32.273438 C 31.421875 32.273438 29.636719 34.058594 29.636719 36.257813 L 29.636719 76.476563 C 29.636719 78.679688 31.421875 80.464844 33.621094 80.464844 L 73.839844 80.464844 C 76.042969 80.464844 77.824219 78.679688 77.824219 76.476563 L 77.824219 36.257813 C 77.824219 34.058594 76.042969 32.273438 73.839844 32.273438 Z M 73.839844 32.273438 "/>
			<path style=" stroke:none;fill-rule:nonzero;fill:rgb(36.999512%,55.332947%,37.333679%);fill-opacity:1;" d="M 73.839844 155.019531 L 33.621094 155.019531 C 31.421875 155.019531 29.636719 156.804688 29.636719 159.003906 L 29.636719 199.222656 C 29.636719 201.425781 31.421875 203.207031 33.621094 203.207031 L 73.839844 203.207031 C 76.042969 203.207031 77.824219 201.425781 77.824219 199.222656 L 77.824219 159.003906 C 77.824219 156.804688 76.042969 155.019531 73.839844 155.019531 Z M 73.839844 155.019531 "/>
			<path style=" stroke:none;fill-rule:nonzero;fill:rgb(47.3526%,37.353516%,63.233948%);fill-opacity:1;" d="M 343.3125 93.644531 L 303.09375 93.644531 C 300.890625 93.644531 299.109375 95.429688 299.109375 97.632813 L 299.109375 137.851563 C 299.109375 140.050781 300.890625 141.835938 303.09375 141.835938 L 343.3125 141.835938 C 345.511719 141.835938 347.296875 140.050781 347.296875 137.851563 L 347.296875 97.632813 C 347.296875 95.429688 345.511719 93.644531 343.3125 93.644531 Z M 343.3125 93.644531 "/>
			<g class="observed">
				<text text-anchor="middle" x="54" y="66">G</text>
				<text text-anchor="middle" x="54" y="189">S</text>
				<text text-anchor="middle" x="323" y="127">A</text>
			</g>
		</g>
		<g id="loadings">
			<path style="fill:none;stroke-width:1.59404;stroke-linecap:butt;stroke-linejoin:miter;stroke:rgb(59.999084%,59.999084%,59.999084%);stroke-opacity:1;stroke-miterlimit:10;" d="M -68.363281 61.373812 L -84.195313 61.373812 " transform="matrix(1,0,0,-1,170.75,117.741)"/>
			<path style=" stroke:none;fill-rule:nonzero;fill:rgb(59.999084%,59.999084%,59.999084%);fill-opacity:1;" d="M 82.011719 56.367188 C 83.902344 56.746094 87.308594 57.503906 89.582031 59.207031 C 87.6875 57.125 87.6875 55.609375 89.582031 53.527344 C 87.308594 55.234375 83.902344 55.988281 82.011719 56.367188 Z M 82.011719 56.367188 "/>
			<path style="fill:none;stroke-width:1.59404;stroke-linecap:butt;stroke-linejoin:miter;stroke:rgb(59.999084%,59.999084%,59.999084%);stroke-opacity:1;stroke-miterlimit:10;" d="M -68.363281 -61.372281 L -84.195313 -61.372281 " transform="matrix(1,0,0,-1,170.75,117.741)"/>
			<path style=" stroke:none;fill-rule:nonzero;fill:rgb(59.999084%,59.999084%,59.999084%);fill-opacity:1;" d="M 82.011719 179.113281 C 83.902344 179.492188 87.308594 180.25 89.582031 181.953125 C 87.6875 179.871094 87.6875 178.355469 89.582031 176.273438 C 87.308594 177.976563 83.902344 178.734375 82.011719 179.113281 Z M 82.011719 179.113281 "/>
			<path style="fill:none;stroke-width:1.59404;stroke-linecap:butt;stroke-linejoin:miter;stroke:rgb(59.999084%,59.999084%,59.999084%);stroke-opacity:1;stroke-miterlimit:10;" d="M 103.796875 -0.0011875 L 119.628906 -0.0011875 " transform="matrix(1,0,0,-1,170.75,117.741)"/>
			<path style=" stroke:none;fill-rule:nonzero;fill:rgb(59.999084%,59.999084%,59.999084%);fill-opacity:1;" d="M 294.921875 117.742188 C 293.03125 117.363281 289.625 116.605469 287.351563 114.902344 C 289.246094 116.984375 289.246094 118.5 287.351563 120.582031 C 289.625 118.875 293.03125 118.121094 294.921875 117.742188 Z M 294.921875 117.742188 "/>
		</g>
		<g id="varOb">
			<g id="varObG">
				<path style="fill:none;stroke-width:0.79701;stroke-linecap:butt;stroke-linejoin:miter;stroke:rgb(59.999084%,59.999084%,59.999084%);stroke-opacity:1;stroke-miterlimit:10;" d="M -145.9375 71.420687 C -169.210938 82.276156 -169.210938 40.471469 -145.9375 51.323031 " transform="matrix(1,0,0,-1,170.75,117.741)"/>
				<path style=" stroke:none;fill-rule:nonzero;fill:rgb(59.999084%,59.999084%,59.999084%);fill-opacity:1;" d="M 27.632813 47.632813 C 26.566406 46.851563 24.671875 45.394531 23.757813 43.683594 C 24.328125 45.519531 23.890625 46.460938 22.113281 47.203125 C 24.015625 46.804688 26.347656 47.320313 27.632813 47.632813 Z M 27.632813 47.632813 "/>
				<path style=" stroke:none;fill-rule:nonzero;fill:rgb(59.999084%,59.999084%,59.999084%);fill-opacity:1;" d="M 27.632813 65.105469 C 26.347656 65.417969 24.015625 65.933594 22.113281 65.53125 C 23.890625 66.277344 24.328125 67.214844 23.757813 69.054688 C 24.671875 67.339844 26.566406 65.886719 27.632813 65.105469 Z M 27.632813 65.105469 "/>
				<path style=" stroke:none;fill-rule:nonzero;fill:rgb(100%,100%,100%);fill-opacity:1;" d="M 17.027344 56.367188 C 17.027344 51.664063 13.214844 47.851563 8.511719 47.851563 C 3.808594 47.851563 0 51.664063 0 56.367188 C 0 61.070313 3.808594 64.882813 8.511719 64.882813 C 13.214844 64.882813 17.027344 61.070313 17.027344 56.367188 Z M 17.027344 56.367188 "/>
				<g transform="translate(7.5,59.75)">
					<text id="bvarObG" class="blabel" text-anchor="middle" transform="rotate(0)">',
str_remove(formatC(1 - r_gg, digits = 2, format = "f"), pattern = "^0"),
'</text>
				</g>
			</g>
			<g id="varObS">
				<path style="fill:none;stroke-width:0.79701;stroke-linecap:butt;stroke-linejoin:miter;stroke:rgb(59.999084%,59.999084%,59.999084%);stroke-opacity:1;stroke-miterlimit:10;" d="M -145.9375 -51.325406 C -169.210938 -40.469938 -169.210938 -82.274625 -145.9375 -71.423063 " transform="matrix(1,0,0,-1,170.75,117.741)"/>
				<path style=" stroke:none;fill-rule:nonzero;fill:rgb(59.999084%,59.999084%,59.999084%);fill-opacity:1;" d="M 27.632813 170.378906 C 26.566406 169.597656 24.671875 168.140625 23.757813 166.429688 C 24.328125 168.265625 23.890625 169.207031 22.113281 169.949219 C 24.015625 169.550781 26.347656 170.066406 27.632813 170.378906 Z M 27.632813 170.378906 "/>
				<path style=" stroke:none;fill-rule:nonzero;fill:rgb(59.999084%,59.999084%,59.999084%);fill-opacity:1;" d="M 27.632813 187.847656 C 26.347656 188.160156 24.015625 188.679688 22.113281 188.277344 C 23.890625 189.023438 24.328125 189.960938 23.757813 191.800781 C 24.671875 190.085938 26.566406 188.632813 27.632813 187.847656 Z M 27.632813 187.847656 "/>
				<path style=" stroke:none;fill-rule:nonzero;fill:rgb(100%,100%,100%);fill-opacity:1;" d="M 17.027344 179.113281 C 17.027344 174.410156 13.214844 170.597656 8.511719 170.597656 C 3.808594 170.597656 0 174.410156 0 179.113281 C 0 183.816406 3.808594 187.628906 8.511719 187.628906 C 13.214844 187.628906 17.027344 183.816406 17.027344 179.113281 Z M 17.027344 179.113281 "/>
				<g transform="translate(7.5,183)">
					<text id="bvarObS" class="blabel" text-anchor="middle" transform="rotate(0)">',
str_remove(formatC(1 - r_ss, digits = 2, format = "f"), pattern = "^0"),
'</text>
				</g>
			</g>
			<g id="varObA">
				<path style="fill:none;stroke-width:0.79701;stroke-linecap:butt;stroke-linejoin:miter;stroke:rgb(59.999084%,59.999084%,59.999084%);stroke-opacity:1;stroke-miterlimit:10;" d="M 181.371094 -10.048063 C 204.644531 -20.903531 204.644531 20.901156 181.371094 10.049594 " transform="matrix(1,0,0,-1,170.75,117.741)"/>
				<path style=" stroke:none;fill-rule:nonzero;fill:rgb(59.999084%,59.999084%,59.999084%);fill-opacity:1;" d="M 349.300781 126.476563 C 350.367188 127.257813 352.261719 128.714844 353.175781 130.425781 C 352.605469 128.589844 353.042969 127.648438 354.816406 126.90625 C 352.917969 127.304688 350.585938 126.789063 349.300781 126.476563 Z M 349.300781 126.476563 "/>
				<path style=" stroke:none;fill-rule:nonzero;fill:rgb(59.999084%,59.999084%,59.999084%);fill-opacity:1;" d="M 349.300781 109.003906 C 350.585938 108.691406 352.917969 108.175781 354.816406 108.578125 C 353.042969 107.832031 352.605469 106.894531 353.175781 105.054688 C 352.261719 106.769531 350.367188 108.222656 349.300781 109.003906 Z M 349.300781 109.003906 "/>
				<path style=" stroke:none;fill-rule:nonzero;fill:rgb(100%,100%,100%);fill-opacity:1;" d="M 376.933594 117.742188 C 376.933594 113.039063 373.121094 109.226563 368.421875 109.226563 C 363.71875 109.226563 359.90625 113.039063 359.90625 117.742188 C 359.90625 122.445313 363.71875 126.257813 368.421875 126.257813 C 373.121094 126.257813 376.933594 122.445313 376.933594 117.742188 Z M 376.933594 117.742188 "/>
				<g transform="translate(370,121.5)">
					<text id="bvarObA" class="blabel" text-anchor="middle" transform="rotate(0)">',
str_remove(formatC(1 - r_aa, digits = 2, format = "f"), pattern = "^0"),
'</text>
				</g>
			</g>
		</g>
	</g>
</svg>')

  )
}







# Define UI ----
ui <- fixedPage(
  id = "fixedpage",
  tags$style(
    HTML(
      "
        input[type=number] {
              -moz-appearance:textfield;
        padding: 5px;
        text-align: center;
        margin:0;
        line-height:1em;

        }
        input[type=number]::{
              -moz-appearance:textfield;
        }
        input[type=number]::-webkit-outer-spin-button,
        input[type=number]::-webkit-inner-spin-button {
              -webkit-appearance: none;
              margin: 0;
        }
        #sidebar {max-width: 340px; min-width:200px;}
        #fixedpage {max-width: 1020px;}
        .form-group {margin:0;}
    "
    )
  ),
  theme = bs_theme(
    base_font = bslib::font_google(
      "Roboto Condensed",
      ital = c(0, 1),
      wght = c(400, 700)
    ),
    primary = scales::alpha(mycolors[1], alpha = 0.7)
  ),
  titlePanel(
    "Measurement Error Affects Specific Learning Disability Identification Accuracy"
  ),
  sidebarLayout(
    sidebarPanel(
      id = "sidebar",
      width = 4,
      tags$table(
        class = "table-condensed w-100",
        tags$thead(tags$tr(
          tags$th(style = "width:40%;"),
          tags$th("Score",
                  class = "text-center",
                  style = "width:30%;"),
          tags$th(em(HTML("r<sub>xx</sub>")),
                  class = "text-center",
                  style = "width:30%;")
        )),
        tags$tr(
          tags$td(class = "align-middle", "General"),
          tags$td(
            # class = "py-0",
            numericInput(
              inputId =
                "generalAbility",
              label = NULL,
              min = 40,
              max = 160,
              value = 100
            )
          ),
          tags$td(
            # class = "py-0",
            numericInput(
              inputId =
                "generalReliability",
              label = NULL,
              min = 0,
              max = .999999,
              value = 0.97
            )
          ),
        ),
        tags$tr(
          tags$td("Specific", class = "align-middle"),
          tags$td(
            # class = "py-0",
            numericInput(
              "specificAbility",
              NULL,
              min = 40,
              max = 160,
              value = 75
            )
          ),
          tags$td(
            # class = "py-0",
            numericInput(
              inputId = "specificReliability",
              label = NULL,
              value = 0.92,
              min = 0,
              max = .99999,
              step = .01
            )
          ),
        ),
        tags$tr(
          tags$td("Academic", class = "align-middle"),
          tags$td(
            # class = "py-0",
            numericInput(
              "academicAbility",
              NULL,
              min = 40,
              max = 160,
              value = 80
            )
          ),
          tags$td(
            # class = "py-0",
            numericInput(
              inputId = "academicReliability",
              label = NULL,
              value = 0.92,
              min = 0,
              max = .99999,
              step = .01
            )
          ),
        )
      ),
      p(),
      tags$hr(),
      tags$table(
        class = "table-condensed w-100",
        tags$thead(tags$tr(
          # style = "border-bottom: 0.5px solid gray;",
          tags$th(style = "width:40%;", "Correlations"),
          tags$td(class = "text-center", style = "width:30%;", "Specific"),
          tags$td(class = "text-center", style = "width:30%;", "Academic")
        )),
        tags$tr(
          tags$td("General", class = "pb-3"),
          tags$td(
            # class = "py-0",
            numericInput(
              inputId = "gscor",
              label = NULL,
              value = 0.70,
              min = -1,
              max = 1,
              step = .01
            )
          ),
          tags$td(
            # class = "py-0",
            numericInput(
              inputId = "gacor",
              label = NULL,
              value = 0.65,
              min = -1,
              max = 1,
              step = .01
            )
          )
        ),
        tags$tr(tags$td("Specific"),
                tags$td(""),
                tags$td(
                  # class = "py-0",
                  numericInput(
                    inputId = "sacor",
                    label = NULL,
                    value = 0.60,
                    min = -1,
                    max = 1,
                    step = .01
                  )
                ))
      ),
      p(),
      tags$hr(),
      tags$table(
        class = "table-condensed w-100",
        tags$tr(
          tags$td("Threshold for Low Scores", style = "width:70%;"),
          tags$td(
            style = "width:30%;",
            numericInput(
              "threshold",
              NULL,
              min = 40,
              max = 160,
              value = 85
            )
          ),
        ),
        tags$tr(tags$td("Buffer Width"),
                tags$td(
                  # class =  "py-1",
                  numericInput(
                    "buffer",
                    NULL,
                    min = 0,
                    max = 60,
                    value = 5
                  )
                ),),
        tags$tr(tags$td("Meaningful Difference"),
                tags$td(
                  numericInput(
                    "meaningfuldifference",
                    NULL,
                    min = -60,
                    max = 60,
                    value = 10
                  )
                ))
      ),
      hr(),
      "Created by ",
        tags$a(target = "_blank",
               href = "https://wjschne.github.io/",
               "W. Joel Schneider")

    ),

    # Show a plot of the generated distribution
    mainPanel(
      width = 8,
      htmlOutput("plotwarnings"),
      tabsetPanel(
        type = "tabs",
        tabPanel("Plot",
                 plotOutput("distPlot")),
        tabPanel("Model Calculations",
                 tags$h3("Simplified Model Based on Specified Reliability and Correlation Coefficients"),
                 htmlOutput("model"),
          p(
            em("Note:"),
            "All observed and true scores are assumed to be multivariate normal."
          ),
          p("Most confidence intervals are univariate. The confidence interval for a true score is based on a single observed score. Specific Learning Disability is a multivariate construct that requires thinking about multivariate confidence intervals."),
          withMathJax(
            p(
              r"(Suppose that observed scores \(G\), \(S\), and \(A\) have reliability coefficients of \(r_{GG}\), \(r_{SS}\), and \(r_{AA}\), respectively. Their respective true scores are \(g\), \(s\), and \(a\). We can represent the three observed scores as a vector \(X=\left\{G,S,A\right\}\), the true scores as vector \(T=\left\{g,s,a\right\}\), and the true score differences as \(D=\left\{g-s,g-a\right\}\). The covariance matrix of the observed scores is:)"
            ),
            p(
              r"($$\mathbf{\Sigma}_X=\left[
              \begin{matrix}\sigma_G^2&\sigma_{GS}&\sigma_{GA}\\
              \sigma_{GS}&\sigma_S^2&\sigma_{SA}\\
              \sigma_{GA}&\sigma_{SA}&\sigma_A^2\\
              \end{matrix}\right]$$)"
            ),
            p(
              "The covariance matrix of the true scores is the same as that of the observed score covariance matrix except that the true score variances are equal to the observed variances multiplied by the appropriate reliability coefficient:"
            ),
            p(
              r"($$\mathbf{\Sigma}_T=
              \begin{bmatrix}
              r_{GG}\sigma_G^2 & \sigma_{GS} & \sigma_{GA}\\
              \sigma_{GS} & r_{SS}\sigma_S^2 & \sigma_{SA}\\
              \sigma_{GA} & \sigma_{SA} & r_{AA}\sigma_A^2
              \end{bmatrix}$$)"
            ),
            p(
              r"(Because the matrix of covariances between the observed and true scores is the same as the true score covariance matrix, the combined covariance matrix is:)"
            ),
            p(
              r"($$\mathbf{\Sigma}_{XT}=\left[\begin{matrix}\mathbf{\Sigma}_X&\mathbf{\Sigma}_T\\
              \mathbf{\Sigma}_T&\mathbf{\Sigma}_T\\
              \end{matrix}\right]$$)"
            ),
            p(
              r"(To create the covariance matrix that includes the variances and covariances of true score differences \(g-s\) and \(g-a\), we create a weight matrix \(\boldsymbol{W}\):)"
            ),
            p(
              r"($$\mathbf{W}=\begin{matrix}
              \begin{matrix}\color{gray}G & \color{gray}S & \color{gray}A & \color{gray}g & \color{gray}s & \color{gray}a & \color{gray}g\color{gray}-\color{gray}a & \color{gray}g\color{gray}-\color{gray}s \phantom{-}\end{matrix}\\
              \begin{bmatrix}
              1&0&0&0&0&0&\phantom{-}0\phantom{-}&\phantom{-}0\\
              0&1&0&0&0&0&\phantom{-}0\phantom{-}&\phantom{-}0\\
              0&0&1&0&0&0&\phantom{-}0\phantom{-}&\phantom{-}0\\
              0&0&0&1&0&0&\phantom{-}1\phantom{-}&\phantom{-}1\\
              0&0&0&0&1&0&-1\phantom{-}&\phantom{-}0\\
              0&0&0&0&0&1&\phantom{-}0\phantom{-}&-1\\
              \end{bmatrix} \begin{matrix}\color{gray}G\\ \color{gray}S\\ \color{gray}A\\ \color{gray}g\\ \color{gray}s\\ \color{gray}a\\\end{matrix}\end{matrix}$$)"
            ),
            p(
              r"(The covariance matrix of all observed scores in vector \(X\), true scores in vector \(T\), and true score differences in vector \(D\) is calculated like so:)"
            ),
            p(
              r"($$\mathbf{\Sigma}_{XTD}=\mathbf{W}^\prime\mathbf{\Sigma}_{XT}\mathbf{W}$$)"
            ),
            p(
              r"(From the \(\mathbf{\Sigma}_{XTD}\) matrix, we can select just the portion of pertaining to the true scores and the true score differences in a covariance matrix we can call \(\mathbf{\Sigma}_{TD}\).)"
            ),
            p(
              r"(Using equations for computing conditional distributions, the means of the true scores and true score differences after controlling for specific observed scores are:)"
            ),
            p(
              r"($$\mu_{TD|X}=\mu_X+\mathbf{\Sigma}_{TD}\mathbf{\Sigma}_X^{-1}\left(X-\mu_X\right)$$)"
            ),
            p(
              r"(Likewise, the covariance matrix of the true scores and true score differences after controlling for specific observed scores is:)"
            ),
            p(
              r"($$\mathbf{\Sigma}_{TD|X}=\mathbf{\Sigma}_{TD}-\mathbf{\Sigma}_{TD}\mathbf{\Sigma}_X^{-1}\mathbf{\Sigma}_{TD}^\prime$$)"
            ),
            p(
              r"(To calculate the multivariate conditional PPV, we specify a multivariate normal distribution for the conditional true scores: \(\mathcal{N}\left(\mu_{TD|X},\ \mathbf{\Sigma}_{TD|X}\right)\). Then we evaluate the cumulative distribution function of the multivariate normal distribution with lower bounds at the diagnostic thresholds for \(G\), \(G-S\), and \(G-A\) and upper bounds at the diagnostic thresholds for \(S\) and \(A\).)"
            )
          )
        )
      )
    ),
  )
)

# Server logic ----
server <- function(input, output) {
    plotstuff <- shiny::reactive({
      make_conditional_ppv_plot(
        General = ifelse(
          is.numeric(input$generalAbility),
          input$generalAbility,
          100),
        Specific = ifelse(
          is.numeric(input$specificAbility),
          input$specificAbility,
          100
        ),
        Academic = ifelse(
          is.numeric(input$academicAbility),
          input$academicAbility,
          100
        ),
        r_gg = ifelse(
          is.numeric(input$generalReliability),
          input$generalReliability,
          0
        ),
        r_ss = ifelse(
          is.numeric(input$specificReliability),
          input$specificReliability,
          0
        ),
        r_aa = ifelse(
          is.numeric(input$academicReliability),
          input$academicReliability,
          0
        ),
        buffer = ifelse(
          is.numeric(input$buffer),
          input$buffer,
          5),
        threshold_g = ifelse(
          is.numeric(input$threshold),
          input$threshold,
          85),
        threshold_s = ifelse(
          is.numeric(input$threshold),
          input$threshold,
          85),
        threshold_a = ifelse(
          is.numeric(input$threshold),
          input$threshold,
          85),
        meaningful_difference = ifelse(
          is.numeric(input$meaningfuldifference),
          input$meaningfuldifference,
          5),
        gscor = ifelse(
          is.numeric(input$gscor),
          input$gscor,
          0.6),
        gacor = ifelse(
          is.numeric(input$gacor),
          input$gacor,
          0.6),
        sacor = ifelse(
          is.numeric(input$sacor),
          input$sacor,
          0.6)
      )
    })


    output$distPlot <- renderPlot(plotstuff()[["plot"]], height = 800)
    output$model <- renderText(plotstuff()[["model"]])
    output$plotwarnings <- renderText(plotstuff()[["plot_warnings"]])

}
ggplot2::theme_set(ggplot2::theme_minimal(base_size = 18))
thematic_shiny(font = font_spec(families = "Roboto Condensed", install = T, update = T))

# Run the application
shinyApp(ui = ui, server = server)
