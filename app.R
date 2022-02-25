#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
library(tidyverse)
library(scales)
library(ggdist)
library(ggtext)
library(ragg)
library(showtext)
library(thematic)
library(bslib)
library(Cairo)
library(showtext)
library(viridis)

# Options
options(shiny.usecairo = T, # use Cairo device for better antialiasing
        scipen = 999 # Do not use scientific notation
        )

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

# Makes geom_text size the same as the base size
# (scaled to the specified ratio)
ggtext_size <- function(base_size, ratio = 0.8) {
    ratio * base_size / 2.845276
}

# SLD colors
mycolors <- c(SLD = "#482576",
              Buffer = "#35608D",
              NotSLD = "#43BF71")
mycolours <- mycolors

lightenedcolors <- tinter::lighten(mycolors, .15)


viridis_start = .20
viridis_end = .75
viridis_alpha = .40

myplot <-
    function(x, # General, specific, and academic scores
             rxx = rep(0.95, length(x)), # 3 reliability scores
             threshold = 85, # Decision threshold
             mu = 100, # Mean
             sigma = 15, # SD
             buffer = 5 # Decision threshold
             ) {

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
            fill = scales::alpha(c(mycolors[c("SLD",
                                              "NotSLD",
                                              "SLD",
                                              "NotSLD",
                                              "Buffer")],
                                   "gray30",
                                   "gray70"),
                                 0.15)
        )

        # Make plot theme match page theme
        thematic_on(font = "Roboto Condensed")

        # Standard error of the estimate
        see <- 15 * sqrt(rxx - rxx ^ 2)

        # Conditional mean
        mu <- (x - 100) * rxx + 100

        # P(x < threshold)
        p_threshold <- pnorm(threshold, mu, see)

        # P(x < buffer)
        p_buffer <- pnorm(threshold + buffer, mu, see)

        # conditional PPV: P(x) is sld-consisent
        conditional_ppv <- (1 - p_buffer[1]) *
            p_threshold[2] *
            p_threshold[3]

        # P(x) is not inconsistent with SLD
        p_not_inconsistent <- (1 - p_threshold[1]) *
            p_buffer[2] *
            p_buffer[3]

        # text output for probabilities
        pp_buffer_label <- pnorm(threshold + buffer, mu, see)
        pp_buffer_label[1] <- 1 - pp_buffer_label[1]

        d <- tibble(
            dist = rep("norm", 4),
            args = list(list(100, 15),
                     list(mu[1], see[1]),
                     list(mu[2], see[2]),
                     list(mu[3], see[3])),
            Ability = c(
                "Population",
                "General\nAbility",
                "Specific\nAbility",
                "Academic\nAbility"
            )
        ) %>%
            mutate(rxx_display = if_else(
                Ability == "Population",
                "",
                paste0("italic(r[xx]) == '",
                       c("", remove_leading_zero(rxx)),
                       "'"))) %>%
            mutate(
                Score = c(100, x),
                mu = c(100, mu),
                sd = c(15, see),
                p = paste0(
                    "P(*X* < ",
                    threshold,
                    ") = ",
                    number(pnorm(threshold, mu, sd),
                           0.01) %>%
                        str_remove("^0")
                ),
                ScoreDisplay = ifelse(
                    Ability == "Population",
                    0,
                    Score),
                p_more = paste0(
                    "P(*X* > ",
                    threshold + buffer,
                    ") = ",
                    number(1 - pnorm(threshold + buffer,
                                     Score,
                                     sd), 0.01) %>%
                        str_remove("^0")
                ),
                Ability = factor(Ability) %>%
                    fct_inorder() %>%
                    fct_rev(),
                p_less_color = case_when(
                    Ability == "Population" ~ "gray30",
                    Ability == "General\nAbility" ~ mycolors["NotSLD"],
                    TRUE ~ mycolors["SLD"]
                ),
                p_more_color = case_when(
                    Ability == "Population" ~ "gray70",
                    Ability == "General\nAbility" ~ mycolors["SLD"],
                    TRUE ~ mycolors["NotSLD"]
                ),
                p_between_color = case_when(
                    Ability == "Population" ~ "gray50",
                    TRUE ~ mycolors["Buffer"])
            )


        myplot <-
            ggplot(d, aes(y = Ability,
                          dist = dist,
                          args = args)) +
            geom_richtext(
                aes(
                    label = p,
                    x = threshold - 1,
                    y = as.numeric(Ability),
                    color = p_less_color
                ),
                vjust = 1.4,
                hjust = 1,
                size = ggtext_size(16),
                label.padding = unit(0, "mm"),
                show.legend = F,
                label.colour = NA
            ) +
            geom_richtext(
                aes(
                    label = p_more,
                    x = threshold + buffer + 1,
                    y = as.numeric(Ability),
                    color = p_more_color,
                ),
                vjust = 1.4,
                hjust = 0,
                size = ggtext_size(16),
                label.padding = unit(0, "mm"),
                show.legend = F,
                label.colour = NA
            ) +
            geom_hline(yintercept = 1:4 - 0.2,
                       size = 0.2,
                       color = "gray30") +
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
                aes(fill = stat(
                    case_when(
                        y == 4 & x < threshold ~ "gray30",
                        y == 4 &
                            x < threshold + buffer ~ mycolors["Buffer"],
                        y == 4 ~ "gray70",
                        (x < threshold & y != 3) ~ mycolors["SLD"],
                        ((x >= threshold + buffer)  &
                             y == 3) ~ mycolors["SLD"],
                        (x > threshold + buffer) |
                            (x < threshold) ~ mycolors["NotSLD"],
                        TRUE ~ mycolors["Buffer"]
                    )
                )),
                height = 0.8,
                show_interval = F,
                alpha = 0.7
            ) +
            geom_point(aes(x = ScoreDisplay),
                       color = "gray20") +
            geom_text(
                aes(label = ScoreDisplay,
                    x = ScoreDisplay),
                vjust = -.5,
                size = 5,
                color = "gray20"
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
            theme(
                plot.title.position = "plot",
                plot.title = element_markdown(size = 16,
                                              color = "gray30"),
                plot.subtitle = element_markdown(size = 16,
                                                 color = "gray30"),
                panel.grid.major.y = element_blank(),
                axis.text.y = element_text(vjust = 0,
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
                family = "Roboto Condensed"
            ) +
            annotate(
                x = threshold + buffer + 1,
                y = 4,
                label = "Not Low",
                geom = "text",
                angle = 0,
                hjust = 0,
                vjust = -0.5,
                size = ggtext_size(16),
                family = "Roboto Condensed"
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
                family = "Roboto Condensed"
            )

        # Return list
        list(
            plot = myplot,
            ppv_label = paste0(
                'All true scores are on the ',
                '<span style="font-weight: bold; color:',
                scales::alpha(mycolors["SLD"], 0.7),
                '">SLD-Likely</span> side of the threshold of ',
                threshold
            ),
            ppv = proportion_round(conditional_ppv, 2) %>%
                remove_leading_zero(2),
            pbuffer_label = paste0(
                'No true score is on the ',
                '<span style="font-weight: bold; color:',
                scales::alpha(mycolors["NotSLD"], 0.7),
                '">SLD-Unlikely</span> side of the ',
                '<span style="font-weight: bold; color:',
                scales::alpha(mycolors["Buffer"], 0.7),
                '">buffer zone</span> (',
                threshold,
                "–",
                threshold + buffer,
                ")"
            ),
            pbuffer = proportion_round(p_not_inconsistent, 2) %>%
                remove_leading_zero(2)

        )
    }

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
  # Regression effects
  b_a.g = 0.5, # g's effect on a
  b_a.s = 0.3, # s's effect on a
  b_s.g = 0.65, # g's effect on s
  myfont = "Roboto Condensed",
  # viridis_start = viridis_start,
  # viridis_end = viridis_end,
  # viridis_alpha = viridis_alpha,
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


  # Explained variance
  s_by_g <- r_gg * (b_s.g ^ 2)
  a_by_g <- r_gg * (b_a.g + b_s.g * b_a.s) ^ 2
  a_by_s <- (r_ss - s_by_g) * (b_a.s ^ 2)

  # Residual variances
  v_s <- r_ss - s_by_g
  v_a <- r_aa - a_by_g - a_by_s


  # Model for simulation
  m <- glue::glue(
    "
s ~ {b_s.g} * g
g ~~ {r_gg} * g
s ~~ {v_s} * s
G ~ 1 * g
G ~~ (1 - {r_gg}) * G
S ~ 1 * s
S ~~ (1 - {r_ss}) * S
a ~ {b_a.g} * g + {b_a.s} * s
a ~~ {v_a} * a
A ~ 1 * a
A ~~ (1 - {r_aa}) * A
"
  )

  # Model-implied Covariance Matrix
  m_cov <- lavaan::fitted(lavaan::sem(m))$cov[v_names, v_names]
  m_cor <- cov2cor(m_cov)
  m_observed_cov <- matrix(c(1, gscor, gacor,
                    gscor, 1, sacor,
                    gacor, sacor, 1), nrow = 3)

  m_true_cov <- m_observed_cov
  diag(m_true_cov) <- r_xx
  m_cov <- rbind(cbind(m_true_cov, m_true_cov),
                 cbind(m_true_cov, m_observed_cov))
  if (det(m_cov) <= 0) stop("This combination of reliability and correlation coefficents is mathematically impossible.")

  colnames(m_cov) <- v_names
  rownames(m_cov) <- v_names
  cov_all <- m_cov * sigma ^ 2
  # True score covariance
  cov_true <- cov_all[v_true, v_true]
  # Observed score covariance
  cov_observed <- cov_all[v_observed, v_observed]
  # Cross covariances
  cov_true_observed <-  cov_all[v_true, v_observed]

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
  w_difference <- (
    "
variable	gs   	ga   	GS   	GA
g       	1.00 	1.00 	0.00 	0.00
s       	-1.00	0.00 	0.00 	0.00
a       	0.00 	-1.00	0.00 	0.00
G       	0.00 	0.00 	1.00 	1.00
S       	0.00 	0.00 	-1.00	0.00
A       	0.00 	0.00 	0.00 	-1.00"
  ) %>%
    I() %>%
    readr::read_tsv(file = ., show_col_types = F) %>%
    tibble::column_to_rownames("variable") %>%
    select(-GS,-GA) %>%
    as.matrix()

  # Full weight matrix
  w <- cbind(diag(6), w_difference) %>%
    `colnames<-`(c(rownames(w_difference),
                   colnames(w_difference)))

  # Conditional Correlations
  cor_conditional <- cov2cor(cov_conditional)

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
             c("", WJSmisc::remove_leading_zero(r_xx)),
             "'"))) %>%
    mutate(
      Score = c(100, General, Specific, Academic),
      mu = c(100, multivariate_mu),
      sd = c(15, multivariate_see),
      p = paste0(
        "P(*",symbol,"* < ",
        threshold,
        ") = ",
        WJSmisc::prob_label(pnorm(threshold, mu, sd),
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
        WJSmisc::prob_label(1 - pnorm(threshold + buffer, mu, sd),
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
      vjust = 1.4,
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
      vjust = 1.4,
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
      axis.text.y = element_text(vjust = 0,lineheight = 1.2,
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
        WJSmisc::prob_label(p_strict,
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
        WJSmisc::prob_label(p_relaxed,
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
    # p_x_strict = c(p_g_strict, p_s_strict, p_a_strict),
    # p_x_relaxed = c(p_g_relaxed, p_s_relaxed, p_a_relaxed)
  ) %>%
    mutate(
      Score = c(General - Specific, General - Academic),
      mu = cond_mu_sigma$condMean[4:5],
      sd = sqrt(diag(cond_mu_sigma$condVar)[4:5]),
      p = paste0(
        "P(*",symbol,"* < ",
        meaningful_difference - buffer,
        ") = ",
        WJSmisc::prob_label(pnorm(meaningful_difference - buffer, mu, sd),
                            digits = 2,
                            max_digits = 4,
                            round_zero_one = F)
      ),
      ScoreDisplay = Score,
      p_more = paste0(
        "P(*",symbol,"* > ",
        meaningful_difference,
        ") = ",
        WJSmisc::prob_label(1 - pnorm(meaningful_difference, mu, sd),
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
      vjust = 1.4,
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
      vjust = 1.4,
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
    # annotate(
    #   x = mean(c(meaningful_difference,
    #              meaningful_difference - buffer)),
    #   y = c(1,2),
    #   vjust = 0.5,
    #   hjust = -0.06,
    #   label = paste0("SLD-Possible"),
    #   geom = "text",
    #   color = tinter::darken(mycols["Buffer"], amount = .5),
    #   angle = 90,
    #   size = ggtext_size(14),
    #   family = myfont
    # ) +
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
      axis.text.y = element_text(vjust = 0,lineheight = 1.2,
                                 hjust = 0.5)
    )

  # Join plots
  gp <- (gp1 / gp2 + patchwork::plot_layout(heights = c(2.1, 1)))

  # Display plot
  gp
}




# layout widths
split_width <- c("50%", "50%")


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
        base_font = bslib::font_google("Roboto Condensed", ital = c(0,1), wght = c(400, 700)),
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
                style = "border-collapse: collapse;",
                tags$thead(tags$tr(
                    tags$td(style = "width:40%;"),
                    tags$td("Ability",
                            class = "text-center",
                            style = "width:30%;"),
                    tags$td(em(HTML("r<sub>xx</sub>")),
                            class = "text-center",
                            style = "width:30%;")
                )),
                tags$tr(
                    tags$td(class = "align-middle pb-3", "General"),
                    tags$td(
                        class = "py-0",
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
                        class = "py-0",
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
                    tags$td("Specific", class = "align-middle  pb-3"),
                    tags$td(
                        class = "py-0",
                        numericInput(
                            "specificAbility",
                            NULL,
                            min = 40,
                            max = 160,
                            value = 75
                        )
                    ),
                    tags$td(
                        class = "py-0",
                        numericInput(
                            inputId = "specificReliability",
                            label = NULL,
                            value = 0.92,
                            min = .6,
                            max = .99999,
                            step = .01
                        )
                    ),
                ),
                tags$tr(
                    tags$td("Academic", class = "align-middle  pb-3"),
                    tags$td(
                        class = "py-0",
                        numericInput(
                            "academicAbility",
                            NULL,
                            min = 40,
                            max = 160,
                            value = 81
                        )
                    ),
                    tags$td(
                        class = "py-0",
                        numericInput(
                            inputId = "academicReliability",
                            label = NULL,
                            value = 0.92,
                            min = .6,
                            max = .99999,
                            step = .01
                        )
                    ),
                )
            ),
            tags$table(class = "table-condensed w-100",
                       tags$thead(
                         tags$tr(
                           # style = "border-bottom: 0.5px solid gray;",
                           tags$td(style = "width:40%;", tags$strong("Correlations"), class = "pt-3"),
                           tags$td(class = "text-center pt-3", style = "width:30%;", "Specific"),
                           tags$td(class = "text-center pt-3", style = "width:30%;", "Academic")
                         )
                       ),
                       tags$tr(
                         tags$td("General", class = "pb-3"),
                         tags$td(
                           class = "py-0",
                           numericInput(
                             inputId = "gscor",
                             label = NULL,
                             value = 0.60,
                             min = -1,
                             max = 1,
                             step = .01
                           )
                         ),
                         tags$td(
                           class = "py-0",
                           numericInput(
                             inputId = "gacor",
                             label = NULL,
                             value = 0.60,
                             min = -1,
                             max = 1,
                             step = .01
                           )
                         )
                       ),
                       tags$tr(
                         tags$td("Specific"),
                         tags$td(""),
                         tags$td(
                           class = "py-0",
                           numericInput(
                             inputId = "sacor",
                             label = NULL,
                             value = 0.60,
                             min = -1,
                             max = 1,
                             step = .01
                           )
                         )
                       )
                       ),
            # tags$table(
            #     class = "table-condensed",
            #     tags$thead(
            #         tags$tr(
            #             style = "border-bottom: 0.5px solid gray;",
            #             tags$td(class = "w-75", "Outcome"),
            #             tags$td(class = "w-25 text-center", "Probability")
            #         )
            #     ),
            #     tags$tr(
            #         style = "border-bottom: 0.5px solid gray; background-color:#E5E0EB",
            #         tags$td(htmlOutput("ppv_label")),
            #         tags$td(htmlOutput("ppv"), class = "text-center")
            #     ),
            #     tags$tr(
            #         style = "border-bottom: 0.5px solid gray; background-color:#E4F6EB",
            #         tags$td(htmlOutput("pbuffer_label")),
            #         tags$td(htmlOutput("pbuffer"), class = "text-center")
            #     )
            # ),
            p(),
            splitLayout(
                cellWidths = split_width,
                numericInput(
                    "threshold",
                    "Threshold",
                    min = 40,
                    max = 160,
                    value = 85
                ),
                numericInput(
                    "buffer",
                    "Buffer",
                    min = 0,
                    max = 15,
                    value = 5
                )
            )

        ),

        # Show a plot of the generated distribution
        mainPanel(
          width = 8,
          tabsetPanel(type = "tabs",
                      tabPanel("Plot",
                               plotOutput("distPlot")),
                      tabPanel(
                        "Calculations",
                        p(em("Assumptions:"), "All observed and true scores are multivariate normal."),
                        withMathJax(
                          p(r"(Suppose that observed scores \(G\), \(S\), and \(A\) have reliability coefficients of \(r_{GG}\), \(r_{SS}\), and \(r_{AA}\), respectively. Their respective true scores are \(g\), \(s\), and \(a\). We can represent the three observed scores as a vector \(X=\left\{G,S,A\right\}\), the true scores as vector \(T=\left\{g,s,a\right\}\), and the true score differences as \(D=\left\{g-s,g-a\right\}\). The covariance matrix of the observed scores is:)"),
                          p(r"($$\mathbf{\Sigma}_X=\left[
\begin{matrix}\sigma_G^2&\sigma_{GS}&\sigma_{GA}\\
	\sigma_{GS}&\sigma_S^2&\sigma_{SA}\\
	\sigma_{GA}&\sigma_{SA}&\sigma_A^2\\
\end{matrix}\right]$$)"),
p("The covariance matrix of the true scores is the same as that of the observed score covariance matrix except that the true score variances are equal to the observed variances multiplied by the appropriate reliability coefficient:"),
p(r"($$\mathbf{\Sigma}_T=
  \begin{bmatrix}
  r_{GG}\sigma_G^2 & \sigma_{GS} & \sigma_{GA}\\
  \sigma_{GS} & r_{SS}\sigma_S^2 & \sigma_{SA}\\
  \sigma_{GA} & \sigma_{SA} & r_{AA}\sigma_A^2
  \end{bmatrix}$$)"),
p(r"(Because the matrix of covariances between the observed and true scores is the same as the true score covariance matrix, the combined covariance matrix is:)"),
p(r"($$\mathbf{\Sigma}_{XT}=\left[\begin{matrix}\mathbf{\Sigma}_X&\mathbf{\Sigma}_T\\
  \mathbf{\Sigma}_T&\mathbf{\Sigma}_T\\
  \end{matrix}\right]$$)"),
p(r"(To create the covariance matrix that includes the variances and covariances of true score differences \(g-s\) and \(g-a\), we create a weight matrix \(\boldsymbol{W}\):)"),
p(r"($$\mathbf{W}=\begin{bmatrix}
  1&0&0&0&0&0&0&0\\
  0&1&0&0&0&0&0&0\\
  0&0&1&0&0&0&0&0\\
  0&0&0&1&0&0&1&1\\
  0&0&0&0&1&0&-1&0\\
  0&0&0&0&0&1&0&-1\\
  \end{bmatrix}$$)"),
p(r"(The covariance matrix of all observed scores in vector \(X\), true scores in vector \(T\), and true score differences in vector \(D\) is calculated like so:)"),
p(r"($$\mathbf{\Sigma}_{XTD}=\mathbf{W}^\prime\mathbf{\Sigma}_{XT}\mathbf{W}$$)"),
p(r"(From the \(\mathbf{\Sigma}_{XTD}\) matrix, we can select just the portion of pertaining to the true scores and the true score differences in a covariance matrix we can call \(\mathbf{\Sigma}_{TD}\).)"),
p(r"(Using equations for computing conditional distributions, the means of the true scores and true score differences after controlling for specific observed scores are:)"),
p(r"($$\mu_{TD|X}=\mu_X+\mathbf{\Sigma}_{TD}\mathbf{\Sigma}_X^{-1}\left(X-\mu_X\right)$$)"),
p(r"(Likewise, the covariance matrix of the true scores and true score differences after controlling for specific observed scores is:)"),
p(r"($$\mathbf{\Sigma}_{TD|X}=\mathbf{\Sigma}_{TD}-\mathbf{\Sigma}_{TD}\mathbf{\Sigma}_X^{-1}\mathbf{\Sigma}_{TD}^\prime$$)"),
p(r"(To calculate the multivariate conditional PPV, we specify a multivariate normal distribution for the conditional true scores: \(\mathcal{N}\left(\mu_{TD|X},\ \mathbf{\Sigma}_{TD|X}\right)\). Then we evaluate the cumulative distribution function of the multivariate normal distribution with lower bounds at the diagnostic thresholds for \(G\), \(G-S\), and \(G-A\) and upper bounds at the diagnostic thresholds for \(S\) and \(A\).)")
)))),
))

# Define server logic required to draw a histogram
server <- function(input, output) {
    # plotstuff <- shiny::reactive({



        # myplot(
        #     x = c(
        #         ifelse(
        #             is.numeric(input$generalAbility),
        #             input$generalAbility,
        #             100
        #         ),
        #         ifelse(
        #             is.numeric(input$specificAbility),
        #             input$specificAbility,
        #             100
        #         ),
        #         ifelse(
        #             is.numeric(input$academicAbility),
        #             input$academicAbility,
        #             100
        #         )
        #     ),
        #     threshold = input$threshold,
        #     rxx = c(
        #         ifelse(
        #             is.numeric(input$generalReliability),
        #             input$generalReliability,
        #             0
        #         ),
        #         ifelse(
        #             is.numeric(input$specificReliability),
        #             input$specificReliability,
        #             0
        #         ),
        #         ifelse(
        #             is.numeric(input$academicReliability),
        #             input$academicReliability,
        #             0
        #         )
        #     ),
        #     buffer = input$buffer
        # )
    # })

    # output$ppv_label = renderText(plotstuff()[["ppv_label"]])
    # output$pbuffer_label <-
    #     renderText(plotstuff()[["pbuffer_label"]])

    # output$ppv <- renderText(plotstuff()[["ppv"]])
    # output$pbuffer <- renderText(plotstuff()[["pbuffer"]])

    output$distPlot <- renderPlot({
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
    }, height = 800)
}
ggplot2::theme_set(ggplot2::theme_minimal(base_size = 18))
thematic_shiny(font = font_spec(families = "Roboto Condensed", install = T, update = T))

# Run the application
shinyApp(ui = ui, server = server)
