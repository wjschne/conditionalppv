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
library(gfonts)
library(bslib)
library(Cairo)
options(shiny.usecairo = T, scipen = 999)
use_font("roboto-condensed", "www/css/roboto-condensed.css")


remove_leading_zero <- function(x, digits = 2, ...) {
    x <- formatC(x, digits, format = "f", ...)
    sub("^-0", "-", sub("^0", "", x))
}

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

# Makes geom_text size the same as the base size (scaled to the specificed ratio)
ggtext_size <- function(base_size, ratio = 0.8) {
    ratio * base_size / 2.845276
}

mycolors <- c(SLD = "#3B528B",
              Buffer = "#21908C",
              NotSLD = "#5DC863")
mycolours <- mycolors


myplot <-
    function(x,
             rxx = rep(0.95, length(x)),
             threshold = 85,
             mu = 100,
             sigma = 15,
             buffer = 5) {


        d_rect <- tibble(
            x = c(40,
                  40,
                  threshold + buffer,
                  threshold + buffer,
                  threshold,
                  40,
                  threshold + buffer),
            y = c(1, 3, 3, 1, 1, 4, 4) - 0.2,
            xmax = c(threshold,
                     threshold,
                     160,
                     160,
                     threshold + buffer,
                     threshold,
                     160),
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

        # Standard error of the estimate
        see <- 15 * sqrt(rxx - rxx ^ 2)
        # Conditional mean
        mu <- (x - 100) * rxx + 100

        # P(x < threshold)
        p_threshold <- pnorm(threshold, mu, see)

        # P(x < buffer)
        p_buffer <- pnorm(threshold + buffer, mu, see)

        # conditional PPV
        conditional_ppv <- (1 - p_buffer[1]) *
            p_threshold[2] *
            p_threshold[3]

        # P(x) is not inconsistent with SLD
        p_not_inconsistent <- (1 - p_threshold[1]) *
            p_buffer[2] *
            p_buffer[3]

        pp_buffer_label <- pnorm(threshold + buffer, mu, see)
        pp_buffer_label[1] <- 1 - pp_buffer_label[1]

        d <- tribble(
            ~ dist,
            ~ args,
            ~ Ability,
            "norm",
            list(100, 15),
            "Population",
            "norm",
            list(mu[1], see[1]),
            "General\nAbility",
            "norm",
            list(mu[2], see[2]),
            "Specific\nAbility",
            "norm",
            list(mu[3], see[3]),
            "Academic\nAbility"
        ) %>%
            mutate(rxx_display = ifelse(
                Ability == "Population",
                "",
                paste0("italic(r[xx]) == '",
                       c("", remove_leading_zero(rxx)),
                       "'")
            )) %>%
            mutate(
                Score = c(100, x),
                sd = c(15, see),
                p = paste0(
                    "P(*X* < ",
                    threshold,
                    ") = ",
                    number(pnorm(threshold, Score, sd),
                           0.01) %>%
                        str_remove("^0")
                ),
                ScoreDisplay = ifelse(Ability == "Population",
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
                p_between_color = case_when(Ability == "Population" ~ "gray50",
                                            TRUE ~ mycolors["Buffer"])
            )


        myplot <-
            ggplot(d, aes(y = Ability, dist = dist, args = args)) +
            geom_richtext(
                aes(
                    label = p,
                    x = threshold - 1,
                    y = as.numeric(Ability),
                    color = p_less_color
                ),
                vjust = 1.6,
                hjust = 1,
                size = ggtext_size(16),
                label.padding = unit(0, "mm"),
                family = "Roboto Condensed",
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
                vjust = 1.6,
                hjust = 0,
                size = ggtext_size(16),
                label.padding = unit(0, "mm"),
                family = "Roboto Condensed",
                show.legend = F,
                label.colour = NA
            ) +
            geom_rect(data = d_rect, aes(xmin = x, ymin = y, xmax = xmax, ymax = ymax, fill = fill), inherit.aes = F) +
            stat_dist_halfeye(
                normalize = "groups",
                aes(fill = stat(
                    case_when(
                        y == 4 & x < threshold ~ "gray30",
                        y == 4 & x < threshold + buffer ~ mycolors["Buffer"],
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
                family = "Roboto Condensed",
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
            theme_minimal(base_family = "Roboto Condensed", base_size = 18) +
        theme(
            plot.title.position = "plot",
            plot.title = element_markdown(size = 16, color = "gray30"),
            plot.subtitle = element_markdown(size = 16, color = "gray30"),
            legend.position = c(0.97, 1.08),
            axis.text.y = element_text(vjust = 0, hjust = 0.5),
            legend.justification = c(0.95, 1),
            legend.text = element_text(color = "white"),
            legend.background = element_blank(), axis.text.y.left = element_text(vjust = 0.5)
        ) +
            annotate(
                x = mean(c(threshold, threshold + buffer)),
                y = 4,
                vjust = 0.5,
                hjust = -0.1,
                label = paste0("Buffer"),
                geom = "text",
                angle = 90,
                size = ggtext_size(16),
                family = "Roboto Condensed"
            ) +
            annotate(
                x = threshold + buffer + 1,
                y = 4,
                label = "Not Weakness",
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
                label = "Weakness",
                geom = "text",
                angle = 0,
                hjust = 1,
                vjust = -0.5,
                size = ggtext_size(16),
                family = "Roboto Condensed"
            )

        list(
            plot = myplot,
            ppv = paste0(
                'Probability all scores are on the <span style="font-weight: bold; color:',
                scales::alpha(mycolors["SLD"], 0.7),
                '">SLD-consistent</span> side of the threshold of ',
                threshold,
                " = ",
                proportion_round(conditional_ppv, 2) %>% str_remove("^0")
            ),
            pbuffer = paste0(
                'Probability no score is on the <span style="font-weight: bold; color:',
                scales::alpha(mycolors["NotSLD"], 0.7),
                '">SLD-inconsistent</span> side of the <span style="font-weight: bold; color:',
                scales::alpha(mycolors["Buffer"], 0.7),
                '">buffer zone</span> (',
                threshold,
                "–",
                threshold + buffer,
                ") = ",
                proportion_round(p_not_inconsistent, 2) %>% str_remove("^0")
            )
        )
    }


# layout widths
split_width <- c("70%", "30%")


# Define UI ----
ui <- fluidPage(
    tags$style(HTML("
        input[type=number] {
              -moz-appearance:textfield;
        }
        input[type=number]::{
              -moz-appearance:textfield;
        }
        input[type=number]::-webkit-outer-spin-button,
        input[type=number]::-webkit-inner-spin-button {
              -webkit-appearance: none;
              margin: 0;
        }
        #sidebar {max-width: 320px;}
    ")),
    theme = bs_theme(
        base_font = "Roboto Condensed",
        primary = scales::alpha(mycolors[1], alpha = 0.7)
    ),
    titlePanel("Decision Errors in SLD Identification"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(

        sidebarPanel(id = "sidebar", width = 4,
            "Set decision threshold, buffer width, standard scores, and reliability coefficients.",hr(),
            splitLayout(cellWidths = split_width,
                        numericInput(
                "threshold",
                "Threshold",
                min = 40,
                max = 160,
                value = 85
            ),
            numericInput("buffer", "Buffer", min = 0, max = 15, value = 5)),
            splitLayout(cellWidths = split_width,
                        numericInput(
                "generalAbility",
                "General Ability",
                min = 40,
                max = 160,
                value = 100
            ),
                numericInput(
                    inputId = "generalReliability",
                    label = em(HTML("r<sub>xx</sub>")),
                    value = 0.97,
                    min = .6,
                    max = .99999,
                    step = .01
                )),
            splitLayout(cellWidths = split_width,
                        numericInput(
                    "specificAbility",
                    "Specific Ability",
                    min = 40,
                    max = 160,
                    value = 75
                ),
                    numericInput(
                        inputId = "specificReliability",
                        label = em(HTML("r<sub>xx</sub>")),
                        value = 0.92,
                        min = .6,
                        max = .99999,
                        step = .01
                    )
                ),
            splitLayout(cellWidths = split_width,
                        numericInput(
                    "academicAbility",
                    "Academic Ability",
                    min = 40,
                    max = 160,
                    value = 81
                ),
                    numericInput(
                        inputId = "academicReliability",
                        label = em(HTML("r<sub>xx</sub>")),
                        value = 0.92,
                        min = .6,
                        max = .99999,
                        step = .01
                    )
                ),
            "Assumptions: Multivariate normality and uncorrelated errors"


        ),

        # Show a plot of the generated distribution
        mainPanel(width = 8,
            htmlOutput("ppv"),
            htmlOutput("pbuffer"),
            plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    plotstuff <- shiny::reactive({
        myplot(
            x = c(
                input$generalAbility,
                input$specificAbility,
                input$academicAbility
            ),
            threshold = input$threshold,
            rxx = c(
                input$generalReliability,
                input$specificReliability,
                input$academicReliability
            ),
            buffer = input$buffer
        )
    })

    output$ppv <- renderText(plotstuff()[["ppv"]])
    output$pbuffer <- renderText(plotstuff()[["pbuffer"]])

    output$distPlot <- renderPlot({
        plotstuff()["plot"]
    }, height = 500, width = 700)
}

# Run the application
shinyApp(ui = ui, server = server)
