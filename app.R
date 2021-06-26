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
mycolors <- c(SLD = "#3B528B",
              Buffer = "#21908C",
              NotSLD = "#5DC863")
mycolours <- mycolors


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
                '">SLD-consistent</span> side of the threshold of ',
                threshold
            ),
            ppv = proportion_round(conditional_ppv, 2) %>%
                remove_leading_zero(2),
            pbuffer_label = paste0(
                'No true score is on the ',
                '<span style="font-weight: bold; color:',
                scales::alpha(mycolors["NotSLD"], 0.7),
                '">SLD-inconsistent</span> side of the ',
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
            tags$table(
                class = "table-condensed",
                tags$thead(
                    tags$tr(
                        style = "border-bottom: 0.5px solid gray;",
                        tags$td(class = "w-75", "Outcome"),
                        tags$td(class = "w-25 text-center", "Probability")
                    )
                ),
                tags$tr(
                    style = "border-bottom: 0.5px solid gray;",
                    tags$td(htmlOutput("ppv_label")),
                    tags$td(htmlOutput("ppv"), class = "text-center")
                ),
                tags$tr(
                    style = "border-bottom: 0.5px solid gray;",
                    tags$td(htmlOutput("pbuffer_label")),
                    tags$td(htmlOutput("pbuffer"), class = "text-center")
                )
            ),
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
            ),
            p(em("Assumptions:"), "All observed and true scores are multivariate normal with uncorrelated errors.")
        ),

        # Show a plot of the generated distribution
        mainPanel(width = 8,
                  plotOutput("distPlot"))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    plotstuff <- shiny::reactive({
        myplot(
            x = c(
                ifelse(
                    is.numeric(input$generalAbility),
                    input$generalAbility,
                    100
                ),
                ifelse(
                    is.numeric(input$specificAbility),
                    input$specificAbility,
                    100
                ),
                ifelse(
                    is.numeric(input$academicAbility),
                    input$academicAbility,
                    100
                )
            ),
            threshold = input$threshold,
            rxx = c(
                ifelse(
                    is.numeric(input$generalReliability),
                    input$generalReliability,
                    0
                ),
                ifelse(
                    is.numeric(input$specificReliability),
                    input$specificReliability,
                    0
                ),
                ifelse(
                    is.numeric(input$academicReliability),
                    input$academicReliability,
                    0
                )
            ),
            buffer = input$buffer
        )
    })

    output$ppv_label = renderText(plotstuff()[["ppv_label"]])
    output$pbuffer_label <-
        renderText(plotstuff()[["pbuffer_label"]])

    output$ppv <- renderText(plotstuff()[["ppv"]])
    output$pbuffer <- renderText(plotstuff()[["pbuffer"]])

    output$distPlot <- renderPlot({
        plotstuff()["plot"]
    }, height = 570)
}
ggplot2::theme_set(ggplot2::theme_minimal(base_size = 18))
thematic_shiny(font = font_spec(families = "Roboto Condensed", install = T, update = T))

# Run the application
shinyApp(ui = ui, server = server)
