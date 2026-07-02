# ==========================================================================
# Exploratory Analysis — circadian / wearable data
# Tab 1: simple histogram        (Thought 1)
# Tab 2: multi-graph playground  (Thought 1 - graphs to try)
# Styled with the author's dashboard theme.  Run: shiny::runApp()
# ==========================================================================

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(nlme)

# ---- data ----------------------------------------------------------------
d <- read.csv("data.csv", check.names = TRUE, stringsAsFactors = FALSE)
d$X <- NULL

# numeric columns available to plot
num_vars <- names(d)[sapply(d, is.numeric)]
num_vars <- setdiff(num_vars, c("date"))

# categorical columns to colour / split by
d$is_weekend <- ifelse(d$is_weekend %in% c("True", "TRUE", TRUE), "Weekend", "Weekday")
group_vars <- c("None" = "none", "Sex" = "sex", "Device" = "device", "Weekend" = "is_weekend")

bg <- function(p) p %>% layout(plot_bgcolor = "#ECF0F5", paper_bgcolor = "#ECF0F5",
                               legend = list(title = list(text = "")))

# ---- Thought 1 helpers ---------------------------------------------------
# circular mean of a vector of hours (acrophase is a time-of-day, 0-24)
circ_mean <- function(h) {
  h <- h[is.finite(h)]; if (!length(h)) return(NA_real_)
  a <- h / 24 * 2 * pi
  ((atan2(mean(sin(a)), mean(cos(a)))) %% (2 * pi)) / (2 * pi) * 24
}

# per-subject weekday / weekend mean acrophase (computed once)
subj_wk <- d %>%
  group_by(subject_id, age, sex) %>%
  summarise(
    weekday = circ_mean(circadian_acrophase[is_weekend == "Weekday"]),
    weekend = circ_mean(circadian_acrophase[is_weekend == "Weekend"]),
    n_weekend = sum(is_weekend == "Weekend"),
    acro_sd = sd(circadian_acrophase), .groups = "drop") %>%
  filter(n_weekend >= 2, is.finite(weekday), is.finite(weekend)) %>%
  mutate(shift = weekend - weekday)   # + = later on weekends (social jetlag)

# age x weekend interaction model (continuous age; fit once)
d$sid <- factor(d$subject_id)
t1_model <- lme(circadian_acrophase ~ age * is_weekend, random = ~1 | sid,
                data = d, method = "ML")
t1_tab <- summary(t1_model)$tTable

# ---- UI ------------------------------------------------------------------
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = "Exploratory Analysis",
    titleWidth = 300,
    dropdownMenu(
      type = "notifications",
      headerText = "Application developed by",
      notificationItem(
        text = tags$div("Antônio Oss Boll",
                        style = "display: inline-block; vertical-align: middle;"),
        icon = icon("id-card"), status = "success"
      )
    )
  ),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Histogram", tabName = "hist_tab",   icon = icon("chart-column")),
      menuItem("Graphs",    tabName = "graphs_tab", icon = icon("chart-line")),
      menuItem("Thought 1: Age & rest", tabName = "thought1", icon = icon("bed"))
    )
  ),

  dashboardBody(
    tabItems(

      # ---- Tab 1: simple histogram ------------------------------------
      tabItem("hist_tab",
        fluidRow(
          box(width = 12, background = "olive",
            p(style = "font-size:16px;text-align:justify",
              "Exploratory app for the circadian-rhythm (wearable) dataset. ",
              "Each row is one participant-day. Pick a variable to see its distribution.")
          )
        ),
        fluidRow(
          box(width = 4, status = "primary", solidHeader = TRUE, title = "Filters",
            selectInput("var", "Variable:", choices = num_vars,
                        selected = "circadian_acrophase"),
            sliderInput("bins", "Number of bins:", min = 5, max = 80, value = 30),
            selectInput("group", "Colour by:", choices = group_vars)
          ),
          box(width = 8, status = "primary", solidHeader = TRUE, title = "Histogram",
            plotlyOutput("hist", height = "420px")
          )
        ),
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE, title = "Summary",
            verbatimTextOutput("summary1")
          )
        )
      ),

      # ---- Tab 2: multi-graph playground ------------------------------
      tabItem("graphs_tab",
        fluidRow(
          box(width = 12, background = "olive",
            p(style = "font-size:16px;text-align:justify",
              "Graph playground: pick a graph type and variables to visualize the ",
              "data in different ways (histogram, density, boxplot, scatter).")
          )
        ),
        fluidRow(
          box(width = 4, status = "primary", solidHeader = TRUE, title = "Controls",
            radioButtons("ptype", "Graph type:",
              choices = c("Histogram" = "hist", "Density" = "dens",
                          "Boxplot" = "box", "Scatter" = "scatter")),
            selectInput("var2", "Variable (X):", choices = num_vars,
                        selected = "circadian_acrophase"),
            conditionalPanel("input.ptype == 'scatter'",
              selectInput("yvar", "Variable (Y):", choices = num_vars, selected = "age")),
            conditionalPanel("input.ptype == 'hist'",
              sliderInput("bins2", "Number of bins:", min = 5, max = 80, value = 30)),
            selectInput("group2", "Colour by:", choices = group_vars)
          ),
          box(width = 8, status = "primary", solidHeader = TRUE, title = "Graph",
            plotlyOutput("plot", height = "440px")
          )
        ),
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE, title = "Summary",
            verbatimTextOutput("summary2")
          )
        )
      ),

      # ---- Tab 3: Thought 1 — Age & days off --------------------------
      tabItem("thought1",
        fluidRow(
          box(width = 12, background = "olive",
            h4(tags$b("Thought 1 — Age and days off")),
            p(style = "font-size:15px;text-align:justify",
              "Hypothesis: age affects the circadian rhythm, and having days off from ",
              "work can restabilize it. Working people should therefore drift on their ",
              "days off (a weekday-to-weekend shift in acrophase), while retired people ",
              "(65+), who have no work schedule, should show little to no weekend shift. ",
              "Use the slider to set the retirement age and compare the two groups.")
          )
        ),
        fluidRow(
          box(width = 4, status = "primary", solidHeader = TRUE, title = "Controls",
            sliderInput("ret_age", "Retirement age (group split):",
                        min = 40, max = 75, value = 65, step = 5),
            helpText("Splits subjects into 'Working' (below) and 'Retired' (at/above). ",
                     "Acrophase = time-of-day of the heart-rate rhythm peak."),
            actionButton("run_person", "Run per-person age test",
                         icon = icon("play"), class = "btn-primary"),
            helpText("Per-person version of the age test: regress each person's own ",
                     "weekend shift on their age (n = people, not days).")
          ),
          valueBoxOutput("t1_vb_work",  width = 4),
          valueBoxOutput("t1_vb_ret",   width = 4)
        ),
        fluidRow(
          box(width = 6, status = "primary", solidHeader = TRUE,
              title = "Weekday vs weekend acrophase, by group",
              plotlyOutput("t1_box", height = 360)),
          box(width = 6, status = "primary", solidHeader = TRUE,
              title = "Weekend - weekday shift vs age (per subject)",
              plotlyOutput("t1_shift", height = 360))
        ),
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
              title = "Weekend - weekday shift, by group (isolates the difference)",
              p(style = "color:#555",
                "Each dot is one person's own weekend-minus-weekday acrophase. This ",
                "removes the large between-person spread, so the group difference is ",
                "easier to see than in the absolute boxplot above. Above 0 = later on ",
                "weekends; on 0 = no shift."),
              plotlyOutput("t1_shiftbox", height = 340))
        ),
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
              title = "Does the weekend shift depend on age? (mixed model)",
              verbatimTextOutput("t1_stats"))
        ),
        fluidRow(
          box(width = 12, status = "warning", solidHeader = TRUE,
              title = "Per-person version: does the weekend shift depend on age? (click the button)",
              plotlyOutput("t1_person_plot", height = 320),
              verbatimTextOutput("t1_person_test"))
        )
      )
    )
  )
)

# ---- SERVER --------------------------------------------------------------
server <- function(input, output) {

  # ----- Tab 1: histogram -----
  output$hist <- renderPlotly({
    g <- if (input$group == "none") NULL else input$group
    p <- ggplot(d, aes(x = .data[[input$var]]))
    p <- if (is.null(g)) p + geom_histogram(bins = input$bins, fill = "#3182bd", colour = "white")
         else p + geom_histogram(aes(fill = .data[[g]]), bins = input$bins,
                                  colour = "white", position = "identity", alpha = 0.6)
    bg(ggplotly(p + labs(x = input$var, y = "Count", fill = NULL) +
                theme_minimal(base_size = 13)))
  })
  output$summary1 <- renderPrint(summary(d[[input$var]]))

  # ----- Tab 2: playground -----
  output$plot <- renderPlotly({
    x <- input$var2; g <- if (input$group2 == "none") NULL else input$group2

    if (input$ptype == "hist") {
      p <- ggplot(d, aes(x = .data[[x]]))
      p <- if (is.null(g)) p + geom_histogram(bins = input$bins2, fill = "#3182bd", colour = "white")
           else p + geom_histogram(aes(fill = .data[[g]]), bins = input$bins2,
                                    colour = "white", position = "identity", alpha = 0.6)
      p <- p + labs(x = x, y = "Count", fill = NULL)

    } else if (input$ptype == "dens") {
      p <- ggplot(d, aes(x = .data[[x]]))
      p <- if (is.null(g)) p + geom_density(fill = "#3182bd", alpha = 0.5)
           else p + geom_density(aes(fill = .data[[g]], colour = .data[[g]]), alpha = 0.4)
      p <- p + labs(x = x, y = "Density", fill = NULL, colour = NULL)

    } else if (input$ptype == "box") {
      p <- ggplot(d, aes(x = if (is.null(g)) factor("all") else .data[[g]], y = .data[[x]]))
      p <- if (is.null(g)) p + geom_boxplot(fill = "#3182bd", alpha = 0.6)
           else p + geom_boxplot(aes(fill = .data[[g]]), alpha = 0.6)
      p <- p + labs(x = NULL, y = x, fill = NULL)

    } else {  # scatter
      y <- input$yvar
      p <- ggplot(d, aes(x = .data[[x]], y = .data[[y]]))
      p <- if (is.null(g)) p + geom_point(colour = "#3182bd", alpha = 0.5)
           else p + geom_point(aes(colour = .data[[g]]), alpha = 0.55)
      p <- p + labs(x = x, y = y, colour = NULL)
    }

    bg(ggplotly(p + theme_minimal(base_size = 13)))
  })

  output$summary2 <- renderPrint({
    if (input$ptype == "scatter") {
      cat("X =", input$var2, "\n"); print(summary(d[[input$var2]]))
      cat("\nY =", input$yvar, "\n"); print(summary(d[[input$yvar]]))
      cat(sprintf("\nPearson correlation r = %.3f",
                  suppressWarnings(cor(d[[input$var2]], d[[input$yvar]], use = "complete.obs"))))
    } else {
      summary(d[[input$var2]])
    }
  })

  # ----- Tab 3: Thought 1 — Age & days off -----
  t1_data <- reactive({
    subj_wk %>% mutate(
      grp = factor(ifelse(age >= input$ret_age,
                          paste0("Retired (", input$ret_age, "+)"),
                          paste0("Working (<", input$ret_age, ")")),
                   levels = c(paste0("Working (<", input$ret_age, ")"),
                              paste0("Retired (", input$ret_age, "+)"))))
  })
  fmt_h <- function(h) sprintf("%+.2f h", h)

  output$t1_vb_work <- renderValueBox({
    s <- t1_data() %>% filter(grepl("^Working", grp))
    valueBox(fmt_h(mean(s$shift)),
             sprintf("Working: avg weekend shift (n=%d)", nrow(s)),
             icon = icon("briefcase"), color = "aqua")
  })
  output$t1_vb_ret <- renderValueBox({
    s <- t1_data() %>% filter(grepl("^Retired", grp))
    valueBox(if (nrow(s)) fmt_h(mean(s$shift)) else "-",
             sprintf("Retired: avg weekend shift (n=%d)", nrow(s)),
             icon = icon("bed"), color = "light-blue")
  })

  # weekday vs weekend acrophase (one point per subject per day-type), by group
  output$t1_box <- renderPlotly({
    long <- t1_data() %>%
      tidyr::pivot_longer(c(weekday, weekend), names_to = "daytype", values_to = "acro") %>%
      mutate(daytype = factor(ifelse(daytype == "weekday", "Weekday", "Weekend"),
                              levels = c("Weekday", "Weekend")))
    p <- ggplot(long, aes(x = grp, y = acro, fill = daytype)) +
      geom_boxplot(alpha = 0.7, outlier.size = 0.6) +
      labs(x = NULL, y = "Mean acrophase [h]", fill = NULL) +
      theme_minimal(base_size = 13)
    bg(ggplotly(p))
  })

  # per-subject weekend-weekday shift vs age, with trend
  output$t1_shift <- renderPlotly({
    df <- t1_data()
    p <- ggplot(df, aes(x = age, y = shift)) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
      geom_point(aes(colour = grp), alpha = 0.7, size = 2) +
      geom_smooth(method = "lm", se = FALSE, colour = "black", linewidth = 0.7) +
      geom_vline(xintercept = input$ret_age, linetype = "dotted", colour = "red") +
      labs(x = "Age (years)", y = "Weekend - weekday shift [h]", colour = NULL) +
      theme_minimal(base_size = 13)
    bg(ggplotly(p))
  })

  # per-subject weekend-weekday shift, by group (isolates the difference)
  output$t1_shiftbox <- renderPlotly({
    df <- t1_data()
    p <- ggplot(df, aes(x = grp, y = shift, fill = grp)) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
      geom_boxplot(alpha = 0.55, width = 0.5, outlier.shape = NA) +
      geom_jitter(width = 0.12, alpha = 0.5, size = 1.4) +
      labs(x = NULL, y = "Weekend - weekday shift [h]", fill = NULL) +
      theme_minimal(base_size = 13) + theme(legend.position = "none")
    bg(ggplotly(p))
  })

  # ----- Per-person version of the age test (runs only on button click) -----
  # graph: each person's weekend shift vs age, with a regression line
  output$t1_person_plot <- renderPlotly({
    if (input$run_person == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
        layout(title = list(text = "Click 'Run per-person age test'", font = list(size = 14)),
               plot_bgcolor = "#ECF0F5", paper_bgcolor = "#ECF0F5"))
    }
    df <- subj_wk %>% mutate(grp = ifelse(age >= input$ret_age, "Retired", "Working"))
    p <- ggplot(df, aes(x = age, y = shift)) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
      geom_point(aes(colour = grp), alpha = 0.7, size = 2) +
      geom_smooth(method = "lm", colour = "black", fill = "grey75", linewidth = 0.8) +
      geom_vline(xintercept = input$ret_age, linetype = "dotted", colour = "red") +
      labs(x = "Age (years)", y = "Weekend - weekday shift [h]  (per person)",
           colour = NULL, title = "Per-person weekend shift vs age") +
      theme_minimal(base_size = 13)
    bg(ggplotly(p))
  })

  # test: lm(shift ~ age) across people = per-person analog of the interaction
  output$t1_person_test <- renderPrint({
    if (input$run_person == 0) {
      cat("Click 'Run per-person age test' to fit lm(weekend_shift ~ age) across the\n",
          nrow(subj_wk), "people - the per-person version of 'does the weekend shift depend on age?'")
      return(invisible())
    }
    df <- subj_wk
    m <- lm(shift ~ age, df); cf <- summary(m)$coefficients["age", ]; ci <- confint(m)["age", ]
    cat("PER-PERSON: does the weekend shift depend on age?\n")
    cat("Model: lm(weekend_shift ~ age),  n =", nrow(df), "people\n")
    cat("--------------------------------------------------------------------\n")
    cat(sprintf("age slope = %+.4f h/yr   95%% CI [%.4f, %.4f]   p = %.4f\n",
                cf["Estimate"], ci[1], ci[2], cf["Pr(>|t|)"]))
    cat(sprintf("   -> per extra year of age, the weekend shift changes by %+.1f min.\n",
                cf["Estimate"] * 60))
    tt <- t.test(df$weekend, df$weekday, paired = TRUE)
    cat(sprintf("\nOverall weekend shift (paired t-test): %+.3f h, p = %.3f\n",
                tt$estimate, tt$p.value))
    cat(sprintf("\nCompare the mixed model above (all days): age x weekend interaction p = %.4f\n",
                t1_tab["age:is_weekendWeekend", "p-value"]))
    cat("Same question, per person (fewer numbers) vs per day (more power).\n")
  })

  output$t1_stats <- renderPrint({
    ia <- t1_tab["age:is_weekendWeekend", ]
    ag <- t1_tab["age", ]
    we <- t1_tab["is_weekendWeekend", ]
    cat("Mixed model:  acrophase ~ age * weekend + (1|subject)\n")
    cat("--------------------------------------------------------\n")
    cat(sprintf("age                : %+.4f h/yr   p = %.4f   (older -> earlier peak)\n", ag["Value"], ag["p-value"]))
    cat(sprintf("weekend            : %+.4f h      p = %.4f\n", we["Value"], we["p-value"]))
    cat(sprintf("age x weekend      : %+.4f h/yr   p = %.4f   <- shrinking weekend shift with age\n", ia["Value"], ia["p-value"]))
    cat("\nInterpretation: a negative age x weekend term means the weekday->weekend\n")
    cat("shift gets smaller (or reverses) as age increases -- consistent with the\n")
    cat("idea that retired people, with no work schedule, drift less on days off.\n")
    grp_now <- t1_data() %>% group_by(grp) %>%
      summarise(mean_shift = round(mean(shift), 2), n = n(), .groups = "drop")
    cat("\nCurrent split:\n"); print(as.data.frame(grp_now), row.names = FALSE)
  })
}

shinyApp(ui, server)
