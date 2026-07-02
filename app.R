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

      # ---- Tab 2: Thought 1 — Age & days off --------------------------
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
            radioButtons("t1_unit", "Analysis unit:",
              choices = c("All observations (all days)" = "obs",
                          "Per person (means)"          = "person"),
              selected = "obs"),
            sliderInput("ret_age", "Retirement age (group split):",
                        min = 40, max = 75, value = 65, step = 5),
            helpText("Every graph and result below updates when you switch the unit. ",
                     "'All observations' uses each day; 'Per person' collapses each ",
                     "person to their own means first.")
          ),
          valueBoxOutput("t1_vb_work",  width = 4),
          valueBoxOutput("t1_vb_ret",   width = 4)
        ),
        fluidRow(
          box(width = 6, status = "primary", solidHeader = TRUE,
              title = "Weekday vs weekend acrophase, by group",
              plotlyOutput("t1_box", height = 360)),
          box(width = 6, status = "primary", solidHeader = TRUE,
              title = "Weekend effect vs age",
              plotlyOutput("t1_shift", height = 360))
        ),
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
              title = "Weekend effect, by group",
              plotlyOutput("t1_shiftbox", height = 340))
        ),
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
              title = "Does the weekend shift depend on age?",
              verbatimTextOutput("t1_stats"))
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

  # ----- Tab 2: Thought 1 — Age & days off -----
  fmt_h    <- function(h) sprintf("%+.2f h", h)
  lvl_work <- reactive(paste0("Group 1 (<", input$ret_age, ")"))
  lvl_ret  <- reactive(paste0("Group 2 (", input$ret_age, "+)"))
  mk_grp   <- function(a) factor(ifelse(a >= input$ret_age, lvl_ret(), lvl_work()),
                                 levels = c(lvl_work(), lvl_ret()))
  is_person <- reactive(input$t1_unit == "person")

  t1_person <- reactive(subj_wk %>% mutate(grp = mk_grp(age)))
  t1_obs    <- reactive(d %>% mutate(grp = mk_grp(age)))

  # long weekday/weekend acrophase (age, grp, daytype, acro), for either unit
  t1_long <- reactive({
    if (is_person()) {
      t1_person() %>%
        tidyr::pivot_longer(c(weekday, weekend), names_to = "daytype", values_to = "acro") %>%
        mutate(daytype = factor(ifelse(daytype == "weekday", "Weekday", "Weekend"),
                                levels = c("Weekday", "Weekend"))) %>%
        select(age, grp, daytype, acro)
    } else {
      t1_obs() %>% transmute(age, grp,
        daytype = factor(is_weekend, levels = c("Weekday", "Weekend")),
        acro = circadian_acrophase)
    }
  })

  # per-group weekend-weekday shift + n, for either unit
  t1_grpshift <- reactive({
    if (is_person()) {
      t1_person() %>% group_by(grp, .drop = FALSE) %>%
        summarise(shift = mean(shift), n = n(), .groups = "drop")
    } else {
      t1_obs() %>% group_by(grp, .drop = FALSE) %>%
        summarise(shift = circ_mean(circadian_acrophase[is_weekend == "Weekend"]) -
                          circ_mean(circadian_acrophase[is_weekend == "Weekday"]),
                  n = n(), .groups = "drop")
    }
  })
  unit_word <- reactive(if (is_person()) "people" else "days")

  output$t1_vb_work <- renderValueBox({
    s <- t1_grpshift() %>% filter(grp == lvl_work())
    valueBox(if (nrow(s) && is.finite(s$shift)) fmt_h(s$shift) else "-",
             sprintf("Group 1: weekend shift (n=%d %s)", if (nrow(s)) s$n else 0, unit_word()),
             icon = icon("briefcase"), color = "aqua")
  })
  output$t1_vb_ret <- renderValueBox({
    s <- t1_grpshift() %>% filter(grp == lvl_ret())
    valueBox(if (nrow(s) && is.finite(s$shift)) fmt_h(s$shift) else "-",
             sprintf("Group 2: weekend shift (n=%d %s)", if (nrow(s)) s$n else 0, unit_word()),
             icon = icon("bed"), color = "light-blue")
  })

  # Graph 1: weekday vs weekend acrophase, by group (boxplot) — both units
  output$t1_box <- renderPlotly({
    ylab <- if (is_person()) "Mean acrophase [h]  (per person)" else "Acrophase [h]  (all days)"
    p <- ggplot(t1_long(), aes(x = grp, y = acro, fill = daytype)) +
      geom_boxplot(alpha = 0.7, outlier.size = 0.5) +
      labs(x = NULL, y = ylab, fill = NULL) + theme_minimal(base_size = 13)
    bg(ggplotly(p))
  })

  # Graph 2: weekend effect vs age — both units
  output$t1_shift <- renderPlotly({
    if (is_person()) {
      p <- ggplot(t1_person(), aes(x = age, y = shift)) +
        geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
        geom_point(aes(colour = grp), alpha = 0.7, size = 2) +
        geom_smooth(method = "lm", se = FALSE, colour = "black", linewidth = 0.7) +
        labs(x = "Age (years)", y = "Weekend - weekday shift [h]", colour = NULL)
    } else {
      p <- ggplot(t1_long(), aes(x = age, y = acro, colour = daytype)) +
        geom_point(alpha = 0.25, size = 1) +
        geom_smooth(method = "lm", se = FALSE, linewidth = 0.9) +
        labs(x = "Age (years)", y = "Acrophase [h]", colour = NULL)
    }
    p <- p + geom_vline(xintercept = input$ret_age, linetype = "dotted", colour = "red") +
      theme_minimal(base_size = 13)
    bg(ggplotly(p))
  })

  # Graph 3: weekend effect by group — both units
  output$t1_shiftbox <- renderPlotly({
    if (is_person()) {
      p <- ggplot(t1_person(), aes(x = grp, y = shift, fill = grp)) +
        geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
        geom_boxplot(alpha = 0.55, width = 0.5, outlier.shape = NA) +
        geom_jitter(width = 0.12, alpha = 0.5, size = 1.4) +
        labs(x = NULL, y = "Weekend - weekday shift [h]", fill = NULL) +
        theme_minimal(base_size = 13) + theme(legend.position = "none")
    } else {
      p <- ggplot(t1_long(), aes(x = grp, y = acro, fill = daytype)) +
        geom_boxplot(alpha = 0.7, outlier.size = 0.5) +
        labs(x = NULL, y = "Acrophase [h]  (all days)", fill = NULL) +
        theme_minimal(base_size = 13)
    }
    bg(ggplotly(p))
  })

  # Stats: matches the selected unit
  output$t1_stats <- renderPrint({
    if (is_person()) {
      df <- t1_person()
      m <- lm(shift ~ age, df); cf <- summary(m)$coefficients["age", ]; ci <- confint(m)["age", ]
      cat("PER PERSON  (n =", nrow(df), "people)  —  lm(weekend_shift ~ age)\n")
      cat("--------------------------------------------------------------------\n")
      cat(sprintf("age slope = %+.4f h/yr   95%% CI [%.4f, %.4f]   p = %.4f\n",
                  cf["Estimate"], ci[1], ci[2], cf["Pr(>|t|)"]))
      cat(sprintf("   -> weekend shift changes by %+.1f min per year of age\n", cf["Estimate"] * 60))
      tt <- t.test(df$weekend, df$weekday, paired = TRUE)
      cat(sprintf("overall weekend shift (paired t): %+.3f h   p = %.3f\n", tt$estimate, tt$p.value))
    } else {
      ag <- t1_tab["age", ]; we <- t1_tab["is_weekendWeekend", ]; ia <- t1_tab["age:is_weekendWeekend", ]
      cat("ALL OBSERVATIONS  (", nrow(d), " days)  —  mixed model\n", sep = "")
      cat("acrophase ~ age * weekend + (1|subject)\n")
      cat("--------------------------------------------------------------------\n")
      cat(sprintf("age            : %+.4f h/yr   p = %.4f   (older -> earlier peak)\n", ag["Value"], ag["p-value"]))
      cat(sprintf("weekend        : %+.4f h      p = %.4f\n", we["Value"], we["p-value"]))
      cat(sprintf("age x weekend  : %+.4f h/yr   p = %.4f   (shift shrinks with age)\n", ia["Value"], ia["p-value"]))
    }
    gs <- t1_grpshift()
    cat("\nCurrent split (", input$t1_unit, "):\n", sep = "")
    print(as.data.frame(gs %>% mutate(shift = round(shift, 3))), row.names = FALSE)
  })
}

shinyApp(ui, server)
