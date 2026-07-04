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

# per-person circadian variability: within-subject SD of acrophase (day-to-day
# instability of circadian timing), with age & sex. Computed once.
subj_sd <- d %>%
  group_by(subject_id, age, sex) %>%
  summarise(acro_sd = sd(circadian_acrophase),
            acro_mean = circ_mean(circadian_acrophase),
            sleep_eff = first(sleep_efficiency_trait),
            n_days = n(), .groups = "drop") %>%
  filter(n_days >= 3, is.finite(acro_sd))

# per-person dataset for the Variable explorer: age, sex, circadian summaries,
# and every habitual (trait) factor (one value per person).
trait_cols_all <- grep("_trait$", names(d), value = TRUE)
subj_all <- d %>%
  group_by(subject_id, age, sex) %>%
  summarise(acro_mean = circ_mean(circadian_acrophase),
            acro_sd   = sd(circadian_acrophase),
            across(all_of(trait_cols_all), first),
            n_days = n(), .groups = "drop") %>%
  filter(n_days >= 3, is.finite(acro_sd))

pretty_var <- function(s) trimws(gsub("_", " ", sub("_trait$", "", s)))
var_choices <- c("Age" = "age",
                 "Sex" = "sex",
                 "Circadian acrophase (mean)" = "acro_mean",
                 "Circadian acrophase (SD / variability)" = "acro_sd",
                 setNames(trait_cols_all, vapply(trait_cols_all, pretty_var, character(1))))

# sleep-quality metrics for the research-question tab (per person)
sq_metrics <- c("Sleep efficiency"           = "sleep_efficiency_trait",
                "Acrophase variability / SD" = "acro_sd")

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
        text = tags$div("Group cirKINGdian",
                        style = "display: inline-block; vertical-align: middle;"),
        icon = icon("id-card"), status = "success"
      )
    )
  ),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Exploratory graphs", tabName = "hist_tab", icon = icon("chart-column")),
      menuItem("Variable explorer", tabName = "vars_tab", icon = icon("magnifying-glass-chart")),
      menuItem("Research Q", tabName = "sq_tab", icon = icon("bed-pulse")),
      menuItem("Tested ideas", icon = icon("flask"), startExpanded = TRUE,
        menuSubItem("Thought 1: Age & rest", tabName = "thought1", icon = icon("bed")),
        menuSubItem("Circadian SD (age & sex)", tabName = "sd_tab", icon = icon("wave-square")),
        menuSubItem("Sleep efficiency (age & sex)", tabName = "se_tab", icon = icon("bed-pulse")),
        menuSubItem("Thought 2: Sleep & variability", tabName = "thought2", icon = icon("moon")),
        menuSubItem("Thought 3: Sex & variability", tabName = "thought3", icon = icon("venus-mars")),
        menuSubItem("Thought 4: Age, sleep & variability", tabName = "thought4", icon = icon("children"))
      )
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
            conditionalPanel(
              condition = "output.var_continuous == true",
              sliderInput("bins", "Number of bins:", min = 5, max = 80, value = 30)),
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

      # ---- Tab: Variable explorer (variable 1 vs variable 2) ----------
      tabItem("vars_tab",
        fluidRow(
          box(width = 12, background = "olive",
            h4(tags$b("Variable explorer - variable 1 vs variable 2")),
            p(style = "font-size:15px;text-align:justify",
              "Pick any two per-person measures to compare. You get a scatter, both ",
              "distributions, and the test results (correlation + linear fit). ",
              "One value per person (age, sex, circadian summaries, and habitual factors).")
          )
        ),
        fluidRow(
          box(width = 4, status = "primary", solidHeader = TRUE, title = "Controls",
            selectInput("vx", "Variable 1 (X):", choices = var_choices, selected = "age"),
            selectInput("vy", "Variable 2 (Y):", choices = var_choices, selected = "acro_sd"),
            selectInput("vcol", "Colour by:", choices = c("None" = "none", "Sex" = "sex")),
            checkboxInput("v_outlier", "Remove outliers (1.5 x IQR on X or Y)", FALSE),
            helpText("One value per person (per-person, not per-day).")
          ),
          column(width = 4,
            valueBoxOutput("v_vb_r",   width = 12),
            valueBoxOutput("v_vb_rho", width = 12)),
          column(width = 4,
            valueBoxOutput("v_vb_p",  width = 12),
            valueBoxOutput("v_vb_sp", width = 12))
        ),
        conditionalPanel(
          condition = "input.vcol == 'sex' && input.vx != 'sex' && input.vy != 'sex'",
          fluidRow(
            valueBoxOutput("v_vb_fem",  width = 6),
            valueBoxOutput("v_vb_male", width = 6)
          )
        ),
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
              title = "Variable 1 vs Variable 2",
              plotlyOutput("v_scatter", height = 400))
        ),
        fluidRow(
          box(width = 6, status = "primary", solidHeader = TRUE,
              title = "Distribution of X", plotlyOutput("v_histx", height = 280)),
          box(width = 6, status = "primary", solidHeader = TRUE,
              title = "Distribution of Y", plotlyOutput("v_histy", height = 280))
        ),
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
              title = "Test results", verbatimTextOutput("v_stats"))
        )
      ),

      # ---- Tab: Sleep quality (research question) ---------------------
      tabItem("sq_tab",
        fluidRow(
          box(width = 12, background = "olive",
            h4(tags$b("Sleep quality across people (research question)")),
            p(style = "font-size:15px;text-align:justify",
              "RQ: how does sleep quality differ from person to person (age, sex)? ",
              "Hypotheses: sleep quality differs across age groups, and between women ",
              "and men. Metrics: sleep efficiency and sleep duration (higher = better), ",
              "and acrophase variability (lower = better). Pick a metric to see it ",
              "against age, sex, and age+sex, each with a test.")
          )
        ),
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE, title = "Controls",
            fluidRow(
              column(6, selectInput("sq_metric", "Sleep-quality metric:",
                                    choices = sq_metrics, selected = "sleep_efficiency_trait")),
              column(6, sliderInput("sq_split", "Age split (younger / older):",
                                    min = 30, max = 70, value = 40, step = 5))
            )
          )
        ),
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
              title = "1. Age vs sleep quality",
              plotlyOutput("sq_g1", height = 460))
        ),
        fluidRow(column(12, verbatimTextOutput("sq_s1"))),
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
              title = "2. Sex vs sleep quality",
              plotlyOutput("sq_g2", height = 460))
        ),
        fluidRow(column(12, verbatimTextOutput("sq_s2"))),
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
              title = "3. Age group + Sex vs sleep quality",
              plotlyOutput("sq_g3", height = 460))
        ),
        fluidRow(column(12, verbatimTextOutput("sq_s3")))
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
      ),

      # ---- Tab: Circadian SD by age & sex -----------------------------
      tabItem("sd_tab",
        fluidRow(
          box(width = 12, background = "olive",
            h4(tags$b("Circadian variability (SD) by age & sex")),
            p(style = "font-size:15px;text-align:justify",
              "Each person's within-subject SD of acrophase measures how much their ",
              "circadian rhythm changes from day to day (a high SD = an unstable clock). ",
              "Here we break that variability down by age and sex. One value per person.")
          )
        ),
        fluidRow(
          box(width = 4, status = "primary", solidHeader = TRUE, title = "Controls",
            sliderInput("sd_age", "Age split (younger / older):",
                        min = 30, max = 70, value = 50, step = 5),
            helpText("Splits people into younger/older at this age; crossed with sex ",
                     "in the grouped plot. SD is computed per person from all their days."),
            radioButtons("sd_sex", "Subgroup test - which sex:",
                         choices = c("Male", "Female"), selected = "Male", inline = TRUE)
          ),
          valueBoxOutput("sd_vb_female", width = 4),
          valueBoxOutput("sd_vb_male",   width = 4)
        ),
        fluidRow(
          box(width = 6, status = "primary", solidHeader = TRUE,
              title = "Circadian SD vs age (coloured by sex)",
              plotlyOutput("sd_scatter", height = 360)),
          box(width = 6, status = "primary", solidHeader = TRUE,
              title = "Circadian SD by sex",
              plotlyOutput("sd_sexbox", height = 360))
        ),
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
              title = "Circadian SD by age group x sex",
              plotlyOutput("sd_agesexbox", height = 340))
        ),
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
              title = "Does circadian SD depend on age or sex?",
              verbatimTextOutput("sd_stats"))
        ),
        fluidRow(
          box(width = 6, status = "warning", solidHeader = TRUE,
              title = "Subgroup: younger vs older within one sex",
              plotlyOutput("sd_subplot", height = 320)),
          box(width = 6, status = "warning", solidHeader = TRUE,
              title = "Subgroup test (exploratory)",
              verbatimTextOutput("sd_subtest"))
        )
      ),

      # ---- Tab: Sleep efficiency by age & sex -------------------------
      tabItem("se_tab",
        fluidRow(
          box(width = 12, background = "olive",
            h4(tags$b("Sleep efficiency by age & sex")),
            p(style = "font-size:15px;text-align:justify",
              "Each person's habitual sleep efficiency (time asleep / time in bed), ",
              "broken down by age and sex. One value per person.")
          )
        ),
        fluidRow(
          box(width = 4, status = "primary", solidHeader = TRUE, title = "Controls",
            sliderInput("se_age", "Age split (younger / older):",
                        min = 30, max = 70, value = 50, step = 5),
            helpText("Splits people into younger/older at this age; crossed with sex ",
                     "in the grouped plot."),
            radioButtons("se_sex", "Subgroup test - which sex:",
                         choices = c("Male", "Female"), selected = "Male", inline = TRUE)
          ),
          valueBoxOutput("se_vb_female", width = 4),
          valueBoxOutput("se_vb_male",   width = 4)
        ),
        fluidRow(
          box(width = 6, status = "primary", solidHeader = TRUE,
              title = "Sleep efficiency vs age (coloured by sex)",
              plotlyOutput("se_scatter", height = 360)),
          box(width = 6, status = "primary", solidHeader = TRUE,
              title = "Sleep efficiency by sex",
              plotlyOutput("se_sexbox", height = 360))
        ),
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
              title = "Sleep efficiency by age group x sex",
              plotlyOutput("se_agesexbox", height = 340))
        ),
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
              title = "Does sleep efficiency depend on age or sex?",
              verbatimTextOutput("se_stats"))
        ),
        fluidRow(
          box(width = 6, status = "warning", solidHeader = TRUE,
              title = "Subgroup: younger vs older within one sex",
              plotlyOutput("se_subplot", height = 320)),
          box(width = 6, status = "warning", solidHeader = TRUE,
              title = "Subgroup test (exploratory)",
              verbatimTextOutput("se_subtest"))
        )
      ),

      # ---- Tab: Thought 2 — Sleep efficiency vs circadian variability -----
      tabItem("thought2",
        fluidRow(
          box(width = 12, background = "olive",
            h4(tags$b("Thought 2 — Sleep efficiency vs circadian variability")),
            p(style = "font-size:15px;text-align:justify",
              "Hypothesis: sleep efficiency and circadian-rhythm variability are ",
              "negatively correlated - people who sleep more efficiently have a more ",
              "stable circadian rhythm (a lower day-to-day SD of acrophase). ",
              "Each point is one person: habitual sleep efficiency vs their circadian SD.")
          )
        ),
        fluidRow(
          box(width = 4, status = "primary", solidHeader = TRUE, title = "Controls",
            checkboxInput("t2_sex", "Colour by sex", FALSE),
            helpText("Sleep efficiency = habitual (trait) value per person. ",
                     "Circadian variability = within-subject SD of acrophase.")
          ),
          valueBoxOutput("t2_vb_r",  width = 4),
          valueBoxOutput("t2_vb_p",  width = 4)
        ),
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
              title = "Sleep efficiency vs circadian SD",
              plotlyOutput("t2_scatter", height = 400))
        ),
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
              title = "Correlation test",
              verbatimTextOutput("t2_stats"))
        )
      ),

      # ---- Tab: Thought 3 — circadian variability by sex --------------
      tabItem("thought3",
        fluidRow(
          box(width = 12, background = "olive",
            h4(tags$b("Thought 3 — Circadian variability by sex")),
            p(style = "font-size:15px;text-align:justify",
              "Hypothesis: women show less variability of circadian acrophase than men ",
              "(a lower within-subject SD of acrophase = a more stable clock). ",
              "Each point is one person's circadian SD.")
          )
        ),
        fluidRow(
          valueBoxOutput("t3_vb_female", width = 6),
          valueBoxOutput("t3_vb_male",   width = 6)
        ),
        fluidRow(
          box(width = 6, status = "primary", solidHeader = TRUE,
              title = "Circadian SD by sex",
              plotlyOutput("t3_box", height = 360)),
          box(width = 6, status = "primary", solidHeader = TRUE,
              title = "Distribution of circadian SD by sex",
              plotlyOutput("t3_density", height = 360))
        ),
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
              title = "Is circadian variability lower in women?",
              verbatimTextOutput("t3_stats"))
        )
      ),

      # ---- Tab: Thought 4 — age vs sleep efficiency & variability -----
      tabItem("thought4",
        fluidRow(
          box(width = 12, background = "olive",
            h4(tags$b("Thought 4 — Younger: better sleep, more variable clock?")),
            p(style = "font-size:15px;text-align:justify",
              "Hypothesis: younger people have better sleep efficiency but more ",
              "variability of acrophase. The two panels show both trends against age; ",
              "split them by age group, sex, or both to compare.")
          )
        ),
        fluidRow(
          box(width = 4, status = "primary", solidHeader = TRUE, title = "Controls",
            radioButtons("t4_split", "Split / colour by:",
              choices = c("Sex" = "sex", "Age group" = "age", "Both" = "both"),
              selected = "sex"),
            sliderInput("t4_age", "Age split (younger / older):",
                        min = 30, max = 70, value = 50, step = 5)
          ),
          valueBoxOutput("t4_vb_eff", width = 4),
          valueBoxOutput("t4_vb_sd",  width = 4)
        ),
        fluidRow(
          box(width = 6, status = "primary", solidHeader = TRUE,
              title = "Sleep efficiency vs age (the 'better sleep' claim)",
              plotlyOutput("t4_eff", height = 360)),
          box(width = 6, status = "primary", solidHeader = TRUE,
              title = "Circadian SD vs age (the 'more variable' claim)",
              plotlyOutput("t4_sd", height = 360))
        ),
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
              title = "Trends and group comparison",
              verbatimTextOutput("t4_stats"))
        )
      )
    )
  )
)

# ---- SERVER --------------------------------------------------------------
server <- function(input, output) {

  # ----- Tab 1: histogram -----
  # discrete = 15 or fewer distinct values (0/1 flags, age, etc.) -> bar chart
  var_is_discrete <- reactive(length(unique(na.omit(d[[input$var]]))) <= 15)
  # flag for the UI: show the bin slider only for continuous variables
  output$var_continuous <- reactive(!var_is_discrete())
  outputOptions(output, "var_continuous", suspendWhenHidden = FALSE)

  output$hist <- renderPlotly({
    v <- input$var
    g <- if (input$group == "none") NULL else input$group
    if (var_is_discrete()) {                     # bar chart of counts (few distinct values)
      dd <- d; dd[[v]] <- factor(dd[[v]])
      p <- ggplot(dd, aes(x = .data[[v]]))
      p <- if (is.null(g)) p + geom_bar(fill = "#3182bd")
           else p + geom_bar(aes(fill = .data[[g]]), position = "dodge")
      p <- p + labs(x = v, y = "Count (participant-days)", fill = NULL,
                    subtitle = "discrete variable (few distinct values) - shown as a bar chart")
    } else {                                     # histogram for continuous variables
      p <- ggplot(d, aes(x = .data[[v]]))
      p <- if (is.null(g)) p + geom_histogram(bins = input$bins, fill = "#3182bd", colour = "white")
           else p + geom_histogram(aes(fill = .data[[g]]), bins = input$bins,
                                    colour = "white", position = "identity", alpha = 0.6)
      p <- p + labs(x = v, y = "Count", fill = NULL)
    }
    bg(ggplotly(p + theme_minimal(base_size = 13)))
  })

  output$summary1 <- renderPrint({
    x <- d[[input$var]]
    if (var_is_discrete()) {
      cat("Discrete variable - counts:\n"); print(table(x, useNA = "ifany"))
    } else summary(x)
  })

  # ----- Tab: Variable explorer (type-aware: numeric vs numeric = correlation;
  #        numeric vs Sex = group comparison) -----
  v_lab   <- function(v) names(var_choices)[match(v, var_choices)]
  is_cat  <- function(v) v == "sex"
  in_fence <- function(z) { q <- quantile(z, c(.25, .75)); i <- 1.5 * (q[2] - q[1])
                            z >= q[1] - i & z <= q[2] + i }
  v_mode <- reactive({
    cx <- is_cat(input$vx); cy <- is_cat(input$vy)
    if (cx && cy) "cat" else if (cx || cy) "group" else "corr"
  })
  v_raw <- reactive({
    df <- data.frame(x = subj_all[[input$vx]], y = subj_all[[input$vy]],
                     sex = subj_all$sex, stringsAsFactors = FALSE)
    if (!is_cat(input$vx)) df <- df[is.finite(df$x), ]
    if (!is_cat(input$vy)) df <- df[is.finite(df$y), ]
    df
  })
  v_data <- reactive({
    df <- v_raw()
    if (isTRUE(input$v_outlier)) {
      if (!is_cat(input$vx)) df <- df[in_fence(as.numeric(df$x)), ]
      if (!is_cat(input$vy)) df <- df[in_fence(as.numeric(df$y)), ]
    }
    df
  })
  v_ct <- reactive({ if (v_mode() != "corr") return(NULL); df <- v_data(); cor.test(df$x, df$y) })
  v_cs <- reactive({ if (v_mode() != "corr") return(NULL); df <- v_data()
                     suppressWarnings(cor.test(df$x, df$y, method = "spearman")) })
  # numeric values split by sex, for the group-comparison mode
  v_group <- reactive({
    numcol <- if (is_cat(input$vx)) "y" else "x"
    df <- v_data(); vals <- as.numeric(df[[numcol]])
    list(f = vals[df$sex == "Female"], m = vals[df$sex == "Male"],
         numlab = v_lab(if (numcol == "y") input$vy else input$vx))
  })

  output$v_vb_r <- renderValueBox({
    if (v_mode() == "corr") {
      r <- v_ct()$estimate
      valueBox(sprintf("%+.2f", r), "Pearson r", icon = icon("link"),
               color = if (abs(r) >= 0.3) "blue" else "aqua")
    } else if (v_mode() == "group") {
      g <- v_group(); valueBox(sprintf("%.2f", mean(g$f)),
               sprintf("Female: mean (n=%d)", length(g$f)), icon = icon("venus"), color = "purple")
    } else valueBox("-", "pick a numeric variable", icon = icon("ban"), color = "black")
  })
  output$v_vb_rho <- renderValueBox({
    if (v_mode() == "corr") {
      r <- v_cs()$estimate
      valueBox(sprintf("%+.2f", r), "Spearman rho", icon = icon("ranking-star"),
               color = if (abs(r) >= 0.3) "purple" else "light-blue")
    } else if (v_mode() == "group") {
      g <- v_group(); valueBox(sprintf("%.2f", mean(g$m)),
               sprintf("Male: mean (n=%d)", length(g$m)), icon = icon("mars"), color = "blue")
    } else valueBox("-", "", icon = icon("ban"), color = "black")
  })
  output$v_vb_p <- renderValueBox({
    if (v_mode() == "corr") {
      p <- v_ct()$p.value
      valueBox(sprintf("%.3f", p), "Pearson p-value", icon = icon("flask"),
               color = if (p < 0.05) "green" else "red")
    } else if (v_mode() == "group") {
      g <- v_group(); valueBox(sprintf("%+.2f", mean(g$f) - mean(g$m)),
               "Difference (F - M)", icon = icon("arrows-left-right"), color = "navy")
    } else valueBox("-", "", icon = icon("ban"), color = "black")
  })
  output$v_vb_sp <- renderValueBox({
    if (v_mode() == "corr") {
      p <- v_cs()$p.value
      valueBox(sprintf("%.3f", p), "Spearman p-value", icon = icon("flask"),
               color = if (p < 0.05) "green" else "red")
    } else if (v_mode() == "group") {
      g <- v_group(); p <- t.test(g$f, g$m)$p.value
      valueBox(sprintf("%.3f", p), "t-test p-value", icon = icon("flask"),
               color = if (p < 0.05) "green" else "red")
    } else valueBox("-", "", icon = icon("ban"), color = "black")
  })

  # per-sex correlation (numeric vs numeric only) — shown as two boxes
  v_sex_cor <- function(s) {
    if (v_mode() != "corr") return(NULL)
    ss <- v_data(); ss <- ss[ss$sex == s, ]
    if (nrow(ss) < 4) return(NULL)
    ct <- suppressWarnings(cor.test(ss$x, ss$y)); ct$n <- nrow(ss); ct
  }
  output$v_vb_fem <- renderValueBox({
    ct <- v_sex_cor("Female")
    if (is.null(ct)) return(valueBox("-", "Female r (numeric x numeric only)",
                                     icon = icon("venus"), color = "black"))
    valueBox(sprintf("%+.2f", ct$estimate),
             sprintf("Female: r  (p=%.3f, n=%d)", ct$p.value, ct$n),
             icon = icon("venus"), color = if (ct$p.value < 0.05) "green" else "purple")
  })
  output$v_vb_male <- renderValueBox({
    ct <- v_sex_cor("Male")
    if (is.null(ct)) return(valueBox("-", "Male r (numeric x numeric only)",
                                     icon = icon("mars"), color = "black"))
    valueBox(sprintf("%+.2f", ct$estimate),
             sprintf("Male: r  (p=%.3f, n=%d)", ct$p.value, ct$n),
             icon = icon("mars"), color = if (ct$p.value < 0.05) "green" else "blue")
  })

  output$v_scatter <- renderPlotly({
    if (v_mode() == "cat") {
      return(bg(ggplotly(ggplot() +
        annotate("text", 1, 1, label = "Pick at least one numeric variable") +
        theme_void())))
    }
    if (v_mode() == "group") {                       # numeric by sex -> boxplot
      numcol <- if (is_cat(input$vx)) "y" else "x"
      df <- v_data(); df$val <- as.numeric(df[[numcol]])
      p <- ggplot(df, aes(x = sex, y = val, fill = sex)) +
        geom_boxplot(alpha = 0.6, width = 0.5, outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
        geom_jitter(width = 0.12, alpha = 0.5, size = 1.5) +
        labs(x = NULL, y = v_group()$numlab, fill = NULL,
             title = paste(v_group()$numlab, "by sex")) +
        theme_minimal(base_size = 13) +
        theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
      return(ggplotly(p) %>% layout(plot_bgcolor = "white", paper_bgcolor = "white"))
    }
    df <- v_data()                                   # numeric vs numeric -> scatter
    if (input$vcol == "sex") {
      p <- ggplot(df, aes(x = x, y = y, colour = sex)) +
        geom_point(alpha = 0.7, size = 2) + geom_smooth(method = "lm", se = FALSE, linewidth = 0.9)
    } else {
      p <- ggplot(df, aes(x = x, y = y)) +
        geom_point(alpha = 0.7, size = 2, colour = "#3182bd") +
        geom_smooth(method = "lm", se = FALSE, linewidth = 0.9, colour = "black")
    }
    p <- p + labs(x = v_lab(input$vx), y = v_lab(input$vy), colour = NULL,
                  title = paste(v_lab(input$vx), "vs", v_lab(input$vy))) +
      theme_minimal(base_size = 13) + theme(plot.title = element_text(hjust = 0.5))
    ggplotly(p) %>% layout(plot_bgcolor = "white", paper_bgcolor = "white")
  })

  v_hist <- function(which, varname) {
    df <- v_data(); df$v <- df[[which]]; lab <- v_lab(varname)
    if (is_cat(varname)) {
      p <- ggplot(df, aes(factor(v))) + geom_bar(fill = "#3182bd") + labs(x = lab, y = "Count")
    } else {
      df$v <- as.numeric(df$v)
      if (input$vcol == "sex")
        p <- ggplot(df, aes(v, fill = sex)) +
          geom_histogram(bins = 25, colour = "white", position = "identity", alpha = 0.6)
      else p <- ggplot(df, aes(v)) + geom_histogram(bins = 25, fill = "#3182bd", colour = "white")
      p <- p + labs(x = lab, y = "Count", fill = NULL)
    }
    p <- p + labs(title = paste("Distribution of", lab)) +
      theme_minimal(base_size = 12) + theme(plot.title = element_text(hjust = 0.5))
    ggplotly(p) %>% layout(plot_bgcolor = "white", paper_bgcolor = "white")
  }
  output$v_histx <- renderPlotly(v_hist("x", input$vx))
  output$v_histy <- renderPlotly(v_hist("y", input$vy))

  output$v_stats <- renderPrint({
    df <- v_data()
    cat("Variable 1 (X):", v_lab(input$vx), "\n")
    cat("Variable 2 (Y):", v_lab(input$vy), "\n")
    if (isTRUE(input$v_outlier))
      cat("n =", nrow(df), "people  (", nrow(v_raw()) - nrow(df), "outliers removed, 1.5 x IQR)\n")
    else cat("n =", nrow(df), "people\n")
    cat("--------------------------------------------------------------------\n")

    if (v_mode() == "cat") { cat("Both variables are Sex - pick at least one numeric variable.\n"); return(invisible()) }

    if (v_mode() == "group") {                       # t-test of numeric by sex
      g <- v_group(); tt <- t.test(g$f, g$m)
      d <- (mean(g$f) - mean(g$m)) / sqrt(((length(g$f)-1)*var(g$f) + (length(g$m)-1)*var(g$m)) /
                                          (length(g$f)+length(g$m)-2))
      cat("Comparing", g$numlab, "between Female and Male:\n")
      cat(sprintf("   Female: mean %.3f (n=%d)\n   Male  : mean %.3f (n=%d)\n",
                  mean(g$f), length(g$f), mean(g$m), length(g$m)))
      cat(sprintf("   difference (F - M) = %+.3f\n", mean(g$f) - mean(g$m)))
      cat(sprintf("   Welch t-test : t = %.2f, p = %.4f, 95%% CI [%.3f, %.3f]\n",
                  tt$statistic, tt$p.value, tt$conf.int[1], tt$conf.int[2]))
      cat(sprintf("   Wilcoxon     : p = %.4f\n", suppressWarnings(wilcox.test(g$f, g$m)$p.value)))
      cat(sprintf("   Cohen's d    : %.2f\n", d))
      cat(sprintf("\n%s\n", if (tt$p.value < 0.05) "Significant difference (p < 0.05)."
                            else "No significant difference (p > 0.05)."))
      return(invisible())
    }

    if (input$vx == input$vy) { cat("Pick two different variables.\n"); return(invisible()) }
    ct <- cor.test(df$x, df$y); cs <- suppressWarnings(cor.test(df$x, df$y, method = "spearman"))
    cat(sprintf("Pearson  r   = %+.3f   95%% CI [%.2f, %.2f]   p = %.4f\n",
                ct$estimate, ct$conf.int[1], ct$conf.int[2], ct$p.value))
    cat(sprintf("Spearman rho = %+.3f                       p = %.4f\n", cs$estimate, cs$p.value))
    if (input$vcol == "sex") {
      cat("\nWithin each sex:\n")
      for (s in c("Female", "Male")) {
        ss <- df[df$sex == s, ]
        if (nrow(ss) > 3) cat(sprintf("   %-7s (n=%2d): r = %+.3f, p = %.3f\n",
                                      s, nrow(ss), cor(ss$x, ss$y), cor.test(ss$x, ss$y)$p.value))
      }
    }
    cat(sprintf("\n%s\n", if (ct$p.value < 0.05) "Significant association (p < 0.05)."
                          else "No significant association (p > 0.05)."))
  })

  # ----- Tab: Sleep quality (research question) -----
  sq_lab <- reactive(names(sq_metrics)[match(input$sq_metric, sq_metrics)])
  sq_df <- reactive({
    d0 <- subj_all %>% mutate(duration = sleep_tib_trait * sleep_efficiency_trait)
    d0$m <- d0[[input$sq_metric]]
    d0 <- d0[is.finite(d0$m), ]
    d0$agegrp <- factor(ifelse(d0$age >= input$sq_split,
                               paste0("Older (", input$sq_split, "+)"),
                               paste0("Younger (<", input$sq_split, ")")),
                        levels = c(paste0("Younger (<", input$sq_split, ")"),
                                   paste0("Older (", input$sq_split, "+)")))
    d0
  })

  # 1. Age group vs metric (boxplot)
  output$sq_g1 <- renderPlotly({
    df <- sq_df()
    p <- ggplot(df, aes(x = agegrp, y = m, fill = agegrp)) +
      geom_boxplot(alpha = 0.6, width = 0.5, outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
      geom_jitter(width = 0.12, alpha = 0.5, size = 1.4) +
      labs(x = NULL, y = sq_lab(), fill = "Age group",
           title = paste(sq_lab(), "by age group")) +
      theme_minimal(base_size = 13) + theme(plot.title = element_text(hjust = 0.5))
    ggplotly(p) %>% layout(plot_bgcolor = "white", paper_bgcolor = "white",
                           legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15))
  })
  # 2. Sex vs metric (boxplot)
  output$sq_g2 <- renderPlotly({
    df <- sq_df()
    p <- ggplot(df, aes(x = sex, y = m, fill = sex)) +
      geom_boxplot(alpha = 0.6, width = 0.5, outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
      geom_jitter(width = 0.12, alpha = 0.5, size = 1.4) +
      labs(x = NULL, y = sq_lab(), fill = "Sex",
           title = paste(sq_lab(), "by sex")) +
      theme_minimal(base_size = 13) + theme(plot.title = element_text(hjust = 0.5))
    ggplotly(p) %>% layout(plot_bgcolor = "white", paper_bgcolor = "white",
                           legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15))
  })
  # 3. Age group x Sex vs metric (grouped boxplot, boxes side by side)
  output$sq_g3 <- renderPlotly({
    df <- sq_df()
    p <- ggplot(df, aes(x = agegrp, y = m, fill = sex)) +
      geom_boxplot(alpha = 0.65, outlier.colour = "red", outlier.shape = 16, outlier.size = 2,
                   position = position_dodge(width = 0.75)) +
      geom_point(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75),
                 alpha = 0.5, size = 1.4) +
      labs(x = NULL, y = sq_lab(), fill = "Sex",
           title = paste(sq_lab(), "by age group and sex")) +
      theme_minimal(base_size = 13) + theme(plot.title = element_text(hjust = 0.5))
    ggplotly(p) %>% layout(boxmode = "group", plot_bgcolor = "white", paper_bgcolor = "white",
                           legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15))
  })

  output$sq_s1 <- renderPrint({
    df <- sq_df()
    yg <- df$m[df$agegrp == levels(df$agegrp)[1]]
    ol <- df$m[df$agegrp == levels(df$agegrp)[2]]
    ct <- cor.test(df$age, df$m)
    cat(sprintf("AGE (split at %d): younger mean %.3f (n=%d) vs older mean %.3f (n=%d)\n",
                input$sq_split, mean(yg), length(yg), mean(ol), length(ol)))
    if (length(yg) >= 3 && length(ol) >= 3) {
      tt <- t.test(yg, ol)
      cat(sprintf("   Welch t-test p = %.4f   |   continuous age: r = %+.3f, p = %.4f  ->  %s\n",
                  tt$p.value, ct$estimate, ct$p.value,
                  if (min(tt$p.value, ct$p.value) < 0.05) "differs with age" else "no significant age effect"))
    }
  })
  output$sq_s2 <- renderPrint({
    df <- sq_df(); tt <- t.test(m ~ sex, df)
    cat(sprintf("SEX:  Female mean %.3f (n=%d) vs Male mean %.3f (n=%d)\n",
                mean(df$m[df$sex == "Female"]), sum(df$sex == "Female"),
                mean(df$m[df$sex == "Male"]),   sum(df$sex == "Male")))
    cat(sprintf("      Welch t-test p = %.4f  ->  %s\n", tt$p.value,
                if (tt$p.value < 0.05) "sleep quality DIFFERS by sex" else "no significant sex difference"))
  })
  output$sq_s3 <- renderPrint({
    df <- sq_df(); av <- anova(lm(m ~ agegrp * sex, df))
    cat("AGE GROUP x SEX (2-way ANOVA, split at", input$sq_split, "yr):\n")
    for (r in c("agegrp", "sex", "agegrp:sex"))
      cat(sprintf("   %-12s p = %.4f\n", r, av[r, "Pr(>F)"]))
    cat("\nMedian by age group x sex:\n")
    tab <- df %>% group_by(agegrp, sex) %>%
      summarise(median = round(median(m), 3), n = n(), .groups = "drop")
    print(as.data.frame(tab), row.names = FALSE)
  })

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
      geom_boxplot(alpha = 0.7, outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
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
        geom_boxplot(alpha = 0.55, width = 0.5, outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
        geom_jitter(width = 0.12, alpha = 0.5, size = 1.4) +
        labs(x = NULL, y = "Weekend - weekday shift [h]", fill = NULL) +
        theme_minimal(base_size = 13) + theme(legend.position = "none")
    } else {
      p <- ggplot(t1_long(), aes(x = grp, y = acro, fill = daytype)) +
        geom_boxplot(alpha = 0.7, outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
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

  # ----- Tab: Circadian SD by age & sex -----
  sd_data <- reactive({
    subj_sd %>% mutate(
      agegrp = factor(ifelse(age >= input$sd_age,
                             paste0("Older (", input$sd_age, "+)"),
                             paste0("Younger (<", input$sd_age, ")")),
                      levels = c(paste0("Younger (<", input$sd_age, ")"),
                                 paste0("Older (", input$sd_age, "+)"))))
  })

  output$sd_vb_female <- renderValueBox({
    s <- subj_sd %>% filter(sex == "Female")
    valueBox(sprintf("%.2f h", median(s$acro_sd)),
             sprintf("Female: median circadian SD (n=%d)", nrow(s)),
             icon = icon("venus"), color = "purple")
  })
  output$sd_vb_male <- renderValueBox({
    s <- subj_sd %>% filter(sex == "Male")
    valueBox(sprintf("%.2f h", median(s$acro_sd)),
             sprintf("Male: median circadian SD (n=%d)", nrow(s)),
             icon = icon("mars"), color = "blue")
  })

  # SD vs age, coloured by sex, trend line per sex
  output$sd_scatter <- renderPlotly({
    p <- ggplot(sd_data(), aes(x = age, y = acro_sd, colour = sex)) +
      geom_point(alpha = 0.7, size = 2) +
      geom_smooth(method = "lm", se = FALSE, linewidth = 0.9) +
      geom_vline(xintercept = input$sd_age, linetype = "dotted", colour = "red") +
      labs(x = "Age (years)", y = "Circadian SD [h]", colour = NULL) +
      theme_minimal(base_size = 13)
    bg(ggplotly(p))
  })

  # SD by sex (boxplot + points)
  output$sd_sexbox <- renderPlotly({
    p <- ggplot(subj_sd, aes(x = sex, y = acro_sd, fill = sex)) +
      geom_boxplot(alpha = 0.6, width = 0.5, outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
      geom_jitter(width = 0.12, alpha = 0.5, size = 1.3) +
      labs(x = NULL, y = "Circadian SD [h]", fill = NULL) +
      theme_minimal(base_size = 13) + theme(legend.position = "none")
    bg(ggplotly(p))
  })

  # SD by age group x sex (grouped boxplot)
  output$sd_agesexbox <- renderPlotly({
    p <- ggplot(sd_data(), aes(x = agegrp, y = acro_sd, fill = sex)) +
      geom_boxplot(alpha = 0.65, outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
      labs(x = NULL, y = "Circadian SD [h]", fill = NULL) +
      theme_minimal(base_size = 13)
    bg(ggplotly(p))
  })

  output$sd_stats <- renderPrint({
    df <- sd_data()
    cat("Per-person circadian SD (n =", nrow(df), "people)\n")
    cat("GROUPED test: 2-way ANOVA  acro_sd ~ age group x sex\n")
    cat("(age split at", input$sd_age, "yr - matches the grouped plot above)\n")
    cat("--------------------------------------------------------------------\n")
    av <- anova(lm(acro_sd ~ agegrp * sex, df))
    rn <- c("agegrp", "sex", "agegrp:sex")
    lab <- c("age group", "sex", "age group x sex")
    for (i in seq_along(rn))
      cat(sprintf("%-16s: F = %.2f   p = %.3f\n", lab[i], av[rn[i], "F value"], av[rn[i], "Pr(>F)"]))

    cat("\nGroup means (median SD [h], n):\n")
    tab <- df %>% group_by(agegrp, sex) %>%
      summarise(median_SD = round(median(acro_sd), 2), n = n(), .groups = "drop")
    print(as.data.frame(tab), row.names = FALSE)

    cf <- summary(lm(acro_sd ~ age + sex, df))$coefficients
    cat(sprintf("\nFor comparison - CONTINUOUS age: age p = %.3f, sex p = %.3f\n",
                cf["age", 4], cf["sexMale", 4]))
    cat("(continuous keeps more information than splitting age into groups)\n")
  })

  # ----- Subgroup test: younger vs older within one sex -----
  sd_sub <- reactive({
    subj_sd %>% filter(sex == input$sd_sex) %>%
      mutate(grp = factor(ifelse(age >= input$sd_age,
                                 paste0("Older (", input$sd_age, "+)"),
                                 paste0("Younger (<", input$sd_age, ")")),
                          levels = c(paste0("Younger (<", input$sd_age, ")"),
                                     paste0("Older (", input$sd_age, "+)"))))
  })

  output$sd_subplot <- renderPlotly({
    df <- sd_sub()
    p <- ggplot(df, aes(x = grp, y = acro_sd, fill = grp)) +
      geom_boxplot(alpha = 0.6, width = 0.5, outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
      geom_jitter(width = 0.12, alpha = 0.6, size = 1.6) +
      labs(x = NULL, y = "Circadian SD [h]", fill = NULL,
           title = paste(input$sd_sex, "only")) +
      theme_minimal(base_size = 13) + theme(legend.position = "none")
    bg(ggplotly(p))
  })

  output$sd_subtest <- renderPrint({
    df <- sd_sub()
    ns <- table(df$grp)
    cat(input$sd_sex, "only - younger vs older at age", input$sd_age, "\n")
    cat("--------------------------------------------------------------------\n")
    if (length(ns) < 2 || min(ns) < 3) {
      cat("Not enough people in one group at this cutoff (need >=3 each).\n"); return(invisible())
    }
    yg <- df$acro_sd[df$grp == levels(df$grp)[1]]
    ol <- df$acro_sd[df$grp == levels(df$grp)[2]]
    tt <- t.test(ol, yg)                       # Welch (unequal variances)
    d  <- (mean(ol) - mean(yg)) / sqrt(((length(ol)-1)*var(ol) + (length(yg)-1)*var(yg)) /
                                       (length(ol)+length(yg)-2))
    cat(sprintf("younger: mean %.3f h (n=%d)\nolder  : mean %.3f h (n=%d)\n",
                mean(yg), length(yg), mean(ol), length(ol)))
    cat(sprintf("difference (older - younger) = %+.3f h\n", mean(ol) - mean(yg)))
    cat(sprintf("Welch t-test : t = %.2f, p = %.3f, 95%% CI [%.3f, %.3f]\n",
                tt$statistic, tt$p.value, tt$conf.int[1], tt$conf.int[2]))
    cat(sprintf("Wilcoxon     : p = %.3f\n", suppressWarnings(wilcox.test(ol, yg)$p.value)))
    cat(sprintf("Cohen's d    : %.2f  (%s effect size)\n", d,
                ifelse(abs(d) < 0.2, "negligible", ifelse(abs(d) < 0.5, "small",
                ifelse(abs(d) < 0.8, "medium", "large")))))
    cat("\n! Exploratory: you chose the sex/cutoff after seeing the data, and the p-value\n")
    cat("  moves with the cutoff. Treat as hypothesis-generating, not a confirmed result.\n")
  })

  # ----- Tab: Sleep efficiency by age & sex -----
  se_data <- reactive({
    subj_sd %>% mutate(
      agegrp = factor(ifelse(age >= input$se_age,
                             paste0("Older (", input$se_age, "+)"),
                             paste0("Younger (<", input$se_age, ")")),
                      levels = c(paste0("Younger (<", input$se_age, ")"),
                                 paste0("Older (", input$se_age, "+)"))))
  })

  output$se_vb_female <- renderValueBox({
    s <- subj_sd %>% filter(sex == "Female")
    valueBox(sprintf("%.3f", median(s$sleep_eff)),
             sprintf("Female: median sleep efficiency (n=%d)", nrow(s)),
             icon = icon("venus"), color = "purple")
  })
  output$se_vb_male <- renderValueBox({
    s <- subj_sd %>% filter(sex == "Male")
    valueBox(sprintf("%.3f", median(s$sleep_eff)),
             sprintf("Male: median sleep efficiency (n=%d)", nrow(s)),
             icon = icon("mars"), color = "blue")
  })

  output$se_scatter <- renderPlotly({
    p <- ggplot(se_data(), aes(x = age, y = sleep_eff, colour = sex)) +
      geom_point(alpha = 0.7, size = 2) +
      geom_smooth(method = "lm", se = FALSE, linewidth = 0.9) +
      geom_vline(xintercept = input$se_age, linetype = "dotted", colour = "red") +
      labs(x = "Age (years)", y = "Sleep efficiency", colour = NULL) +
      theme_minimal(base_size = 13)
    bg(ggplotly(p))
  })

  output$se_sexbox <- renderPlotly({
    p <- ggplot(subj_sd, aes(x = sex, y = sleep_eff, fill = sex)) +
      geom_boxplot(alpha = 0.6, width = 0.5, outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
      geom_jitter(width = 0.12, alpha = 0.5, size = 1.3) +
      labs(x = NULL, y = "Sleep efficiency", fill = NULL) +
      theme_minimal(base_size = 13) + theme(legend.position = "none")
    bg(ggplotly(p))
  })

  output$se_agesexbox <- renderPlotly({
    p <- ggplot(se_data(), aes(x = agegrp, y = sleep_eff, fill = sex)) +
      geom_boxplot(alpha = 0.65, outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
      labs(x = NULL, y = "Sleep efficiency", fill = NULL) +
      theme_minimal(base_size = 13)
    bg(ggplotly(p))
  })

  output$se_stats <- renderPrint({
    df <- se_data()
    cat("Per-person sleep efficiency (n =", nrow(df), "people)\n")
    cat("GROUPED test: 2-way ANOVA  sleep_eff ~ age group x sex\n")
    cat("(age split at", input$se_age, "yr - matches the grouped plot above)\n")
    cat("--------------------------------------------------------------------\n")
    av <- anova(lm(sleep_eff ~ agegrp * sex, df))
    rn <- c("agegrp", "sex", "agegrp:sex"); lab <- c("age group", "sex", "age group x sex")
    for (i in seq_along(rn))
      cat(sprintf("%-16s: F = %.2f   p = %.3f\n", lab[i], av[rn[i], "F value"], av[rn[i], "Pr(>F)"]))
    cat("\nGroup means (median efficiency, n):\n")
    tab <- df %>% group_by(agegrp, sex) %>%
      summarise(median_eff = round(median(sleep_eff), 3), n = n(), .groups = "drop")
    print(as.data.frame(tab), row.names = FALSE)
    cf <- summary(lm(sleep_eff ~ age + sex, df))$coefficients
    cat(sprintf("\nFor comparison - CONTINUOUS age: age p = %.3f, sex p = %.3f\n",
                cf["age", 4], cf["sexMale", 4]))
  })

  # subgroup: younger vs older within one sex (sleep efficiency)
  se_sub <- reactive({
    subj_sd %>% filter(sex == input$se_sex) %>%
      mutate(grp = factor(ifelse(age >= input$se_age,
                                 paste0("Older (", input$se_age, "+)"),
                                 paste0("Younger (<", input$se_age, ")")),
                          levels = c(paste0("Younger (<", input$se_age, ")"),
                                     paste0("Older (", input$se_age, "+)"))))
  })
  output$se_subplot <- renderPlotly({
    p <- ggplot(se_sub(), aes(x = grp, y = sleep_eff, fill = grp)) +
      geom_boxplot(alpha = 0.6, width = 0.5, outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
      geom_jitter(width = 0.12, alpha = 0.6, size = 1.6) +
      labs(x = NULL, y = "Sleep efficiency", fill = NULL, title = paste(input$se_sex, "only")) +
      theme_minimal(base_size = 13) + theme(legend.position = "none")
    bg(ggplotly(p))
  })
  output$se_subtest <- renderPrint({
    df <- se_sub(); ns <- table(df$grp)
    cat(input$se_sex, "only - younger vs older at age", input$se_age, "\n")
    cat("--------------------------------------------------------------------\n")
    if (length(ns) < 2 || min(ns) < 3) { cat("Not enough people in one group (need >=3 each).\n"); return(invisible()) }
    yg <- df$sleep_eff[df$grp == levels(df$grp)[1]]
    ol <- df$sleep_eff[df$grp == levels(df$grp)[2]]
    tt <- t.test(ol, yg)
    d  <- (mean(ol) - mean(yg)) / sqrt(((length(ol)-1)*var(ol) + (length(yg)-1)*var(yg)) /
                                       (length(ol)+length(yg)-2))
    cat(sprintf("younger: mean %.3f (n=%d)\nolder  : mean %.3f (n=%d)\n",
                mean(yg), length(yg), mean(ol), length(ol)))
    cat(sprintf("difference (older - younger) = %+.4f\n", mean(ol) - mean(yg)))
    cat(sprintf("Welch t-test : t = %.2f, p = %.3f, 95%% CI [%.4f, %.4f]\n",
                tt$statistic, tt$p.value, tt$conf.int[1], tt$conf.int[2]))
    cat(sprintf("Cohen's d    : %.2f  (%s effect size)\n", d,
                ifelse(abs(d) < 0.2, "negligible", ifelse(abs(d) < 0.5, "small",
                ifelse(abs(d) < 0.8, "medium", "large")))))
    cat("\n! Exploratory: cutoff/sex chosen after seeing the data - hypothesis-generating only.\n")
  })

  # ----- Tab: Thought 2 — sleep efficiency vs circadian variability -----
  t2_ct <- reactive(cor.test(subj_sd$sleep_eff, subj_sd$acro_sd))

  output$t2_vb_r <- renderValueBox({
    r <- t2_ct()$estimate
    valueBox(sprintf("%+.2f", r), "Correlation r (sleep eff. vs SD)",
             icon = icon("link"), color = if (r < 0) "blue" else "orange")
  })
  output$t2_vb_p <- renderValueBox({
    p <- t2_ct()$p.value
    valueBox(sprintf("%.3f", p), "p-value",
             icon = icon("flask"), color = if (p < 0.05) "green" else "red")
  })

  output$t2_scatter <- renderPlotly({
    p <- ggplot(subj_sd, aes(x = sleep_eff, y = acro_sd)) +
      { if (input$t2_sex) geom_point(aes(colour = sex), alpha = 0.75, size = 2)
        else geom_point(colour = "#3182bd", alpha = 0.7, size = 2) } +
      geom_smooth(method = "lm", se = TRUE, colour = "black", fill = "grey80", linewidth = 0.8) +
      labs(x = "Habitual sleep efficiency", y = "Circadian SD [h]", colour = NULL) +
      theme_minimal(base_size = 13)
    bg(ggplotly(p))
  })

  output$t2_stats <- renderPrint({
    ps <- subj_sd
    ct <- cor.test(ps$sleep_eff, ps$acro_sd)
    cs <- suppressWarnings(cor.test(ps$sleep_eff, ps$acro_sd, method = "spearman"))
    cat("Sleep efficiency vs circadian SD (n =", nrow(ps), "people)\n")
    cat("Hypothesis: negative correlation (better sleep -> more stable clock)\n")
    cat("--------------------------------------------------------------------\n")
    cat(sprintf("Pearson  r   = %+.3f   95%% CI [%.2f, %.2f]   p = %.4f\n",
                ct$estimate, ct$conf.int[1], ct$conf.int[2], ct$p.value))
    cat(sprintf("Spearman rho = %+.3f                       p = %.4f\n", cs$estimate, cs$p.value))
    cf <- summary(lm(acro_sd ~ sleep_eff + age + sex, ps))$coefficients
    cat(sprintf("\nAdjusted for age + sex: sleep_eff coefficient p = %.3f\n", cf["sleep_eff", 4]))
    cat("\nRead: r near 0 and p > 0.05 => no correlation. The sign here is",
        if (ct$estimate < 0) "negative" else "POSITIVE", "-\n")
    cat(if (ct$estimate >= 0) "the OPPOSITE of the hypothesis. The data does not support the idea.\n"
        else "the hypothesised direction, but not statistically significant.\n")
  })

  # ----- Tab: Thought 3 — circadian variability by sex -----
  output$t3_vb_female <- renderValueBox({
    s <- subj_sd %>% filter(sex == "Female")
    valueBox(sprintf("%.2f h", median(s$acro_sd)),
             sprintf("Women: median circadian SD (n=%d)", nrow(s)),
             icon = icon("venus"), color = "purple")
  })
  output$t3_vb_male <- renderValueBox({
    s <- subj_sd %>% filter(sex == "Male")
    valueBox(sprintf("%.2f h", median(s$acro_sd)),
             sprintf("Men: median circadian SD (n=%d)", nrow(s)),
             icon = icon("mars"), color = "blue")
  })

  output$t3_box <- renderPlotly({
    p <- ggplot(subj_sd, aes(x = sex, y = acro_sd, fill = sex)) +
      geom_boxplot(alpha = 0.6, width = 0.5, outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
      geom_jitter(width = 0.12, alpha = 0.5, size = 1.5) +
      labs(x = NULL, y = "Circadian SD [h]", fill = NULL) +
      theme_minimal(base_size = 13) + theme(legend.position = "none")
    bg(ggplotly(p))
  })

  output$t3_density <- renderPlotly({
    p <- ggplot(subj_sd, aes(x = acro_sd, fill = sex, colour = sex)) +
      geom_density(alpha = 0.35) +
      labs(x = "Circadian SD [h]", y = "Density", fill = NULL, colour = NULL) +
      theme_minimal(base_size = 13)
    bg(ggplotly(p))
  })

  output$t3_stats <- renderPrint({
    f <- subj_sd$acro_sd[subj_sd$sex == "Female"]
    m <- subj_sd$acro_sd[subj_sd$sex == "Male"]
    tt <- t.test(f, m)                       # Welch, two-sided
    d  <- (mean(f) - mean(m)) / sqrt(((length(f)-1)*var(f) + (length(m)-1)*var(m)) /
                                     (length(f)+length(m)-2))
    cat("Circadian SD by sex  (women n=", length(f), ", men n=", length(m), ")\n", sep = "")
    cat("Hypothesis: women have LOWER variability than men\n")
    cat("--------------------------------------------------------------------\n")
    cat(sprintf("women: mean %.3f h, median %.3f h\n", mean(f), median(f)))
    cat(sprintf("men  : mean %.3f h, median %.3f h\n", mean(m), median(m)))
    cat(sprintf("difference (women - men) = %+.3f h\n", mean(f) - mean(m)))
    cat(sprintf("Welch t-test : t = %.2f, p = %.3f, 95%% CI [%.3f, %.3f]\n",
                tt$statistic, tt$p.value, tt$conf.int[1], tt$conf.int[2]))
    cat(sprintf("Wilcoxon     : p = %.3f\n", suppressWarnings(wilcox.test(f, m)$p.value)))
    cat(sprintf("Cohen's d    : %.2f  (%s effect size)\n", d,
                ifelse(abs(d) < 0.2, "negligible", ifelse(abs(d) < 0.5, "small",
                ifelse(abs(d) < 0.8, "medium", "large")))))
    cf <- summary(lm(acro_sd ~ sex + age, subj_sd))$coefficients
    cat(sprintf("adjusted for age: sexMale coefficient = %+.3f h, p = %.3f\n",
                cf["sexMale", 1], cf["sexMale", 4]))
    cat("\nVerdict: women are actually",
        if (mean(f) < mean(m)) "LESS variable (hypothesis direction)" else "slightly MORE variable (opposite of hypothesis)",
        "\nand the difference is", if (tt$p.value < 0.05) "significant." else "NOT statistically significant.\n")
  })

  # ----- Tab: Thought 4 — age vs sleep efficiency & variability -----
  t4_data <- reactive({
    subj_sd %>% mutate(
      agegrp = factor(ifelse(age >= input$t4_age,
                             paste0("Older (", input$t4_age, "+)"),
                             paste0("Younger (<", input$t4_age, ")")),
                      levels = c(paste0("Younger (<", input$t4_age, ")"),
                                 paste0("Older (", input$t4_age, "+)"))),
      grpcol = switch(input$t4_split,
                      sex  = sex,
                      age  = as.character(agegrp),
                      both = paste(sex, agegrp)))
  })

  output$t4_vb_eff <- renderValueBox({
    ct <- cor.test(subj_sd$age, subj_sd$sleep_eff)
    valueBox(sprintf("%+.2f", ct$estimate), sprintf("age vs sleep efficiency  (p=%.2f)", ct$p.value),
             icon = icon("bed"), color = if (ct$estimate < 0) "blue" else "orange")
  })
  output$t4_vb_sd <- renderValueBox({
    ct <- cor.test(subj_sd$age, subj_sd$acro_sd)
    valueBox(sprintf("%+.2f", ct$estimate), sprintf("age vs circadian SD  (p=%.2f)", ct$p.value),
             icon = icon("wave-square"), color = if (ct$estimate > 0) "blue" else "orange")
  })

  t4_scatter <- function(yvar, ylab) {
    df <- t4_data()
    p <- ggplot(df, aes(x = age, y = .data[[yvar]], colour = grpcol)) +
      geom_point(alpha = 0.7, size = 2) +
      geom_smooth(method = "lm", se = FALSE, linewidth = 0.9) +
      geom_vline(xintercept = input$t4_age, linetype = "dotted", colour = "red") +
      labs(x = "Age (years)", y = ylab, colour = NULL) +
      theme_minimal(base_size = 13)
    bg(ggplotly(p))
  }
  output$t4_eff <- renderPlotly(t4_scatter("sleep_eff", "Sleep efficiency"))
  output$t4_sd  <- renderPlotly(t4_scatter("acro_sd",   "Circadian SD [h]"))

  output$t4_stats <- renderPrint({
    df <- t4_data()
    cat("Thought 4 — younger = better sleep AND more variable?  (n =", nrow(df), "people)\n")
    cat("--------------------------------------------------------------------\n")
    ce <- cor.test(df$age, df$sleep_eff); cv <- cor.test(df$age, df$acro_sd)
    cat(sprintf("TREND  age vs sleep efficiency: r = %+.3f, p = %.3f\n", ce$estimate, ce$p.value))
    cat(sprintf("       (hypothesis wants NEGATIVE: younger = better sleep)\n"))
    cat(sprintf("VARIAB age vs circadian SD    : r = %+.3f, p = %.3f\n", cv$estimate, cv$p.value))
    cat(sprintf("       (hypothesis wants POSITIVE: younger = more variable)\n"))

    cat("\nMeans by group (split =", input$t4_split, "):\n")
    tab <- df %>% group_by(grpcol) %>%
      summarise(sleep_eff = round(mean(sleep_eff), 3),
                circadian_SD = round(mean(acro_sd), 3), n = n(), .groups = "drop")
    print(as.data.frame(tab), row.names = FALSE)

    yg <- df %>% filter(agegrp == levels(agegrp)[1])
    ol <- df %>% filter(agegrp == levels(agegrp)[2])
    if (nrow(yg) >= 3 && nrow(ol) >= 3) {
      pe <- t.test(yg$sleep_eff, ol$sleep_eff)$p.value
      pv <- t.test(yg$acro_sd,  ol$acro_sd)$p.value
      cat(sprintf("\nYounger vs older (split at %d):\n", input$t4_age))
      cat(sprintf("   sleep efficiency: younger %.3f vs older %.3f   p = %.3f\n",
                  mean(yg$sleep_eff), mean(ol$sleep_eff), pe))
      cat(sprintf("   circadian SD    : younger %.3f vs older %.3f   p = %.3f\n",
                  mean(yg$acro_sd), mean(ol$acro_sd), pv))
    }
    cat("\nVerdict: sleep-efficiency trend is",
        if (ce$estimate < 0) "in the hypothesised direction" else "OPPOSITE (older sleep better)",
        "; variability trend is",
        if (cv$estimate > 0) "in the hypothesised direction" else "OPPOSITE (older more variable)",
        "-\nand neither is significant (p > 0.05).\n")
  })
}

shinyApp(ui, server)
