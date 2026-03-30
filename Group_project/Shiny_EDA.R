library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)
library(ggcorrplot)
library(broom)

# ── Load data ─────────────────────────────────────────────────────────────────
customer_clean <- read_csv("customer_data.csv") %>%
  mutate(
    active_products = rowSums(across(c(savings_account, credit_card, personal_loan,
                                       investment_account, insurance_product)),
                              na.rm = TRUE),
    has_complaint       = as.integer(!is.na(complaint_topics)),
    has_feature_request = as.integer(!is.na(feature_requests)),
    income_bracket      = factor(income_bracket,
                                 levels = c("Low", "Medium", "High", "Very High")),
    clv_segment         = factor(clv_segment,
                                 levels = c("Bronze", "Silver", "Gold", "Platinum")),
    tenure_bucket       = cut(customer_tenure,
                              breaks = c(0, 3, 6, 9, 12, Inf),
                              labels = c("0-3m", "3-6m", "6-9m", "9-12m", "12m+"))
  )

corr_vars <- c(
  "age", "household_size", "active_products", "feature_usage_diversity",
  "failed_transactions", "tx_count", "base_satisfaction", "tx_satisfaction",
  "product_satisfaction", "satisfaction_score", "support_tickets_count",
  "resolved_tickets_ratio", "app_store_rating", "monthly_transaction_count",
  "customer_tenure", "churn_probability"
)

clv_colours <- c(
  "Bronze"   = "#bfdbfe",
  "Silver"   = "#60a5fa",
  "Gold"     = "#2563eb",
  "Platinum" = "#1e3a8a"
)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  
  tags$head(tags$style(HTML("
    body { font-family: 'Segoe UI', sans-serif; }
    .well { background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px; }
    .metric-box { background: #fff; border: 1px solid #e2e8f0; border-radius: 8px;
                  padding: 14px 16px; margin-bottom: 12px; }
    .metric-label { font-size: 10px; font-weight: 700; text-transform: uppercase;
                    color: #94a3b8; letter-spacing: 0.1em; margin-bottom: 4px; }
    .metric-value { font-size: 22px; font-weight: 700; color: #0f1e35; }
    .metric-value.green { color: #16a34a; }
    .metric-value.blue  { color: #2563eb; }
    .metric-note  { font-size: 10px; color: #94a3b8; margin-top: 2px; }
    .section-title { font-size: 15px; font-weight: 700; color: #0f1e35; margin: 16px 0 6px; }
    .info-text { font-size: 13px; color: #64748b; margin-bottom: 12px; }
    .obs-box { background: #eff6ff; border-left: 3px solid #2563eb;
               padding: 10px 14px; border-radius: 0 6px 6px 0;
               margin-top: 12px; font-size: 13px; color: #1e3a8a; }
    .obs-box ul { margin: 6px 0 0 0; padding-left: 18px; }
    hr { border-color: #e2e8f0; }
    .nav-tabs > li.active > a {
      border-top: 2px solid #2563eb !important;
      color: #2563eb !important; font-weight: 600;
    }
  "))),
  
  titlePanel(
    div(style = "color:#0f1e35; font-weight:700;",
        "EDA & CDA",
        span(" — Colombian Fintech Customers",
             style = "font-weight:400; color:#64748b; font-size:16px;"))
  ),
  
  tabsetPanel(
    
    # ══ TAB 1: EDA ════════════════════════════════════════════════════════════
    tabPanel("Exploratory Data Analysis",
             br(),
             tabsetPanel(
               
               # ── EDA 1: Churn Distribution ──
               tabPanel("Churn Distribution",
                        br(),
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       div(class = "section-title", "Options"),
                                       hr(),
                                       sliderInput("binwidth", label = "Histogram bin width",
                                                   min = 0.005, max = 0.05, value = 0.01, step = 0.005),
                                       hr(),
                                       div(class = "info-text",
                                           "Adjust bin width to explore the shape of the churn probability distribution.")
                          ),
                          mainPanel(width = 9,
                                    div(class = "info-text",
                                        "Distribution of churn probability across all customers. Dashed line shows the mean."),
                                    plotlyOutput("churn_hist", height = "420px"),
                                    div(class = "obs-box",
                                        tags$b("Key observations:"),
                                        tags$ul(
                                          tags$li("All customers have some churn risk."),
                                          tags$li("Most customers are moderate to high risk — distribution is right-skewed."),
                                          tags$li("Churn probability is capped at 0.5.")
                                        )
                                    )
                          )
                        )
               ),
               
               # ── EDA 2: Correlation Heatmap ──
               tabPanel("Correlation Heatmap",
                        br(),
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       div(class = "section-title", "Variable Selection"),
                                       hr(),
                                       checkboxGroupInput("corr_vars_selected", label = "Select variables:",
                                                          choices = corr_vars, selected = corr_vars),
                                       hr(),
                                       div(class = "info-text",
                                           "Select or deselect variables to update the correlation heatmap.")
                          ),
                          mainPanel(width = 9,
                                    div(class = "info-text",
                                        "Pairwise correlations between selected numeric variables."),
                                    plotOutput("corr_heatmap", height = "520px"),
                                    div(class = "obs-box",
                                        tags$b("Key observations:"),
                                        tags$ul(
                                          tags$li("Strongest positive correlation: active products vs feature usage diversity."),
                                          tags$li("Age is positively correlated with churn probability."),
                                          tags$li("Base satisfaction and overall satisfaction score are highly correlated.")
                                        )
                                    )
                          )
                        )
               ),
               
               # ── EDA 3: Churn vs Age ──
               tabPanel("Churn vs Age",
                        br(),
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       div(class = "section-title", "Filters"),
                                       hr(),
                                       checkboxGroupInput("income_filter", label = "Income brackets:",
                                                          choices  = c("Low", "Medium", "High", "Very High"),
                                                          selected = c("Low", "Medium", "High", "Very High")),
                                       hr(),
                                       checkboxGroupInput("gender_filter", label = "Genders:",
                                                          choices  = c("Male", "Female", "Other"),
                                                          selected = c("Male", "Female", "Other")),
                                       hr(),
                                       div(class = "info-text",
                                           "Filter to compare churn-age trends across groups.")
                          ),
                          mainPanel(width = 9,
                                    div(class = "info-text",
                                        "Churn probability vs age, faceted by income bracket. Each line represents a gender group."),
                                    plotlyOutput("churn_age", height = "420px"),
                                    div(class = "obs-box",
                                        tags$b("Key observations:"),
                                        tags$ul(
                                          tags$li("Age is positively correlated with churn across every group."),
                                          tags$li("Effect is stronger at higher income levels, especially for 'Other' gender."),
                                          tags$li("Male and female trends are similar at lower income levels.")
                                        )
                                    )
                          )
                        )
               ),
               
               # ── EDA 4: Satisfaction Analysis ──
               tabPanel("Satisfaction Analysis",
                        br(),
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       div(class = "section-title", "Options"),
                                       hr(),
                                       checkboxGroupInput("clv_filter", label = "CLV segments:",
                                                          choices  = c("Bronze", "Silver", "Gold", "Platinum"),
                                                          selected = c("Bronze", "Silver", "Gold", "Platinum")),
                                       hr(),
                                       radioButtons("sat_chart", label = "Chart view:",
                                                    choices = c(
                                                      "Satisfaction vs Churn (scatter)" = "scatter",
                                                      "Satisfaction over Tenure (line)"  = "line"
                                                    ),
                                                    selected = "scatter"),
                                       hr(),
                                       div(class = "info-text", "Toggle between scatter and line chart views.")
                          ),
                          mainPanel(width = 9,
                                    plotlyOutput("sat_plot", height = "450px"),
                                    div(class = "obs-box",
                                        tags$b("Key observations:"),
                                        tags$ul(
                                          tags$li("Product satisfaction appears categorical and does not align with other measures."),
                                          tags$li("CLV categories overlap completely in the scatter plot."),
                                          tags$li("Bronze customer satisfaction trends downward over time.")
                                        )
                                    )
                          )
                        )
               )
             )
    ),
    
    # ══ TAB 2: CDA ════════════════════════════════════════════════════════════
    tabPanel("Confirmatory Data Analysis",
             br(),
             tabsetPanel(
               
               # ── CDA 1: Correlation & Regression ──
               tabPanel("Correlation & Regression",
                        br(),
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       div(class = "section-title", "Regression Variables"),
                                       hr(),
                                       checkboxGroupInput("reg_vars",
                                                          label = "Predictors for regression model:",
                                                          choices = c(
                                                            "Age"               = "age",
                                                            "Base satisfaction" = "base_satisfaction",
                                                            "Active products"   = "active_products",
                                                            "Customer tenure"   = "customer_tenure",
                                                            "Feature diversity" = "feature_usage_diversity",
                                                            "TX count"          = "tx_count"
                                                          ),
                                                          selected = c("age", "base_satisfaction", "active_products")),
                                       hr(),
                                       div(class = "info-text",
                                           "Select predictors to update the regression model of churn probability.")
                          ),
                          mainPanel(width = 9,
                                    fluidRow(
                                      column(4, div(class = "metric-box",
                                                    div(class = "metric-label", "Age vs Churn — r"),
                                                    div(class = "metric-value blue", textOutput("cor_age")),
                                                    div(class = "metric-note", "Pearson correlation")
                                      )),
                                      column(4, div(class = "metric-box",
                                                    div(class = "metric-label", "Products vs Features — r"),
                                                    div(class = "metric-value blue", textOutput("cor_prod")),
                                                    div(class = "metric-note", "Pearson correlation")
                                      )),
                                      column(4, div(class = "metric-box",
                                                    div(class = "metric-label", "Satisfaction vs Score — r"),
                                                    div(class = "metric-value blue", textOutput("cor_sat")),
                                                    div(class = "metric-note", "Pearson correlation")
                                      ))
                                    ),
                                    hr(),
                                    div(class = "info-text",
                                        "Coefficient plot — points right of 0 increase churn, left of 0 decrease it."),
                                    plotlyOutput("reg_plot", height = "350px"),
                                    div(class = "metric-box", style = "margin-top:12px;",
                                        div(class = "metric-label", "Model R²"),
                                        div(class = "metric-value", textOutput("r_squared")),
                                        div(class = "metric-note",
                                            "Low R² indicates other factors also explain churn.")
                                    ),
                                    div(class = "obs-box",
                                        tags$b("Key observations:"),
                                        tags$ul(
                                          tags$li("Age increases churn probability."),
                                          tags$li("More products reduces churn probability."),
                                          tags$li("Higher satisfaction reduces churn probability.")
                                        )
                                    )
                          )
                        )
               ),
               
               # ── CDA 2: ANOVA ──
               tabPanel("ANOVA Tests",
                        br(),
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       div(class = "section-title", "Test Selection"),
                                       hr(),
                                       radioButtons("anova_test", label = "Select ANOVA test:",
                                                    choices = c(
                                                      "Churn ~ Income (one-way)"         = "income",
                                                      "Churn ~ Gender (one-way)"          = "gender",
                                                      "Churn ~ Income × Gender (two-way)" = "interaction",
                                                      "Churn ~ CLV (one-way)"             = "clv_churn",
                                                      "Satisfaction ~ CLV (one-way)"      = "clv_sat"
                                                    ),
                                                    selected = "income"),
                                       hr(),
                                       div(class = "info-text",
                                           "Select a test to view the ANOVA summary with F-statistic and p-value.")
                          ),
                          mainPanel(width = 9,
                                    fluidRow(
                                      column(4, div(class = "metric-box",
                                                    div(class = "metric-label", "F-statistic"),
                                                    div(class = "metric-value blue", textOutput("f_stat")),
                                                    div(class = "metric-note", "Higher = more significant")
                                      )),
                                      column(4, div(class = "metric-box",
                                                    div(class = "metric-label", "p-value"),
                                                    div(class = "metric-value", textOutput("p_val")),
                                                    div(class = "metric-note", "< 0.05 = statistically significant")
                                      )),
                                      column(4, div(class = "metric-box",
                                                    div(class = "metric-label", "Significant?"),
                                                    div(class = "metric-value green", textOutput("sig_result")),
                                                    div(class = "metric-note", "At 0.05 level")
                                      ))
                                    ),
                                    hr(),
                                    div(class = "section-title", "ANOVA Summary Table"),
                                    verbatimTextOutput("anova_table"),
                                    div(class = "obs-box", style = "margin-top:12px;",
                                        tags$b("Key observations:"),
                                        tags$ul(
                                          tags$li("Gender is not significant for churn prediction (p > 0.05)."),
                                          tags$li("Income bracket is statistically significant but a weak predictor."),
                                          tags$li("CLV predicts satisfaction score but not churn probability.")
                                        )
                                    )
                          )
                        )
               )
             )
    )
  )
)

# ── SERVER ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # EDA 1: Churn histogram
  output$churn_hist <- renderPlotly({
    mean_val <- mean(customer_clean$churn_probability, na.rm = TRUE)
    p <- ggplot(customer_clean, aes(x = churn_probability)) +
      geom_histogram(binwidth = input$binwidth, fill = "#bfdbfe", colour = "white") +
      geom_vline(xintercept = mean_val, colour = "#2563eb",
                 linetype = "dashed", linewidth = 0.8) +
      annotate("text", x = mean_val + 0.015, y = 500,
               label = paste0("Mean: ", round(mean_val, 3)),
               colour = "#2563eb", size = 3.5) +
      labs(title = "Distribution of churn probability",
           x = "Churn probability", y = "Count") +
      theme_minimal(base_size = 12)
    ggplotly(p)
  })
  
  # EDA 2: Correlation heatmap
  output$corr_heatmap <- renderPlot({
    req(length(input$corr_vars_selected) >= 2)
    corrmatrix <- cor(customer_clean[, input$corr_vars_selected],
                      use = "pairwise.complete.obs")
    ggcorrplot(corrmatrix, method = "square", type = "lower",
               lab = TRUE, lab_size = 2.5, tl.cex = 9,
               colors = c("#1e3a8a", "#f8fafc", "#2563eb"),
               title = "Correlation Heatmap") +
      theme_minimal() +
      theme(plot.title  = element_text(size = 13, face = "bold", hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
            axis.text.y = element_text(size = 8),
            axis.title  = element_blank(),
            plot.margin = margin(20, 20, 20, 20))
  })
  
  # EDA 3: Churn vs Age
  output$churn_age <- renderPlotly({
    df_filtered <- customer_clean %>%
      filter(income_bracket %in% input$income_filter,
             gender %in% input$gender_filter) %>%
      mutate(income_bracket = factor(income_bracket,
                                     levels = c("Low", "Medium", "High", "Very High")))
    p <- df_filtered %>%
      ggplot(aes(x = age, y = churn_probability, colour = gender)) +
      geom_smooth(method = "lm", se = FALSE) +
      facet_wrap(~ income_bracket) +
      scale_colour_manual(values = c("Male" = "#2563eb",
                                     "Female" = "#60a5fa",
                                     "Other"  = "#1e3a8a")) +
      labs(title = "Churn probability vs age, by gender and income bracket",
           x = "Age", y = "Churn probability", colour = "Gender") +
      theme_minimal(base_size = 12)
    ggplotly(p)
  })
  
  # EDA 4: Satisfaction
  output$sat_plot <- renderPlotly({
    df_sat <- customer_clean %>%
      filter(clv_segment %in% input$clv_filter) %>%
      mutate(clv_segment = factor(clv_segment,
                                  levels = c("Bronze", "Silver", "Gold", "Platinum")))
    if (input$sat_chart == "scatter") {
      p <- df_sat %>%
        select(clv_segment, base_satisfaction, tx_satisfaction,
               product_satisfaction, churn_probability) %>%
        pivot_longer(cols = c(base_satisfaction, tx_satisfaction, product_satisfaction),
                     names_to = "satisfaction_type", values_to = "score") %>%
        mutate(satisfaction_type = recode(satisfaction_type,
                                          "base_satisfaction" = "Base", "tx_satisfaction" = "Transaction",
                                          "product_satisfaction" = "Product")) %>%
        ggplot(aes(x = score, y = churn_probability, colour = clv_segment)) +
        geom_point(alpha = 0.05, size = 0.6) +
        geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
        scale_colour_manual(values = clv_colours) +
        facet_wrap(~ satisfaction_type, ncol = 3, scales = "free_x") +
        labs(title = "Satisfaction vs Churn by CLV", x = "Satisfaction score",
             y = "Churn probability", colour = "CLV segment") +
        theme_minimal(base_size = 11)
    } else {
      p <- df_sat %>%
        group_by(tenure_bucket, clv_segment) %>%
        summarise(avg_satisfaction = mean(satisfaction_score, na.rm = TRUE),
                  .groups = "drop") %>%
        ggplot(aes(x = tenure_bucket, y = avg_satisfaction,
                   colour = clv_segment, group = clv_segment)) +
        geom_line(linewidth = 1) +
        geom_point(size = 2.5) +
        scale_colour_manual(values = clv_colours) +
        labs(title = "Satisfaction over Tenure by CLV", x = "Tenure",
             y = "Avg satisfaction score", colour = "CLV segment") +
        theme_minimal(base_size = 12)
    }
    ggplotly(p)
  })
  
  # CDA: Pearson correlations
  output$cor_age  <- renderText({
    round(cor.test(customer_clean$age, customer_clean$churn_probability)$estimate, 3)
  })
  output$cor_prod <- renderText({
    round(cor.test(customer_clean$active_products,
                   customer_clean$feature_usage_diversity)$estimate, 3)
  })
  output$cor_sat  <- renderText({
    round(cor.test(customer_clean$base_satisfaction,
                   customer_clean$satisfaction_score)$estimate, 3)
  })
  
  # CDA: Regression
  churn_model <- reactive({
    req(length(input$reg_vars) >= 1)
    lm(as.formula(paste("churn_probability ~",
                        paste(input$reg_vars, collapse = " + "))),
       data = customer_clean)
  })
  
  output$r_squared <- renderText({
    round(summary(churn_model())$r.squared, 3)
  })
  
  output$reg_plot <- renderPlotly({
    df_coef <- tidy(churn_model(), conf.int = TRUE) %>%
      filter(term != "(Intercept)")
    p <- ggplot(df_coef, aes(x = estimate, y = term,
                             xmin = conf.low, xmax = conf.high)) +
      geom_pointrange(colour = "#2563eb") +
      geom_vline(xintercept = 0, linetype = "dashed", colour = "#0f1e35") +
      labs(title    = "Churn Probability Predictors",
           subtitle = paste("R² =", round(summary(churn_model())$r.squared, 3)),
           x = "Coefficient estimate", y = NULL) +
      theme_minimal(base_size = 12) +
      theme(plot.title    = element_text(size = 13, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(size = 10, hjust = 0.5, colour = "#64748b"))
    ggplotly(p)
  })
  
  # CDA: ANOVA
  anova_result <- reactive({
    switch(input$anova_test,
           "income"      = aov(churn_probability ~ income_bracket, data = customer_clean),
           "gender"      = aov(churn_probability ~ gender,         data = customer_clean),
           "interaction" = aov(churn_probability ~ income_bracket * gender,
                               data = customer_clean),
           "clv_churn"   = aov(churn_probability ~ clv_segment,    data = customer_clean),
           "clv_sat"     = aov(satisfaction_score ~ clv_segment,   data = customer_clean)
    )
  })
  
  output$anova_table  <- renderPrint({ summary(anova_result()) })
  output$f_stat       <- renderText({
    round(summary(anova_result())[[1]][["F value"]][1], 2)
  })
  output$p_val        <- renderText({
    p <- summary(anova_result())[[1]][["Pr(>F)"]][1]
    ifelse(p < 0.001, "< 0.001", round(p, 4))
  })
  output$sig_result   <- renderText({
    p <- summary(anova_result())[[1]][["Pr(>F)"]][1]
    ifelse(p < 0.05, "Yes ✓", "No ✗")
  })
}

shinyApp(ui = ui, server = server)
    