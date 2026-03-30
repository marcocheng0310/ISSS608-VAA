library(shiny)
library(poLCA)
library(ggplot2)
library(plotly)
library(tidyverse)

df_clustering     <- readRDS("df_clustering.rds")
lca_model_default <- readRDS("lca_model.rds")

# ── Variable choices ──────────────────────────────────────────────────────────
var_choices <- c(
  "Income Bracket"         = "income_cat",
  "CLV Segment"            = "clv_cat",
  "Credit Card"            = "credit_card",
  "Investment Account"     = "investment_account",
  "Personal Loan"          = "personal_loan",
  "Insurance Product"      = "insurance_product",
  "Bill Payment User"      = "bill_payment_user",
  "Auto Savings"           = "auto_savings",
  "App Login Frequency"    = "login_cat",
  "Feature Usage"          = "feature_cat",
  "Transaction Count"      = "tx_count_cat",
  "Transaction Frequency"  = "tx_freq_cat",
  "Weekend Transactions"   = "weekend_cat",
  "Avg Transaction Value"  = "avg_tx_cat",
  "Satisfaction Score"     = "satisfaction_cat",
  "NPS Score"              = "nps_cat",
  "Sentiment"              = "sentiment_cat",
  "Churn Risk"             = "churn_cat",
  "Customer Segment (ref)" = "segment_cat"
)

# ── Helper: recode binary numeric vars for display ────────────────────────────
recode_binary <- function(df, var) {
  if (var == "credit_card")        df <- df %>% mutate(credit_card        = ifelse(credit_card == 2,        "Has Credit Card",   "No Credit Card"))
  if (var == "investment_account") df <- df %>% mutate(investment_account = ifelse(investment_account == 2, "Has Investment",    "No Investment"))
  if (var == "personal_loan")      df <- df %>% mutate(personal_loan      = ifelse(personal_loan == 2,      "Has Loan",          "No Loan"))
  if (var == "insurance_product")  df <- df %>% mutate(insurance_product  = ifelse(insurance_product == 2,  "Has Insurance",     "No Insurance"))
  if (var == "bill_payment_user")  df <- df %>% mutate(bill_payment_user  = ifelse(bill_payment_user == 2,  "Bill Payment User", "Not User"))
  if (var == "auto_savings")       df <- df %>% mutate(auto_savings       = ifelse(auto_savings == 2,       "Auto Savings On",   "Auto Savings Off"))
  df
}

# ── Helper: compute entropy ───────────────────────────────────────────────────
compute_entropy <- function(model) {
  entropy_fn  <- function(p) sum(-p * log(p))
  error_prior <- entropy_fn(model$P)
  error_post  <- mean(apply(model$posterior, c(1, 2), entropy_fn), na.rm = TRUE)
  round((error_prior - error_post) / error_prior, 3)
}

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  
  tags$head(tags$style(HTML("
    .well { background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px; }
    .metric-box { background: #fff; border: 1px solid #e2e8f0; border-radius: 8px;
                  padding: 14px 16px; margin-bottom: 12px; }
    .metric-label { font-size: 10px; font-weight: 700; text-transform: uppercase;
                    color: #94a3b8; letter-spacing: 0.1em; margin-bottom: 4px; }
    .metric-value { font-size: 22px; font-weight: 700; color: #0f1e35; }
    .metric-value.green { color: #16a34a; }
    .metric-note  { font-size: 10px; color: #94a3b8; margin-top: 2px; }
    .section-title { font-size: 15px; font-weight: 700; color: #0f1e35; margin: 16px 0 6px; }
    .info-text { font-size: 13px; color: #64748b; margin-bottom: 12px; }
    hr { border-color: #e2e8f0; }
  "))),
  
  titlePanel(
    div(style = "color:#0f1e35; font-weight:700;",
        "Clustering Analysis",
        span(" — Colombian Fintech Customers",
             style = "font-weight:400; color:#64748b; font-size:16px;"))
  ),
  
  sidebarLayout(
    
    # ── SIDEBAR ──
    sidebarPanel(
      width = 3,
      
      div(class = "section-title", "Model Parameters"),
      hr(),
      
      sliderInput("nclass",
                  label = "Number of Classes",
                  min = 2, max = 7, value = 5, step = 1),
      
      sliderInput("nrep",
                  label = "Repetitions (nrep)",
                  min = 1, max = 5, value = 5, step = 1),
      
      hr(),
      
      selectInput("plot_var",
                  label = "Variable to Visualise",
                  choices  = var_choices,
                  selected = "income_cat"),
      
      hr(),
      
      actionButton("run_lca", "▶  Run Model",
                   class = "btn-primary btn-block",
                   style = "font-weight:600; width:100%;"),
      
      br(),
      div(class = "info-text",
          "⚡ Default view loads instantly from a pre-saved model.",
          br(), br(),
          "Changing nclass or nrep and clicking Run Model will re-fit — this takes 1–2 minutes.")
    ),
    
    # ── MAIN ──
    mainPanel(
      width = 9,
      
      # Metric row
      fluidRow(
        column(3,
               div(class = "metric-box",
                   div(class = "metric-label", "BIC"),
                   div(class = "metric-value", textOutput("bic_val")),
                   div(class = "metric-note", "Lower is better")
               )
        ),
        column(3,
               div(class = "metric-box",
                   div(class = "metric-label", "AIC"),
                   div(class = "metric-value", textOutput("aic_val")),
                   div(class = "metric-note", "Lower is better")
               )
        ),
        column(3,
               div(class = "metric-box",
                   div(class = "metric-label", "Entropy"),
                   div(class = "metric-value green", textOutput("entropy_val")),
                   div(class = "metric-note", "Closer to 1 is better")
               )
        ),
        column(3,
               div(class = "metric-box",
                   div(class = "metric-label", "Class Shares"),
                   verbatimTextOutput("class_shares")
               )
        )
      ),
      
      hr(),
      
      # Charts
      tabsetPanel(
        tabPanel("Class on x-axis",
                 br(),
                 div(class = "info-text",
                     "Each bar shows the composition of a latent class by the selected variable."),
                 plotlyOutput("fig1", height = "400px")
        ),
        tabPanel("Variable on x-axis",
                 br(),
                 div(class = "info-text",
                     "Each bar shows how each category of the selected variable is distributed across latent classes."),
                 plotlyOutput("fig2", height = "400px")
        )
      ),
      
      hr(),
      
      # Class interpretation
      div(class = "section-title", "Class Interpretation"),
      div(class = "info-text",
          "Based on conditional item response probabilities from the LCA model.
         Entropy of 0.931 indicates well-separated and clearly defined classes."),
      
      tags$ul(style = "font-size:13px; line-height:1.8;",
              tags$li(tags$b("Class 1 — Mainstream Mid-Income Savers (36.2%): "),
                      "Largest segment. High savings and credit card adoption, lower investment uptake. Medium churn risk."),
              tags$li(tags$b("Class 2 — Full-Product Power Users (21.4%): "),
                      "Near-universal product adoption. Highest feature diversity. Most engaged multi-product customers."),
              tags$li(tags$b("Class 3 — Single-Feature Minimal Users (17.4%): "),
                      "100% use exactly one app feature. Lowest product adoption. Strongest cross-sell opportunity."),
              tags$li(tags$b("Class 4 — Dissatisfied At-Risk Customers (5.9%): "),
                      "100% low satisfaction, 84% NPS Detractors. Highest churn risk. Predominantly Gold CLV — urgent retention priority."),
              tags$li(tags$b("Class 5 — High-Volume Transaction Users (19.1%): "),
                      "50% Very High transaction count. Predominantly Gold CLV. Payment-focused, underutilise wealth products.")
      )
    )
  )
)

# ── SERVER ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # Store current model — starts with pre-saved default
  current_model <- reactiveVal(lca_model_default)
  
  # Re-run only when button is clicked
  observeEvent(input$run_lca, {
    
    withProgress(message = "Running LCA model — please wait...", value = 0, {
      
      set.seed(1234)
      
      f <- as.formula(cbind(
        income_cat, clv_cat,
        savings_account, credit_card, personal_loan,
        investment_account, insurance_product,
        bill_payment_user, auto_savings,
        login_cat, feature_cat,
        tx_count_cat, tx_freq_cat, weekend_cat,
        satisfaction_cat, nps_cat, sentiment_cat,
        churn_cat
      ) ~ 1)
      
      incProgress(0.3, message = "Fitting model...")
      
      model <- poLCA(f, df_clustering,
                     nclass  = input$nclass,
                     nrep    = input$nrep,
                     maxiter = 5000,
                     verbose = FALSE)
      
      incProgress(1, message = "Done!")
      current_model(model)
    })
  })
  
  # Dataset with predicted class appended
  df_with_class <- reactive({
    req(current_model())
    df_clustering %>%
      mutate(class = factor(current_model()$predclass))
  })
  
  # ── Metrics ──
  output$bic_val <- renderText({
    req(current_model())
    format(round(current_model()$bic), big.mark = ",")
  })
  
  output$aic_val <- renderText({
    req(current_model())
    format(round(current_model()$aic), big.mark = ",")
  })
  
  output$entropy_val <- renderText({
    req(current_model())
    compute_entropy(current_model())
  })
  
  output$class_shares <- renderText({
    req(current_model())
    shares <- round(current_model()$P * 100, 1)
    paste(paste0("C", seq_along(shares), ": ", shares, "%"), collapse = "\n")
  })
  
  # ── Chart 1: class on x, variable as fill ──
  output$fig1 <- renderPlotly({
    req(df_with_class())
    var       <- input$plot_var
    df_plot   <- recode_binary(df_with_class(), var)
    var_label <- names(var_choices)[var_choices == var]
    
    plot_table <- df_plot %>%
      group_by(class, .data[[var]]) %>%
      summarise(counts = n(), .groups = "drop")
    
    p <- ggplot(plot_table,
                aes(fill = .data[[var]], y = counts, x = class)) +
      geom_bar(position = "fill", stat = "identity") +
      labs(x = "Latent Class", y = "Proportion",
           fill = var_label,
           title = paste("Class distribution by", var_label)) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
    
    ggplotly(p) %>%
      layout(legend = list(orientation = "h", y = -0.25))
  })
  
  # ── Chart 2: variable on x, class as fill ──
  output$fig2 <- renderPlotly({
    req(df_with_class())
    var       <- input$plot_var
    df_plot   <- recode_binary(df_with_class(), var)
    var_label <- names(var_choices)[var_choices == var]
    
    plot_table <- df_plot %>%
      group_by(.data[[var]], class) %>%
      summarise(counts = n(), .groups = "drop")
    
    p <- ggplot(plot_table,
                aes(fill = class, y = counts, x = .data[[var]])) +
      geom_bar(position = "fill", stat = "identity") +
      labs(x = var_label, y = "Proportion",
           fill = "Class",
           title = paste(var_label, "distribution by class")) +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1),
            legend.position = "bottom")
    
    ggplotly(p) %>%
      layout(legend = list(orientation = "h", y = -0.3))
  })
}

# ── Run ───────────────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)

