# ============================================================================
# FinSight — Decision Tree & Random Forest (Standalone Shiny App)
# ISSS608 Visual Analytics Group Project
#
# Required files in the same folder:
#   - customer_data.csv
#   - lca_model.rds (from poLCA Clustering app)
# ============================================================================

library(shiny)
library(rpart)
library(rpart.plot)
library(ranger)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(DT)

# ══════════════════════════════════════════════════════════════════════════════
# DATA PREPARATION
# ══════════════════════════════════════════════════════════════════════════════

prepare_data <- function(filepath) {
  df <- read.csv(filepath, stringsAsFactors = FALSE)
  
  product_cols <- c("savings_account", "credit_card", "personal_loan",
                    "investment_account", "insurance_product")
  for (pc in product_cols) df[[pc]] <- as.integer(as.logical(df[[pc]]))
  
  df$active_products     <- rowSums(df[, product_cols], na.rm = TRUE)
  df$has_feature_request <- as.integer(!is.na(df$feature_requests) & df$feature_requests != "")
  df$has_complaint       <- as.integer(!is.na(df$complaint_topics)  & df$complaint_topics  != "")
  df$credit_utilization_ratio[is.na(df$credit_utilization_ratio)] <- 0
  
  logical_cols <- c("bill_payment_user", "auto_savings_enabled")
  for (lc in logical_cols) {
    if (lc %in% names(df)) df[[lc]] <- as.integer(as.logical(df[[lc]]))
  }
  
  cat_cols <- c("gender", "income_bracket", "education_level", "marital_status",
                "acquisition_channel", "preferred_transaction_type", "feedback_sentiment")
  for (col in cat_cols) {
    if (col %in% names(df)) df[[col]] <- as.factor(df[[col]])
  }
  
  base_features <- c(
    "age", "gender", "income_bracket", "education_level",
    "marital_status", "household_size",
    "app_logins_frequency", "feature_usage_diversity",
    "bill_payment_user", "auto_savings_enabled",
    "tx_count", "avg_tx_value", "transaction_frequency",
    "weekend_transaction_ratio", "customer_tenure",
    "satisfaction_score", "nps_score", "base_satisfaction",
    "support_tickets_count", "resolved_tickets_ratio",
    "has_complaint", "has_feature_request", "credit_utilization_ratio"
  )
  
  attr(df, "features_A") <- c("active_products", base_features)
  attr(df, "features_B") <- base_features
  attr(df, "target")     <- "churn_probability"
  return(df)
}

load_lca_classes <- function(rds_path = "lca_model.rds") {
  if (!file.exists(rds_path)) return(NULL)
  tryCatch({
    model <- readRDS(rds_path)
    as.integer(model$predclass)
  }, error = function(e) NULL)
}

extract_rules <- function(model) {
  frame  <- model$frame
  leaves <- frame[frame$var == "<leaf>", ]
  if (nrow(leaves) == 0)
    return(data.frame(Rule = "Tree has no leaves", Prediction = NA, N = NA))
  paths <- path.rpart(model, nodes = as.integer(rownames(leaves)), print.it = FALSE)
  rules_df <- data.frame(
    Node       = as.integer(names(paths)),
    Rule       = sapply(paths, function(p) paste(p[-1], collapse = " & ")),
    Prediction = round(leaves$yval, 4),
    N          = leaves$n,
    stringsAsFactors = FALSE
  )
  rules_df <- rules_df[order(-rules_df$Prediction), ]
  rules_df$Rank     <- seq_len(nrow(rules_df))
  rules_df$Coverage <- paste0(round(rules_df$N / sum(rules_df$N) * 100, 1), "%")
  rules_df[, c("Rank", "Rule", "Prediction", "N", "Coverage")]
}

compute_metrics <- function(actual, predicted) {
  residuals <- actual - predicted
  ss_res <- sum(residuals^2)
  ss_tot <- sum((actual - mean(actual))^2)
  data.frame(
    Metric = c("R\u00B2", "RMSE", "MAE"),
    Value  = c(
      round(1 - ss_res / ss_tot, 4),
      round(sqrt(mean(residuals^2)), 4),
      round(mean(abs(residuals)), 4)
    )
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════════════════════

ui <- fluidPage(
  
  tags$head(tags$style(HTML("
    .well { background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px; }
    .metric-box { background: #fff; border: 1px solid #e2e8f0; border-radius: 8px;
                  padding: 14px 16px; margin-bottom: 12px; }
    .metric-label { font-size: 10px; font-weight: 700; text-transform: uppercase;
                    color: #94a3b8; letter-spacing: 0.1em; margin-bottom: 4px; }
    .metric-value { font-size: 22px; font-weight: 700; color: #0f1e35; }
    .metric-note  { font-size: 10px; color: #94a3b8; margin-top: 2px; }
    .section-title { font-size: 15px; font-weight: 700; color: #0f1e35; margin: 16px 0 6px; }
    .info-text { font-size: 13px; color: #64748b; margin-bottom: 12px; }
    .status-ready { background: #dcfce7; border: 1px solid #bbf7d0; color: #15803d;
                    padding: 10px 14px; border-radius: 6px; margin-bottom: 14px; font-size: 13px; }
    .status-waiting { background: #f1f5f9; border: 1px solid #e2e8f0; color: #64748b;
                      padding: 10px 14px; border-radius: 6px; margin-bottom: 14px; font-size: 13px; }
    .lca-box { background: #eff6ff; border: 1px solid #bfdbfe; border-radius: 6px;
               padding: 10px 12px; margin-top: 8px; }
    hr { border-color: #e2e8f0; }
  "))),
  
  titlePanel(
    div(style = "color:#0f1e35; font-weight:700;",
        "Decision Tree & Random Forest",
        span(" — Colombian Fintech Customers",
             style = "font-weight:400; color:#64748b; font-size:16px;"))
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      
      div(class = "section-title", "Model Configuration"),
      hr(),
      
      radioButtons("dt_segment", "Segment",
                   choices  = c("Global" = "global", "Per-LCA Class" = "lca"),
                   selected = "global", inline = TRUE),
      
      conditionalPanel(
        condition = "input.dt_segment == 'lca'",
        uiOutput("dt_lca_class_ui")
      ),
      
      hr(),
      
      radioButtons("dt_model_variant", "Feature Set",
                   choices = c(
                     "Model A (with active_products)"    = "A",
                     "Model B (without active_products)" = "B"
                   ),
                   selected = "A"),
      
      hr(),
      
      div(class = "section-title", "rpart Hyperparameters"),
      
      sliderInput("dt_cp",       "Complexity Parameter (cp)",
                  min = 0.0001, max = 0.05, value = 0.01, step = 0.0001),
      sliderInput("dt_maxdepth", "Max Depth",
                  min = 2, max = 15, value = 6, step = 1),
      sliderInput("dt_minsplit", "Min Split",
                  min = 10, max = 200, value = 20, step = 10),
      
      conditionalPanel(
        condition = "input.dt_segment == 'global'",
        hr(),
        div(class = "section-title", "ranger Benchmark"),
        sliderInput("dt_num_trees", "Number of Trees",
                    min = 50, max = 500, value = 100, step = 50),
        div(class = "info-text",
            "\u26a1 Keep trees low (50\u2013100) to avoid memory limits on shinyapps.io.")
      ),
      
      hr(),
      
      sliderInput("dt_split",  "Training Set Ratio",
                  min = 0.5, max = 0.9, value = 0.7, step = 0.05),
      
      sliderInput("dt_sample", "Max rows for modelling (000s)",
                  min = 5, max = 48, value = 10, step = 1),
      
      hr(),
      
      actionButton("dt_run", "\u25b6  Run Model",
                   class = "btn-primary",
                   style = "font-weight:600; width:100%;"),
      
      br(), br(),
      
      div(class = "lca-box",
          div(style = "font-size:11px; font-weight:700; color:#1d4ed8; margin-bottom:6px;",
              "LCA Data"),
          uiOutput("lca_file_status"),
          br(),
          actionButton("reload_lca", "Reload lca_model.rds",
                       class = "btn-default",
                       style = "font-size:11px; width:100%;")
      )
    ),
    
    mainPanel(
      width = 9,
      
      uiOutput("dt_status_msg"),
      
      tabsetPanel(
        id = "dt_tabs", type = "tabs",
        
        tabPanel("Tree Plot",
                 br(),
                 div(class = "info-text", textOutput("dt_tree_title")),
                 plotOutput("dt_tree_plot", height = "550px"),
                 br(),
                 verbatimTextOutput("dt_tree_summary")
        ),
        
        tabPanel("Evaluation",
                 br(),
                 fluidRow(
                   column(6,
                          div(class = "section-title", "rpart \u2014 Regression Tree"),
                          tableOutput("dt_rpart_metrics"),
                          br(),
                          div(class = "info-text", "Actual vs Predicted \u2014 rpart"),
                          plotlyOutput("dt_rpart_scatter", height = "350px")
                   ),
                   column(6, uiOutput("dt_ranger_panel"))
                 )
        ),
        
        tabPanel("Variable Importance",
                 br(),
                 fluidRow(
                   column(6,
                          div(class = "section-title", "rpart \u2014 Variable Importance"),
                          plotlyOutput("dt_rpart_vimp", height = "450px")
                   ),
                   column(6, uiOutput("dt_ranger_vimp_panel"))
                 )
        ),
        
        tabPanel("Decision Rules",
                 br(),
                 div(class = "section-title",
                     "Interpretable Decision Paths (sorted by churn risk)"),
                 div(class = "info-text",
                     "Each row represents a leaf node path from root to terminal node."),
                 DTOutput("dt_rules_table")
        )
      )
    )
  )
)

# ══════════════════════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════════════════════

server <- function(input, output, session) {
  
  full_data   <- reactive({ prepare_data("customer_data.csv") })
  lca_classes <- reactiveVal(load_lca_classes())
  
  observeEvent(input$reload_lca, {
    classes <- load_lca_classes()
    lca_classes(classes)
    if (!is.null(classes)) {
      showNotification(
        paste0("LCA reloaded: ", length(unique(classes)), " classes, n=", length(classes)),
        type = "message", duration = 3)
    } else {
      showNotification("lca_model.rds not found.", type = "error")
    }
  })
  
  output$lca_file_status <- renderUI({
    classes <- lca_classes()
    if (!is.null(classes)) {
      div(style = "font-size:11px; color:#15803d;",
          paste0("\u2713 Loaded: ", length(unique(classes)),
                 " classes, ", length(classes), " observations"))
    } else {
      div(style = "font-size:11px; color:#b45309;",
          "\u26a0 lca_model.rds not found. Per-LCA mode unavailable.")
    }
  })
  
  output$dt_lca_class_ui <- renderUI({
    classes <- lca_classes()
    if (is.null(classes)) {
      div(class = "info-text",
          style = "color:#dc2626; background:#fef2f2; border:1px solid #fecaca;
                   padding:8px 10px; border-radius:6px;",
          "\u26a0 lca_model.rds not found. Run the Clustering app first, then click Reload.")
    } else {
      unique_classes <- sort(unique(classes))
      choices <- setNames(unique_classes, paste("Class", unique_classes))
      selectInput("dt_lca_class_select", "Select LCA Class",
                  choices = choices, selected = unique_classes[1])
    }
  })
  
  dt_model_data <- reactive({
    df      <- full_data()
    classes <- lca_classes()
    if (!is.null(classes) && length(classes) == nrow(df))
      df$lca_class <- as.factor(classes)
    df
  })
  
  dt_results <- reactiveVal(NULL)
  
  observeEvent(input$dt_run, {
    
    df      <- dt_model_data()
    segment <- input$dt_segment
    variant <- input$dt_model_variant
    target  <- attr(full_data(), "target")
    
    features <- if (variant == "A") attr(full_data(), "features_A") else attr(full_data(), "features_B")
    
    if (segment == "global" && "lca_class" %in% names(df))
      features <- c(features, "lca_class")
    
    if (segment == "lca") {
      if (is.null(lca_classes())) {
        showNotification("LCA classes not available. Load lca_model.rds first.", type = "error")
        return()
      }
      selected_class <- as.integer(input$dt_lca_class_select)
      df <- df[df$lca_class == selected_class, ]
      features <- setdiff(features, "lca_class")
    }
    
    if (nrow(df) < 50) {
      showNotification("Too few observations for this segment.", type = "error")
      return()
    }
    
    model_cols <- intersect(c(target, features), names(df))
    df_model   <- df[, model_cols, drop = FALSE]
    df_model   <- df_model[complete.cases(df_model), ]
    
    # Cap rows to avoid memory crash on shinyapps.io
    max_rows <- input$dt_sample * 1000
    if (nrow(df_model) > max_rows) {
      set.seed(2025)
      df_model <- df_model[sample(nrow(df_model), max_rows), ]
    }
    
    set.seed(2025)
    n   <- nrow(df_model)
    idx <- sample(n, size = floor(n * input$dt_split))
    train <- df_model[idx, ]
    test  <- df_model[-idx, ]
    
    features_in_data <- intersect(features, names(train))
    fml <- as.formula(paste(target, "~", paste(features_in_data, collapse = " + ")))
    
    withProgress(message = "Fitting rpart tree...", value = 0.3, {
      rpart_model <- rpart(fml, data = train, method = "anova",
                           control = rpart.control(
                             cp = input$dt_cp, maxdepth = input$dt_maxdepth, minsplit = input$dt_minsplit))
      
      cp_table <- rpart_model$cptable
      if (nrow(cp_table) > 1) {
        min_idx   <- which.min(cp_table[, "xerror"])
        se_thresh <- cp_table[min_idx, "xerror"] + cp_table[min_idx, "xstd"]
        cp_1se    <- cp_table[cp_table[, "xerror"] <= se_thresh, "CP"][1]
        rpart_pruned <- prune(rpart_model, cp = cp_1se)
      } else {
        rpart_pruned <- rpart_model
      }
      
      rpart_pred    <- predict(rpart_pruned, newdata = test)
      rpart_metrics <- compute_metrics(test[[target]], rpart_pred)
    })
    
    ranger_model <- NULL; ranger_pred <- NULL; ranger_metrics <- NULL
    
    if (segment == "global") {
      withProgress(message = "Fitting ranger forest...", value = 0.6, {
        ranger_model   <- ranger(fml, data = train,
                                 num.trees = input$dt_num_trees,
                                 importance = "impurity", seed = 2025)
        ranger_pred    <- predict(ranger_model, data = test)$predictions
        ranger_metrics <- compute_metrics(test[[target]], ranger_pred)
      })
    }
    
    rules      <- extract_rules(rpart_pruned)
    rpart_vimp <- rpart_pruned$variable.importance
    
    rpart_vimp_df <- if (is.null(rpart_vimp) || length(rpart_vimp) == 0) {
      data.frame(Variable = "None", Importance = 0)
    } else {
      data.frame(Variable = names(rpart_vimp),
                 Importance = as.numeric(rpart_vimp)) %>% arrange(desc(Importance))
    }
    
    ranger_vimp_df <- if (!is.null(ranger_model)) {
      vimp <- ranger_model$variable.importance
      data.frame(Variable = names(vimp),
                 Importance = as.numeric(vimp)) %>% arrange(desc(Importance))
    } else NULL
    
    dt_results(list(
      segment        = segment, variant = variant,
      lca_class      = if (segment == "lca") input$dt_lca_class_select else NULL,
      n_train        = nrow(train), n_test = nrow(test),
      rpart_model    = rpart_pruned, rpart_pred = rpart_pred, rpart_metrics = rpart_metrics,
      ranger_model   = ranger_model, ranger_pred = ranger_pred, ranger_metrics = ranger_metrics,
      rules          = rules, rpart_vimp = rpart_vimp_df, ranger_vimp = ranger_vimp_df,
      test_actual    = test[[target]], features_used = features_in_data
    ))
    
    showNotification("Model fitted successfully!", type = "message", duration = 3)
  })
  
  # ── Outputs ────────────────────────────────────────────────────────────────
  
  output$dt_status_msg <- renderUI({
    res <- dt_results(); lca <- lca_classes()
    if (is.null(res)) {
      lca_note <- if (!is.null(lca))
        paste0(" \u00b7 LCA loaded (", length(unique(lca)), " classes)")
      else " \u00b7 LCA not loaded"
      div(class = "status-waiting",
          paste0("Configure parameters and click \u25b6 Run Model to start.", lca_note))
    } else {
      seg_label <- if (res$segment == "global") "Global" else paste("LCA Class", res$lca_class)
      var_label <- if (res$variant == "A") "Model A" else "Model B"
      div(class = "status-ready",
          paste0("\u2713 ", seg_label, " \u00b7 ", var_label,
                 " \u00b7 Train: ", res$n_train, " \u00b7 Test: ", res$n_test,
                 " \u00b7 Features: ", length(res$features_used)))
    }
  })
  
  output$dt_tree_title <- renderText({
    res <- dt_results()
    if (is.null(res)) return("Run model to see tree")
    seg <- if (res$segment == "global") "Global" else paste("LCA Class", res$lca_class)
    paste("Pruned Regression Tree \u2014", seg, "|",
          if (res$variant == "A") "Model A" else "Model B")
  })
  
  output$dt_tree_plot <- renderPlot({
    res <- dt_results(); req(res)
    rpart.plot(res$rpart_model, type = 4, extra = 101, fallen.leaves = TRUE,
               shadow.col = "gray80", branch.lty = 1, box.palette = "RdYlGn",
               main = "", cex = 0.75, roundint = FALSE)
  }, res = 120)
  
  output$dt_tree_summary <- renderPrint({
    res <- dt_results(); req(res)
    cat("\u2500\u2500 CP Table (cross-validation) \u2500\u2500\n\n")
    printcp(res$rpart_model)
  })
  
  output$dt_rpart_metrics <- renderTable({
    res <- dt_results(); req(res); res$rpart_metrics
  }, striped = TRUE, bordered = TRUE, width = "100%")
  
  output$dt_rpart_scatter <- renderPlotly({
    res <- dt_results(); req(res)
    p <- ggplot(data.frame(Actual = res$test_actual, Predicted = res$rpart_pred),
                aes(x = Actual, y = Predicted)) +
      geom_point(alpha = 0.15, size = 1, colour = "#2563eb") +
      geom_abline(slope = 1, intercept = 0, colour = "#e11d48",
                  linetype = "dashed", linewidth = 0.8) +
      labs(x = "Actual Churn Probability", y = "Predicted Churn Probability") +
      theme_minimal() + coord_equal()
    ggplotly(p, tooltip = c("x", "y")) %>% layout(hoverlabel = list(bgcolor = "white"))
  })
  
  output$dt_ranger_panel <- renderUI({
    res <- dt_results(); req(res)
    if (is.null(res$ranger_model)) {
      div(class = "info-text", style = "text-align:center; padding:30px;",
          "ranger benchmark is only available in Global mode.")
    } else {
      tagList(
        div(class = "section-title", "ranger \u2014 Random Forest Benchmark"),
        tableOutput("dt_ranger_metrics"), br(),
        div(class = "info-text", "Actual vs Predicted \u2014 ranger"),
        plotlyOutput("dt_ranger_scatter", height = "350px")
      )
    }
  })
  
  output$dt_ranger_metrics <- renderTable({
    res <- dt_results(); req(res, res$ranger_metrics); res$ranger_metrics
  }, striped = TRUE, bordered = TRUE, width = "100%")
  
  output$dt_ranger_scatter <- renderPlotly({
    res <- dt_results(); req(res, res$ranger_pred)
    p <- ggplot(data.frame(Actual = res$test_actual, Predicted = res$ranger_pred),
                aes(x = Actual, y = Predicted)) +
      geom_point(alpha = 0.15, size = 1, colour = "#16a34a") +
      geom_abline(slope = 1, intercept = 0, colour = "#e11d48",
                  linetype = "dashed", linewidth = 0.8) +
      labs(x = "Actual Churn Probability", y = "Predicted Churn Probability") +
      theme_minimal() + coord_equal()
    ggplotly(p, tooltip = c("x", "y")) %>% layout(hoverlabel = list(bgcolor = "white"))
  })
  
  output$dt_rpart_vimp <- renderPlotly({
    res <- dt_results(); req(res)
    vimp_df <- res$rpart_vimp
    if (vimp_df$Variable[1] == "None")
      return(plotly_empty() %>% layout(title = "No variable importance available"))
    vimp_df <- head(vimp_df, 15)
    vimp_df$Variable <- factor(vimp_df$Variable, levels = rev(vimp_df$Variable))
    vimp_df$is_lca   <- grepl("lca_class", vimp_df$Variable)
    p <- ggplot(vimp_df, aes(x = Importance, y = Variable, fill = is_lca)) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = c("FALSE" = "#2563eb", "TRUE" = "#e11d48")) +
      labs(x = "Importance (Reduction in SSE)", y = NULL, subtitle = "Red = LCA-derived feature") +
      theme_minimal() + theme(axis.text.y = element_text(size = 10))
    ggplotly(p, tooltip = c("x", "y")) %>% layout(hoverlabel = list(bgcolor = "white"))
  })
  
  output$dt_ranger_vimp_panel <- renderUI({
    res <- dt_results(); req(res)
    if (is.null(res$ranger_vimp)) {
      div(class = "info-text", style = "text-align:center; padding:30px;",
          "ranger importance is only available in Global mode.")
    } else {
      tagList(
        div(class = "section-title", "ranger \u2014 Variable Importance"),
        plotlyOutput("dt_ranger_vimp", height = "450px")
      )
    }
  })
  
  output$dt_ranger_vimp <- renderPlotly({
    res <- dt_results(); req(res, res$ranger_vimp)
    vimp_df <- head(res$ranger_vimp, 15)
    vimp_df$Variable <- factor(vimp_df$Variable, levels = rev(vimp_df$Variable))
    vimp_df$is_lca   <- grepl("lca_class", vimp_df$Variable)
    p <- ggplot(vimp_df, aes(x = Importance, y = Variable, fill = is_lca)) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = c("FALSE" = "#16a34a", "TRUE" = "#e11d48")) +
      labs(x = "Importance (Impurity)", y = NULL, subtitle = "Red = LCA-derived feature") +
      theme_minimal() + theme(axis.text.y = element_text(size = 10))
    ggplotly(p, tooltip = c("x", "y")) %>% layout(hoverlabel = list(bgcolor = "white"))
  })
  
  output$dt_rules_table <- renderDT({
    res <- dt_results(); req(res)
    datatable(res$rules,
              options = list(
                pageLength = 10, scrollX = TRUE,
                order = list(list(0, "asc")),
                columnDefs = list(
                  list(width = "50px",  targets = 0),
                  list(width = "500px", targets = 1),
                  list(width = "100px", targets = 2),
                  list(width = "80px",  targets = 3),
                  list(width = "80px",  targets = 4)
                )
              ),
              rownames = FALSE, class = "compact stripe hover"
    ) %>%
      formatStyle("Prediction",
                  background = styleColorBar(c(0, 0.5), "#FF6B6B"),
                  backgroundSize = "90% 70%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "left")
  })
}

shinyApp(ui = ui, server = server)
