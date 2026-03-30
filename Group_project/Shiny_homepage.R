library(shiny)

# ── URLs ──────────────────────────────────────────────────────────────────────
url_eda        <- "https://chenghowang.shinyapps.io/Group_project_EDA/"
url_clustering <- "https://chenghowang.shinyapps.io/Group_project_Clustering/"
url_dt         <- "https://chenghowang.shinyapps.io/Group_project_Cluster_Tree/"

# ══════════════════════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════════════════════

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("

      * { box-sizing: border-box; margin: 0; padding: 0; }
      body { font-family: 'Segoe UI', sans-serif; background: #fff; color: #0f1e35; }
      a { text-decoration: none; }

      /* ── Navbar ── */
      .top-nav {
        background: #0f1e35;
        padding: 0 40px;
        height: 52px;
        display: flex;
        align-items: center;
        justify-content: space-between;
      }
      .nav-brand {
        font-size: 16px;
        font-weight: 700;
        color: #fff;
      }
      .nav-brand span { color: #60a5fa; }
      .nav-links { display: flex; gap: 8px; }
      .nav-link {
        color: #94a3b8;
        font-size: 13px;
        padding: 6px 14px;
        border-radius: 6px;
        font-weight: 500;
        cursor: pointer;
      }
      .nav-link:hover { color: #fff; background: rgba(255,255,255,0.08); }

      /* ── Hero ── */
      .hero {
        background: linear-gradient(135deg, #0f1e35 0%, #1a3558 55%, #1e4d8c 100%);
        padding: 90px 40px 80px;
        text-align: center;
        color: #fff;
      }
      .hero-tag {
        font-size: 11px;
        letter-spacing: 0.18em;
        text-transform: uppercase;
        color: #93c5fd;
        margin-bottom: 18px;
      }
      .hero h1 {
        font-size: 52px;
        font-weight: 700;
        line-height: 1.15;
        margin-bottom: 16px;
      }
      .hero h1 span { color: #60a5fa; }
      .hero p {
        font-size: 17px;
        color: #cbd5e1;
        max-width: 600px;
        margin: 0 auto 36px;
        line-height: 1.7;
      }
      .hero-btns { display: flex; gap: 14px; justify-content: center; flex-wrap: wrap; }
      .btn-hero-primary {
        background: #2563eb;
        color: #fff;
        padding: 13px 32px;
        border-radius: 8px;
        font-size: 14px;
        font-weight: 600;
        border: 2px solid #2563eb;
        display: inline-block;
      }
      .btn-hero-primary:hover { background: #1d4ed8; }
      .btn-hero-outline {
        background: transparent;
        color: #fff;
        padding: 13px 32px;
        border-radius: 8px;
        font-size: 14px;
        font-weight: 600;
        border: 2px solid rgba(255,255,255,0.35);
        display: inline-block;
      }
      .btn-hero-outline:hover { border-color: #fff; }

      /* ── Sections ── */
      .section { padding: 72px 40px; max-width: 1100px; margin: 0 auto; }
      .section-dark { background: #f8fafc; padding: 72px 40px; }
      .section-dark .inner { max-width: 1100px; margin: 0 auto; }
      .section-label {
        font-size: 11px;
        font-weight: 700;
        letter-spacing: 0.16em;
        text-transform: uppercase;
        color: #2563eb;
        margin-bottom: 10px;
      }
      .section-title { font-size: 32px; font-weight: 700; color: #0f1e35; margin-bottom: 12px; }
      .section-sub { font-size: 15px; color: #64748b; max-width: 580px; line-height: 1.7; margin-bottom: 48px; }

      /* ── Intro cards ── */
      .intro-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(220px, 1fr)); gap: 24px; }
      .intro-card { background: #fff; border: 1px solid #e2e8f0; border-radius: 12px; padding: 28px 24px; }
      .intro-icon { width: 44px; height: 44px; border-radius: 10px; background: #dbeafe;
                    display: flex; align-items: center; justify-content: center;
                    font-size: 20px; margin-bottom: 14px; }
      .intro-card h4 { font-size: 15px; font-weight: 700; margin-bottom: 8px; color: #0f1e35; }
      .intro-card p  { font-size: 13px; color: #64748b; line-height: 1.6; margin: 0; }

      /* ── Figures bar ── */
      .figures-bar { background: #0f1e35; padding: 56px 40px; text-align: center; }
      .figures-inner { max-width: 1100px; margin: 0 auto; }
      .figures-title { font-size: 11px; letter-spacing: 0.16em; text-transform: uppercase;
                       color: #60a5fa; margin-bottom: 36px; }
      .figures-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(180px, 1fr)); gap: 32px; }
      .fig-num  { font-size: 38px; font-weight: 700; color: #fff; line-height: 1; }
      .fig-num span { color: #60a5fa; }
      .fig-label { font-size: 13px; color: #94a3b8; margin-top: 6px; }

      /* ── App module cards ── */
      .modules-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(230px, 1fr)); gap: 20px; }
      .mod-card { border: 1px solid #e2e8f0; border-radius: 12px; overflow: hidden;
                  background: #fff; display: flex; flex-direction: column; }
      .mod-card:hover { box-shadow: 0 8px 32px rgba(0,0,0,0.08); }
      .mod-header { padding: 20px 20px 14px; border-bottom: 1px solid #f1f5f9; }
      .mod-num { font-size: 10px; font-weight: 700; letter-spacing: 0.12em;
                 text-transform: uppercase; color: #2563eb; margin-bottom: 6px; }
      .mod-header h3 { font-size: 15px; font-weight: 700; color: #0f1e35; }
      .mod-body { padding: 14px 20px 20px; flex: 1; display: flex; flex-direction: column; }
      .mod-body p { font-size: 13px; color: #64748b; line-height: 1.6; margin: 0 0 12px; flex: 1; }
      .mod-tags { display: flex; flex-wrap: wrap; gap: 6px; margin-bottom: 14px; }
      .tag { font-size: 10px; font-weight: 600; padding: 3px 8px; border-radius: 20px;
             background: #f1f5f9; color: #475569; font-family: monospace; }
      .mod-link {
        display: inline-block;
        font-size: 12px;
        font-weight: 600;
        color: #2563eb;
        border: 1.5px solid #2563eb;
        padding: 7px 14px;
        border-radius: 6px;
        text-align: center;
        transition: all .15s;
      }
      .mod-link:hover { background: #2563eb; color: #fff; }

      /* ── Team ── */
      .team-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(260px, 1fr)); gap: 24px; }
      .team-card { background: #fff; border: 1px solid #e2e8f0; border-radius: 12px;
                   padding: 28px 24px; text-align: center; }
      .team-avatar { width: 64px; height: 64px; border-radius: 50%;
                     display: flex; align-items: center; justify-content: center;
                     font-size: 22px; font-weight: 700; color: #fff; margin: 0 auto 14px; }
      .av-blue  { background: #2563eb; }
      .av-green { background: #16a34a; }
      .av-amber { background: #d97706; }
      .team-card h4 { font-size: 15px; font-weight: 700; margin-bottom: 4px; }
      .team-card .role { font-size: 12px; color: #2563eb; font-weight: 600; margin-bottom: 10px; }
      .team-card p { font-size: 13px; color: #64748b; line-height: 1.6; margin: 0; }

      /* ── CTA ── */
      .cta-section { text-align: center; padding: 72px 40px; }
      .btn-launch {
        display: inline-block;
        background: #2563eb;
        color: #fff;
        padding: 16px 48px;
        border-radius: 8px;
        font-size: 16px;
        font-weight: 700;
        border: 2px solid #2563eb;
        margin: 6px;
      }
      .btn-launch:hover { background: #1d4ed8; }
      .btn-launch-outline {
        display: inline-block;
        background: transparent;
        color: #2563eb;
        padding: 16px 32px;
        border-radius: 8px;
        font-size: 14px;
        font-weight: 600;
        border: 2px solid #2563eb;
        margin: 6px;
      }
      .btn-launch-outline:hover { background: #eff6ff; }

      /* ── Footer ── */
      .footer { background: #0f1e35; color: #64748b; text-align: center;
                padding: 32px 20px; font-size: 13px; }
      .footer span { color: #60a5fa; }

    "))
  ),
  
  # ── Navbar ──────────────────────────────────────────────────────────────────
  div(class = "top-nav",
      div(class = "nav-brand", "Fin", tags$span("Sight")),
      div(class = "nav-links",
          tags$a(class = "nav-link", href = "#", "Home"),
          tags$a(class = "nav-link",
                 href = "https://chenghowang.shinyapps.io/Group_project_EDA/",
                 target = "_blank", "EDA & CDA"),
          tags$a(class = "nav-link",
                 href = "https://chenghowang.shinyapps.io/Group_project_Clustering/",
                 target = "_blank", "Clustering"),
          tags$a(class = "nav-link",
                 href = "https://chenghowang.shinyapps.io/Group_project_Cluster_Tree/",
                 target = "_blank", "Decision Tree")
      )
  ),
  
  # ── Hero ────────────────────────────────────────────────────────────────────
  div(class = "hero",
      div(class = "hero-tag", "ISSS608 \u00b7 Visual Analytics Project \u00b7 AY2025-26"),
      tags$h1("Fin", tags$span("Sight")),
      tags$p("A web-enabled visual analytics application for democratising
            customer intelligence in Colombian fintech \u2014 built with R Shiny."),
      div(class = "hero-btns",
          tags$a(class = "btn-hero-primary",
                 href = "https://chenghowang.shinyapps.io/Group_project_EDA/",
                 target = "_blank", "\u25b6 Launch App"),
          tags$a(class = "btn-hero-outline",
                 href = "https://isss608-vaa.netlify.app/proposal",
                 target = "_blank", "Read Proposal")
      )
  ),
  
  # ── Intro ───────────────────────────────────────────────────────────────────
  div(class = "section",
      div(class = "section-label", "What we built"),
      div(class = "section-title", "Four modules. One application."),
      tags$p(class = "section-sub",
             "FinSight integrates exploratory analysis, statistical testing,
       latent class clustering, and predictive modelling into a single
       interactive Shiny platform."),
      div(class = "intro-grid",
          div(class = "intro-card",
              div(class = "intro-icon", "\U0001f4ca"),
              tags$h4("Exploratory Data Analysis"),
              tags$p("Visualise customer demographics, product adoption, churn distribution
                and satisfaction patterns interactively.")
          ),
          div(class = "intro-card",
              div(class = "intro-icon", "\U0001f52c"),
              tags$h4("Confirmatory Data Analysis"),
              tags$p("Run Pearson correlation, ANOVA, and multiple linear regression to
                statistically validate key relationships.")
          ),
          div(class = "intro-card",
              div(class = "intro-icon", "\U0001f9e9"),
              tags$h4("Clustering \u2014 LCA"),
              tags$p("Discover 5 latent customer segments using Latent Class Analysis
                across 18 behavioural and attitudinal variables.")
          ),
          div(class = "intro-card",
              div(class = "intro-icon", "\U0001f333"),
              tags$h4("Decision Tree & Random Forest"),
              tags$p("Predict churn probability using rpart and ranger, with interactive
                parameter tuning and model comparison.")
          )
      )
  ),
  
  # ── Figures ─────────────────────────────────────────────────────────────────
  div(class = "figures-bar",
      div(class = "figures-inner",
          div(class = "figures-title", "Key Figures from the Dataset"),
          div(class = "figures-grid",
              div(div(class = "fig-num", "48", tags$span(",723")),
                  div(class = "fig-label", "Customers analysed")),
              div(div(class = "fig-num", "54"),
                  div(class = "fig-label", "Variables in dataset")),
              div(div(class = "fig-num", tags$span("0."), "931"),
                  div(class = "fig-label", "LCA entropy \u2014 excellent fit")),
              div(div(class = "fig-num", "5"),
                  div(class = "fig-label", "Latent customer segments")),
              div(div(class = "fig-num", "99", tags$span("%+")),
                  div(class = "fig-label", "Decision tree accuracy"))
          )
      )
  ),
  
  # ── App modules ─────────────────────────────────────────────────────────────
  div(class = "section-dark",
      div(class = "inner",
          div(class = "section-label", "The application"),
          div(class = "section-title", "Shiny App Modules"),
          tags$p(class = "section-sub",
                 "Each module is independently deployed. Click to open in a new tab."),
          div(class = "modules-grid",
              
              div(class = "mod-card",
                  div(class = "mod-header",
                      div(class = "mod-num", "Tab 01"),
                      tags$h3("Exploratory Data Analysis")),
                  div(class = "mod-body",
                      tags$p("Interactive histograms, correlation heatmap, faceted scatter
                    plots by income and gender, satisfaction trends by CLV and tenure."),
                      div(class = "mod-tags",
                          tags$span(class = "tag", "ggplot2"),
                          tags$span(class = "tag", "plotly"),
                          tags$span(class = "tag", "ggcorrplot")),
                      tags$a(class = "mod-link",
                             href = "https://chenghowang.shinyapps.io/Group_project_EDA/",
                             target = "_blank", "\u25b6 Open in App")
                  )
              ),
              
              div(class = "mod-card",
                  div(class = "mod-header",
                      div(class = "mod-num", "Tab 02"),
                      tags$h3("Confirmatory Data Analysis")),
                  div(class = "mod-body",
                      tags$p("Pearson correlation tests, one-way and two-way ANOVA, multiple
                    linear regression with interactive variable selection."),
                      div(class = "mod-tags",
                          tags$span(class = "tag", "broom"),
                          tags$span(class = "tag", "ggplot2"),
                          tags$span(class = "tag", "patchwork")),
                      tags$a(class = "mod-link",
                             href = "https://chenghowang.shinyapps.io/Group_project_EDA/",
                             target = "_blank", "\u25b6 Open in App")
                  )
              ),
              
              div(class = "mod-card",
                  div(class = "mod-header",
                      div(class = "mod-num", "Tab 03"),
                      tags$h3("Clustering \u2014 LCA")),
                  div(class = "mod-body",
                      tags$p("Latent Class Analysis with adjustable classes and repetitions.
                    BIC, AIC, entropy metrics. Interactive stacked bar charts for
                    all 18 variables."),
                      div(class = "mod-tags",
                          tags$span(class = "tag", "poLCA"),
                          tags$span(class = "tag", "plotly"),
                          tags$span(class = "tag", "tidyverse")),
                      tags$a(class = "mod-link",
                             href = "https://chenghowang.shinyapps.io/Group_project_Clustering/",
                             target = "_blank", "\u25b6 Open in App")
                  )
              ),
              
              div(class = "mod-card",
                  div(class = "mod-header",
                      div(class = "mod-num", "Tab 04"),
                      tags$h3("Decision Tree & Random Forest")),
                  div(class = "mod-body",
                      tags$p("Pruned regression tree and random forest with interactive
                    parameter tuning. Variable importance plot and decision rules table."),
                      div(class = "mod-tags",
                          tags$span(class = "tag", "rpart"),
                          tags$span(class = "tag", "ranger"),
                          tags$span(class = "tag", "caret")),
                      tags$a(class = "mod-link",
                             href = "https://chenghowang.shinyapps.io/Group_project_Cluster_Tree/",
                             target = "_blank", "\u25b6 Open in App")
                  )
              )
          )
      )
  ),
  
  # ── CTA ─────────────────────────────────────────────────────────────────────
  div(class = "cta-section",
      div(class = "section-label", "Live applications"),
      div(class = "section-title", "Try FinSight"),
      tags$p(class = "section-sub", style = "margin: 0 auto 32px;",
             "All three modules are deployed on shinyapps.io.
       Click below to open each one directly."),
      tags$a(class = "btn-launch",
             href = "https://chenghowang.shinyapps.io/Group_project_EDA/",
             target = "_blank", "\u25b6 EDA & CDA"),
      tags$a(class = "btn-launch-outline",
             href = "https://chenghowang.shinyapps.io/Group_project_Clustering/",
             target = "_blank", "Clustering"),
      tags$a(class = "btn-launch-outline",
             href = "https://chenghowang.shinyapps.io/Group_project_Cluster_Tree/",
             target = "_blank", "Decision Tree")
  ),
  
  # ── Team ────────────────────────────────────────────────────────────────────
  div(class = "section-dark",
      div(class = "inner",
          div(class = "section-label", "The team"),
          div(class = "section-title", "Meet FinSight"),
          tags$p(class = "section-sub",
                 "A team of three MITB students applying visual analytics to
         real-world fintech data."),
          div(class = "team-grid",
              div(class = "team-card",
                  div(class = "team-avatar av-blue", "TC"),
                  tags$h4("Teo Cher In"),
                  div(class = "role", "EDA + CDA \u00b7 Tab 1 & 2"),
                  tags$p("Exploratory and confirmatory analysis modules, including
                  correlation, ANOVA, and regression testing.")
              ),
              div(class = "team-card",
                  div(class = "team-avatar av-green", "CW"),
                  tags$h4("Cheng Ho Wang"),
                  div(class = "role", "Clustering (LCA) \u00b7 Tab 3"),
                  tags$p("Latent Class Analysis module, variable preparation,
                  model evaluation, and interactive Shiny visualisations.")
              ),
              div(class = "team-card",
                  div(class = "team-avatar av-amber", "YZ"),
                  tags$h4("Ye Zhili"),
                  div(class = "role", "Decision Tree & RF \u00b7 Tab 4"),
                  tags$p("Classification tree and random forest modelling,
                  hyperparameter tuning, and model comparison.")
              )
          )
      )
  ),
  
  # ── Footer ──────────────────────────────────────────────────────────────────
  div(class = "footer",
      tags$p(tags$span("FinSight"),
             " \u00b7 ISSS608 Visual Analytics & Applications \u00b7
            Singapore Management University \u00b7 2026")
  )
)

# ══════════════════════════════════════════════════════════════════════════════
server <- function(input, output, session) {}

shinyApp(ui = ui, server = server)
