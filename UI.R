library(shiny)
library(DT)

source("global.R")

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: 'Helvetica Neue', Arial, sans-serif; background: #f7f9fc; }
    .title-bar { background: #1a3a5c; color: white; padding: 18px 24px 14px;
                 margin-bottom: 20px; border-radius: 6px; }
    .title-bar h2 { margin: 0; font-size: 1.4rem; font-weight: 600; }
    .title-bar p  { margin: 4px 0 0; font-size: 0.85rem; opacity: .75; }
    .kpi-box { background: white; border-radius: 8px; padding: 14px 18px;
               box-shadow: 0 1px 4px rgba(0,0,0,.08); text-align: center; }
    .kpi-box .num  { font-size: 2rem; font-weight: 700; color: #1a3a5c; }
    .kpi-box .lbl  { font-size: 0.78rem; color: #666; text-transform: uppercase;
                     letter-spacing: .05em; }
    .well { border-radius: 8px; border: none;
            box-shadow: 0 1px 4px rgba(0,0,0,.07); background: white; }
  "))),
  
  tags$script(HTML("
  $(document).ready(function(){
    $('[data-toggle=\"tooltip\"]').tooltip({ html: true });
  });
")),
  
  div(class = "title-bar",
      div(style = "display:flex; justify-content:space-between; align-items:center;",
          div(
            h2("Journal Data Policies Explorer"),
            p("Version 1, 2026-01-06")
          ),
          actionButton("show_about", "About",
                       style = "background:transparent; border:1px solid rgba(255,255,255,.6);
                 color:white; font-size:.82rem; padding:4px 24px;
                 border-radius:4px; cursor:pointer;")
      )
  ),
  
  fluidRow(
    
    # ── Sidebar ──────────────────────────────────────────────────────────────
    column(3,
           wellPanel(
             h5("Filters", style = "margin-top:0; font-weight:700; color:#1a3a5c;"),
             selectInput("pub", "Publisher", publishers, width = "100%"),
             selectInput("oa",  "Open Access", oa_opts,  width = "100%"),
             checkboxGroupInput("policy", "Data-sharing policy",
                                choiceNames = list(
                                  tags$span("No Policy",
                                            tags$i(class = "glyphicon glyphicon-info-sign",
                                                   style = "color:#888; margin-left:5px; cursor:help;",
                                                   `data-toggle` = "tooltip", `data-placement` = "right",
                                                   title = "The journal does not have a posted policy that encourages or requires authors to share their data and/or code with the published article (or explicitly states that there is no sharing policy in place).")),
                                  tags$span("Encouraged",
                                            tags$i(class = "glyphicon glyphicon-info-sign",
                                                   style = "color:#888; margin-left:5px; cursor:help;",
                                                   `data-toggle` = "tooltip", `data-placement` = "right",
                                                   title = "The journal has a posted policy that encourages (but does not require) authors to share their data and/or code with the published article.")),
                                  tags$span("Required",
                                            tags$i(class = "glyphicon glyphicon-info-sign",
                                                   style = "color:#888; margin-left:5px; cursor:help;",
                                                   `data-toggle` = "tooltip", `data-placement` = "right",
                                                   title = "The journal has a posted policy that requires authors to share their data and/or code with the published article (but the journal does not verify the results).")),
                                  tags$span("Verified",
                                            tags$i(class = "glyphicon glyphicon-info-sign",
                                                   style = "color:#888; margin-left:5px; cursor:help;",
                                                   `data-toggle` = "tooltip", `data-placement` = "right",
                                                   title = "The journal has a posted policy that requires authors to share their data and/or code with the published article and the journal verifies the results."))
                                ),
                                choiceValues = policy_levels,
                                selected     = policy_levels,
                                inline       = FALSE
             )
           )
    ),
    
    # ── Main panel ───────────────────────────────────────────────────────────
    column(9,
           
           # KPI row
           fluidRow(
             column(3, div(class = "kpi-box",
                           div(class = "num", textOutput("kpi_no_policy")),
                           div(class = "lbl", "No Policy")
             )),
             column(3, div(class = "kpi-box",
                           div(class = "num", textOutput("kpi_encouraged")),
                           div(class = "lbl", "Encouraged")
             )),
             column(3, div(class = "kpi-box",
                           div(class = "num", textOutput("kpi_required")),
                           div(class = "lbl", "Required")
             )),
             column(3, div(class = "kpi-box",
                           div(class = "num", textOutput("kpi_verified")),
                           div(class = "lbl", "Verified")
             ))
           ),
           
           br(),
           
           tabsetPanel(
             tabPanel("Policy breakdown",
                      br(),
                      plotOutput("bar_publisher", height = "420px")
             ),
             tabPanel("Policy share",
                      br(),
                      plotOutput("pie_overall", height = "380px")
             ),
             tabPanel("All journals",
                      br(),
                      DTOutput("table")
             )
           )
    )
  )
)