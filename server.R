library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

server <- function(input, output, session) {
  
  # ── About modal ───────────────────────────────────────────────────────────
  observeEvent(input$show_about, {
    showModal(modalDialog(
      title = "About this app",
      tagList(
        h5("Version 3, 2026-07-30", style = "margin-top:1.2em; font-weight:700;"),
        p("This app lets you explore journal data policies longitudinally across two studies (2008 and 2024). It was created by Lara Skelly, for Loughborough University, with the assistance of Claude.ai."),
        h5("Data", style = "margin-top:1.2em; font-weight:700;"),
        p("Journal metadata (publisher, open-access status, subjects) is sourced from ",
          a("OpenAlex", href = "https://openalex.org", target = "_blank"),
          ". Policy audit answers are from ",
          a("Rainey & Roe (2024).", href = "https://doi.org/10.7910/DVN/0YSJCX", target = "_blank")),
        h5("Policy mapping", style = "margin-top:1.2em; font-weight:700;"),
        p("2008 used a three-tier scale (No Policy / Weak Policy / Strong Policy); 2024 used a four-tier scale (No Policy / Encouraged / Required / Verified). For the combined view, 2008 results are mapped onto the 2024 scale: Weak Policy \u2192 Encouraged, Strong Policy \u2192 Required. \"Verified\" was not assessed in 2008."),
        h5("Acknowledgements", style = "margin-top:1.2em; font-weight:700;"),
        p("Beta viewers: Katie Fraser (Loughborough University), Nicola Howe (Newcastle University)"),
        h5("More information", style = "margin-top:1.2em; font-weight:700;"),
        p(a("JDPMeta project", href = "https://doi.org/10.17605/OSF.IO/5JRSU", target = "_blank")),
        p(a("GitHub", href = "https://github.com/lboro-rdm/JDP-Meta", target = "_blank")),
        p(a("Accessibility statement", href = "https://doi.org/10.17028/rd.lboro.28525481", target = "_blank"))
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # ── Filtered data ─────────────────────────────────────────────────────────
  filt <- reactive({
    d <- df
    if (input$pub  != "All") d <- filter(d, publisher   == input$pub)
    if (input$oa   != "All") d <- filter(d, oa_status    == input$oa)
    if (input$year != "All") d <- filter(d, study_year   == as.integer(input$year))
    if (length(input$policy)) {
      d <- filter(d, policy_unified %in% input$policy)
    } else {
      d <- filter(d, FALSE)
    }
    d
  })
  
  # ── KPI boxes: distinct journals per unified policy tier ──────────────────
  kpi <- function(lvl) renderText({
    filt() |>
      filter(policy_unified == lvl) |>
      distinct(journal) |>
      nrow() |>
      as.character()
  })
  output$kpi_no_policy  <- kpi("No Policy")
  output$kpi_encouraged <- kpi("Encouraged")
  output$kpi_required   <- kpi("Required")
  output$kpi_verified   <- kpi("Verified")
  
  # ── Policy over time ────────────────────────────────────────────────────
  output$bar_year <- renderPlot({
    d <- filt() |> filter(!is.na(policy_unified))
    validate(need(nrow(d) > 0, "No data for the current filters."))
    
    d <- d |> count(study_year, policy_unified)
    
    ggplot(d, aes(x = factor(study_year), y = n, fill = policy_unified)) +
      geom_col(position = "dodge", width = .6) +
      scale_fill_manual(values = policy_colours_unified, name = "Policy", drop = FALSE) +
      labs(x = "Study year", y = "Number of journals",
           title = "Policy tiers over time") +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "bottom",
        plot.title       = element_text(face = "bold", color = "#1a3a5c")
      )
  })
  
  # ── Stacked bar: policy by publisher ───────────────────────────────────────
  output$bar_publisher <- renderPlot({
    d <- filt() |>
      filter(!is.na(publisher), publisher != "", !is.na(policy_unified)) |>
      count(publisher, study_year, policy_unified) |>
      group_by(publisher) |>
      mutate(total = sum(n)) |>
      ungroup() |>
      mutate(publisher = reorder(publisher, total))
    
    validate(need(nrow(d) > 0, "No data for the current filters."))
    
    p <- ggplot(d, aes(x = publisher, y = n, fill = policy_unified)) +
      geom_col(width = .7) +
      scale_fill_manual(values = policy_colours_unified, name = "Policy", drop = FALSE) +
      coord_flip() +
      labs(x = NULL, y = "Number of journals",
           title = "Data-sharing policy by publisher") +
      theme_minimal(base_size = 13) +
      theme(
        legend.position    = "bottom",
        panel.grid.major.y = element_blank(),
        plot.title         = element_text(face = "bold", color = "#1a3a5c")
      )
    
    if (input$year == "All") p <- p + facet_wrap(~ study_year, ncol = 2)
    p
  })
  
  # ── Pie: overall policy share ──────────────────────────────────────────────
  output$pie_overall <- renderPlot({
    d <- filt() |>
      filter(!is.na(policy_unified)) |>
      count(study_year, policy_unified) |>
      group_by(study_year) |>
      mutate(
        pct   = n / sum(n),
        label = paste0(policy_unified, "\n", n, " (", scales::percent(pct, 1), ")")
      ) |>
      ungroup()
    
    validate(need(nrow(d) > 0, "No data for the current filters."))
    
    p <- ggplot(d, aes(x = "", y = n, fill = policy_unified)) +
      geom_col(width = 1, colour = "white") +
      scale_fill_manual(values = policy_colours_unified, name = NULL) +
      coord_polar(theta = "y") +
      geom_text(aes(label = label),
                position = position_stack(vjust = .5),
                size = 4, colour = "white", fontface = "bold") +
      labs(title = "Overall policy distribution") +
      theme_void(base_size = 13) +
      theme(
        legend.position = "none",
        plot.title      = element_text(face = "bold", color = "#1a3a5c",
                                       hjust = .5, margin = margin(b = 10))
      )
    
    if (input$year == "All") p <- p + facet_wrap(~ study_year)
    p
  })
  
  # ── Data table ───────────────────────────────────────────────────────────
  output$table <- renderDT({
    d <- filt() |>
      transmute(
        Journal                 = journal,
        Publisher                = publisher,
        `Open Access`            = oa_status,
        `Study Year`             = study_year,
        `Data-sharing Policy`    = as.character(policy_unified),
        `Original Wording`       = as.character(policy_raw),
        Details = sapply(long_answer, function(txt) {
          if (is.na(txt)) return("")
          as.character(
            tags$span(
              "\u24d8",
              title = txt,
              style = "cursor:help; border-bottom:1px dotted #666;"
            )
          )
        }, USE.NAMES = FALSE),
        `Audit Date` = audit_date
      )
    
    datatable(
      d,
      rownames = FALSE,
      filter   = "top",
      escape   = FALSE,
      options  = list(pageLength = 15, scrollX = TRUE)
    ) |>
      formatStyle(
        "Data-sharing Policy",
        backgroundColor = styleEqual(
          unified_levels,
          c("#ffe0de", "#ffd8b5", "#cce5f5", "#c9ddf5")
        )
      )
  })
}