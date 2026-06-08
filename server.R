library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

server <- function(input, output, session) {
  
  # ── About modal ─────────────────────────────────────────────────────────────
  observeEvent(input$show_about, {
    showModal(modalDialog(
      title = "About this app",
      tagList(
        h5("Version 1.1, 2026-06-04", style = "margin-top:1.2em; font-weight:700;"),
        p("This app lets you explore journal data policies. It was created by Lara Skelly, for Loughborough University, with the assistance of Claude.ai."),
        h5("Data", style = "margin-top:1.2em; font-weight:700;"),
        p("Journal metadata (publisher, open-access status, subjects) is sourced from ",
          a("OpenAlex", href = "https://openalex.org", target = "_blank"),
          ". Policy audit answers are from ",
          a("Rainey & Roe (2024).", href = "https://doi.org/10.7910/DVN/0YSJCX", target = "_blank")),
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
  
  # ── Filtered data ───────────────────────────────────────────────────────────
  filt <- reactive({
    d <- df
    if (input$pub != "All") d <- filter(d, publisher == input$pub)
    if (input$oa  != "All") d <- filter(d, oa_status  == input$oa)
    if (length(input$policy)) d <- filter(d, data_sharing %in% input$policy)
    d
  })
  
  # ── Sidebar count ───────────────────────────────────────────────────────────
  output$n_shown <- renderText(nrow(filt()))
  
  # ── KPI boxes ───────────────────────────────────────────────────────────────
  kpi <- function(lvl) renderText({
    as.character(sum(filt()$data_sharing == lvl, na.rm = TRUE))
  })
  output$kpi_no_policy  <- kpi("No Policy")
  output$kpi_encouraged <- kpi("Encouraged")
  output$kpi_required   <- kpi("Required")
  output$kpi_verified   <- kpi("Verified")
  
  # ── Stacked bar: policy by publisher ────────────────────────────────────────
  output$bar_publisher <- renderPlot({
    d <- filt() |>
      filter(!is.na(publisher), publisher != "") |>
      count(publisher, data_sharing) |>
      group_by(publisher) |>
      mutate(total = sum(n)) |>
      ungroup() |>
      mutate(publisher = reorder(publisher, total))
    
    ggplot(d, aes(x = publisher, y = n, fill = data_sharing)) +
      geom_col(width = .7) +
      scale_fill_manual(values = policy_colours, name = "Policy", drop = FALSE) +
      coord_flip() +
      labs(x = NULL, y = "Number of journals",
           title = "Data-sharing policy by publisher") +
      theme_minimal(base_size = 13) +
      theme(
        legend.position    = "bottom",
        panel.grid.major.y = element_blank(),
        plot.title         = element_text(face = "bold", color = "#1a3a5c")
      )
  })
  
  # ── Pie: overall policy share ────────────────────────────────────────────────
  output$pie_overall <- renderPlot({
    d <- filt() |>
      count(data_sharing) |>
      mutate(
        pct   = n / sum(n),
        label = paste0(data_sharing, "\n", n, " (", scales::percent(pct, 1), ")")
      )
    
    ggplot(d, aes(x = "", y = n, fill = data_sharing)) +
      geom_col(width = 1, colour = "white") +
      scale_fill_manual(values = policy_colours, name = NULL) +
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
  })
  
  # ── Data table ───────────────────────────────────────────────────────────────
  output$table <- renderDT({
    d <- filt() |>
      mutate(
        `Data-sharing Policy` = paste0(
          '<span title="', htmltools::htmlEscape(long_answer), '" ',
          'style="cursor:help; border-bottom:1px dotted #666;">',
          htmltools::htmlEscape(as.character(data_sharing)),
          '</span>'
        )
      ) |>
      select(
        Journal               = journal,
        Publisher             = publisher,
        `Open Access`         = oa_status,
        `Data-sharing Policy`,
        `Audit Year`          = audit_date
      )
    
    datatable(
      d,
      rownames  = FALSE,
      filter    = "top",
      escape    = FALSE,
      options   = list(pageLength = 15, scrollX = TRUE)
    ) |>
      formatStyle(
        "Data-sharing Policy",
        backgroundColor = styleEqual(
          paste0('<span title="', htmltools::htmlEscape(
            c(
              "The journal does not have a posted policy that encourages or requires authors to share their data and/or code with the published article (or explicitly states that there is no sharing policy in place).",
              "The journal has a posted policy that encourages (but does not require) authors to share their data and/or code with the published article.",
              "The journal has a posted policy that requires authors to share their data and/or code with the published article (but the journal does not verify the results).",
              "The journal has a posted policy that requires authors to share their data and/or code with the published article and the journal verifies the results."
            )
          ), '" style="cursor:help; border-bottom:1px dotted #666;">',
          htmltools::htmlEscape(policy_levels), '</span>'),
          c("#ffe0de", "#ffd8b5", "#cce5f5", "#c9ddf5")
        )
      )
  })
}