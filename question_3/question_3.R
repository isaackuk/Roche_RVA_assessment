library(shiny)
library(pharmaverseadam)
library(tidyverse)
library(ggplot2)

# ── Data preparation ──────────────────────────────────────────────────────────
adae <- pharmaverseadam::adae

severity_levels <- c("MILD", "MODERATE", "SEVERE")

adae_clean <- adae %>%
  filter(!is.na(AESEV), !is.na(AESOC), !is.na(ACTARM)) %>%
  mutate(
    AESEV     = factor(toupper(AESEV), levels = severity_levels),
    AESOC     = toupper(AESOC),
    ACTARM    = as.character(ACTARM)
  )

treatment_arms <- sort(unique(adae_clean$ACTARM))

severity_palette <- c(
  "MILD"     = "#FDCAB5",   # light salmon
  "MODERATE" = "#F4845F",   # coral
  "SEVERE"   = "#C0392B"    # deep red
)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:wght@300;400;600&family=IBM+Plex+Mono:wght@400;600&display=swap');

      * { box-sizing: border-box; margin: 0; padding: 0; }

      body {
        background-color: #FFFFFF;
        font-family: 'IBM Plex Sans', sans-serif;
        color: #1C1C1C;
      }

      .page-header {
        background: #FFFFFF;
        color: #1C1C1C;
        padding: 22px 36px 18px;
        display: flex;
        align-items: baseline;
        gap: 16px;
      }
      .page-header h1 {
        font-family: 'IBM Plex Mono', monospace;
        font-size: 20px;
        font-weight: 600;
        letter-spacing: 0.04em;
      }

      .layout-wrapper {
        display: flex;
        gap: 0;
        min-height: calc(100vh - 66px);
      }

      .sidebar-panel {
        width: 220px;
        min-width: 220px;
        background: #edebeb;
        padding: 28px 20px;
      }

      .sidebar-panel .filter-label {
        font-size: 13px;
        font-weight: 600;
        letter-spacing: 0.12em;
        color: #1C1C1C;
        margin-bottom: 12px;
      }

      /* Custom checkbox styling */
      .shiny-input-container .checkbox label {
        font-size: 13px;
        color: #1C1C1C;
        cursor: pointer;
        padding-left: 4px;
      }
      .shiny-input-container input[type='checkbox'] {
        accent-color: #006fe6;
        width: 14px;
        height: 14px;
        cursor: pointer;
      }

      .stats-row {
        margin-top: 28px;
        border-top: 1px solid #D8D4CC;
        padding-top: 20px;
        display: flex;
        flex-direction: column;
        gap: 14px;
      }
      .stat-item {
        display: flex;
        flex-direction: column;
      }
      .stat-value {
        font-family: 'IBM Plex Mono', monospace;
        font-size: 22px;
        font-weight: 600;
        color: #B03030;
        line-height: 1;
      }
      .stat-label {
        font-size: 10px;
        color: #888;
        letter-spacing: 0.08em;
        text-transform: uppercase;
        margin-top: 3px;
      }

      .main-panel {
        flex: 1;
        padding: 28px 36px;
        overflow-x: auto;
      }

      .chart-title {
        font-family: 'IBM Plex Mono', monospace;
        font-size: 13px;
        font-weight: 600;
        letter-spacing: 0.06em;
        color: #555;
        margin-bottom: 16px;
        padding-bottom: 10px;
      }

      .plot-container {
        background: #FFFFFF;
        padding: 20px 16px 12px;
      }
    "))
  ),
  
  # Header
  div(class = "page-header",
      tags$h1("AE Summary Interactive Dashboard")
  ),
  
  # Body
  div(class = "layout-wrapper",
      
      # Sidebar
      div(class = "sidebar-panel",
          div(class = "filter-label", "Select Treatment Arm(s):"),
          checkboxGroupInput(
            inputId  = "treatment_filter",
            label    = NULL,
            choices  = treatment_arms,
            selected = treatment_arms
          ),
          
      ),
      
      # Main chart area
      div(class = "main-panel",
          div(class = "chart-title", "Unique Subjects per SOC and Severity Level"),
          div(class = "plot-container",
              plotOutput("ae_plot", height = "680px")
          )
      )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # Reactive: filter data by selected arms
  filtered_data <- reactive({
    req(input$treatment_filter)
    adae_clean %>%
      filter(ACTARM %in% input$treatment_filter)
  })
  
  # Reactive: compute per-SOC-severity unique subject counts
  plot_data <- reactive({
    filtered_data() %>%
      group_by(AESOC, AESEV) %>%
      summarise(N = n_distinct(USUBJID), .groups = "drop")
  })
  
  # Reactive: SOC order by total unique subjects (descending → top of chart)
  soc_order <- reactive({
    plot_data() %>%
      group_by(AESOC) %>%
      summarise(Total = sum(N), .groups = "drop") %>%
      arrange(Total) %>%           # ascending so coord_flip puts highest on top
      pull(AESOC)
  })
  
  # Main bar chart
  output$ae_plot <- renderPlot({
    
    pd  <- plot_data()
    ord <- soc_order()
    
    if (nrow(pd) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "No data for selected treatment arms.",
                   size = 5, color = "#888888") +
          theme_void()
      )
    }
    
    pd <- pd %>%
      mutate(AESOC = factor(AESOC, levels = ord))
    
    ggplot(pd, aes(x = AESOC, y = N, fill = AESEV)) +
      geom_col(position = position_stack(reverse = TRUE),
               width = 0.7) +
      coord_flip() +
      scale_fill_manual(
        values = severity_palette,
        labels = str_to_title,
        name   = "Severity",
        drop   = FALSE
      ) +
      scale_y_continuous(
        expand = expansion(mult = c(0, 0.04)),
        labels = scales::label_comma()
      ) +
      labs(
        x = "System Organ Class",
        y = "Number of Unique Subjects"
      ) +
      theme_minimal(base_family = "sans") +
      theme(
        # Grid
        panel.grid.major.y = element_line(colour = "grey94", linewidth = 0.4),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_line(color = "#E8E4DE", linewidth = 0.4),
        
        # Axes
        axis.text.y  = element_text(size = 8,  color = "#333333",
                                    hjust = 1, margin = margin(r = 4)),
        axis.text.x  = element_text(size = 8,  color = "#555555"),
        axis.title.x = element_text(size = 9,  color = "#555555",
                                    margin = margin(t = 10)),
        axis.title.y = element_text(size = 9,  color = "#555555",
                                    margin = margin(r = 10)),
        axis.ticks   = element_blank(),
        
        # Legend
        legend.position      = "right",
        legend.title         = element_text(size = 9, face = "bold",
                                            color = "#333333"),
        legend.text          = element_text(size = 8, color = "#333333"),
        legend.key.size      = unit(12, "pt"),
        legend.spacing.y     = unit(4,  "pt"),
        legend.background    = element_blank(),
        
        # Plot margins
        plot.margin  = margin(8, 16, 8, 8),
        plot.background  = element_rect(fill = "#FFFFFF", color = NA),
        panel.background = element_rect(fill = "#FFFFFF", color = NA)
      )
  }, res = 110)
}

# ── Launch ─────────────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
