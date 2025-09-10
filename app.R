library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(DT)
library(stats)
library(heatmaply)
library(processx)

ui <- dashboardPage(
  dashboardHeader(title = "MAG Quality Dashboard (Inspired by BIgMAG)"),
  dashboardSidebar(
    width = 350,
    h4("Choose Your Workflow:", style = "color: #3c8dbc; margin-bottom: 15px;"),
    radioButtons("workflow_type",
                 label = NULL,
                 choices = list(
                   "📊 Upload processed data (TSV/CSV)" = "upload_data",
                   "🧬 Analyze raw MAG files (via MAGFlow)" = "analyze_mags"
                 ),
                 selected = "upload_data"),
    hr(style = "border-color: #ddd;"),
    conditionalPanel(
      condition = "input.workflow_type == 'upload_data'",
      div(
        style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; border-left: 4px solid #28a745;",
        h5("📊 Data Upload Workflow", style = "color: #28a745; margin-top: 0;"),
        p("Upload your pre-processed MAG quality assessment results", style = "font-size: 12px; color: #666;"),
        fileInput("upload",
                  "Select TSV/CSV File",
                  accept = c(".tsv", ".csv"),
                  buttonLabel = "Browse...",
                  placeholder = "No file selected"),
        p("Required columns: Sample_ID, Taxonomy_Level, Completeness, Contamination, CSS, N50, Complete_SCO",
          style = "font-size: 11px; color: #666; font-style: italic; margin-top: -10px;")
      )
    ),
    conditionalPanel(
      condition = "input.workflow_type == 'analyze_mags'",
      div(
        style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; border-left: 4px solid #007bff;",
        h5("🧬 MAG Analysis Workflow", style = "color: #007bff; margin-top: 0;"),
        p("Upload raw MAG FASTA files to run complete analysis pipeline", style = "font-size: 12px; color: #666;"),
        fileInput("mag_files",
                  "Select MAG FASTA Files",
                  accept = ".fasta",
                  multiple = TRUE,
                  buttonLabel = "Browse...",
                  placeholder = "No files selected"),
        div(
          style = "margin-top: -10px;",
          selectInput("execution_mode",
                      HTML("<strong style='color: #2c3e50;'>Execution Mode</strong>"),
                      choices = list(
                        "🐳 Local (Docker)" = "Local (Docker)",
                        "☁️ Cloud" = "Cloud"
                      ),
                      selected = "Local (Docker)")
        ),
        div(
          style = "background-color: #e3f2fd; padding: 10px; border-radius: 5px; margin-top: 10px;",
          p("💡 Requires Nextflow and Docker installed for local execution",
            style = "font-size: 11px; margin: 0; color: #1976d2;")
        )
      )
    ),
    hr(style = "border-color: #ddd;"),
    conditionalPanel(
      condition = "output.show_settings",
      div(
        style = "margin: 0 10px;",
        h5("⚙️ Analysis Settings", style = "color: #ffffff; font-weight: 600;"),
        selectInput("tax_level",
                    HTML("<strong style='color: #ffffff;'>Taxonomy Level for Matrix</strong>"),
                    choices = c("All"),
                    selected = "All"),
        selectInput("color_by",
                    HTML("<strong style='color: #ffffff;'>Color Scatters by</strong>"),
                    choices = c("Taxonomy_Level"),
                    selected = "Taxonomy_Level"),
        sliderInput("comp_threshold",
                    HTML("<strong style='color: #ffffff;'>Completeness Threshold (%)</strong>"),
                    min = 0, max = 100, value = 50,
                    step = 5)
      )
    ),
    div(
      style = "margin-top: 20px; display: flex; justify-content: center; align-items: center; width: 100%;",
      conditionalPanel(
        condition = "input.workflow_type == 'upload_data'",
        actionButton("analyze",
                     "📊 Load & Analyze Data",
                     class = "btn-success btn-lg",
                     style = "max-width: 280px; width: 95%; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;")
      ),
      conditionalPanel(
        condition = "input.workflow_type == 'analyze_mags'",
        actionButton("analyze",
                     "🚀 Run MAGFlow Analysis",
                     class = "btn-primary btn-lg",
                     style = "max-width: 280px; width: 95%; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;")
      )
    ),
    div(
      style = "margin-top: 15px; text-align: center;", # Center the container
      div(
        style = "padding: 8px 12px; background-color: #f1f3f4; border-radius: 5px; display: inline-block; text-align: center;",
        span("Status: ", style = "font-weight: bold; margin-right: 5px; color: #1a2529;"), # Keep original dark color
        textOutput("analysis_status", inline = TRUE)
      )
    )
  ),
  dashboardBody(
    tags$head(

      tags$style(HTML("
      
      /* Ensure the header stays fixed at the top */
      .main-header {
        position: fixed !important;
        top: 0;
        left: 0;
        right: 0;
        z-index: 1030;
        height: 50px;
      }
      
      /* Sidebar: Fixed position, scrolls independently */
      .main-sidebar {
        position: fixed !important;
        top: 5px;
        left: 0;
        width: 350px;
        height: calc(100vh - 50px);
        overflow-y: auto;
        overflow-x: hidden;
        z-index: 1020;
      }
      
      /* Reduce top margin for first sidebar content */
      .main-sidebar .sidebar-menu,
      .main-sidebar > div {
        padding-top: 10px !important;
      }
      
      /* Specific targeting for Choose Your Workflow heading */
      .main-sidebar h4:first-child {
        margin-top: 10px !important;
        margin-bottom: 15px !important;
      }
      
      /* Sidebar scrollbar styling */
      .main-sidebar::-webkit-scrollbar {
        width: 8px;
      }
      .main-sidebar::-webkit-scrollbar-thumb {
        background-color: #3c8dbc;
        border-radius: 4px;
      }
      .main-sidebar::-webkit-scrollbar-track {
        background-color: #f1f3f4;
      }
      
      /* Content wrapper: Account for fixed sidebar */
      .content-wrapper {
        margin-left: 350px !important;
        margin-top: 50px !important;
        min-height: calc(100vh - 50px) !important;
        padding: 0 !important;
      }
      
      /* Content area */
      .content {
        padding: 0 !important;
        min-height: 0 !important;
      }
      
      /* Tab container - work WITH Shiny's structure, not against it */
      .tabbable {
        margin: 0 !important;
        padding: 0 !important;
      }
      
      /* Tabs: Simple horizontal layout */
/* Tabs: Fixed at the top of the content area */
.nav-tabs {
  position: fixed !important;
  top: 50px !important; /* Below the dashboard header */
  left: 350px !important; /* Align with content area, accounting for sidebar width */
  right: 0 !important;
  background-color: #f8f9fa !important;
  border-bottom: 1px solid #ddd !important;
  margin: 0 !important;
  padding: 0 15px !important;
  display: flex !important;
  flex-wrap: nowrap !important; /* Prevent wrapping */
  overflow-x: auto !important; /* Allow horizontal scrolling on narrow screens */
  align-items: stretch !important;
  justify-content: flex-start !important;
  z-index: 1000 !important; /* Ensure tabs stay above content */
  min-height: 50px !important;
  white-space: nowrap !important; /* Keep tabs in a single row */
}

/* Tab items: Natural width, left-aligned */
.nav-tabs > li {
  float: none !important;
  display: flex !important;
  margin: 0 !important;
  position: relative !important;
}

/* Tab links: Clean styling */
.nav-tabs > li > a {
  display: flex !important;
  align-items: center !important;
  justify-content: center !important;
  padding: 12px 20px !important;
  margin: 0 !important;
  border: 1px solid transparent !important;
  border-bottom: none !important;
  border-radius: 4px 4px 0 0 !important;
  color: #666 !important;
  font-size: 14px !important;
  font-weight: 500 !important;
  text-decoration: none !important;
  background-color: transparent !important;
  white-space: nowrap !important;
  min-height: 48px !important;
  box-sizing: border-box !important;
  transition: all 0.2s ease !important;
}

/* Tab hover state */
.nav-tabs > li > a:hover {
  background-color: #e9ecef !important;
  color: #3c8dbc !important;
  border-color: #e9ecef !important;
}

/* Active tab */
.nav-tabs > li.active > a,
.nav-tabs > li.active > a:hover,
.nav-tabs > li.active > a:focus {
  background-color: #fff !important;
  color: #3c8dbc !important;
  border: 1px solid #ddd !important;
  border-bottom: 1px solid #fff !important;
  font-weight: 600 !important;
  cursor: default !important;
}

/* Adjust tab content to prevent overlap with fixed tabs */
.tab-content {
  background-color: #fff !important;
  min-height: calc(100vh - 150px) !important;
  padding: 20px !important;
  margin-top: 100px !important; /* Increased to account for header (50px) + tabs (50px) */
  border: none !important;
}

/* Ensure active tab pane is properly offset */
.tab-content > .tab-pane.active {
  margin-top: 0 !important; /* No additional margin needed, handled by .tab-content */
  min-height: calc(100vh - 190px) !important;
  overflow-y: auto !important;
}

/* Optional: Style the scrollbar for horizontal tab scrolling */
.nav-tabs::-webkit-scrollbar {
  height: 8px !important;
}
.nav-tabs::-webkit-scrollbar-thumb {
  background-color: #3c8dbc !important;
  border-radius: 4px !important;
}
.nav-tabs::-webkit-scrollbar-track {
  background-color: #f1f3f4 !important;
}
      /* Ensure tab panes scroll correctly */
      .tab-pane {
        min-height: calc(100vh - 190px) !important;
        overflow-y: auto !important;
      }
            
      /* Plots and tables */
      .shiny-plot-output, #data_table {
        width: 100%;
        min-height: 400px;
      }
      
      #tax_matrix, #cluster_heatmap, #summary_bars, 
      #checkm_scatter, #busco_scatter, #quast_box, #gunc_box {
        # min-height: 500px !important;
      }
      
      /* Body: Allow normal scrolling within content areas */
      body {
        overflow-x: hidden !important;
      }
      
      /* Retain existing input styling */
      .radio input[type='radio'] {
        -webkit-appearance: none;
        -moz-appearance: none;
        appearance: none;
        width: 16px;
        height: 16px;
        border: 2px solid #e8ecef;
        border-radius: 50%;
        position: relative;
        vertical-align: middle;
        margin-right: 8px;
      }
      
      .radio input[type='radio']:checked::after {
        content: '';
        width: 10px;
        height: 10px;
        border-radius: 50%;
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
      }
      
      .radio input[type='radio'][value='upload_data']:checked::after {
        background-color: #28a745;
      }
      
      .radio input[type='radio'][value='analyze_mags']:checked::after {
        background-color: #007bff;
      }
      
      .radio label {
        font-size: 14px;
        font-weight: 500;
        margin-bottom: 8px;
        padding: 8px;
        border-radius: 5px;
        transition: all 0.2s;
        color: #e8ecef;
      }
      
      .radio input[type='radio']:checked + span {
        border-left: 3px solid;
        color: #ffffff;
        font-weight: 600;
      }
      
      .radio input[type='radio'][value='upload_data']:checked + span {
        border-left-color: #28a745;
      }
      
      .radio input[type='radio'][value='analyze_mags']:checked + span {
        border-left-color: #007bff;
      }
      
      label[for='upload'], label[for='mag_files'] {
        color: #1a2529;
        font-weight: 600;
      }
      
      .shiny-input-container:has(input[type='file']) {
        margin-bottom: 5px !important;
      }
      
      #upload, #mag_files {
        margin-bottom: 0px !important;
      }
      
      #execution_mode {
        margin-top: 5px !important;
      }
      
      .file-input .btn {
        background-color: #28a745;
        color: #ffffff;
        border-color: #28a745;
      }
      
      .file-input .form-control {
        color: #1a2529;
        background-color: #ffffff;
        border: 1px solid #ced4da;
      }
      
      .file-input .form-control::placeholder {
        color: #495057;
      }
      
      label[for='execution_mode'] {
        color: #1a2529 !important;
        font-weight: 600 !important;
        background-color: #ffffff !important;
        padding: 5px 10px;
        border-radius: 4px;
        display: inline-block;
      }
      
      .selectize-input[for='execution_mode'] {
        background-color: #ffffff !important;
        color: #1a2529 !important;
        border: 1px solid #ced4da !important;
      }
      
      div:has(> select#execution_mode) {
        background-color: #ffffff !important;
        padding: 10px;
        border-radius: 5px;
      }
      
      .selectize-input {
        background-color: #ffffff;
        color: #1a2529;
        border: 1px solid #ced4da;
      }
      
      .selectize-input input {
        color: #1a2529;
      }
      
      .selectize-dropdown {
        background-color: #ffffff;
        color: #1a2529;
      }
      
      #analysis_status {
        color: #1a2529;
        font-weight: 500;
      }
      
      div[style*='background-color: #f1f3f4'] h6 {
        color: #1a2529;
      }
      
      .sidebar {
        padding-left: 10px;
        padding-right: 10px;
      }
      
      .sidebar-menu {
        font-size: 13px;
      }
      
      .btn-lg {
        font-size: 16px;
        font-weight: 600;
        padding: 12px 20px;
      }
      
      .btn {
        box-sizing: border-box;
      }
      
      "))
            
    ),
    tabsetPanel(
      tabPanel("Summary",
               uiOutput("summary_content") # Wrap in uiOutput for conditional rendering
      ),
      tabPanel("Scatterplots",
               uiOutput("scatter_content")
      ),
      tabPanel("Boxplots",
               uiOutput("boxplot_content")
      ),
      tabPanel("Taxonomy Matrix",
               uiOutput("tax_matrix_content")
      ),
      tabPanel("Cluster Heatmap",
               uiOutput("heatmap_content")
      ),
      tabPanel("Raw Data",
               uiOutput("data_table_content")
      ),
      tabPanel("About / How To",
               fluidRow(
                 column(12,
                        div(
                          style = "max-width: 800px; margin: 20px auto;",
                          h3("About MAG Quality Dashboard"),
                          div(
                            style = "background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin-bottom: 20px;",
                            h4("📋 Two Ways to Use This Dashboard"),
                            div(
                              style = "display: flex; gap: 20px; margin-top: 15px;",
                              div(
                                style = "flex: 1; background-color: white; padding: 15px; border-radius: 8px; border: 1px solid #28a745;",
                                h5("📊 Option 1: Upload Processed Data", style = "color: #28a745;"),
                                p("If you already have MAG quality assessment results from tools like CheckM2, BUSCO, QUAST, and GUNC:"),
                                tags$ol(
                                  tags$li("Select 'Upload processed data' in the sidebar"),
                                  tags$li("Upload your TSV/CSV file with the required columns"),
                                  tags$li("Click 'Load & Analyze Data' to generate visualizations")
                                )
                              ),
                              div(
                                style = "flex: 1; background-color: white; padding: 15px; border-radius: 8px; border: 1px solid #007bff;",
                                h5("🧬 Option 2: Analyze Raw MAG Files", style = "color: #007bff;"),
                                p("If you have raw MAG FASTA files and want to run the complete analysis pipeline:"),
                                tags$ol(
                                  tags$li("Select 'Analyze raw MAG files' in the sidebar"),
                                  tags$li("Upload your FASTA files"),
                                  tags$li("Choose execution mode (Local Docker or Cloud)"),
                                  tags$li("Click 'Run MAGFlow Analysis'")
                                )
                              )
                            )
                          ),
                          h4("📋 Required Data Columns"),
                          p("For uploaded TSV/CSV files, ensure these columns are present:"),
                          tags$ul(
                            tags$li(tags$strong("Sample_ID:"), " Unique identifier for each sample"),
                            tags$li(tags$strong("Taxonomy_Level:"), " Taxonomic classification"),
                            tags$li(tags$strong("Completeness:"), " MAG completeness percentage"),
                            tags$li(tags$strong("Contamination:"), " MAG contamination percentage"),
                            tags$li(tags$strong("CSS:"), " Contamination score from GUNC"),
                            tags$li(tags$strong("N50:"), " Assembly N50 statistic"),
                            tags$li(tags$strong("Complete_SCO:"), " Complete single-copy orthologs from BUSCO")
                          ),
                          h4("🛠️ Prerequisites for MAGFlow Analysis"),
                          tags$ul(
                            tags$li("Nextflow installed and available in PATH"),
                            tags$li("Docker installed and running"),
                            tags$li("Internet connection for downloading pipeline and containers")
                          ),
                          h4("📊 Dashboard Features"),
                          tags$ul(
                            tags$li(tags$strong("Summary:"), " Overview of MAG quality distribution"),
                            tags$li(tags$strong("Scatterplots:"), " CheckM2 and BUSCO quality assessments"),
                            tags$li(tags$strong("Boxplots:"), " Assembly statistics and contamination scores"),
                            tags$li(tags$strong("Taxonomy Matrix:"), " Presence/absence of different taxonomic groups"),
                            tags$li(tags$strong("Cluster Heatmap:"), " Sample clustering based on quality metrics"),
                            tags$li(tags$strong("Raw Data:"), " Interactive table with download capability")
                          )
                        )
                 )
               )
      )
    )
  )
)




server <- function(input, output, session) {
  # Status message for sidebar
  output$analysis_status <- renderText({
    if (input$workflow_type == "upload_data") {
      if (is.null(input$upload)) {
        "Ready - Please upload your data file"
      } else {
        "File uploaded - Ready to analyze"
      }
    } else {
      if (is.null(input$mag_files) || length(input$mag_files$name) == 0) {
        "Ready - Please upload MAG files"
      } else {
        paste("Files ready:", length(input$mag_files$name), "MAG files uploaded")
      }
    }
  })
  
  # Show settings panel when data is available
  output$show_settings <- reactive({
    if (input$workflow_type == "upload_data") {
      !is.null(input$upload)
    } else {
      !is.null(input$mag_files) && length(input$mag_files$name) > 0
    }
  })
  outputOptions(output, "show_settings", suspendWhenHidden = FALSE)
  
  # Reactive data for uploaded TSV/CSV
  data_reactive <- reactive({
    if (input$workflow_type == "upload_data") {
      req(input$upload)
      df <- readr::read_tsv(input$upload$datapath)
      # Validate required columns
      required_cols <- c("Sample_ID", "Taxonomy_Level", "Completeness", "Contamination", "CSS", "N50", "Complete_SCO")
      missing_cols <- setdiff(required_cols, colnames(df))
      validate(
        need(length(missing_cols) == 0, paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
      )
      df
    } else {
      return(NULL)  # Will handle in processed_data
    }
  })
  
  # Process data when "Analyze" button is clicked
  processed_data <- eventReactive(input$analyze, {
    withProgress(message = "Running analysis...", value = 0, {
      if (input$workflow_type == "analyze_mags") {
        req(input$mag_files)
        # Handle MAG analysis
        incProgress(0.1, detail = "Preparing files...")
        mag_dir <- tempdir()
        sample_dir <- file.path(mag_dir, "sample1")
        dir.create(sample_dir, showWarnings = FALSE)
        for (i in seq_along(input$mag_files$datapath)) {
          file.copy(input$mag_files$datapath[i], file.path(sample_dir, input$mag_files$name[i]))
        }
        output_dir <- tempdir()
        
        if (input$execution_mode == "Cloud") {
          output$analysis_status <- renderText("Cloud execution not directly supported. Please set up cloud backend and run MAGFlow manually. See About tab for instructions.")
          return(NULL)
        } else {
          # Local Docker
          incProgress(0.2, detail = "Running MAGFlow...")
          tryCatch({
            processx::run("nextflow", args = c("run", "https://github.com/jeffe107/MAGFlow", "-profile", "docker", "--files", shQuote(file.path(sample_dir, "*")), "--outdir", output_dir), echo = TRUE)
            final_df_path <- file.path(output_dir, "final_df.tsv")
            if (!file.exists(final_df_path)) {
              stop("final_df.tsv not found in output.")
            }
            df <- readr::read_tsv(final_df_path)
          }, error = function(e) {
            output$analysis_status <- renderText(paste("Error running analysis:", e$message, "Ensure Nextflow and Docker are installed."))
            return(NULL)
          })
        }
      } else {
        df <- data_reactive()
      }
      
      if (is.null(df)) return(NULL)
      
      incProgress(0.7, detail = "Processing data...")
      df <- df %>%
        dplyr::filter(Completeness >= input$comp_threshold) %>%
        dplyr::mutate(Quality = dplyr::case_when(
          Completeness > 90 & Contamination < 5 ~ "High",
          Completeness > 50 & Contamination < 10 ~ "Medium",
          TRUE ~ "Low"
        ),
        GUNC_Pass = CSS < 0.45)  # Example threshold from GUNC
      
      # Add stats: e.g., Kruskal-Wallis on completeness by sample
      kw_test <- kruskal.test(Completeness ~ Sample_ID, data = df)
      # Welch ANOVA for N50 by sample
      anova_test <- oneway.test(N50 ~ Sample_ID, data = df, var.equal = FALSE)
      
      incProgress(1, detail = "Done")
      output$analysis_status <- renderText("Analysis complete")
      list(df = df, kw_pval = kw_test$p.value, anova_pval = anova_test$p.value)
    })
  })
  
  # Conditional rendering for each tab
  output$summary_content <- renderUI({
    req(processed_data())
    tagList(
      plotlyOutput("summary_bars", height = "auto"),
      textOutput("stats_summary")
    )
  })
  
  output$scatter_content <- renderUI({
    req(processed_data())
    tagList(
      plotlyOutput("checkm_scatter", height = "auto"),
      plotlyOutput("busco_scatter", height = "auto")
    )
  })
  
  output$boxplot_content <- renderUI({
    req(processed_data())
    tagList(
      plotlyOutput("quast_box", height = "auto"),
      textOutput("quast_p"),
      plotlyOutput("gunc_box", height = "auto")
    )
  })
  
  output$tax_matrix_content <- renderUI({
    req(processed_data())
    plotlyOutput("tax_matrix", height = "auto")
  })
  
  output$heatmap_content <- renderUI({
    req(processed_data())
    plotlyOutput("cluster_heatmap", height = "auto")
  })
  
  output$data_table_content <- renderUI({
    req(processed_data())
    tagList(
      DTOutput("data_table"),
      downloadButton("downloadData", "Download Processed Data")
    )
  })
  
  # Summary Bars: % High/Medium/Low quality, GUNC pass
  output$summary_bars <- renderPlotly({
    req(processed_data())
    df <- processed_data()$df
    summary_df <- df %>%
      dplyr::group_by(Sample_ID) %>%
      dplyr::summarise(High_Q = sum(Quality == "High") / n() * 100,
                       Med_Q = sum(Quality == "Medium") / n() * 100,
                       Low_Q = sum(Quality == "Low") / n() * 100,
                       GUNC_Pass_Pct = mean(GUNC_Pass) * 100)
    summary_long <- summary_df %>%
      tidyr::pivot_longer(cols = c(High_Q, Med_Q, Low_Q), names_to = "Quality", values_to = "Pct")
    p <- ggplot(summary_long, aes(x = Sample_ID, y = Pct, fill = Quality)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(y = "% MAGs")
    ggplotly(p) %>% layout(height = min(400, max(200, length(unique(summary_long$Sample_ID)) * 50)))
  })
  
  output$stats_summary <- renderText({
    req(processed_data())
    paste("Kruskal-Wallis p-value for completeness across samples:", round(processed_data()$kw_pval, 4))
  })
  
  # CheckM2 Scatter: Completeness vs Contamination
  output$checkm_scatter <- renderPlotly({
    req(processed_data())
    p <- ggplot(processed_data()$df, aes(x = Completeness, y = Contamination, color = get(input$color_by), text = Sample_ID)) +
      geom_point() +
      theme_minimal() +
      labs(color = input$color_by)
    ggplotly(p, tooltip = c("text", "color"))
  })
  
  # BUSCO Scatter: Complete_SCO vs Contamination
  output$busco_scatter <- renderPlotly({
    req(processed_data())
    p <- ggplot(processed_data()$df, aes(x = Complete_SCO, y = Contamination, color = get(input$color_by), text = Sample_ID)) +
      geom_point() +
      theme_minimal() +
      labs(color = input$color_by)
    ggplotly(p, tooltip = c("text", "color"))
  })
  
  # Boxplots: N50 by Sample for QUAST
  output$quast_box <- renderPlotly({
    req(processed_data())
    p <- ggplot(processed_data()$df, aes(x = Sample_ID, y = N50)) +
      geom_boxplot() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  output$quast_p <- renderText({
    req(processed_data())
    paste("Welch ANOVA p-value for N50 across samples:", round(processed_data()$anova_pval, 4))
  })
  
  # GUNC Box: CSS by Sample
  output$gunc_box <- renderPlotly({
    req(processed_data())
    p <- ggplot(processed_data()$df, aes(x = Sample_ID, y = CSS)) +
      geom_boxplot() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  # Taxonomy Presence/Absence Matrix
  output$tax_matrix <- renderPlotly({
    req(processed_data())
    df <- processed_data()$df
    
    # Filter by selected taxonomy level if not "All"
    if (input$tax_level != "All") {
      df <- df %>% dplyr::filter(Taxonomy_Level == input$tax_level)
    }
    
    # Create presence/absence matrix
    matrix_df <- df %>%
      dplyr::filter(!is.na(Taxonomy_Level)) %>%
      dplyr::distinct(Sample_ID, Taxonomy_Level) %>%
      dplyr::mutate(presence = 1) %>%
      tidyr::complete(Sample_ID, Taxonomy_Level, fill = list(presence = 0))
    
    p <- ggplot(matrix_df, aes(x = Sample_ID, y = Taxonomy_Level, fill = factor(presence))) +
      geom_tile(color = "white", size = 0.5) +
      scale_fill_manual(values = c("0" = "lightgray", "1" = "darkblue"),
                        name = "Present",
                        labels = c("0" = "No", "1" = "Yes")) +
      labs(title = "Taxonomy Presence/Absence Matrix",
           x = "Sample ID",
           y = "Taxonomy Level") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid = element_blank(),
            plot.title = element_text(hjust = 0.5))
    
    ggplotly(p, tooltip = c("x", "y", "fill")) %>%
      layout(height = min(800, max(400, length(unique(matrix_df$Taxonomy_Level)) * 20)))
  })
  
  # Cluster Heatmap: Average metrics by sample
  output$cluster_heatmap <- renderPlotly({
    req(processed_data())
    avg_df <- processed_data()$df %>%
      dplyr::group_by(Sample_ID) %>%
      dplyr::summarise(across(c(Completeness, Contamination, Complete_SCO, CSS), mean, na.rm = TRUE))
    heatmaply(as.matrix(avg_df[, -1]), labRow = avg_df$Sample_ID) %>%
      layout(height = min(800, max(400, length(unique(avg_df$Sample_ID)) * 30)))
  })
  
  # Raw Table: Interactive with filters
  output$data_table <- renderDT({
    req(processed_data())
    datatable(processed_data()$df, options = list(scrollY = "500px", scrollX = TRUE, pageLength = 10, searchHighlight = TRUE))
  })
  
  # Download handler for processed data
  output$downloadData <- downloadHandler(
    filename = function() {
      "processed_mag_quality.tsv"
    },
    content = function(file) {
      readr::write_tsv(processed_data()$df, file)
    }
  )
}

shinyApp(ui, server)