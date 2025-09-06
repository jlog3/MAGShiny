library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(DT)
library(stats)
library(heatmaply)  # For interactive heatmaps

ui <- dashboardPage(
  dashboardHeader(title = "MAG Quality Dashboard (Inspired by BIgMAG)"),
  dashboardSidebar(
    fileInput("upload", "Upload TSV/CSV File", accept = c(".tsv", ".csv")),
    selectInput("tax_level", "Taxonomy Level for Matrix", choices = c("Genus", "Family", "Order"), selected = "Genus"),
    actionButton("analyze", "Run Analysis")
  ),
  dashboardBody(
    tabsetPanel(
      tabPanel("Summary",
               plotlyOutput("summary_bars"),
               textOutput("stats_summary")
      ),
      tabPanel("Scatterplots",
               plotlyOutput("checkm_scatter"),
               plotlyOutput("busco_scatter")
      ),
      tabPanel("Boxplots",
               plotlyOutput("quast_box"),
               plotlyOutput("gunc_box")
      ),
      tabPanel("Taxonomy Matrix",
               plotlyOutput("tax_matrix")
      ),
      tabPanel("Cluster Heatmap",
               plotlyOutput("cluster_heatmap")
      ),
      tabPanel("Raw Data",
               DTOutput("data_table")
      )
    )
  )
)

server <- function(input, output, session) {
  data_reactive <- reactive({
    req(input$upload)
    readr::read_tsv(input$upload$datapath)  # Or read_csv if CSV
  })
  
  processed_data <- eventReactive(input$analyze, {
    df <- data_reactive()
    # Basic processing: Filter, group by sample, compute quality categories
    df <- df %>%
      mutate(Quality = case_when(
        Completeness > 90 & Contamination < 5 ~ "High",
        Completeness > 50 & Contamination < 10 ~ "Medium",
        TRUE ~ "Low"
      ),
      GUNC_Pass = CSS < 0.45)  # Example threshold from GUNC
    # Add stats: e.g., Kruskal-Wallis on completeness by sample
    kw_test <- kruskal.test(Completeness ~ Sample_ID, data = df)
    list(df = df, kw_pval = kw_test$p.value)
  })
  
  # Summary Bars: % High/Medium/Low quality, GUNC pass, etc.
  output$summary_bars <- renderPlotly({
    req(processed_data())
    df <- processed_data()$df
    summary_df <- df %>%
      group_by(Sample_ID) %>%
      summarise(High_Q = mean(Quality == "High") * 100,
                Med_Q = mean(Quality == "Medium") * 100,
                GUNC_Pass_Pct = mean(GUNC_Pass) * 100)
    p <- ggplot(summary_df, aes(x = Sample_ID)) +
      geom_bar(aes(y = High_Q, fill = "High"), stat = "identity") +
      geom_bar(aes(y = Med_Q, fill = "Medium"), stat = "identity") +
      theme_minimal() + labs(y = "% MAGs")
    ggplotly(p)
  })
  
  output$stats_summary <- renderText({
    req(processed_data())
    paste("Kruskal-Wallis p-value for completeness across samples:", processed_data()$kw_pval)
  })
  
  # CheckM2 Scatter: Completeness vs Contamination, colored by Taxonomy
  output$checkm_scatter <- renderPlotly({
    req(processed_data())
    p <- ggplot(processed_data()$df, aes(x = Completeness, y = Contamination, color = Taxonomy_Level, text = Sample_ID)) +
      geom_point() + theme_minimal()
    ggplotly(p, tooltip = "text")
  })
  
  # BUSCO Scatter: Similar to above
  output$busco_scatter <- renderPlotly({
    # Implement similarly
  })
  
  # Boxplots: e.g., N50 by Sample for QUAST
  output$quast_box <- renderPlotly({
    req(processed_data())
    p <- ggplot(processed_data()$df, aes(x = Sample_ID, y = N50)) +
      geom_boxplot() + theme_minimal()
    ggplotly(p)
  })
  
  # GUNC Box: CSS by Sample
  output$gunc_box <- renderPlotly({
    # Implement similarly
  })
  
  # Taxonomy Presence/Absence Matrix: Pivot to wide format
  output$tax_matrix <- renderPlotly({
    req(processed_data())
    matrix_df <- processed_data()$df %>%
      pivot_wider(names_from = input$tax_level, values_from = Some_Count, values_fill = 0)  # Adjust based on data
    heatmaply(matrix_df, Rowv = FALSE, Colv = FALSE)
  })
  
  # Cluster Heatmap: Average metrics by sample
  output$cluster_heatmap <- renderPlotly({
    req(processed_data())
    avg_df <- processed_data()$df %>%
      group_by(Sample_ID) %>%
      summarise(across(c(Completeness, Contamination, Complete_SCO, CSS), mean, na.rm = TRUE))
    heatmaply(as.matrix(avg_df[, -1]), labRow = avg_df$Sample_ID)
  })
  
  # Raw Table: Interactive with filters
  output$data_table <- renderDT({
    req(processed_data())
    datatable(processed_data()$df, options = list(pageLength = 10, searchHighlight = TRUE))
  })
}

shinyApp(ui, server)