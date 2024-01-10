library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(agricolae)
library(car)
library(DT)

ui <- dashboardPage(
  dashboardHeader(title = "Product Placement Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload Data", tabName = "upload", icon = icon("cloud-upload")),
      menuItem("Data Analysis", tabName = "analysis", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "upload",
        fluidPage(
          titlePanel("Upload Data"),
          sidebarLayout(
            sidebarPanel(
              fileInput("file_input", "Choose CSV or XLSX file"),
              actionButton("load_data_btn", "Load Data", icon = icon("upload"))
            ),
            mainPanel(
              h4("Data Preview"),
              DTOutput("data_preview"),
              tags$hr(),  # Add a horizontal line for separation
              h4("Data Summary"),
              DTOutput("data_summary")
            )
          )
        )
      ),
      tabItem(
        tabName = "analysis",
        fluidPage(
          titlePanel("Data Analysis"),
          fluidRow(
            column(
              width = 4,
              selectInput("placement_selector", "Select Placement Variable", choices = NULL),
              tags$hr(),  # Add a horizontal line for separation
              actionButton("run_analysis_btn", "Run Analysis", icon = icon("play")),
              checkboxGroupInput("analysis_options",
                                 label = "Select Analyses to Perform",
                                 choices = c("ANOVA", "Tukey's HSD", "Duncan's Test", 
                                             "Durbin-Watson Test", "Bartlett's Test", 
                                             "Shapiro-Wilk Test", "Recommendation"),
                                 selected = c("ANOVA", "Tukey's HSD", "Recommendation")
              )
            ),
            column(
              width = 8,
              h4("Click-Through Rate (CTR) Plot"),
              plotOutput("ctr_plot")
            ),
            column(
              width = 12,
              h4("ANOVA Analysis"),
              verbatimTextOutput("anova_result"),
              h4("Tukey's HSD"),
              verbatimTextOutput("tukey_result"),
              plotOutput("tukey_plot"),
              h4("Duncan's Test"),
              verbatimTextOutput("duncan_result"),
              h4("Durbin-Watson Test"),
              verbatimTextOutput("durbin_watson_test"),
              h4("Bartlett's Test"),
              verbatimTextOutput("bartlett_test"),
              h4("Shapiro-Wilk Test"),
              verbatimTextOutput("shapiro_test"),
              h4("Recommendation"),
              verbatimTextOutput("recommendation")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  
  analyses <- reactive({
    analyses <- c()
    if ("ANOVA" %in% input$analysis_options) analyses <- c(analyses, "ANOVA")
    if ("Tukey's HSD" %in% input$analysis_options) analyses <- c(analyses, "Tukey's HSD")
    if ("Duncan's Test" %in% input$analysis_options) analyses <- c(analyses, "Duncan's Test")
    if ("Durbin-Watson Test" %in% input$analysis_options) analyses <- c(analyses, "Durbin-Watson Test")
    if ("Bartlett's Test" %in% input$analysis_options) analyses <- c(analyses, "Bartlett's Test")
    if ("Shapiro-Wilk Test" %in% input$analysis_options) analyses <- c(analyses, "Shapiro-Wilk Test")
    if ("Recommendation" %in% input$analysis_options) analyses <- c(analyses, "Recommendation")
    analyses
  })
  
  observeEvent(input$run_analysis_btn, {
    req(data())
    
    if ("ANOVA" %in% analyses()) {
      output$anova_result <- renderPrint({
        anova_result <- aov(x ~ placement, data = data())
        summary(anova_result)
      })
    } else {
      output$anova_result <- renderPrint(NULL)
    }
    
    if ("Tukey's HSD" %in% analyses()) {
      output$tukey_result <- renderPrint({
        anova_result <- aov(x ~ placement, data = data())
        TukeyHSD(anova_result, ordered = TRUE)
      })
      output$tukey_plot <- renderPlot({
        anova_result <- aov(x ~ placement, data = data())
        plot(TukeyHSD(anova_result), las = 1)
      })
    } else {
      output$tukey_result <- renderPrint(NULL)
      output$tukey_plot <- renderPlot(NULL)
    }
    
    if ("Duncan's Test" %in% analyses()) {
      output$duncan_result <- renderPrint({
        anova_result <- aov(x ~ placement, data = data())
        duncan.test(anova_result, "placement", alpha = 0.05, console = TRUE)
      })
    } else {
      output$duncan_result <- renderPrint(NULL)
    }
    
    if ("Durbin-Watson Test" %in% analyses()) {
      output$durbin_watson_test <- renderPrint({
        anova_result <- aov(x ~ placement, data = data())
        durbinWatsonTest(anova_result)
      })
    } else {
      output$durbin_watson_test <- renderPrint(NULL)
    }
    
    if ("Bartlett's Test" %in% analyses()) {
      output$bartlett_test <- renderPrint({
        bartlett_test <- bartlett.test(x ~ placement, data = data())
        bartlett_test
      })
    } else {
      output$bartlett_test <- renderPrint(NULL)
    }
    
    if ("Shapiro-Wilk Test" %in% analyses()) {
      output$shapiro_test <- renderPrint({
        anova_result <- aov(x ~ placement, data = data())
        shapiro_test <- shapiro.test(anova_result$residuals)
        shapiro_test
      })
    } else {
      output$shapiro_test <- renderPrint(NULL)
    }
    
    if ("Recommendation" %in% analyses()) {
      output$recommendation <- renderText({
        placement_data <- data.frame(
          Placement = data()$placement,
          CTR = as.numeric(data()$x)
        )
        grouped_data <- split(placement_data$CTR, placement_data$Placement)
        anova_result <- aov(x ~ placement, data = data())
        mean_ctr <- sapply(grouped_data, mean)
        best_placement <- names(mean_ctr)[which.max(mean_ctr)]
        paste("Based on the analysis, it is recommended to focus on",
              best_placement, "placement for better Click-Through Rates.")
      })
    } else {
      output$recommendation <- renderText(NULL)
    }
  })
  
  observeEvent(input$load_data_btn, {
    req(input$file_input)
    
    file_type <- tools::file_ext(input$file_input$name)
    if (file_type %in% c("csv", "xlsx")) {
      if (file_type == "csv") {
        data_value <- read.csv(input$file_input$datapath)
      } else if (file_type == "xlsx") {
        data_value <- readxl::read_excel(input$file_input$datapath)
      }
      
      if (all(c("placement", "x") %in% colnames(data_value))) {
        data(data_value)
      } else {
        showModal(modalDialog(
          title = "Invalid Data",
          "Uploaded data must contain columns for 'placement' and 'x'.",
          easyClose = TRUE
        ))
      }
    } else {
      showModal(modalDialog(
        title = "Invalid File",
        "Please upload a valid CSV or XLSX file.",
        easyClose = TRUE
      ))
    }
  })
  
  observeEvent(data(), {
    updateSelectInput(session, "placement_selector", choices = colnames(data()))
  })
  
  output$data_preview <- renderDT({
    req(data())
    datatable(data())
  })
  
  output$data_summary <- renderDT({
    req(data())
    summary_data <- summary(data())
    summary_data <- as.data.frame(summary_data)
    summary_data$Variable <- rownames(summary_data)
    datatable(summary_data, options = list(dom = 't'))
  })
  
  output$ctr_plot <- renderPlot({
    req(data())
    placement_data <- data.frame(
      Placement = data()$placement,
      CTR = as.numeric(data()$x)
    )
    mean_ctr_by_placement <- aggregate(CTR ~ Placement, data = placement_data, mean)
    barplot(
      mean_ctr_by_placement$CTR,
      names.arg = mean_ctr_by_placement$Placement,
      col = "skyblue",
      main = "Mean Click-Through Rate (CTR) by Ad Placement",
      ylab = "Mean CTR",
      xlab = "Placement",
      border = "white"
    )
  })
  
}

shinyApp(ui, server)
