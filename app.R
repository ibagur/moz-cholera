library(shiny)
library(here)
library(shinybusy)

# Define the UI
ui <- fluidPage(
  #add_busy_spinner(spin = "fading-circle", position = "top-right"),
  add_busy_gif(src = "https://raw.githubusercontent.com/ibagur/images/main/unicef_logo_clockwise.gif", position = "top-right", height = 80, width = 80),
  #add_busy_bar(color = "#FF0000")
  titlePanel("Cholera Dataset Analysis"),
  sidebarLayout(
    sidebarPanel(
      width = 3, 
      fileInput('dataset', 'Choose Latest Bulletin File', accept = c(".xlsx")),
      actionButton("runAnalysis", "Run Analysis", style = "margin-bottom: 10px;"),
      wellPanel(
        textOutput("result")
      ),
      uiOutput("downloadButton1"),  # Placeholder for the first download button
      uiOutput("downloadButton2"),  # Repeat for each plot
      uiOutput("downloadButton3"),
      uiOutput("downloadButton4"),
      uiOutput("downloadButton5"),
      uiOutput("downloadButton6")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Preview 1",
                 fluidRow(
                   column(6, 
                          #withLoader(imageOutput("map_png"), type="html", loader="loader1")
                          imageOutput("map_png")
                          ),
                   column(6,
                          imageOutput("plot2_png"),
                          imageOutput("plot3_png")
                   )
                 )
        ),
        tabPanel("Preview 2",
                 fluidRow(
                   column(12,
                          imageOutput("plot4_png")
                   )
                 )
        ), 
        tabPanel("Preview 3",
                 fluidRow(
                   column(6, 
                          imageOutput("plot5_png")
                          ),
                   column(6, 
                          uiOutput("tableOutput")
                          #reactableOutput("table")
                          )
                 )
        )
      )
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Default paths for images
  defaultMapPath <- here("R", "moz-cholera", "plots", "cholera_map.png")
  defaultPlot2Path <- here("R", "moz-cholera", "plots", "province_daily_trend_plot.png")
  defaultPlot3Path <- here("R", "moz-cholera", "plots", "province_weekly_bar_plot.png")
  defaultPlot4Path <- here("R", "moz-cholera", "plots", "district_biweekly_plot.png")
  defaultPlot5Path <- here("R", "moz-cholera", "plots", "cholera_active_occupancy_district_plot.png")
  defaultTablePath <- here("R", "moz-cholera", "plots", "cholera_active_occupancy_district_react.png")
  
  # Modify renderImage functions to use default paths
  output$map_png <- renderImage({
    list(src = defaultMapPath, width = "100%", alt = "Cholera map")
  }, deleteFile = FALSE)
  
  output$plot2_png <- renderImage({
    list(src = defaultPlot2Path, width = "100%", alt = "Daily trend plot")
  }, deleteFile = FALSE)
  
  output$plot3_png <- renderImage({
    list(src = defaultPlot3Path, width = "100%", alt = "Weekly bar plot")
  }, deleteFile = FALSE)
  
  output$plot4_png <- renderImage({
    list(src = defaultPlot4Path, width = "100%", alt = "Bi-weekly plot")
  }, deleteFile = FALSE)
  
  output$plot5_png <- renderImage({
    list(src = defaultPlot5Path, width = "100%", alt = "Occupancy district plot")
  }, deleteFile = FALSE)

  output$table_default <- renderImage({
    list(src = defaultTablePath, width = "100%", alt = "Occupancy reactable")
  }, deleteFile = FALSE)  
  
  # Placeholder for image (make sure your image is in the correct folder)
  output$tableOutput <- renderUI({
    imageOutput("table_default")
  })
  
  # Reactive to indicate when analysis is not done
  analysisRun <- reactiveVal(FALSE)
  
  observeEvent(input$runAnalysis, {
    # Set analysisRun to TRUE since analysis is triggered
    analysisRun(TRUE)
    
    req(input$dataset)  # Ensure a file is uploaded
    
    # Construct the target path dynamically using 'here'
    targetPath <- here("R", "moz-cholera", "data", input$dataset$name)
    
    # Ensure the directory exists
    dir.create(dirname(targetPath), recursive = TRUE, showWarnings = FALSE)
    
    # Move the uploaded file to the target directory
    file.rename(input$dataset$datapath, targetPath)
    
    # Run analysis scripts

    withProgress(message = 'Analysis in progress', value = 0, {
      
      #source(here("R", "moz-cholera", "moz-cholera.R"))

      # 0. Load libraries
      incProgress(1/5, detail = "Loading libraries...")
      
      library(here)
      source(here("R", "_common", "required.R"))
      source(here("R", "_common", "required-plot.R"))
      source(here("R", "_common", "utils.R"))
      source(here("R", "_common", "config.R"))
      source(here("R", "moz-cholera", paste0("01-", "moz-cholera", "-required.R")))
      source(here("R", "moz-cholera", paste0("02-", "moz-cholera", "-functions.R")))
      source(here("R", "moz-cholera", paste0("03-", "moz-cholera", "-config.R")))
      
      incProgress(1/5, detail = "Loading data...")
      
      # 1. Load data
      source(here("R", "moz-cholera", paste0("04-", "moz-cholera", "-dataload.R")))
      
      incProgress(1/5, detail = "Processing data...")
      
      # 2. Process data
      source(here("R", "moz-cholera", paste0("05-", "moz-cholera", "-process.R")))
      
      incProgress(1/5, detail = "Building plots...")
      
      # 3. Create the plots
      source(here("R", "moz-cholera", paste0("07-", "moz-cholera", "-plot.R")))      
     
      # Finalize the progress bar
      setProgress(message = "Process completed.")
    })  
  
    # Assume the plot is saved as 'plot.pdf' in a known directory
    plotPath1 <- reactive({
      here("R", "moz-cholera", "plots", "cholera_map.pdf")
    })
    
    plotPath2 <- reactive({
      here("R", "moz-cholera", "plots", "province_daily_trend_plot.pdf")
    })
    
    plotPath3 <- reactive({
      here("R", "moz-cholera", "plots", "province_weekly_bar_plot.pdf")
    })
    
    plotPath4 <- reactive({
      here("R", "moz-cholera", "plots", "district_biweekly_plot.pdf")
    })
    
    plotPath5 <- reactive({
      here("R", "moz-cholera", "plots", "cholera_active_occupancy_district_plot.pdf")
    })
    
    plotPath6 <- reactive({
      here("R", "moz-cholera", "plots", "cholera_active_occupancy_district_react.png")
    })
    
    # Provide feedback to the user
    output$result <- renderText("Analysis completed successfully!")

    # Save the plot as a reactive expression if it depends on inputs or other reactive values
    plot6 <- reactive({
      cholera_active_occupancy_district_react
    })
    
    # For images
    output$map_png <- renderImage({
      # Return a list containing the image's src and, optionally, height and width
      list(src = here("R", "moz-cholera", "plots", "cholera_map.png"),
           width = "100%",  # Optional: Specify width
           #height = 400, # Optional: Specify height,
           alt = "This is alternate text"
      )
    }, deleteFile = FALSE)
    
    output$plot2_png <- renderImage({
      # Return a list containing the image's src and, optionally, height and width
      list(src = here("R", "moz-cholera", "plots", "province_daily_trend_plot.png"),
           width = "100%",  # Optional: Specify width
           #height = 400, # Optional: Specify height,
           alt = "This is alternate text"
      )
    }, deleteFile = FALSE)
    
    output$plot3_png <- renderImage({
      # Return a list containing the image's src and, optionally, height and width
      list(src = here("R", "moz-cholera", "plots", "province_weekly_bar_plot.png"),
           width = "100%",  # Optional: Specify width
           #height = 400, # Optional: Specify height,
           alt = "This is alternate text"
      )
    }, deleteFile = FALSE)
    
    output$plot4_png <- renderImage({
      # Return a list containing the image's src and, optionally, height and width
      list(src = here("R", "moz-cholera", "plots", "district_biweekly_plot.png"),
           width = "100%",  # Optional: Specify width
           #height = 400, # Optional: Specify height,
           alt = "This is alternate text"
      )
    }, deleteFile = FALSE)
    
    output$plot5_png <- renderImage({
      # Return a list containing the image's src and, optionally, height and width
      list(src = here("R", "moz-cholera", "plots", "cholera_active_occupancy_district_plot.png"),
           width = "100%",  # Optional: Specify width
           #height = 400, # Optional: Specify height,
           alt = "This is alternate text"
      )
    }, deleteFile = FALSE)
    
    output$table <- renderReactable({
      plot6()
    })
    
    # Conditional output for table or default image
    output$tableOutput <- renderUI({
      if (analysisRun()) {
        # If analysis has been run, display the reactable output
        reactableOutput("table")
      }
    })
    
    # After the plot is generated, display the download button
    output$downloadButton1 <- renderUI({
      downloadButton("downloadPlot1", "Download map", style = "margin-bottom: 10px;")
    })
    
    output$downloadButton2 <- renderUI({
      downloadButton("downloadPlot2", "Download daily trend", style = "margin-bottom: 10px;")
    })
    
    output$downloadButton3 <- renderUI({
      downloadButton("downloadPlot3", "Download weekly trend", style = "margin-bottom: 10px;")
    })
    
    output$downloadButton4 <- renderUI({
      downloadButton("downloadPlot4", "Download bi-weekly chart", style = "margin-bottom: 10px;")
    })
    
    output$downloadButton5 <- renderUI({
      downloadButton("downloadPlot5", "Download occupancy plot", style = "margin-bottom: 10px;")
    })
    
    output$downloadButton6 <- renderUI({
      downloadButton("downloadPlot6", "Download occupancy table", style = "margin-bottom: 10px;")
    })

    # Add this block for the download handler
    output$downloadPlot1 <- downloadHandler(
      filename = function() {
        "cholera_map.pdf"
      },
      content = function(file) {
        file.copy(plotPath1(), file)
      }
    )
    
    output$downloadPlot2 <- downloadHandler(
      filename = function() {
        "province_daily_trend_plot.pdf"
      },
      content = function(file) {
        file.copy(plotPath2(), file)
      }
    )
    
    output$downloadPlot3 <- downloadHandler(
      filename = function() {
        "province_weekly_trend_bar.pdf"
      },
      content = function(file) {
        file.copy(plotPath3(), file)
      }
    )
    
    output$downloadPlot4 <- downloadHandler(
      filename = function() {
        "district_biweekly_plot.pdf"
      },
      content = function(file) {
        file.copy(plotPath4(), file)
      }
    )
    
    output$downloadPlot5 <- downloadHandler(
      filename = function() {
        "cholera_active_occupancy_district_plot.pdf"
      },
      content = function(file) {
        file.copy(plotPath5(), file)
      }
    )
    
    output$downloadPlot6 <- downloadHandler(
      filename = function() {
        "cholera_active_occupancy_district_react.png"
      },
      content = function(file) {
        file.copy(plotPath6(), file)
      }
    )
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)
