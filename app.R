# 01-kmeans-app

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

library(shiny)
library(dplyr)
library(ggplot2)

# make file location the wd
#this.dir <- dirname(parent.frame(2)$ofile)
#setwd(this.dir)

allData <- read.csv("C:\\GitHubRepos\\CCII_ShinyApp\\AllData(RA2).csv", header = TRUE)

ui <- fluidPage(
  headerPanel('RA2 CCII Arable crop'),
  sidebarPanel(
    selectInput('scn', 'Select the scenario', as.character(unique(allData$thisScenario))),
    selectInput('crop', 'Select the crop', as.character(unique(allData$CurrentCrop))),
    selectInput('soil', 'Select the soil', as.character(unique(allData$thisSoil))),
    selectInput('xcol', 'Select X Variable', names(allData)),
    selectInput('ycol', 'Select Y Variable', names(allData),
      selected = names(allData)[[12]]),
    numericInput('clusters', 'Cluster count', 3,
      min = 1, max = 9)
  ),
  mainPanel(
  #  plotOutput('plot1')
    tabsetPanel(
      tabPanel("Plot results", plotOutput("plot1"), plotOutput("plot2")),
      tabPanel("Map analysis", verbatimTextOutput("summary")),
      tabPanel("Tables", tableOutput("table"))
    )
    
  )
)

server <- function(input, output) {

  selectedData <- reactive({
    
    # Due to dplyr issue #318, we need temp variables for input values
    crop <- input$crop
    soil <- input$soil
    scn <- input$scn
    
    allData <- allData %>%
      filter( CurrentCrop == crop & 
                thisSoil == soil  &
               thisScenario == "base"
              )
    
    
    allData[, c(input$xcol, input$ycol)]
  })
  
  # fut
  selectedDataFut <- reactive({
    
    # Due to dplyr issue #318, we need temp variables for input values
    crop <- input$crop
    soil <- input$soil
    scn <- input$scn
    
    allData <- allData %>%
      filter( CurrentCrop == crop & 
                thisSoil == soil  &
                thisScenario == "fut1"
      )
    
    
    allData[, c(input$xcol, input$ycol)]
  })
  

  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  clustersFut <- reactive({
    kmeans(selectedDataFut(), input$clusters)
  })

  # first graph
  output$plot1 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         main="Baseline",
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4) 
    

    })
  
  # second graph
  output$plot2 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedDataFut(),
         main="Future",
         col = clustersFut()$cluster,
         pch = 20, cex = 3)
    points(clustersFut()$centers, pch = 4, cex = 4, lwd = 4) 
    
    
  })
  
  

}

shinyApp(ui = ui, server = server)
