# 01-kmeans-app

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

library(shiny)
library(dplyr)
library(ggplot2)

allData <- read.csv("C:\\GitHubRepos\\CCII_ShinyApp\\AllData(RA2).csv", header = TRUE)

allData <- allData %>%
  mutate(Lat_Long = paste0(thisLat,"_",thisLong), FUE = TotalBiomass/PTfert)

#-------------THE UI ------------------------------------------------


ui <- fluidPage(
  headerPanel('RA2 CCII Arable crop'),
  sidebarPanel(
   # selectInput('gc', 'Grid cell', as.character(unique(allData$Lat_Long))),
    selectInput('scn', 'Climate cenario', as.character(unique(allData$thisScenario))),
    selectInput('crop', 'Crop type', as.character(unique(allData$CurrentCrop))),
    selectInput('soil', 'Soil water holding capacity', as.character(unique(allData$thisSoil))),
    selectInput('xcol', 'Select X Variable', names(allData)),
    selectInput('ycol', 'Select Y Variable', names(allData),
      selected = names(allData)[[12]]),
    numericInput('clusters', 'Cluster count', 3,
      min = 1, max = 9)
  ),
  mainPanel(
  
    tabsetPanel(
      tabPanel("Region analysis", plotOutput("plot1"), plotOutput("plot2")),
      
    
      tabPanel("Location analysis", 
               selectInput('gc', 'Grid cell', as.character(unique(allData$Lat_Long))),
               tableOutput("table"), 
               plotOutput("plot3"),
               plotOutput("plot4")
               
               ),
      
      tabPanel("Spatial analysis", verbatimTextOutput("summary"), textOutput("text1"))
      
    )
    
  )
)

#-------------------------- THE SERVER -----------------------------------------------------


server <- function(input, output) {

  selectedData <- reactive({
    
    # Due to dplyr issue #318, we need temp variables for input values
    gc <- input$gc
    crop <- input$crop
    soil <- input$soil
    scn <- input$scn
    
    allData <- allData %>%
      filter(  #Lat_Long == gc &
                 CurrentCrop == crop & 
                thisSoil == soil  &
               thisScenario == "base"
              )
    
    
    allData[, c(input$xcol, input$ycol)]
  })
  
  # fut
  selectedDataFut <- reactive({
    
    # Due to dplyr issue #318, we need temp variables for input values
    gc <- input$gc
    crop <- input$crop
    soil <- input$soil
    scn <- input$scn
    
    allData <- allData %>%
      filter( #Lat_Long == gc &
              CurrentCrop == crop & 
                thisSoil == soil  &
                thisScenario == "fut1"
      )
    
    
    allData[, c(input$xcol, input$ycol)]
  })
  
  
  # pixelData
  selectedDataPix <- reactive({
    
    # Due to dplyr issue #318, we need temp variables for input values
    gc <- input$gc
    crop <- input$crop
    soil <- input$soil
    scn <- input$scn
    
    allData <- allData %>%
      filter( Lat_Long == gc &
        CurrentCrop == crop & 
          thisSoil == soil  &
          thisScenario == scn
      )
    
    
    allData[, c(input$xcol, input$ycol)]
  })
  
  
  
# cluster
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  clustersFut <- reactive({
    kmeans(selectedDataFut(), input$clusters)
  })
  
  meanValue <- reactive({
    
    mean(selectedDataFut())
    
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
  
  # third graph
  output$plot3 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    boxplot(selectedDataPix()[1],
         main="Future",
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4) 
    
    
  })
  
  # forth graph
  output$plot4 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    boxplot(selectedDataPix()[2],
            main=input$x,
            col = clusters()$cluster,
            pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4) 
    
    
  })
  
  # means
  output$text1 <- renderText({ 
    "Mean value is: "
  })
  
  
}

shinyApp(ui = ui, server = server)
