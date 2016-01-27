# 01-kmeans-app

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

library(shiny)
library(dplyr)
library(ggplot2)
library(leaflet)
library(raster)
library(ggvis)
library(maptools)
library(rgeos)
library(maps)
library(rgdal)
#install.packages('raster', repos = 'http://r-forge.r-project.org/', type = 'source')

allData <- read.csv("C:\\GitHubRepos\\CCII_ShinyApp\\AllData(RA2).csv", header = TRUE)

allData <- allData %>%
  mutate(Lat_Long = paste0(thisLat,"_",thisLong), FUE = TotalBiomass/PTfert)

r <- raster("C:\\apsim_dev\\Projects\\CCII\\GIS_layers\\CaseStudy\\Filter_ArableKaituna.tif")

# add 'Kaituna' catchment 
pathShapeFile <- 'C:/apsim_dev/Projects/CCII/GIS_layers/CaseStudy/lowerKaitunabnd(WGS84).shp'
sf2 <- readShapeSpatial(pathShapeFile, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#sf2 <- readOGR("C:/apsim_dev/Projects/CCII/GIS_layers/CaseStudy","lowerKaitunabnd(WGS84)")
sf2 <- gSimplify(sf2,tol=.01,topologyPreserve = TRUE)
#plot(sf2, bg="transparent", xlim=c(167.2,178.55),main = "name")

#-------------THE UI ------------------------------------------------


ui <- fluidPage(
  
  headerPanel('RA2 CCII Arable crop'),
  
  sidebarPanel(width = 2,
    selectInput('mainvar', 'Select the output variable:', names(allData)),
    
    tags$hr(),
    h4(tags$b("Calculation details")),
    radioButtons("stats", "Statistics:",
                 inline = TRUE,
                 c("Average" = "av","CV (%)" = "cv")),
    
    radioButtons("diff", "Comparison method:",
                 inline = TRUE,
                 c("Absolute" = "abs","Relative" = "rel")),
   
    tags$hr(),
   h4(tags$b("Refence scenario")),
    selectInput('scn', 'Climate scenario 1', as.character(unique(allData$thisScenario))),
    selectInput('crop', 'Crop type 1', as.character(unique(allData$CurrentCrop))),
    selectInput('soil', 'Soil type 1', as.character(unique(allData$thisSoil))),
    
    tags$hr(),
    h4(tags$b("Alternative scenario")),
    selectInput('scn2', 'Climate scenario 2', as.character(unique(allData$thisScenario))),
    selectInput('crop2', 'Crop type 2 ', as.character(unique(allData$CurrentCrop))),
    selectInput('soil2', 'Soil type 2', as.character(unique(allData$thisSoil))),
    
    tags$hr(),
    h4(tags$b("Graphing")),

    selectInput('xcol', 'Select driving variable (X in graph)', names(allData)),
    selectInput('ycol', 'Select response variable (Y in graph and in the maps)', names(allData),
      selected = names(allData)[[12]]),
    numericInput('clusters', 'Cluster count', 3,
      min = 1, max = 9)
  ),
  
  mainPanel(
  
    tabsetPanel(
      
      # tab 1
      tabPanel("Spatial analysis", 
               verbatimTextOutput("summary"), 
               textOutput("text1"),
               radioButtons("statOut", "Statistics to output:",
                            inline = TRUE,
                            c("Average" = "av",
                              "Median" = "med",
                              "Coefficient of Variation" = "cv",
                              "Standard Deviation" = "sd")),
               leafletOutput("basemap"),
               #  leafletOutput("map_result"), # this becomes a new map, so all has to be fit in the basemap for overlay?
               p(),
               actionButton("recalc", "New points"),
               sliderInput("slider1", 
                           label = h4("Raster transparency"), 
                           min = 0, max = 1, value = 0.5),
               actionButton("mapUpdateButton", "Update Maps"),
               tableOutput("table1"),
               
               uiOutput("ggvis_ui"), # FIXME: trying to do transparency in slide
               ggvisOutput("ggvis")
      ),
      
      
      # tab 2
      tabPanel("Region analysis", plotOutput("plot1"), plotOutput("plot2")
               ),
      
      # tab 3
      tabPanel("Location analysis", 
               selectInput('gc', 'Grid cell', as.character(unique(allData$Lat_Long))),
               tableOutput("table"), 
               plotOutput("plot3"),
               plotOutput("plot4")
               )
      
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
    "Analyse raster outputs "
  })
  
  # map points FIXME: Adding this crahes the data selection
  # FIXME: not being able to pass slider transparency as argument 
 # sliderValue <- observe({input$slider1})
 
  pal <- colorNumeric(c("#CD3333", "#FF8C00","#458B00"), values(r),
                      na.color = "transparent")
  sliderValue <- 0.5
  
  # Set location of points in NZ
  points_map <- eventReactive(input$recalc, {
    cbind(rnorm(10) * 2 + 176, rnorm(10) + -38) # random coordinates: Use the selected coordinates
  }, ignoreNULL = FALSE)
  
  output$basemap <- renderLeaflet({
    leaflet() %>%
      setView(lng = 176.272, lat = -38.0, zoom = 8) %>%
      addTiles() %>%
    # addPolygons(sf2, lng = 176.272, lat = -38.0, fill = TRUE) %>%
  #    addCircles(lng = 176.272, lat = -38.0, radius = 50,fillOpacity = 0.2) %>%
      addRectangles(176, -38.25, 176.53, -37.67,fillOpacity = 0.05) %>%
    #  addRasterImage(r, colors = pal, opacity = input_slider(0, 1, value = 0.5, map=sliderValue)) %>%
      addRasterImage(r, colors = pal, opacity = sliderValue) %>%
      addLegend(pal = pal, values = values(r), title = "The legend") %>%
     # bind_shiny("ggvis", "ggvis_ui") %>%
      #addProviderTiles("OpenTopoMap", options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(data = points_map())
  })
  
  # Update button
  newRaster <- eventReactive(input$mapUpdateButton, {
    
 # Update the rasyter
    
  })
  
  # Print table of data to be resterised
  output$table1 <- renderTable({
 
    varToRaster <- match(input$ycol, names(allData))
    
    allData %>%
    dplyr::select(thisLat, thisLong, varToRaster) %>%
    head()
  })
  
  # Create raster image
  output$map_result <- renderLeaflet({
    
    varToRaster <- match(input$ycol, names(allData))
    
    allData %>%
      dplyr::select(thisLat, thisLong, varToRaster) %>%
      head()
  })
  

}

shinyApp(ui = ui, server = server)
