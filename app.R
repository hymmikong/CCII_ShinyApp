# CCII - MBIE prototype App (RA2)

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))


# load libraries (FIXME: delete the ones not used anymore)
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
#install.packages('raster', repos = 'http://r-forge.r-project.org/', type = 'source') # using new raster lib


# load raw data
allData <- read.csv("C:\\GitHubRepos\\CCII_ShinyApp\\data\\AllData(RA2).csv", header = TRUE)

# Costomise data
allData <- allData %>%
  mutate(Lat_Long = paste0(thisLat,"_",thisLong), FUE = TotalBiomass/PTfert)

# load support rasters (FIXME: delete this after if not needed - for testing now)
r <- raster("C:\\apsim_dev\\Projects\\CCII\\GIS_layers\\CaseStudy\\Filter_ArableKaituna.tif")

# Load polygon maps for 'Kaituna' catchment (FIX<E: Not working) 
pathShapeFile <- 'C:/apsim_dev/Projects/CCII/GIS_layers/CaseStudy/lowerKaitunabnd(WGS84).shp'
sf2 <- readShapeSpatial(pathShapeFile, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#sf2 <- readOGR("C:/apsim_dev/Projects/CCII/GIS_layers/CaseStudy","lowerKaitunabnd(WGS84)")
sf2 <- gSimplify(sf2,tol=.01,topologyPreserve = TRUE)
#plot(sf2, bg="transparent", xlim=c(167.2,178.55),main = "name")


#-------------THE UI ------------------------------------------------


ui <- fluidPage(
  
  headerPanel('RA2 CCII-MBIE Broadacre Crops'),
  
  # Side panel details
  sidebarPanel(width = 2,
    # input variable
    selectInput('mainvar', 'Select the output variable:', names(allData)),
    
    # input stats
    tags$hr(),
    h4(tags$b("Calculation details")),
    radioButtons("stats", "Statistics:",
                 inline = TRUE,
                 c("Average" = "av","CV (%)" = "cv")),
    radioButtons("comp", "Comparison method:",
                 inline = TRUE,
                 c("Absolute" = "abs","Relative" = "rel")),
    
   # input scenario 1 (baseline)
    tags$hr(),
    h4(tags$b("Refence scenario")),
    selectInput('scn', 'Climate scenario 1', as.character(unique(allData$thisScenario))),
    selectInput('crop', 'Crop type 1', as.character(unique(allData$CurrentCrop))),
    selectInput('soil', 'Soil type 1', as.character(unique(allData$thisSoil))),
    
   # input scenario 2 (alternative)
    tags$hr(),
    h4(tags$b("Alternative scenario")),
    selectInput('scn2', 'Climate scenario 2', as.character(unique(allData$thisScenario))),
    selectInput('crop2', 'Crop type 2 ', as.character(unique(allData$CurrentCrop))),
    selectInput('soil2', 'Soil type 2', as.character(unique(allData$thisSoil))),
    
   # input graphing details
    tags$hr(),
    h4(tags$b("Graphing")),
    selectInput('xcol', 'Select driving variable (X in graph)', names(allData)),
    selectInput('ycol', 'Select response variable (Y in graph and in the maps)', names(allData),
      selected = names(allData)[[12]]),
    numericInput('clusters', 'Cluster count', 3,
      min = 1, max = 9)
  ),
  
  # Main panel details
  mainPanel(
  
    tabsetPanel(
      
      # tab 1
      tabPanel("Spatial analysis", 
               #  verbatimTextOutput("summary"), 
               #  textOutput("text1"),
             
               # show map
               leafletOutput("basemap"),
               p(),
               
               # map specific controls
             #  actionButton("recalc", "New points"),
               p(),
               actionButton("mapUpdateButton", "Update Maps"),
               p(),
               sliderInput("slider1", 
                           label = h4("Raster transparency"), 
                           min = 0, max = 1, value = 0.5),
               p(),
               tableOutput("table1"),
             tableOutput("table2"),
             leafletOutput("map_result"),
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

# shinyServer(function(input, output) { # why some examples use this syntaxe instead?
server <- function(input, output) {
  
  # set up user specific variables (run ONCE when loading or re-freshing)
  # FIXME: any variable to add at this level?
  
  
  # Function to select stat type
  statTypeFunc <- function(x, type) {
    switch(type,
           av = mean(x),
           cv = sd(x)/mean(x))
  }
  
  # simple cv function
  cvFunc <- function(x) {
     cv <- round((sd(x)/mean(x))*100,1)
  }
  
  
  
  # reactive expression to filter data of BASE raster
  dataRaster1 <- reactive({
    
    crop <- input$crop
    soil <- input$soil
    scn <- input$scn
    
    allData <- allData %>%
      filter(CurrentCrop == crop & 
               thisSoil == soil  &
               thisScenario == scn
      )
    #  allData[,input$mainvar]
    allData
  })
    
  # reactive expression to filter data of ALTERNATIVE raster
  dataRaster2 <- reactive({
    
    crop2 <- input$crop2
    soil2 <- input$soil2
    scn2 <- input$scn2
    
    allData <- allData %>%
      filter(CurrentCrop == crop2 & 
               thisSoil == soil2  &
               thisScenario == scn2
      )
    #  allData[,input$mainvar]
    allData
  })
  

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
 

  
  # Set location of points in NZ
  points_map <- eventReactive(input$recalc, {
    cbind(rnorm(10) * 2 + 176, rnorm(10) + -38) # random coordinates: Use the selected coordinates
  }, ignoreNULL = FALSE)
  
  
  
  # Create raster image
  
  theRaster <- eventReactive(input$mapUpdateButton, {
    
    # create data-frame
    varToRaster <- match(input$mainvar, names(allData))
    
    df <- dataRaster1()
    
    df <- df %>%
      dplyr::select(thisLat, thisLong, varToRaster) %>%
      group_by(thisLat, thisLong) %>%
      mutate(varToRaster = mean(varToRaster))
    
    # rasterise
    spg <- data.frame(df$thisLong, df$thisLat, df[3])
    coordinates(spg) <- ~ df.thisLong + df.thisLat # Attention to variable names
    gridded(spg) <- TRUE
    rast <- raster(spg)
    proj4string(rast) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    rast
  })
  
  
  # get map arguments
  pal <- colorNumeric(c("#CD3333", "#FF8C00","#458B00"), values(r),
                      na.color = "transparent")
  sliderValue <- 0.5
  
  # create main map
  output$basemap <- renderLeaflet({
    leaflet() %>%
      setView(lng = 176.272, lat = -38.0, zoom = 8) %>%
      addTiles() %>%
    # addPolygons(sf2, lng = 176.272, lat = -38.0, fill = TRUE) %>%
  #    addCircles(lng = 176.272, lat = -38.0, radius = 50,fillOpacity = 0.2) %>%
      addRectangles(176, -38.25, 176.53, -37.67,fillOpacity = 0.05) %>%
    #  addRasterImage(r, colors = pal, opacity = input_slider(0, 1, value = 0.5, map=sliderValue)) %>%
      addRasterImage(r, colors = pal, opacity = sliderValue) %>%
     # addRasterImage(theRaster(), colors = pal, opacity = sliderValue) %>%
      addLegend(pal = pal, values = values(r), title = "The legend") %>%
     # bind_shiny("ggvis", "ggvis_ui") %>%
      #addProviderTiles("OpenTopoMap", options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(data = points_map())
  })
  
  # Update button
  newRaster <- eventReactive(input$mapUpdateButton, {
    
 # Update the rasyter
    
  })
  
  # Print tables of data to be resterised
  
  # Table raster1
  output$table1 <- renderTable({
 
    varToRaster <- match(input$mainvar, names(allData))
    
   df <- dataRaster1()
    
    df %>%
    dplyr::select(thisLat, thisLong, varToRaster) %>%
      group_by(thisLat, thisLong) %>%
      summarise_each(funs(mean,cvFunc)) %>%
      mutate(click1 = input$stats,click2 = input$comp) %>%    
      head(50)
  })
  
  # Table raster2
  output$table2 <- renderTable({
    
    varToRaster <- match(input$mainvar, names(allData))
    
    df <- dataRaster2()
    
    df %>%
      dplyr::select(thisLat, thisLong, varToRaster) %>%
      group_by(thisLat, thisLong) %>%
      summarise_each(funs(mean,cvFunc)) %>%
      mutate(click1 = input$stats,click2 = input$comp) %>%
      head(50)
  })
  
}

shinyApp(ui = ui, server = server)
