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
library(sp)
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
    selectInput('mainvar', 'Select the output variable:', names(allData),selected = names(allData)[[22]]),
    
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
    h4(tags$b("Refence scenario (baseline)")),
    selectInput('scn', 'Climate scenario 1', as.character(unique(allData$thisScenario))),
    selectInput('crop', 'Crop type 1', as.character(unique(allData$CurrentCrop))),
    selectInput('soil', 'Soil type 1', as.character(unique(allData$thisSoil))),
    
   # input scenario 2 (alternative)
    tags$hr(),
    h4(tags$b("Alternative scenario")),
    selectInput('scn2', 'Climate scenario 2', as.character(unique(allData$thisScenario)),selected = as.character(unique(allData$thisScenario))[[2]]),
    selectInput('crop2', 'Crop type 2 ', as.character(unique(allData$CurrentCrop))),
    selectInput('soil2', 'Soil type 2', as.character(unique(allData$thisSoil))),
   
   # graph set up
   tags$hr(),
   h4(tags$b("Graphing details")),
   radioButtons("graphType", "Select type of graph:",
                inline = TRUE,
                c("Histogram" = "h","Box plot" = "b"))

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
               actionButton("recalc", "New points"),
               p(),
               actionButton("mapUpdateButton", "Update Maps"),
               p(),
               actionButton("saveDiffRaster", "Save map"),
               p(),
               sliderInput("slider1", 
                           label = h4("Raster transparency"), 
                           min = 0, max = 1, value = 0.5),
               p(),
               tableOutput("table1"),
             tableOutput("table2"),
             tableOutput("table3"),
             plotOutput("plot7"),
             leafletOutput("map_result"),
               uiOutput("ggvis_ui"), # FIXME: trying to do transparency in slide
               ggvisOutput("ggvis")
      ),
      
      
      # tab 2
      tabPanel("Factor analysis",
               # input graphing details
               tags$hr(),
               h4(tags$b("Graphing")),
               selectInput('xcol', 'Select driving variable (X axes)', names(allData),selected = names(allData)[[12]]),
            #   selectInput('ycol', 'Select response variable (Y)', names(allData), selected = names(allData)[[15]]),
            #   selectInput('contFact', 'Select contrast factor (panels)', names(allData),selected = names(allData)[[9]]),
               numericInput('clusters', 'Cluster count', 3,
                            min = 1, max = 9),
               p(),
               plotOutput("plot1"), 
               p(),
               plotOutput("plot2")
               ),
      
      # tab 3
      tabPanel("Grid-cell analysis", 
               p(),
               selectInput('gc', 'Grid cell', as.character(unique(allData$Lat_Long))),
              # tableOutput("table"), 
               p(),
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
  
  # reactive expressions to filter data of BASE raster
  
  
  # select stats
  statSelection <- reactive({
    clickStats <- input$stats
    keepVar <- as.numeric(ifelse(clickStats == "av", 3, 4)) # FIXME: this is selected by hand now, make it smarter later
    keepVar
  })
  
  # select variable
  varSelection <- reactive({
    varToRaster <- match(input$mainvar, names(allData))
    varToRaster
  })
  
  # select comparison method
  compSelection <- reactive({
    clickComp <- input$comp
    x <- ifelse(clickComp == "abs", "abs", "rel") # FIXME: this is selected by hand now, make it smarter later
    x
  })
  
  # baseline scenario
  rasterDF_Base <- reactive({

    # define scenario
    crop <- input$crop
    soil <- input$soil
    scn <- input$scn
    

    r <- allData %>%
      filter(CurrentCrop == crop & 
               thisSoil == soil  &
               thisScenario == scn) %>%
      dplyr::select(thisLat,thisLong, varSelection()) %>%
      group_by(thisLat, thisLong) %>%
      summarise_each(funs(mean,cvFunc)) %>%
      dplyr::select(thisLat, thisLong, statSelection())

    r
    
  })
  
  # alternative scenario
  rasterDF_Alt <- reactive({

    # define scenario
    crop <- input$crop2
    soil <- input$soil2
    scn <- input$scn2
    
    r <- allData %>%
      filter(CurrentCrop == crop & 
               thisSoil == soil  &
               thisScenario == scn) %>%
      dplyr::select(thisLat,thisLong, varSelection()) %>%
      group_by(thisLat, thisLong) %>%
      summarise_each(funs(mean,cvFunc)) %>%
      dplyr::select(thisLat, thisLong, statSelection())
    
    r

  })
  
  # end of raster df selection
  

  # select full dataset of selected variable (i.e. Y axes, the variable rasterised)
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
               thisScenario == scn
              )
    
    
    allData[, c(input$xcol, input$mainvar)]
  })
  
  # select driving variable (X axes)
  selectedDataFut <- reactive({
        # Due to dplyr issue #318, we need temp variables for input values
    crop2 <- input$crop2
    soil2 <- input$soil2
    scn2 <- input$scn2
    
    allData <- allData %>%
      filter( CurrentCrop == crop2 & 
                thisSoil == soil2  &
                thisScenario == scn2
      )
    
    allData[, c(input$xcol, input$mainvar)]
  })
  
  
  # Pixel Data (boxplots)
  
  #(baseline)
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
    
    allData[, c(input$xcol, input$mainvar)]
  })
  
  # Alternative
  selectedDataPix_Alt <- reactive({
    
    # Due to dplyr issue #318, we need temp variables for input values
    gc <- input$gc
    crop2 <- input$crop2
    soil2 <- input$soil2
    scn2 <- input$scn2
    
    allData <- allData %>%
      filter( Lat_Long == gc & # Note that's the same lat/long for both graphs
                CurrentCrop == crop2 & 
                thisSoil == soil2  &
                thisScenario == scn2
      )
    
    allData[, c(input$xcol, input$mainvar)]
  })
  
  
# cluster
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  cluster_Alt <- reactive({
    kmeans(selectedDataFut(), input$clusters)
  })
  

  # first graph
  output$plot1 <- renderPlot({
    par(mar = c(5.1, 4.1, 2, 1))
    plot(selectedData(),
         main="Reference (baseline)",
        # title("Title", line = -2),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4) 
    

    })
  
  # second graph
  output$plot2 <- renderPlot({
    par(mar = c(5.1, 4.1, 2, 1))
    plot(selectedDataFut(),
         main="Alternative scenario",
       #  title(main ="Title", line = -2),
         col = cluster_Alt()$cluster,
         pch = 20, cex = 3)
    points(cluster_Alt()$centers, pch = 4, cex = 4, lwd = 4) 
    
    
  })
  
  # third graph (Pixel analysis)
  output$plot3 <- renderPlot({
    
    ymin <- min(min(selectedDataPix()[2]), min(selectedDataPix_Alt()[2]))
    ymax <- max(max(selectedDataPix()[2]), max(selectedDataPix_Alt()[2]))
    
    par(mar = c(5.1, 4.1, 2, 1))
    
    if(input$graphType == "b") {
      
      boxplot(selectedDataPix()[2],
              main="Baseline",
              col = clusters()$cluster,
              horizontal=TRUE,
              ylim=c(ymin, ymax),
              pch = 20, cex = 3)
      points(clusters()$centers, pch = 4, cex = 4, lwd = 4) 

    } else {
    
    hist(as.numeric(unlist(selectedDataPix()[2])),
         main="Baseline",
         xlim=c(ymin, ymax),
         pch = 20, cex = 3)
    }
    
    
  })
  
  # forth graph
  output$plot4 <- renderPlot({
    
    ymin <- min(min(selectedDataPix()[2]), min(selectedDataPix_Alt()[2]))
    ymax <- max(max(selectedDataPix()[2]), max(selectedDataPix_Alt()[2]))
    

    par(mar = c(5.1, 4.1, 2, 1))
    
    if(input$graphType == "b") {
    
    boxplot(selectedDataPix_Alt()[2],
            main= "Alternative",
            col = clusters()$cluster,
            horizontal=TRUE,
            ylim=c(ymin, ymax),
            pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4) 
    
    } else {
      
      hist(as.numeric(unlist(selectedDataPix_Alt()[2])),
           main="Baseline",
           xlim=c(ymin, ymax),
           pch = 20, cex = 3)
    }
    
    
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
  
  
  
  
  # Create diff raster image
  
 # newRaster_DF <- eventReactive(input$mapUpdateButton, { # to be added as raster in teh main map
    newRaster_DF <- reactive({ # to be added as raster in teh main map
     # calculate fifference map (as df)
    
    r1 <- rasterDF_Base()
    r2 <- rasterDF_Alt()
    
    df_diff <- merge(r1, r2, by = c("thisLat","thisLong"))
    
    compType <- compSelection()
    
    if(compType == "abs") {
      
     df_diff$diff <- df_diff[4] - df_diff[3] 
      
    } else {
      
      df_diff$diff <- round( ( (df_diff[4] - df_diff[3]) / df_diff[3] ) * 100 , 2) # (base-fut)/base
    } 
    
  # trim df to lat/long/var  
   df_diff <- df_diff %>%
   dplyr::select(thisLat,thisLong, diff)
    
    df_diff
    })
 # }, ignoreNULL = FALSE)
  
  
  # rasterise diff df
  newRaster_Layer <- reactive({
    df_raster <- data.frame(as.numeric(unlist(newRaster_DF()))) # FIXME: not sure if/why unlist here
    spg <- df_raster

    coordinates(spg) <- ~ df_raster.thisLong + df_raster.thisLat # FIXME: breaks here 'df_raster.thisLong' not found
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
    #  addRasterImage(newRaster_Layer(), colors = pal, opacity = sliderValue) %>%
      addLegend(pal = pal, values = values(r), title = "The legend") %>%
     # bind_shiny("ggvis", "ggvis_ui") %>%
      #addProviderTiles("OpenTopoMap", options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(data = points_map())
  })
  

  
  # Print tables of data to be resterised
  #graph raster3
  output$plot7 <- renderPlot({
    par(mar = c(5.1, 4.1, 2, 1))
    df <- newRaster_DF()
    x    <- df[, "diff"]
  #  hist(as.numeric(unlist(x)),
  #       main= "Distribution of scanario differences"
  #       )
    
    
    if(input$graphType == "b") {
      
      boxplot(as.numeric(unlist(x)),
              main="Baseline",
              horizontal=TRUE,
              pch = 20, cex = 3)
      
    } else {
      
      hist(as.numeric(unlist(x)),
      main="Baseline",
      pch = 20, cex = 3)
      
      
    }
    
    
    
    
    
  })
  
  
  # Table raster1
  output$table1 <- renderTable({
 # rasterDF_Base()
  })
  
  # Table raster2
  output$table2 <- renderTable({
  #  rasterDF_Alt()
  })
  
  # Table raster3
  output$table3 <- renderTable({
  #  newRaster_DF()
  })
  

  
}

shinyApp(ui = ui, server = server)
