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

# Select variables of interest based on listed outputs
varList <- read.csv("C:\\GitHubRepos\\CCII_ShinyApp\\data\\variableNames.csv", header = TRUE)
selectedVars_df <- varList %>%
  filter(include == "yes")

varNames <- as.character(selectedVars_df$variable)

# find col positions that hold the variables of interesi in the raw df
selectColNos <- match(varNames,names(allData))

allData <- allData  %>%
  dplyr::select(selectColNos)

fullNames <- as.character(selectedVars_df$fullName)

# Customise data (FIXME: this should be done earlier in raw dataset)
allData <- allData %>%
  mutate(Lat_Long = paste0(thisLat,"_",thisLong), 
         FUE = TotalBiomass/PTfert)

# Load polygon maps for 'Kaituna' catchment
pathShapeFile <- 'C:/apsim_dev/Projects/CCII/GIS_layers/CaseStudy/lowerKaitunabnd(WGS84).shp'
sf2 <- readShapeSpatial(pathShapeFile, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# sf2 <- gSimplify(sf2,tol=.01,topologyPreserve = TRUE)


#------------- THE UI ------------------------------------------------


ui <- fluidPage(
  
  headerPanel('Analysis App - RA2 CCII-MBIE Broadacre Crops'),
  
  # Side panel details
  sidebarPanel(width = 2,
   selectInput('mainvar', 'Select the output variable:', fullNames, selected = fullNames[14]),
    
    # Show selection
    textOutput("text1"),

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
   
   # graph controls
   tags$hr(),
   h4(tags$b("Graphing details")),
   radioButtons("graphType", "Select type of graph:",
                inline = TRUE,
                c("Box plot" = "b","Histogram" = "h")),
 
  # download controls
  tags$hr(),
  h4(tags$b("Download selected data")),
  downloadButton("downloadData", "Download data"),
  p(),
  radioButtons("filetype", "File type:",
              choices = c("csv (table)", "tif (raster image)"))
  ),
  
  # Main panel details
  mainPanel(
  
    # First tab
    tabsetPanel(
      
      # tab 1
      tabPanel("Spatial analysis", 
               # show map
               leafletOutput("basemap"),
               p(),
               sliderInput("slider1", 
                           label = h4("Raster transparency"), 
                           min = 0, max = 1, value = 0.5),
               p(),
              # tableOutput("table1"), # tables for testing app
              # tableOutput("table2"),
              # tableOutput("table3"),
               plotOutput("plot7"),
               leafletOutput("map_result")
      ),
      
      
      # tab 2
      tabPanel("Factor analysis",
               # input graphing details
               tags$hr(),
               h4(tags$b("Graphing")),
               selectInput('xcol', 'Select driving variable (X axes)', names(allData),selected = names(allData)[[12]]),
               numericInput('clusters', 'Cluster count', 3, min = 1, max = 9),
               p(),
               plotOutput("plot1"), 
               p(),
               plotOutput("plot2")
               ),
      
      # tab 3
      tabPanel("Grid-cell analysis", 
               p(),
               selectInput('gc', 'Grid cell', as.character(unique(allData$Lat_Long))),
               p(),
               plotOutput("plot3"),
               p(),
               plotOutput("plot4")
              
               )
    )
  )
)


#-------------------------- THE SERVER -----------------------------------------------------

# shinyServer(function(input, output) { # why some examples use this syntaxe instead?
server <- function(input, output) {
  
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
  
  
  # -------------- Reactive expressions to filter data of BASE raster ------------------
  
  # Name of selected variable in original file. Converts "selected" name to name in "raw" data
  mainVarSelec <- reactive({ # aims to substiture most varNames
   buf <-  selectedVars_df %>%
     filter(fullName == as.character(input$mainvar))
   as.character(buf$variable) # returns var name in raw data
    
  })
  
  # Units of selected variable
  varUnits <- reactive({
    varDetails <-  selectedVars_df %>%
      filter(variable == mainVarSelec())
   paste0("(",as.character(varDetails[,"unit"]),")") 
  })
  
  # select stats
  statSelection <- reactive({
    clickStats <- input$stats
    keepVar <- as.numeric(ifelse(clickStats == "av", 3, 4)) # FIXME: this is selected by hand now, make it smarter later
    keepVar
  })
  
  # select variable
  varSelection <- reactive({
    varToRaster <- match(mainVarSelec(), names(allData))
    varToRaster
  })
  
  # select comparison method
  compSelection <- reactive({
    clickComp <- input$comp
    x <- ifelse(clickComp == "abs", "abs", "rel") # FIXME: this is selected by hand now, make it smarter later
    x
  })
  
  
  # ----------------- Subset data for each specific analysis ---------------------------------
  
  # Raster data (summarised by pixel with average or CV%) --------------
  
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
  
  # Dataset for graphing (all data) ------------------------------
  
  # select full (all years) dataset of selected variable (i.e. Y axes, the variable rasterised)
  selectedData_Base <- reactive({
    
    #gc <- input$gc
    crop <- input$crop
    soil <- input$soil
    scn <- input$scn
    
    allData <- allData %>%
      filter(  #Lat_Long == gc &
                 CurrentCrop == crop & 
                thisSoil == soil  &
               thisScenario == scn
              )
    
    
    allData[, c(input$xcol, mainVarSelec())]
  })
  
  # select driving variable for graph (X axes)
  selectedData_Alt <- reactive({
        
    crop2 <- input$crop2
    soil2 <- input$soil2
    scn2 <- input$scn2
    
    allData <- allData %>%
      filter( CurrentCrop == crop2 & 
                thisSoil == soil2  &
                thisScenario == scn2
      )
    allData[, c(input$xcol, mainVarSelec())]
  })
  
  
  # Pixel Data for graphing (filtered by lat-long)
  
  #(baseline)
  selectedDataPix_Base <- reactive({
    
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
    allData[, c(input$xcol, mainVarSelec())]
  })
  
  # Alternative
  selectedDataPix_Alt <- reactive({
    
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
    
    allData[, c(input$xcol, mainVarSelec())]
  })
  
  
  # Cluster analysis --------------------------------------------------
  
  clusters <- reactive({
    kmeans(selectedData_Base(), input$clusters)
  })
  
  cluster_Alt <- reactive({
    kmeans(selectedData_Alt(), input$clusters)
  })
  
  
  # Draw graphs -----------------------------------------------------------

  
  axesLimits <- reactive({
    xmin <- min(min(selectedDataPix_Base()[1]), min(selectedDataPix_Alt()[1]))
    xmax <- max(max(selectedDataPix_Base()[1]), max(selectedDataPix_Alt()[1]))
    ymin <- min(min(selectedDataPix_Base()[2]), min(selectedDataPix_Alt()[2]))
    ymax <- max(max(selectedDataPix_Base()[2]), max(selectedDataPix_Alt()[2]))
    x <- data.frame(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
    x
  })
  
  
  # X Y comparison of variables ------------
  
  # first graph
  output$plot1 <- renderPlot({
    par(mar = c(5.1, 4.1, 2, 1))
    
    plot(selectedData_Base(),
         main="Reference (baseline)",
        # title("Title", line = -2),
         col = clusters()$cluster,
        xlim=c(axesLimits()$xmin, axesLimits()$xmax),
        ylim=c(axesLimits()$ymin, axesLimits()$ymax),
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4) 
    

    })
  
  # second graph
  output$plot2 <- renderPlot({
    par(mar = c(5.1, 4.1, 2, 1))
    
    plot(selectedData_Alt(),
         main="Alternative scenario",
       #  title(main ="Title", line = -2),
       xlim=c(axesLimits()$xmin, axesLimits()$xmax),
       ylim=c(axesLimits()$ymin, axesLimits()$ymax),
         col = cluster_Alt()$cluster,
         pch = 20, cex = 3)
    points(cluster_Alt()$centers, pch = 4, cex = 4, lwd = 4) 
    
    
  })
  
  # Distribution graphs (within pixel) ------------
  
  # third graph (Pixel analysis)
  output$plot3 <- renderPlot({
  
    par(mar = c(5.1, 4.1, 2, 1))
    
    if(input$graphType == "b") {
      
      boxplot(selectedDataPix_Base()[2],
              main="Baseline",
              col = clusters()$cluster,
              horizontal=TRUE,
              ylim=c(axesLimits()$ymin, axesLimits()$ymax),
              pch = 20, cex = 3)
      points(clusters()$centers, pch = 4, cex = 4, lwd = 4) 

    } else {
    
    hist(as.numeric(unlist(selectedDataPix_Base()[2])),
         main="Baseline",
         col = clusters()$cluster,
         xlim=c(ymin, ymax),
         pch = 20, cex = 3)
    }
    
    
  })
  
  # forth graph
  output$plot4 <- renderPlot({
    
    par(mar = c(5.1, 4.1, 2, 1))
    
    if(input$graphType == "b") {
    
    boxplot(selectedDataPix_Alt()[2],
            main= "Alternative",
            col = clusters()$cluster,
            horizontal=TRUE,
            ylim=c(axesLimits()$ymin, axesLimits()$ymax),
            pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4) 
    
    } else {
      
      hist(as.numeric(unlist(selectedDataPix_Alt()[2])),
           main="Baseline",
           col = clusters()$cluster,
           xlim=c(axesLimits()$ymin, axesLimits()$ymax),
           pch = 20, cex = 3)
    }
    
    
  })
  

# Create diff dataframe to be rasterised -------------------------
    
  rasterDF_Diff <- reactive({ # to be added as raster in the main map
      
    # calculate fifference map (as df)
    r1 <- rasterDF_Base()
    r2 <- rasterDF_Alt()
    df_diff <- merge(r1, r2, by = c("thisLat","thisLong"))
    
    compType <- compSelection() # method of comparison
    
    if(compType == "abs") {
     df_diff$diff <- df_diff[4] - df_diff[3] # alt - base
    } else {
      df_diff$diff <- round( ( (df_diff[4] - df_diff[3]) / df_diff[3] ) * 100 , 2) # (base-fut)/base as percent
    } 
    
  # trim df to a simple lat/long/var  
   df_diff <- df_diff %>%
   dplyr::select(thisLat,thisLong, diff)
    
    df_diff
    
    })
  
  # Create a RASTER of the diff df ------------------------------------------ FIXME: Not working yet
  
  newRaster_Layer <- reactive ({
    df <- rasterDF_Diff()
    spg <- data.frame(df$thisLong, df$thisLat, df$diff)
    coordinates(spg) <- ~ df.thisLong + df.thisLat # Attention to variable names
    gridded(spg) <- TRUE
    r <- raster(spg)
    proj4string(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    r
  })
  

  # create main map------------------------
  
  output$basemap <- renderLeaflet({
    leaflet() %>%
      setView(lng = 176.272, lat = -38.0, zoom = 8) %>%
      addTiles()
  })
  
  # update legend FIXME: Not working yet
  
  # raster transparancy
  sl <- reactive ({
    input$slider1
  })
  
  # manage dynamic bit of rasters to be added to main map
  observe({
    pal <- colorNumeric(c("#8B0000","#EE4000", "#FFA500","#008B45"), 
                        values(newRaster_Layer()), na.color = "transparent")
    leafletProxy("basemap", data = c(newRaster_Layer(),input$slider1)) %>%
      clearShapes() %>% # does it clear old raster?
      clearControls() %>% # necessary to remove old legend
      addPolygons(data=sf2, fill = F ,opacity = 0.01, weight = 1) %>%
      addRasterImage(newRaster_Layer(),opacity = sl()) %>%
      addLegend(pal = pal, values = values(newRaster_Layer()), 
                title = ifelse((compSelection() == "abs"| 
                                  statSelection() == "av"), varUnits(), "(%)")) # FIXME: Use % or CV% if relative selected
  })
  
  # Graph diff distrubution of raster DF (across pixels)
  output$plot7 <- renderPlot({
    
    par(mar = c(5.1, 4.1, 2, 1))
    
    df <- rasterDF_Diff()
    x    <- df[, "diff"]
    
    if(input$graphType == "b") {
      
      boxplot(as.numeric(unlist(x)),
              main="Distribution of differences between scenarios",
              horizontal=TRUE,
              col = clusters()$cluster,
              pch = 20, cex = 3)
      
    } else {
      
      hist(as.numeric(unlist(x)),
           main="Distribution of differences between scenarios",
           col = clusters()$cluster,
           pch = 20, cex = 3)
    }
    
  })
  
  
  # Tables for testing
  
  # Table raster1
  output$table1 <- renderTable({
 # rasterDF_Base()
  })
  
  # Table raster2
  output$table2 <- renderTable({
  #  rasterDF_Alt()
  })
  
  # Table raster3 Used for testing
  output$table3 <- renderTable({
   # rasterDF_Diff()
  })
  
  # Show variable name
  output$text1 <- renderText({ 
  varUnits()
  })  
  
  # Download data ----------------------------- FIXME: file content is not as expected
  
  output$downloadData <- downloadHandler(
    filename = function() { paste(input$mainvar, '.txt', sep=' ') },
   # filename = "test",
    content = function(file) {
      # FIXME: outputting individul values within the summary function (quick fix with new df)
      df <- data.frame(lat = rasterDF_Diff()$thisLat, lon = rasterDF_Diff()$thisLong, Difference = rasterDF_Diff()$diff)
      thisHeader <- paste0("#",input$mainvar," ",varUnits()," ", as.character(statSelection()))
      # FIME: Add header with meta-data
      write.table(df, file, row.names=F)
    }
  )
}

shinyApp(ui = ui, server = server)
