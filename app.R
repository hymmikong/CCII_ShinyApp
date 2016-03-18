# CCII - MBIE prototype App (RA2)

#palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
#  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

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
library(gplots)
library(htmltools)
#install.packages('raster', repos = 'http://r-forge.r-project.org/', type = 'source') # using new raster lib

# load raw data
allData <- read.csv("C:\\GitHubRepos\\CCII_ShinyApp\\data\\AllData(RA2)_v2.csv", header = TRUE)

# Select variables of interest based on listed outputs
varList <- read.csv("C:\\GitHubRepos\\CCII_ShinyApp\\data\\variableNames_v2.csv", header = TRUE)
selectedVars_df <- varList %>%
  filter(include == "yes")

all.factors <- varList %>%
  filter(is.factor == "yes")

varNames <- as.character(selectedVars_df$variable)

# find col positions that hold the variables of interest in the raw df
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



#-------------------------------------------------------------------
#------------- THE UI ------------------------------------------------
#--------------------------------------------------------------------

ui <- fluidPage(
  
  headerPanel('Analysis App - RA2 CCII-MBIE Broadacre Crops'),
  p(),
  
  # Side panel details
  sidebarPanel(width = 2,
               selectInput('mainvar', 'Select the output variable:', fullNames, selected = fullNames[12]),
               
               # Show selection
               textOutput("text1"),
               
               # input stats
               tags$hr(),
               h4(tags$b("Calculation details")),
               radioButtons("stats", "Statistics:",
                            inline = TRUE,
                            c("Average" = "av","Variability (CV%)" = "cv")),
               
               tags$hr(),
               h4(tags$b("Select scenario elements")),
               fluidRow(
                 column(6,
                        h4(tags$b("Refence")),
                        selectInput('gcm', 'GCM #1', as.character(unique(allData$thisGCM))),
                        selectInput('rcp', 'RCP #1', as.character(unique(allData$thisRCP))),
                        selectInput('scn', 'Time #1', as.character(unique(allData$thisScenario))),
                        selectInput('crop', 'Cultivar #1', as.character(unique(allData$CurrentCrop))),
                        selectInput('soil', 'Soil #1 ', as.character(unique(allData$thisSoil)))
                        
                 ),
                 column(6,
                        h4(tags$b("Alternative")),
                        selectInput('gcm2','GCM #2', as.character(unique(allData$thisGCM))),
                        selectInput('rcp2', 'RCP #2', as.character(unique(allData$thisRCP))),
                        selectInput('scn2', 'Time #2', as.character(unique(allData$thisScenario)),selected = as.character(unique(allData$thisScenario))[[1]]),
                        selectInput('crop2', 'Cultivar #2', as.character(unique(allData$CurrentCrop))),
                        selectInput('soil2', 'Soil #2', as.character(unique(allData$thisSoil)))
                        
                 )
               ),
               
               
               
               
               # graph controls
               tags$hr(),
               h4(tags$b("Graphing set up")),
               radioButtons("graphType", "Select type of graph:",
                            inline = TRUE,
                            c("Box plot" = "b","Histogram" = "h")),
               sliderInput("bins",
                           "Histogram number of bins:",
                           min = 1,
                           max = 20,
                           value = 5),
               
               # raster transparency
               p(),
               sliderInput("slider1", 
                           label = h4(tags$b("Raster transparency")), 
                           min = 0, max = 1, value = 0.5),
               
               # download controls
               tags$hr(),
               h4(tags$b("Download selected data")),
               downloadButton("downloadData", "Download data"),
               p(),
               radioButtons("fileType", "Select type of file:",
                            inline = TRUE,
                            c("Text" = "txt","GeoTiff" = "tif"))
  ),
  
  # Main panel details
  mainPanel(
    
    # First tab
    tabsetPanel(
      
      # tab - Spatial analysis
      tabPanel("Regional analysis",
               
               # main maps
               fluidRow(
                 column(6,
                        p(),
                        h4(tags$b("Reference scenario")),
                        leafletOutput("basemap1", width = "100%")
                 ),
                 column(6,
                        p(),
                        h4(tags$b("Alternative scenario")),
                        leafletOutput("basemap2", width = "100%")
                 )
               ),
               
               
               # Add other fluid row here for graphs???? FIXME
               # main graphs
               fluidRow(
                 column(6,
                        p(),
                        plotOutput("plot10")
                 ),
                 column(6,
                        p(),
                        plotOutput("plot11")
                 )
               )
               
               
      ),
      
      # tab - Difference map analysis
      tabPanel("Difference maps",
               # show map
               p(),
               
               fluidRow(
                 column(12,
                        radioButtons("comp", "Comparison method",
                                     inline = TRUE,
                                     c("Absolute" = "abs","Relative (%)" = "rel"))
                 )
               ),
               
               h4(tags$b("Differences between selected scenarios")),
               leafletOutput("basemap3"),
               tags$hr(),
               
               fluidRow(
                 column(6,
                        h4(tags$b("Distribution across all grid-cells")),
                        plotOutput("plot7")
                 ),
                 column(6,
                        h4(tags$b("Inter-annual variability within selected grid-cell:")),
                        p(),
                        plotOutput("plot5")
                 )
               )
               
      ),
      
      
      # tab - Grid cell analysis FIXME: to be moved to Difference map analysis
      tabPanel("Grid-cell analysis", 
               p(),
               #    h4(tags$b("Graphs show the distribution of 20 year simulations within a grid-cell")),
               p(),
               #     selectInput('gc', 'Grid cell', as.character(unique(allData$Lat_Long))),
               h5(tags$b("Click in the location of interest")),
               p(),
               leafletOutput("basemap4"),
               p(),
               # h4(tags$b(textOutput("text2")),align = "center"), # lat/long
               p(),
               h4(tags$b("Reference scenario")),
               p(),
               plotOutput("plot3"),
               p(),
               #     h4(tags$b("Alternative scenario")),
               p(),
               #     plotOutput("plot4"),
               #   p(),
               #   h4(tags$b("Difference for alternative scenarios")),
               #    p(),
               #    plotOutput("plot5")
               p()
      ),
      
      
      # tab - Factor analysis
      tabPanel("Factor analysis",
               # input graphing details
               p(),
               #    h4(tags$b("Relationship between output variables")),
               p(),
               p(),
               selectInput('xcol', 'Select driving variable (X axes)', names(allData),selected = names(allData)[[12]]),
               p(),
               h4(tags$b("Reference scenario")),
               p(),
               plotOutput("plot1"), 
               p(),
               #    h4(tags$b("Alternative scenario")),
               p(),
               #   plotOutput("plot2"),
               p(),
               #     selectInput('colFacet', 'Select factor for colour', names(allData),selected = names(allData)[[12]]),
               p(),
               
               
               fluidRow(
                 column(6,
                        h4(tags$b("Relationship between variables")),
                        plotOutput("plot2")
                 ),
                 column(6,
                        h4(tags$b("Distribution")),
                        p(),
                        plotOutput("plot33")
                 )
               ),
               
               p(),
               numericInput('clusters', 'Cluster count', 3, min = 1, max = 9)
               
               
               #    selectInput('symFacet', 'Select factor for symbols', names(allData),selected = names(allData)[[12]])
      )
      
    )
  )
)

# ------------------------------------------------------------------------------------------
#-------------------------- THE SERVER -----------------------------------------------------
#-------------------------------------------------------------------------------------------

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
  
  # function to find clossest value
  closestValue <- function (x, vec) {
    intervalNo <- findInterval(x, vec)
    lowerValue <- vec[pmax(1, intervalNo)]
    upperValue <- vec[pmin(length(vec), intervalNo+1)]
    ifelse(x - lowerValue < upperValue - x, lowerValue, upperValue)
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
    clickStats <- input$stats
    varDetails <-  selectedVars_df %>%
      filter(variable == mainVarSelec())
    # paste0("(",as.character(varDetails[,"unit"]),")") 
    # as.character(varDetails[,"unit"]) 
    
    ifelse(clickStats == "av",as.character(varDetails[,"unit"]), "CV%")
    
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
  
  
  # DATAFRAME subset -----------------------------------------------------------------
  
  # Raster dataframe (summarised by pixel with average or CV%) --------------
  
  # baseline scenario
  rasterDF_Base <- reactive({
    # define scenario
    gcm <- input$gcm
    rcp <- input$rcp
    crop <- input$crop
    soil <- input$soil
    scn <- input$scn
    
    r <- allData %>%
      filter(
        thisRCP == rcp &
          thisGCM == gcm &
          CurrentCrop == crop & 
          thisSoil == soil  &
          thisScenario == scn) %>%
      dplyr::select(thisLat,thisLong, varSelection()) %>%
      group_by(thisLat, thisLong) %>%
      summarise_each(funs(mean,cvFunc)) %>%
      dplyr::select(thisLat, thisLong, thisVar = statSelection())
    r
  })
  
  # alternative scenario
  rasterDF_Alt <- reactive({
    # define scenario
    gcm <- input$gcm2
    rcp <- input$rcp2
    crop <- input$crop2
    soil <- input$soil2
    scn <- input$scn2
    
    r <- allData %>%
      filter(
        thisRCP == rcp &
          thisGCM == gcm &
          CurrentCrop == crop & 
          thisSoil == soil  &
          thisScenario == scn) %>%
      dplyr::select(thisLat,thisLong, varSelection()) %>%
      group_by(thisLat, thisLong) %>%
      summarise_each(funs(mean,cvFunc)) %>%
      dplyr::select(thisLat, thisLong, thisVar = statSelection())
    r
  })
  
  
  # Create difference dataframe
  
  rasterDF_Diff <- reactive({ 
    
    # calculate fifference map (as df)
    r1 <- rasterDF_Base()
    r2 <- rasterDF_Alt()
    df_diff <- merge(r1, r2, by = c("thisLat","thisLong"))
    
    compType <- compSelection() # method of comparison
    
    if(compType == "abs") {
      df_diff$thisVar <- df_diff[4] - df_diff[3] # alt - base
    } else {
      df_diff$thisVar <- round( ( (df_diff[4] - df_diff[3]) / df_diff[3] ) * 100 , 2) # (base-fut)/base as percent
    } 
    
    # trim df to a simple lat/long/var  
    df_diff <- df_diff %>%
      dplyr::select(thisLat,thisLong, thisVar)
    
    df_diff
    
  })
  
  
  # Dataset for graphing (all data) ------------------------------
  
  # select full (all years) dataset of selected variable (i.e. Y axes, the variable rasterised)
  selectedData_Base <- reactive({
    rcp <- input$rcp
    gcm <- input$gcm
    crop <- input$crop
    soil <- input$soil
    scn <- input$scn
    
    allData <- allData %>%
      filter(   
        thisRCP == rcp &
          thisGCM == gcm &
          CurrentCrop == crop & 
          thisSoil == soil  &
          thisScenario == scn
      )
    
    
    allData[, c(input$xcol, mainVarSelec())]
  })
  
  # select driving variable for graph (X axes)
  selectedData_Alt <- reactive({
    rcp <- input$rcp2
    gcm <- input$gcm2
    crop2 <- input$crop2
    soil2 <- input$soil2
    scn2 <- input$scn2
    
    allData <- allData %>%
      filter( 
        thisRCP == rcp &
          thisGCM == gcm &
          CurrentCrop == crop2 & 
          thisSoil == soil2  &
          thisScenario == scn2
      )
    allData[, c(input$xcol, mainVarSelec())]
  })
  
  
  # Pixel Data for graphing (filtered by lat-long) -----------
  
  # select lat / long approximate coordinates by click
  coordSelectBaseMap3 <- reactive({
    lat <-  as.numeric(as.character(input$basemap3_click$lat))
    lng <-  as.numeric(as.character(input$basemap3_click$lng))
    
    lat.vec <- sort(as.numeric(unique(allData$thisLat)))
    lng.vec <- sort(as.numeric(unique(allData$thisLong)))
    
    # FIXME: closest value needs to change to approx to deal with NAs
    lat.slc <- closestValue(lat,lat.vec)
    lng.slc <- closestValue(lng,lng.vec)
    
    # lat.slc <- approx(lat,y = NULL,lat.vec, method="constant",rule = 2, f=1)
    # lng.slc <- approx(lng,y = NULL, lng.vec,  method="constant",rule = 2, f=1)
    
    x <- c(lat.slc,lng.slc)
    
    return(x)
    
  })
  
  
  # baseline
  selectedDataPix_Base <- reactive({
    
    # values before click # FIXME: not working yet: need to start from selected map
    if(is.null(as.numeric(coordSelectBaseMap3()))) {
      lat <- -37.925
      lng <- 176.275
    } else {
      lat <- coordSelectBaseMap3()[1]
      lng <- coordSelectBaseMap3()[2]
    }
    
    # Due to dplyr issue #318, we need temp variables for input values
    # gc <- input$gc
    # lat <- coordSelectBaseMap4()[1]
    # lng <- coordSelectBaseMap4()[2]
    rcp <- input$rcp
    gcm <- input$gcm
    crop <- input$crop
    soil <- input$soil
    scn <- input$scn
    
    allData <- allData %>%
      filter( #Lat_Long == gc &
        thisRCP == rcp &
          thisGCM == gcm &
          thisLat == lat &
          thisLong == lng &
          CurrentCrop == crop & 
          thisSoil == soil  &
          thisScenario == scn
      )
    allData[, c(input$xcol, mainVarSelec())]
    # allData[, mainVarSelec()]
  })
  
  # Alternative
  selectedDataPix_Alt <- reactive({
    
    # values before click # FIXME: not working yet
    if(is.null(as.numeric(coordSelectBaseMap3()))) {
      lat <- -37.925
      lng <- 176.275
    } else {
      lat <- coordSelectBaseMap3()[1]
      lng <- coordSelectBaseMap3()[2]
    }
    # gc <- input$gc
    #   lat <- coordSelectBaseMap4()[1]
    #   lng <- coordSelectBaseMap4()[2]
    rcp <- input$rcp2
    gcm <- input$gcm2
    crop2 <- input$crop2
    soil2 <- input$soil2
    scn2 <- input$scn2
    
    allData <- allData %>%
      filter( #Lat_Long == gc & # Note that's the same lat/long for both graphs
        thisRCP == rcp &
          thisGCM == gcm &
          thisLat == lat &
          thisLong == lng &
          CurrentCrop == crop2 & 
          thisSoil == soil2  &
          thisScenario == scn2
      )
    
    allData[, c(input$xcol, mainVarSelec())]
    # allData[, mainVarSelec()]
  })
  
  
  
  # Cluster analysis --------------------------------------------------
  
  clusters <- reactive({
    kmeans(selectedData_Base(), input$clusters)
  })
  
  cluster_Alt <- reactive({
    kmeans(selectedData_Alt(), input$clusters)
  })
  
  
  # GRAPHS 1 -----------------------------------------------------------
  
  # create axes limits for all graphs
  
  # For the whole dataframe
  axesLimits <- reactive({
    xmin <- min(min(selectedData_Base()[1]), min(selectedData_Alt()[1]))
    xmax <- max(max(selectedData_Base()[1]), max(selectedData_Alt()[1]))
    ymin <- min(min(selectedData_Base()[2]), min(selectedData_Alt()[2]))
    ymax <- max(max(selectedData_Base()[2]), max(selectedData_Alt()[2]))
    x <- data.frame(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
    x
  })
  
  # For the selection of individual pixels
  axesLimits_Pix <- reactive({
    xmin <- min(min(selectedDataPix_Base()[1]), min(selectedDataPix_Alt()[1]))
    xmax <- max(max(selectedDataPix_Base()[1]), max(selectedDataPix_Alt()[1]))
    ymin <- min(min(selectedDataPix_Base()[2]), min(selectedDataPix_Alt()[2]))
    ymax <- max(max(selectedDataPix_Base()[2]), max(selectedDataPix_Alt()[2]))
    x <- data.frame(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
    x
  })
  
  
  # histogram
  output$plot10 <- renderPlot({
    
    # par(mar = c(5.1, 4.1, 2, 1))
    
    x    <- as.numeric(unlist(selectedData_Base()[2]))
    
    # thisUnit <- ifelse((compSelection() == "rel"| statSelection() == 4), "%", varUnits())
    thisUnit <- varUnits()
    
    # sort out limits of axes
    yAxesLims <- c(axesLimits()$ymin, axesLimits()$ymax)
    
    if(input$graphType == "b") {
      
      boxplot(x,
              main=" ",
              col = "lightgrey",
              horizontal=TRUE,
              ylim= yAxesLims,
              xlab=paste0(mainVarSelec()," (", thisUnit,")"),
              pch = 20, cex = 3)
      points(clusters()$centers, pch = 4, cex = 4, lwd = 4) 
      
    } else {
      
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      hist(x,
           main=" ",
           col = "lightgrey",
           breaks = bins,
           xlim= yAxesLims,
           xlab=paste0(mainVarSelec()," (", thisUnit,")"),
           pch = 20, cex = 3)
    }
    
    
  })
  
  
  # alternative
  
  # histogram
  output$plot11 <- renderPlot({
    
    # par(mar = c(5.1, 4.1, 2, 1))
    
    x    <- as.numeric(unlist(selectedData_Alt()[2]))
    
    # thisUnit <- ifelse((compSelection() == "rel"| statSelection() == 4), "%", varUnits())
    thisUnit <- varUnits()
    
    # sort out limits of axes
    yAxesLims <- c(axesLimits()$ymin, axesLimits()$ymax)
    
    if(input$graphType == "b") {
      
      boxplot(x,
              main=" ",
              col = "lightgrey",
              horizontal=TRUE,
              ylim= yAxesLims,
              xlab=paste0(mainVarSelec()," (", thisUnit,")"),
              pch = 20, cex = 3)
      points(clusters()$centers, pch = 4, cex = 4, lwd = 4) 
      
    } else {
      
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      hist(x,
           main=" ",
           col = "lightgrey",
           breaks = bins,
           xlim= yAxesLims,
           xlab=paste0(mainVarSelec()," (", thisUnit,")"),
           pch = 20, cex = 3)
    }
    
    
  })
  
  # X Y graphic comparison of variables----------
  
  # XY scatter graph 
  output$plot1 <- renderPlot({
    
    if (is.character(selectedData_Base()))
      return(NULL)
    
    df_bas <- selectedData_Base() # FIXME: repeated code: make it single
    df_bas$scn <- "base"
    df_alt <- selectedData_Alt()
    df_alt$scn <- "alt"
    df_merge <- rbind(df_bas,df_alt)
    
    df_merge %>%
      ggplot(aes_string(x=input$xcol, y=mainVarSelec())) + 
      geom_point(aes(colour = as.factor(scn)), size = 3) +
      theme(legend.position = "top", text = element_text(size=20))
    # theme(legend.position = c(0.1, 0.8), text = element_text(size=20))
    
  })
  
  # alternative graph
  output$plot2 <- renderPlot({
    
    if (is.character(selectedData_Base()))
      return(NULL)
    
    df_bas <- selectedData_Base() # FIXME: repeated code: make it single
    df_bas$scn <- "base"
    df_alt <- selectedData_Alt()
    df_alt$scn <- "alt"
    df_merge <- rbind(df_bas,df_alt)
    
    df_merge %>%
      ggplot(aes_string(mainVarSelec())) + 
      geom_density(aes(colour = as.factor(scn)), size = 2) +
      theme(legend.position = "top", text = element_text(size=20))+
      ggtitle(as.character(input$mainvar))
    # theme(legend.position = c(0.1, 0.8), text = element_text(size=20))
    
  })
  
  
  output$plot33 <- renderPlot({
    
    if (is.character(selectedData_Base()))
      return(NULL)
    
    df_bas <- selectedData_Base() # FIXME: repeated code: make it single
    df_bas$scn <- "base"
    df_alt <- selectedData_Alt()
    df_alt$scn <- "alt"
    df_merge <- rbind(df_bas,df_alt)
    
    df_merge %>%
      ggplot(aes_string(input$xcol)) + 
      geom_density(aes(colour = as.factor(scn)), size = 2) +
      theme(legend.position = "top", text = element_text(size=20)) +
      ggtitle(as.character(input$xcol))
    # theme(legend.position = c(0.1, 0.8), text = element_text(size=20))
    
  })
  
  
  
  
  
  
  # Distribution graphs (within pixel)-------------
  
  # base graph
  output$plot3 <- renderPlot({
    
    par(mar = c(5.1, 4.1, 2, 1))
    
    x    <- as.numeric(unlist(selectedDataPix_Base()[2]))
    
    # thisUnit <- ifelse((compSelection() == "rel"| statSelection() == 4), "%", varUnits())
    thisUnit <- varUnits()
    
    # sort out limits of axes
    yAxesLims <- c(axesLimits_Pix()$ymin, axesLimits_Pix()$ymax)
    
    if(input$graphType == "b") {
      
      boxplot(x,
              main=" ",
              col = "lightgrey",
              horizontal=TRUE,
              ylim= yAxesLims,
              xlab=paste0(mainVarSelec()," (", thisUnit,")"),
              pch = 20, cex = 3)
      points(clusters()$centers, pch = 4, cex = 4, lwd = 4) 
      
    } else {
      
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      hist(x,
           main=" ",
           col = "lightgrey",
           breaks = bins,
           xlim= yAxesLims,
           xlab=paste0(mainVarSelec()," (", thisUnit,")"),
           pch = 20, cex = 3)
    }
    
    
  })
  
  # alternative graph
  output$plot4 <- renderPlot({
    
    par(mar = c(5.1, 4.1, 2, 1))
    
    x    <- as.numeric(unlist(selectedDataPix_Alt()[2]))
    
    # thisUnit <- ifelse((compSelection() == "rel"| statSelection() == 4), "%", varUnits()) # FIXME: remove code duplication
    thisUnit <- varUnits()
    
    if(input$graphType == "b") {
      
      boxplot(x,
              main= " ",
              col = "lightgrey",
              horizontal=TRUE,
              ylim=c(axesLimits_Pix()$ymin, axesLimits_Pix()$ymax),
              xlab=paste0(mainVarSelec()," (", thisUnit,")"),
              pch = 20, cex = 3)
      points(clusters()$centers, pch = 4, cex = 4, lwd = 4) 
      
    } else {
      
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      hist(x,
           main=" ",
           col = "lightgrey",
           breaks = bins,
           xlim=c(axesLimits_Pix()$ymin, axesLimits_Pix()$ymax),
           xlab=paste0(mainVarSelec()," (", thisUnit,")"),
           pch = 20, cex = 3)
    }
    
    
  })
  
  # Differences within a gridcell
  output$plot5 <- renderPlot({
    
    par(mar = c(5.1, 4.1, 2, 1))
    
    bas <-   data.frame(thisVar = as.numeric(unlist(selectedDataPix_Base()[2])))
    alt <-   data.frame(thisVar = as.numeric(unlist(selectedDataPix_Alt()[2])))
    
    diff_abs <- alt - bas
    diff_rel <- (alt-mean(bas$thisVar))/mean(bas$thisVar)*100 # (alt-bas)/bas*100 
    
    # FIXME: Not cohercing to a vector
    x  <- ifelse(compSelection() == "abs", diff_abs, diff_rel)
    
    thisUnit <- ifelse((compSelection() == "rel"| statSelection() == 4), "%", varUnits()) # FIXME: remove code duplication
    
    if(input$graphType == "b") {
      
      boxplot(x,
              main= " ",
              col = "lightgrey",
              horizontal=TRUE,
              #      ylim=c(min(min(bas,alt)), max(max(bas,alt))),
              xlab=paste0("Difference in ", mainVarSelec()," (", thisUnit,")"),
              pch = 20, cex = 3)
      points(clusters()$centers, pch = 4, cex = 4, lwd = 4) 
      
    } else {
      
      x <- as.numeric(unlist(x))
      
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # FIXME: invalid 'type' (list) of argument
      hist(x,
           main=" ",
           col = "lightgrey",
           breaks = bins,
           #   xlim=c(axesLimits_Pix()$ymin, axesLimits_Pix()$ymax),
           xlab=paste0("Difference in ",mainVarSelec()," (", thisUnit,")"),
           pch = 20, cex = 3)
    }
    
    
  })
  
  
  # RASTERISE DFs ------------------------------------------ FIXME: Not fully working yet
  
  # FIXME: Can u set up a rasterMyDf function to avoid this code duplication?
  rasterMyDf <- function(x) {
    reactive ({
      df <- x
      spg <- data.frame(df$thisLong, df$thisLat, df$thisVar)
      coordinates(spg) <- ~ df.thisLong + df.thisLat # Attention to variable names
      gridded(spg) <- TRUE
      r <- raster(spg)
      proj4string(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      r
    })
  }
  
  # rasterise DFs with custumised function
  # base_rasterLayer <- rasterMyDf (rasterDF_Base())
  # base_rasterLayer <- rasterMyDf (rasterDF_Alt())
  #diff_rasterLayer <- rasterMyDf (rasterDF_Diff()) # FIXME: This removes reactivity to layer change - TRY again now
  
  
  base_rasterLayer <- reactive ({
    df <- rasterDF_Base()
    spg <- data.frame(df$thisLong, df$thisLat, df$thisVar)
    coordinates(spg) <- ~ df.thisLong + df.thisLat # Attention to variable names
    gridded(spg) <- TRUE
    r <- raster(spg)
    proj4string(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    r
  })
  
  
  alt_rasterLayer <- reactive ({
    df <- rasterDF_Alt()
    spg <- data.frame(df$thisLong, df$thisLat, df$thisVar)
    coordinates(spg) <- ~ df.thisLong + df.thisLat # Attention to variable names
    gridded(spg) <- TRUE
    r <- raster(spg)
    proj4string(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    r
  })
  
  # Diff raster
  diff_rasterLayer <- reactive ({
    df <- rasterDF_Diff()
    spg <- data.frame(df$thisLong, df$thisLat, df$thisVar)
    coordinates(spg) <- ~ df.thisLong + df.thisLat # Attention to variable names
    gridded(spg) <- TRUE
    r <- raster(spg)
    proj4string(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    r
  })
  
  
  # MAPPING ---------------------------------------------------
  
  content <- paste(sep = "<br/>",
                   "<b><a href='https://www.boprc.govt.nz/environment/rivers-and-drainage/kaituna-catchment-control-scheme/'>Kaituna catchment</a></b>",
                   "Kaituna Catchment",
                   "Bay of Plenty, NZ"
  )
  
  
  # Default function: add parameters if necessary to taylor maps later
  createMainMap <- function() {
    renderLeaflet({
      leaflet() %>%
        setView(lng = 176.272, lat = -38.0, zoom = 9) %>%
        addTiles()  %>%
        addPolygons(data=sf2, fill = F ,opacity = 0.7, weight = 2, group = "Catchment Borders") %>%       
        addLayersControl(
          overlayGroups = c("Catchment Borders","Rasters"),
          options = layersControlOptions(collapsed = FALSE))
    } )
  }
  
  # Create basemaps
  output$basemap1 <- createMainMap() # base 1 (base)
  output$basemap2 <- createMainMap() # base 2 (for alternative)
  output$basemap3 <- createMainMap() # base 3 (for difference)
  output$basemap4 <- createMainMap() # base 4 (selected pixel)
  
  # raster transparancy FIXME: not working yet
  sl <- reactive ({
    input$slider1
  })
  
  # raster base
  observe({
    pal <- colorNumeric(c("#8B0000","#EE4000", "#FFA500","#008B45"), 
                        values(base_rasterLayer()), na.color = "transparent")
    
    
    # valRasters <- c(rasterDF_Base()$thisVar, rasterDF_Alt()$thisVar)
    valRasters <- rasterDF_Base()$thisVar
    
    leafletProxy("basemap1", data = c(base_rasterLayer(), sl())) %>%
      clearGroup(group="Rasters") %>%
      clearControls() %>% # necessary to remove old legend
      addRasterImage(base_rasterLayer(),colors = pal, opacity = sl(), group = "Rasters") %>%
      #  addLegend(pal = pal, values = values(base_rasterLayer()),
      addLegend(pal = pal, values = valRasters, 
                title = varUnits()) # FIXME: Use % or CV% if relative selected
  })
  
  # raster alternative
  observe({
    pal <- colorNumeric(c("#8B0000","#EE4000", "#FFA500","#008B45"), 
                        values(alt_rasterLayer()), na.color = "transparent")
    
    
    # valRasters <- c(rasterDF_Base()$thisVar, rasterDF_Alt()$thisVar)
    valRasters <- rasterDF_Alt()$thisVar
    
    leafletProxy("basemap2", data = c(alt_rasterLayer(), sl())) %>%
      clearGroup(group="Rasters") %>% 
      clearControls() %>% # necessary to remove old legend
      addRasterImage(alt_rasterLayer(),colors = pal, opacity = sl(), group = "Rasters") %>%
      #  addLegend(pal = pal, values = values(alt_rasterLayer()), 
      addLegend(pal = pal, values = valRasters, # FIXME: legend is rescaling
                title = varUnits()) # FIXME: Use % or CV% if relative selected
  })
  
  # add raster difference
  observe({
    pal <- colorNumeric(c("#ffffe5", "#fff7bc", "#fee391","#fec44f","#fe9929","#ec7014","#cc4c02","#8c2d04"), 
                        values(diff_rasterLayer()), na.color = "transparent")
    
    lng <- ifelse(is.null(input$basemap3_click$lng), 178,input$basemap3_click$lng)
    lat <- ifelse(is.null(input$basemap3_click$lat), -38,input$basemap3_click$lat)
    
    
    thisTitle <- ifelse((compSelection() == "rel"| statSelection() == 4), "(%)", varUnits()) # FIXME: the use of int for statSel is not intuitive
    
    leafletProxy("basemap3", data = c(diff_rasterLayer(), sl())) %>%
      clearGroup(group="Rasters") %>%
      clearControls() %>% # necessary to remove old legend
      clearMarkers() %>%
      addRasterImage(diff_rasterLayer(),colors = pal, opacity = sl(), group = "Rasters") %>%
      addMarkers(lng,lat) %>%
      addLegend(pal = pal, values = values(diff_rasterLayer()), 
                title = thisTitle) # FIXME: Use % or CV% if relative selected
  })
  
  
  # add raster to base 4 (FIXME: temporary test)
  observe({
    pal <- colorNumeric(c("#8B0000","#EE4000", "#FFA500","#008B45"), 
                        values(base_rasterLayer()), na.color = "transparent")
    
    lng <- ifelse(is.null(input$basemap4_click$lng), 178,input$basemap4_click$lng)
    lat <- ifelse(is.null(input$basemap4_click$lat), -38,input$basemap4_click$lat)
    
    valRasters <- c(rasterDF_Base()$thisVar, rasterDF_Alt()$thisVar)
    
    leafletProxy("basemap4", data = c(base_rasterLayer(), sl())) %>%
      clearGroup(group="Rasters") %>%
      clearMarkers() %>%
      clearControls() %>% # necessary to remove old legend
      addRasterImage(base_rasterLayer(),colors = pal, opacity = sl(), group = "Rasters") %>%
      addMarkers(lng,lat) %>%
      #  addLegend(pal = pal, values = values(base_rasterLayer()),
      addLegend(pal = pal, values = valRasters, 
                title = varUnits()) # FIXME: Use % or CV% if relative selected
  })
  
  # add polygon custom function
  addMyPolygon <- function (x, y) {
    leafletProxy(x, data = y) %>%
      addPolygons(data=y, fill = F ,opacity = 0.7, weight = 2, group = "Catchment Borders") %>%       
      addLayersControl(
        overlayGroups = "Kaituna catchment",
        options = layersControlOptions(collapsed = FALSE))
  }
  
  # addMyPolygon("basemap1",sf2)
  # addMyPolygon("basemap2",sf2)
  # addMyPolygon("basemap3",sf2)
  # addMyPolygon("basemap4",sf2)
  
  
  # GRAPHS ---------------------------------------------------------------------
  
  
  # Graph diff distrubution of raster DF (across grid cells)
  output$plot7 <- renderPlot({
    
    par(mar = c(5.1, 4.1, 2, 1))
    
    thisUnit <- ifelse((compSelection() == "rel"| statSelection() == 4), "%", varUnits()) # FIXME: remove code duplication
    
    df <- rasterDF_Diff()
    x    <- df[, "thisVar"]
    
    if(input$graphType == "b") {
      
      boxplot(as.numeric(unlist(x)),
              main=" ",
              horizontal=TRUE,
              col = "lightgrey",
              xlab=paste0(mainVarSelec()," (", thisUnit,")"),
              pch = 20, cex = 3)
      
    } else {
      
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      hist(as.numeric(unlist(x)),
           main=" ",
           col = "lightgrey",
           breaks = bins,
           xlab=paste0(mainVarSelec()," (", thisUnit,")"),
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
    paste0("Variable unit is: ",varUnits())
    
  })  
  
  # Show variable name (test only)
  output$text2 <- renderText({ 
    lat <-  as.numeric(as.character(input$basemap4_click$lat))
    lng <-  as.numeric(as.character(input$basemap4_click$lng))
    
    lat.vec <- sort(as.numeric(unique(allData$thisLat)))
    lng.vec <- sort(as.numeric(unique(allData$thisLong)))
    
    # FIXME: closest value needs to change to approx to deal with NAs
    lat.slc <- closestValue(lat,lat.vec)
    lng.slc <- closestValue(lng,lng.vec)
    
    # lat.slc <- approx(lat,y = NULL,lat.vec, method="constant",rule = 2, f=1)
    # lng.slc <- approx(lng,y = NULL, lng.vec,  method="constant",rule = 2, f=1)
    
    paste0("Selected grid-cell has Latitude: ", lat.slc," Longitude: " ,lng.slc)
    
  }) 
  
  # DOWNLOAD  ----------------------------- FIXME: file content is not as expected
  
  output$downloadData <- downloadHandler(
    
    # ext <- ifelse(input$fileType == "txt",".txt",".tif"),
    
    filename = function() { paste(input$mainvar, input$fileType, sep=".") },
    
    #  filename = function() {paste0(input$mainvar,"_",input$compSelection,"_",input$statSelection,".",input$fileType) },
    
    content = function(file) {
      
      df <- data.frame(lat = rasterDF_Diff()$thisLat, lon = rasterDF_Diff()$thisLong, Difference = rasterDF_Diff()$thisVar)
      
      if(input$fileType == "txt") {
        
        thisHeader <- paste0("#",input$mainvar," ",varUnits()," ", as.character(statSelection()))
        # FIME: Add header with meta-data
        # writeLines(c("Hello","World"), file(file))
        write.table(df, file, row.names=F)
        #  write.csv(df, file ,row.names=F)
        
      } else {
        
        # save as raster
        r <- diff_rasterLayer()
        proj4string(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
        
        res <- writeRaster(r, filename=file, format="GTiff", overwrite=TRUE)
        
        # Show the corresponding output filename
        print(res@file@name)
        
        # Rename it to the correct filename
        file.rename(res@file@name, file)
        
        
      }
      
    }
  )
  
}

shinyApp(ui = ui, server = server)
