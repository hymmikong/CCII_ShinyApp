# CCII - MBIE prototype App (RA2)

#palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
#  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

# load libraries (FIXME: delete the ones not used anymore)
library(shiny)
library(shinyFiles)
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

# for debbuging
DEBUG <- F
if(DEBUG == T){
  input <- list()
}

# select directory with raw data
rootDir <- "C:\\GitHubRepos\\CCII_ShinyApp\\data\\"

# select root directory (where all data will sit)
allData <- read.csv(paste0(rootDir, "AllData(RA2).csv"), header = TRUE)

# select variables of interest based on listed outputs
varList <- read.csv(paste0(rootDir, "variableNames_v3.csv"), header = TRUE)

# select the polygons (Kaituna region boundaries)
pathShapeFile <- paste0(rootDir, "lowerKaitunabnd(WGS84).shp")

selectedVars_df <- varList %>%
  filter(include == "yes")

all.factors <- varList %>%
  filter(is.factor == "yes")

numVars_df <- varList %>%
  filter(is.factor == "no")

varNames <- as.character(selectedVars_df$variable)

# find col positions that hold the variables of interest in the raw df
selectColNos <- match(varNames,names(allData))

allData <- allData  %>%
  dplyr::select(selectColNos)

fullNames <- as.character(selectedVars_df$fullName)

numVarNames <- as.character(numVars_df$fullName)

# Customise data (FIXME: this should be done earlier in raw dataset)
allData <- allData %>%
# subset(thisGCM != "ERA") %>%
  mutate(Lat_Long = paste0(thisLat,"_",thisLong))


# Load polygon maps for 'Kaituna' catchment
# pathShapeFile <- 'C:/apsim_dev/Projects/CCII/GIS_layers/CaseStudy/lowerKaitunabnd(WGS84).shp'
sf2 <- readShapeSpatial(pathShapeFile, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# sf2 <- gSimplify(sf2,tol=.01,topologyPreserve = TRUE)


#-------------------------------------------------------------------
#------------- THE UI ------------------------------------------------
#--------------------------------------------------------------------

ui <- fluidPage(
  
  headerPanel('Spatial analysis App - RA2 CCII-MBIE - Arable Crops'),
  p(),
  
  # Side panel details
  sidebarPanel(width = 3,
               
             #  fileInput("file1", 'Choose the path to input data:'),
             h4(tags$b("Select input data folder:")),
             shinyDirButton('directory', 'Folder select', 'Please select a folder'),
               
               tags$hr(),
             
               selectInput('mainvar', 'Select the output variable:', fullNames, selected = numVarNames[17]),
               
               # Show selection
               textOutput("text1"),
               
               tags$hr(),
               h4(tags$b("Construct scenarios:")),
               fluidRow(
                 column(5,
                        h4(tags$b("Refence")),
                        selectInput('gcm', 'GCM #1', as.character(unique(allData$thisGCM)),selected = as.character(unique(allData$thisGCM))[[2]]),
                        selectInput('rcp', 'Climate #1', as.character(unique(allData$thisRCP)),selected = as.character(unique(allData$thisRCP))[[2]]),
                        selectInput('scn', 'Time #1', as.character(unique(allData$thisScenario)),selected = as.character(unique(allData$thisScenario))[[3]]),
                        selectInput('crop', 'Crop #1', as.character(unique(allData$thisCrop))),
                        selectInput('cult', 'Cultivar #1', as.character(unique(allData$thisCultivar))),
                        selectInput('soil', 'Soil #1 ', as.character(unique(allData$thisSoil)))
                        
                 ),
                 column(5,
                        h4(tags$b("Alternative")),
                        selectInput('gcm2','GCM #2', as.character(unique(allData$thisGCM)),selected = as.character(unique(allData$thisGCM))[[2]]),
                        selectInput('rcp2', 'Climate #2', as.character(unique(allData$thisRCP)),selected = as.character(unique(allData$thisRCP))[[2]]),
                        selectInput('scn2', 'Time #2', as.character(unique(allData$thisScenario)),selected = as.character(unique(allData$thisScenario))[[1]]),
                        selectInput('crop2', 'Crop #2', as.character(unique(allData$thisCrop))),
                        selectInput('cult2', 'Cultivar #2', as.character(unique(allData$thisCultivar))),
                        selectInput('soil2', 'Soil #2', as.character(unique(allData$thisSoil)))
                        
                 ),
                 column(2,
                        hr(),
                        hr(),
                        textOutput("flag_gcm"),
                        hr(),
                        hr(),
                        textOutput("flag_rcp"),
                        hr(),
                        hr(),
                        textOutput("flag_scn"),
                        hr(),
                        hr(),
                        textOutput("flag_crop"),
                        hr(),
                        hr(),
                        textOutput("flag_cult"),
                        hr(),
                        hr(),
                        textOutput("flag_soil")
                 )
               ),
               
               # download controls
               tags$hr(),
               h4(tags$b("Download selected data")),
               radioButtons("fileToDownload", "Select output:",
                               inline = TRUE,
                               c("Reference" = "dl_ref",
                                 "Alternative" = "dl_alt",
                                 "Difference" = "dl_dif")),
               p(),
               radioButtons("fileType", "Select output format:",
                            inline = TRUE,
                            c("Text" = "txt","GeoTiff" = "tif")),
            downloadButton("downloadData", "Download data")
  ),
  
  # Main panel details
  mainPanel(
    
    # First tab
    tabsetPanel(
      
      # tab - Spatial analysis
      tabPanel("Regional analysis",

           # map controls
           fluidRow(
             column(4,
                    p(),
                    radioButtons("stats", "Choose statistics:",
                               #  inline = TRUE,
                                 c("Average" = "av",
                                   "Inter-annual variability (CV%)" = "cv"))
                    
                   ),
             column(4,
                    p(),
                    # select diff method
                    radioButtons("comp", "Comparison method",
                             #    inline = TRUE,
                                 c("Absolute" = "abs","Relative (%)" = "rel"))
                    
             ),
             column(4,
                    # raster transparency
                    p(),
                    sliderInput("slider1", 
                                label = "Raster transparency", 
                                min = 0, max = 1, value = 0.5)
             )
           ),
           
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
                        h4(tags$b("Differences between selected scenarios")),
             #           plotOutput("plot10")
                        leafletOutput("basemap3")
                 ),
                 column(6,
                        p(),
                        h4(tags$b("Frequency distribution of values across all grid-cells")),
                        plotOutput("plot11")
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
               h4(tags$b("Inter-annual variability in selected pixel")),
               p(),
               plotOutput("plot3"),
               p(),
               #     h4(tags$b("Alternative scenario")),
               p(),
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
             #  selectInput('xcol', 'Select driving variable (X axes)', fullNames, selected =  numVarNames[17]),
               p(),
               h4(tags$b("Relationship between selected variables")),
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
                        h4(tags$b("Distribution of X-axes values")),
                        plotOutput("plot33")
                 ),
                 column(6,
                        h4(tags$b("Distribution of Y-axes values")),
                        p(),
                        plotOutput("plot2")
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

server <- function(input, output, session) {
  
  
  # selection of path to input data # FIXME: Frrezes app after selection is made
#  volumes <- getVolumes() # c('R Installation'= R.home())
#  shinyDirChoose(input, 'directory', updateFreq = 2000, roots=volumes, session=session, restrictions=system.file(package='base'))
#
#  output$directorypath <- renderPrint({parseDirPath(volumes, input$directory)})
#  
# thisPath <- reactive({
#    if(is.null(parseDirPath(volumes, input$directory))) {return(NULL)}
#    
#    return(parseDirPath(volumes, input$directory) )
#    
#  })
  
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
  
  
  # flags to show which factors are being compared
  flagFact <- reactive({ # aims to substiture most varNames

  f1 <-  ifelse(input$gcm == input$gcm2,"=", "Diff")
  f2 <-  ifelse(input$rcp == input$rcp2,"=", "Diff")
  f3 <-  ifelse(input$scn == input$scn2,"=", "Diff")
  f4 <-  ifelse(input$crop == input$crop2,"=", "Diff")
  f5 <-  ifelse(input$cult == input$cult2,"=", "Diff")
  f6 <-  ifelse(input$soil == input$soil2,"=", "Diff")
    
  f <- c(f1, f2, f3, f4, f5, f6)  
  
  return(f)
  
  })
  
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
  
  # select X axes variable in factor analysis
  # Name of selected variable in original file. Converts "selected" name to name in "raw" data
  xAxesVarSelec <- reactive({ # aims to substiture most varNames
    
    buf <-  selectedVars_df %>%
      filter(fullName == as.character(input$xcol))
    
    as.character(buf$variable) # returns var name in raw data
    
  })
  
  
  # DATAFRAME subset -----------------------------------------------------------------
  
  # Create common trimmed dfs for use across other elements
  
  # trim selected reference (baseline) scenario
  df_Base <- reactive ({
    
    rcp <- input$rcp
    gcm <- input$gcm
    crop <- input$crop
    cult <- input$cult
    soil <- input$soil
    scn <- input$scn
    
    bf <- allData %>%
      filter(   
        thisRCP == rcp &
          thisGCM == gcm &
          thisCrop == crop & 
          thisCultivar == cult &
          thisSoil == soil  &
          thisScenario == scn
      )
    
    validate(need(nrow(bf)>0,'Empty dataframe'))
    
    return(bf)
    
  })
  
  # trim selected alternative scenario
  df_Alt <- reactive ({

      rcp <- input$rcp2
      gcm <- input$gcm2
      crop2 <- input$crop2
      cult2 <- input$cult2
      soil2 <- input$soil2
      scn2 <- input$scn2
    
    bf <- allData %>%
      filter( 
        thisRCP == rcp &
          thisGCM == gcm &
          thisCrop == crop2 & 
          thisCultivar == cult2 &
          thisSoil == soil2  &
          thisScenario == scn2
      )
    
    validate(need(nrow(bf)>0,'Empty dataframe'))
    
    return(bf)
    
  })
  
  
  # Raster dataframe (summarised by pixel with average or CV%) --------------
  
  # baseline scenario
  rasterDF_Base <- reactive({
      
   r <- df_Base() %>%
      dplyr::select(thisLat,thisLong, varSelection()) %>%
      group_by(thisLat, thisLong) %>%
      summarise_each(funs(mean,cvFunc)) %>%
      dplyr::select(thisLat, thisLong, thisVar = statSelection())
   
   validate(need(nrow(r)>0,'Empty dataframe'))
   
    return(r)
    
  })
  
  # alternative scenario
  rasterDF_Alt <- reactive({
    
      r <- df_Alt() %>%
      dplyr::select(thisLat,thisLong, varSelection()) %>%
      group_by(thisLat, thisLong) %>%
      summarise_each(funs(mean,cvFunc)) %>%
      dplyr::select(thisLat, thisLong, thisVar = statSelection())
      
      validate(need(nrow(r)>0,'Empty dataframe'))
      
    return(r)
    
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
    
    validate(need(nrow(df_diff)>0,'Empty dataframe'))
    
    return(df_diff)
    
  })
  
  
  # Dataset for graphing (all data) ------------------------------
  
  # select full (all years) dataset of selected variable (i.e. Y axes, the variable rasterised)
  selectedData_Base <- reactive({
    
  bf <- df_Base()[, c(input$xcol, mainVarSelec())] # filter only? FIXME: can yo graph directly from df_BAse?
  
  validate(need(nrow(bf)>0,'Empty dataframe'))
  
  return(bf)

  })
  
  # select driving variable for graph (X axes)
  selectedData_Alt <- reactive({
    
    bf <- df_Alt()[, c(input$xcol, mainVarSelec())]
    
    validate(need(nrow(bf)>0,'Empty dataframe'))
    
    return(bf)
    
  })
  
  
  # Pixel Data for graphing (filtered by lat-long) -----------
  
  # select lat / long approximate coordinates by click
  coordSelectBaseMap4 <- reactive({
    lat <-  as.numeric(as.character(input$basemap4_click$lat))
    lng <-  as.numeric(as.character(input$basemap4_click$lng))
    
    lat.vec <- sort(as.numeric(unique(allData$thisLat)))
    lng.vec <- sort(as.numeric(unique(allData$thisLong)))
    
    # FIXME: closest value needs to change to approx to deal with NAs
    lat.slc <- closestValue(lat,lat.vec)
    lng.slc <- closestValue(lng,lng.vec)
    
    # lat.slc <- approx(lat,y = NULL,lat.vec, method="constant",rule = 2, f=1)
    # lng.slc <- approx(lng,y = NULL, lng.vec,  method="constant",rule = 2, f=1)
    
    x <- c(lat.slc,lng.slc)
    
    validate(need(!is.null(x),'Select pixel')) # FIXME: message at open: replacement has 1 row, data has 0
    
    return(x)
    
  })
  
  
  # baseline
  selectedDataPix_Base <- reactive({
    
    # values before click # FIXME: not working yet: need to start from selected map
    if(is.null(as.numeric(coordSelectBaseMap4()))) {
      lat <- -37.925
      lng <- 176.275
    } else {
      lat <- coordSelectBaseMap4()[1]
      lng <- coordSelectBaseMap4()[2]
    }
    
    # trim to selected coordinates FIXME: Untested
    bf <- df_Base() %>%
      filter(thisLat == lat &
             thisLong == lng)
    
   # validate(need(nrow(bf)>0,'Empty dataframe'))

    bf[, c(input$xcol, mainVarSelec())]
    

  })
  
  # Alternative
  selectedDataPix_Alt <- reactive({
    
    # values before click # FIXME: not working yet
    if(is.null(as.numeric(coordSelectBaseMap4()))) {
      lat <- -37.925
      lng <- 176.275
    } else {
      lat <- coordSelectBaseMap4()[1]
      lng <- coordSelectBaseMap4()[2]
    }

    # trim to selected coordinates FIXME: Untested
    bf <- df_Alt() %>%
      filter(thisLat == lat &
             thisLong == lng)
    
   # validate(need(nrow(bf)>0,'Empty dataframe'))
    
    bf[, c(input$xcol, mainVarSelec())]
    

  })
  
  
  
  # Cluster analysis --------------------------------------------------
  
  clusters <- reactive({
    kmeans(selectedData_Base(), input$clusters)
  })
  
  cluster_Alt <- reactive({
    kmeans(selectedData_Alt(), input$clusters)
  })
  
 ##############################################################
 # --------- RASTERISE DFs -------------------FIXME: Not fully working yet
 ################################################################ 
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
    
    validate(need(nrow(r)>0,'Empty dataframe'))
    
    return(r)
  })
  
  
  alt_rasterLayer <- reactive ({
    df <- rasterDF_Alt()
    spg <- data.frame(df$thisLong, df$thisLat, df$thisVar)
    coordinates(spg) <- ~ df.thisLong + df.thisLat # Attention to variable names
    gridded(spg) <- TRUE
    r <- raster(spg)
    proj4string(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    
    validate(need(nrow(r)>0,'Empty dataframe'))
    
    return(r)
  })
  
  # Diff raster
  diff_rasterLayer <- reactive ({
    df <- rasterDF_Diff()
    spg <- data.frame(df$thisLong, df$thisLat, df$thisVar)
    coordinates(spg) <- ~ df.thisLong + df.thisLat # Attention to variable names
    gridded(spg) <- TRUE
    r <- raster(spg)
    proj4string(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    
    validate(need(nrow(r)>0,'Empty dataframe'))
    
    return(r)
  })
  
  ################################
  # MAPPING ----------------------
  ################################
  
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
  
  # raster transparancy 
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
  #  pal <- colorNumeric(c("#ffffe5", "#fff7bc", "#fee391","#fec44f","#fe9929","#ec7014","#cc4c02","#8c2d04"), 
  #                      values(diff_rasterLayer()), na.color = "transparent")
    
    pal <- colorNumeric(c("#8B0000","#EE4000", "#FFA500","#008B45"), 
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
    #  pal <- colorNumeric(c("#ffffe5", "#fff7bc", "#fee391","#fec44f","#fe9929","#ec7014","#cc4c02","#8c2d04"), 
    #                      values(diff_rasterLayer()), na.color = "transparent")
    
    pal <- colorNumeric(c("#8B0000","#EE4000", "#FFA500","#008B45"), 
                        values(diff_rasterLayer()), na.color = "transparent")
    
    lng <- ifelse(is.null(input$basemap4_click$lng), 178,input$basemap4_click$lng)
    lat <- ifelse(is.null(input$basemap4_click$lat), -38,input$basemap4_click$lat)
    
    
    thisTitle <- ifelse((compSelection() == "rel"| statSelection() == 4), "(%)", varUnits()) # FIXME: the use of int for statSel is not intuitive
    
    leafletProxy("basemap4", data = c(diff_rasterLayer(), sl())) %>%
      clearGroup(group="Rasters") %>%
      clearControls() %>% # necessary to remove old legend
      clearMarkers() %>%
      addRasterImage(diff_rasterLayer(),colors = pal, opacity = sl(), group = "Rasters") %>%
      addMarkers(lng,lat) %>%
      addLegend(pal = pal, values = values(diff_rasterLayer()), 
                title = thisTitle) # FIXME: Use % or CV% if relative selected
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
  
  #########################
  # Tables for testing
  #########################
  
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
  
  
  # text flags for selected scenarios
  # Show variable name
  output$flag_gcm <- renderText({ 
    #paste0("gcm","0")
    flagFact()[1]
  }) 
  output$flag_rcp <- renderText({ 
    #paste0("rcp","0")
    flagFact()[2]
  }) 
  output$flag_scn <- renderText({ 
    #paste0("scn","0")
    flagFact()[3]
  }) 
  output$flag_crop <- renderText({ 
    #paste0("crop","0")
    flagFact()[4]
  }) 
  output$flag_cult <- renderText({ 
   # paste0("cult","0")
    flagFact()[5]
  }) 
  output$flag_soil <- renderText({ 
    paste0("soil","0")
    flagFact()[6]
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
  
  
  #############################################################
  # ---- Graphs ----------------------------
  #############################################################
  
  # diff of rasters
  output$plot11 <- renderPlot({ 
    
  #  if(nrow(selectedData_Base()) == 0 |  # FIXME: check if these are needed now that we have validate
  #     nrow(selectedData_Alt()) == 0
  #  )  {return(NULL) }
    
    
  #  if (is.character(selectedData_Base()))
  #    return(NULL)
    
    df_bas <- selectedData_Base() # FIXME: repeated code: make it single
    df_bas$scn <- "base"
    df_alt <- selectedData_Alt()
    df_alt$scn <- "alt"
    df_merge <- rbind(df_bas,df_alt)
    
    # FIXME: this has to select CV when stat option is ticked
    df_merge %>%
      ggplot(aes_string(mainVarSelec()), aes(..count..)) + 
      geom_density(aes(colour = as.factor(scn),fill = as.factor(scn)), size = 2, alpha = 0.5) +
      theme(legend.position = c(.2, .8),text = element_text(size=20)) +
      theme(legend.title=element_blank()) +
      # scale_colour_brewer(name = "Scenario", ) +
      # ggtitle(as.character(input$mainvar)) +
      #   guides(fill = guide_legend(keywidth = 2, keyheight = 2)) +
      scale_x_continuous(name=paste0(as.character(input$mainvar)," (",as.character(varUnits()),")"))
    # theme(legend.position = c(0.1, 0.8), text = element_text(size=20))

    
  })
  
  
  # within selected single pixel data
  output$plot3 <- renderPlot({ 
    
    if (is.character(selectedDataPix_Base()))
      return(NULL)
    
    df_bas <- selectedDataPix_Base() # FIXME: repeated code: make it single
    df_bas$scn <- "base"
    df_alt <- selectedDataPix_Alt()
    df_alt$scn <- "alt"
    df_merge <- rbind(df_bas,df_alt)
    
    # FIXME: this has to select CV when stat option is ticked
    df_merge %>%
      ggplot(aes_string(mainVarSelec()), aes(..count..)) + 
      geom_density(aes(colour = as.factor(scn),fill = as.factor(scn)), size = 2, alpha = 0.5) +
      theme(legend.position = c(.2, .8),text = element_text(size=20)) +
      theme(legend.title=element_blank()) +
      # scale_colour_brewer(name = "Scenario", ) +
      # ggtitle(as.character(input$mainvar)) +
      scale_x_continuous(name=paste0(as.character(input$mainvar)," (",as.character(varUnits()),")"))
    # theme(legend.position = c(0.1, 0.8), text = element_text(size=20))
    
  })

  
  # X and Y comparoison of factors
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
      geom_smooth(aes(colour = as.factor(scn)))+
      theme(legend.title=element_blank()) +
      theme(legend.position = c(.2, .8),text = element_text(size=20)) 
    #  scale_x_continuous(name=paste0(as.character(?)," (",as.character(varUnits()),")")) FIXME: need a new var name and unit as render
    # theme(legend.position = c(0.1, 0.8), text = element_text(size=20))
    
  })
  
  # distribution of x-axes variable
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
      geom_density(aes( fill = as.factor(scn), colour = as.factor(scn)), alpha= 0.5) + # , fill = as.factor(scn),  alpha = 0.5
      theme(legend.position = c(.2, .8),text = element_text(size=20)) +
      theme(legend.title=element_blank()) +
      #  ggtitle(as.character(input$mainvar)) +
      scale_x_continuous(name=paste0(as.character(input$mainvar)," (",as.character(varUnits()),")"))
    # theme(legend.position = c(0.1, 0.8), text = element_text(size=20))
    
  })
  
  # distribution of y-axes values
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
      geom_density(aes(fill = as.factor(scn), colour = as.factor(scn)), alpha= 0.5) + # order of factors matter
      theme(legend.title=element_blank()) +
      theme(legend.position = c(.2, .8),text = element_text(size=20)) # +
    #  ggtitle(as.character(input$xcol)) 
    # theme(legend.position = c(0.1, 0.8), text = element_text(size=20))
    
  })
  
 
  ######################################################
  # --------- DOWNLOAD  ----------------------------- FIXME: need file naming
  ######################################################
  
  # select the output type (FIXME: why not do all in same txt file or zipped 3 raster for GeoTiff?)
  datasetInput <- reactive({
    switch(input$fileToDownload,
           "dl_ref" = rasterDF_Base(),
           "dl_alt" = rasterDF_Alt(),
           "dl_dif" = rasterDF_Diff())
  })
  
  
  output$downloadData <- downloadHandler(
    
    # ext <- ifelse(input$fileType == "txt",".txt",".tif"),
    
    filename = function() { paste(input$mainvar, input$fileType, sep=".") },
    
    #  filename = function() {paste0(input$mainvar,"_",input$compSelection,"_",input$statSelection,".",input$fileType) },
    
    content = function(file) {
      
      # add a switch here to select the dataset (loop though?)
      # rasterDF_Base()
      # rasterDF_Alt()
      
      
   #   df <- data.frame(lat = rasterDF_Diff()$thisLat, lon = rasterDF_Diff()$thisLong, Difference = rasterDF_Diff()$thisVar)
       df <- data.frame(lat = datasetInput()$thisLat, lon = datasetInput()$thisLong, PixelValue = datasetInput()$thisVar)
      
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
