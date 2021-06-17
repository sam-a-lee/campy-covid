################ 
# General info #
################

# WIDE FORMAT
# COVID VIS SHINY DASHBOARD
# June 16, 2021
# Samantha Lee

# The purpose of this script is to generate a map of epi-cluster cohesian data
# data plots data at time point 1 and time point 2 simulateanously 
# to show how the geographic centroid of clusters changes 


###############################
# load the required libraries #
###############################

library(shinydashboard)
library(readxl) # for reading xlsx files 
library(shiny) # for running shiny apps
library(shinyWidgets) # for better slider/dropdown options
library(here) # for clear dir calling
library(tidyverse) # tidy data/ kind of used
library(leaflet) # for map visualizations
library(maps) # for drawing maps/getting map data
library(RColorBrewer) # for prettier colours
library(pals) # for large colour palettes (similar to brewer)
library(plotly) # for interactive ggplots
library(crosstalk) # for shazred data
library(DT) # data table
library(htmltools) # for html input
library(lubridate) # for dates 
library(geosphere) # for directions between points 
library(shinydashboardPlus) # for collapsable boxes
library(data.table) # for melting
# library(shinyjqui) # for resizable/movable boxes


#######################
# load and clean data #
#######################

# input an input_data/data in the app when uploading
# load pre-processed data with epi-cluster cohesion info.
eccdata <- read_xlsx(here("input_data", "2021-05-03_0.Europe_1st wave.9000_Merged_strain_results.xlsx"))

# remove non-informative cols
eccdata <- eccdata[,-c(32:35)]

# rename columns
# remove spaces, all lowercase
colnames(eccdata) <- c("strain", "country", "province", "city", "strain_latitude", 
                       "strain_longitude", "day", "month", "year", "present_at_tp1", 
                       "tp1_cluster", "tp1_cluster_size_2", "tp1_t0_ecc_0.1.0", 
                       "tp1_t0_ecc_0.0.1", "present_at_tp2", "tp2_cluster", 
                       "tp2_cluster_size_2", "tp2_t0_ecc_0.1.0", 
                       "tp2_t0_ecc_0.0.1", "delta_ecc_0.1.0", "delta_ecc_0.0.1", 
                       "avg_tp1_date", "avg_tp1_temporal_dist_days", 
                       "avg_tp1_latitude", "avg_tp1_longitude", 
                       "avg_tp1_geo_dist_km", "avg_tp2_date", 
                       "avg_tp2_temporal_dist_days", "avg_tp2_latitude", 
                       "avg_tp2_longitude", "avg_tp2_geo_dist_km", 
                       "tp1_cluster_size", "tp2_cluster_size", 
                       "delta_cluster_size", "num_additional_tp1_strains_tp2", 
                       "num_novel_tp2_strains", "overall_cluster_growth_rate", "cluster_novel_growth_rate", 
                       "type")

# remove 0 for now as its causing problems
eccdata <- eccdata %>% subset(tp1_cluster != 0 )


#################################
# type convert selected columns #
#################################

# covert appropriate columns to numeric
eccdata[,c(5:10, 12:15, 17:21, 23:26, 28:39)] <- 
    sapply(eccdata[,c(5:10, 12:15, 17:21, 23:26, 28:39)], as.numeric)

# convert character columns and grouping vars to factor
# e.g. names, whether a strain is present at tp1
eccdata[,c(1:4, 10:11, 15:16)] <- 
    sapply(eccdata[,c(1:4, 10:11, 15:16)], as.factor)

# format date columns
eccdata$avg_tp1_date <- as.Date(eccdata$avg_tp1_date, format = "%d-%m-%y") 
eccdata$avg_tp2_date <- as.Date(eccdata$avg_tp2_date, format = "%d-%m-%y")


###########################################
# subset out cluster specific infomation  #
###########################################

# users of this app more interested in cluster info
# avg tp1 lng and lat is different for present_at_tp1 == 1 and present_at_tp1 == 1 
# avg tp2 lng and lat is the same for everything
# since we are interested in clusters
# take average tp1 lng and at of present_at_tp1 (only tp1 strains) 
# and use tp2 avg for tp2

# tp1 cluster data
clusters <- eccdata %>% 
    group_by(tp1_cluster) %>%
    subset(present_at_tp1==1) %>%
    summarise(avg_tp1_longitude = mean(avg_tp1_longitude),
              avg_tp1_latitude = mean(avg_tp1_latitude),
              tp1_cluster_size_2 = mean(tp1_cluster_size_2),
              tp1_t0_ecc_0.0.1 = mean(tp1_t0_ecc_0.0.1, na.rm=T),
              tp1_t0_ecc_0.1.0 = mean(tp1_t0_ecc_0.1.0, na.rm=T),
              avg_tp1_date = mean(avg_tp1_date),
              tp2_cluster = unique(tp2_cluster)[1],
              avg_tp2_longitude = mean(avg_tp2_longitude),
              avg_tp2_latitude = mean(avg_tp2_latitude),
              tp2_cluster_size_2 = mean(tp2_cluster_size_2),
              tp2_t0_ecc_0.1.0 = mean(tp2_t0_ecc_0.1.0),
              tp2_t0_ecc_0.0.1 = mean(tp2_t0_ecc_0.0.1), 
              delta_ecc_0.1.0 = mean(delta_ecc_0.1.0),
              delta_ecc_0.0.1 = mean(delta_ecc_0.0.1),
              avg_tp1_date = mean(avg_tp1_date),
              avg_tp1_temporal_dist_days = mean(avg_tp1_temporal_dist_days, na.rm=T),
              avg_tp1_geo_dist_km = mean(avg_tp1_geo_dist_km, na.rm=T),
              avg_tp2_date = mean(avg_tp2_date),
              avg_tp2_temporal_dist_days = mean(avg_tp2_temporal_dist_days),
              avg_tp2_geo_dist_km = mean(avg_tp2_geo_dist_km), 
              tp1_cluster_size = mean(tp1_cluster_size),
              tp2_cluster_size = mean(tp2_cluster_size),
              delta_cluster_size = mean(delta_cluster_size),
              num_additional_tp1_strains_tp2 = mean(num_additional_tp1_strains_tp2),
              num_novel_tp2_strains = mean(num_novel_tp2_strains),
              overall_cluster_growth_rate = mean(overall_cluster_growth_rate),
              cluster_novel_growth_rate = mean(cluster_novel_growth_rate))

# tp1 clusters summarized by novel tp2 strains
clusters_0 <- eccdata %>% 
    group_by(tp1_cluster) %>%
    subset(present_at_tp1==0) %>%
    summarise(avg_tp1_longitude = mean(avg_tp1_longitude),
              avg_tp1_latitude = mean(avg_tp1_latitude),
              tp1_cluster_size_2 = mean(tp1_cluster_size_2),
              tp1_t0_ecc_0.0.1 = mean(tp1_t0_ecc_0.0.1, na.rm=T),
              tp1_t0_ecc_0.1.0 = mean(tp1_t0_ecc_0.1.0, na.rm=T),
              avg_tp1_date = mean(avg_tp1_date),
              tp2_cluster = unique(tp2_cluster)[1],
              avg_tp2_longitude = mean(avg_tp2_longitude),
              avg_tp2_latitude = mean(avg_tp2_latitude),
              tp2_cluster_size_2 = mean(tp2_cluster_size_2),
              tp2_t0_ecc_0.1.0 = mean(tp2_t0_ecc_0.1.0),
              tp2_t0_ecc_0.0.1 = mean(tp2_t0_ecc_0.0.1), 
              delta_ecc_0.1.0 = mean(delta_ecc_0.1.0),
              delta_ecc_0.0.1 = mean(delta_ecc_0.0.1),
              avg_tp1_date = mean(avg_tp1_date),
              avg_tp1_temporal_dist_days = mean(avg_tp1_temporal_dist_days, na.rm=T),
              avg_tp1_geo_dist_km = mean(avg_tp1_geo_dist_km, na.rm=T),
              avg_tp2_date = mean(avg_tp2_date),
              avg_tp2_temporal_dist_days = mean(avg_tp2_temporal_dist_days),
              avg_tp2_geo_dist_km = mean(avg_tp2_geo_dist_km), 
              tp1_cluster_size = mean(tp1_cluster_size),
              tp2_cluster_size = mean(tp2_cluster_size),
              delta_cluster_size = mean(delta_cluster_size),
              num_additional_tp1_strains_tp2 = mean(num_additional_tp1_strains_tp2),
              num_novel_tp2_strains = mean(num_novel_tp2_strains),
              overall_cluster_growth_rate = mean(overall_cluster_growth_rate),
              cluster_novel_growth_rate = mean(cluster_novel_growth_rate))


################################################
# change in latitude and longitude of clusters #
################################################

# (North = 0 and 360, East = 90, Sout = 180, and West = 270  degrees).
# negative bearings means being measaured counterclockwise from north
# 8 wind compass rose each direction is 45° from the next. 
# 16 wind compass rose points at a 22+1⁄2° angle from its two neighbours

# get bearing (direction of change)
clusters$bearing <- bearing(as.matrix(clusters[,c("avg_tp1_longitude", "avg_tp1_latitude")]),
                            as.matrix(clusters[,c("avg_tp2_longitude", "avg_tp2_latitude")]))

# convert bearing to compass direction
# centre on directions 
clusters$cardinal <- sapply(clusters$bearing, function(x){
    
    if(x <= 11.25 & x >= 0) {return("N")}
    if(x > 11.25 & x <= 33.75) {return("NNE")}
    if(x > 33.75 & x <= 56.25) {return("NE")}
    if(x > 56.25 & x <= 78.75) {return("ENE")}
    if(x > 78.75 & x <= 101.25) {return("E")}
    if(x > 101.25 & x <= 123.75) {return("ESE")}
    if(x > 123.75 & x <= 146.25) {return("SE")}
    if(x > 146.25 & x <= 168.75) {return("SSE")}
    if(x > 168.5 & x <= 180) {return("S")}
    if(x < -168.5 & x >= -180) {return("S")}
    if(x < -146.25 & x >= -168.5) {return("SSW")}
    if(x < -123.75 & x >= -146.25) {return("SW")}
    if(x < -101.25 & x >= -123.75) {return("WSW")}
    if(x < -78.75 & x >= -101.25) {return("W")}
    if(x < -56.25 & x >= -78.75) {return("WNW")}
    if(x < -33.75 & x >= -56.25) {return("NW")}
    if(x < -11.25 & x >= -33.75) {return("NNW")}
    if(x < 0 & x >= -11.25) {return("N")}
})  

# convert to factor
clusters$cardinal <- as.factor(clusters$cardinal)

# set levels 
levels(clusters$cardinal) <- c("N", "NNE","NE", "ENE", "E", "ESE", "SE", "SSE", "S",  
                               "SSW", "SW", "WSW", "W", "WNW",  "NW","NNW")


##########################################
# subset out strain specific infomation  #
##########################################

# strain info could still be important
# subset strain info to a seprate data frame

strains <- eccdata %>%
    select(strain, country, province, city, year, month, day,
           strain_latitude, strain_longitude, present_at_tp1, 
           tp1_cluster, present_at_tp2, tp2_cluster)


#################################################
# create columns for jittered strain locations  #
#################################################

# create additional latitude and longitude for tp1 and tp2 strains and clusters
# 12-13 decimal places in lats and longs
# too long, round to four for our purposes

strains <- strains %>%
    mutate(strain_latitude_jit = round(jitter(as.numeric(strain_latitude),10,1),digits=4)) %>%
    mutate(strain_longitude_jit = round(jitter(as.numeric(strain_longitude),10,1),digits=4))

# make column for strain date instead of having three separate columns
strains$strain_date <- paste(strains$day, 
                             strains$month, 
                             strains$year, 
                             sep = "-")

strains$strain_date <- as.Date(strains$strain_date, format = "%d-%m-%y")

# create time difference for ridgeline plots 
strains <- strains %>% mutate(tp1_stain_time_diff = as.numeric(abs(as.Date("01-01-2020", format = "%d-%m-%y") - as.Date(strain_date, format = "%d-%m-%y"))))
strains <- strains %>% mutate(tp2_stain_time_diff = as.numeric(abs(as.Date("01-01-2020", format = "%d-%m-%y") - as.Date(strain_date, format = "%d-%m-%y"))))


##################
# crosstalk keys #
##################

# create unique row ids for shared cluster and strain data
clusters$key <- as.numeric(rownames(clusters))
strains$key <- as.numeric(rownames(strains))


#############################
# USER INTERFACE/CLINT SIDE #
#############################

header <- dashboardHeader(title = "ECC Vis")

# the dashboard side bar
sidebar <- dashboardSidebar(
    sidebarMenu(id = 'sidebarmenu',
                
                # Visualization tab
                menuItem("Visualizations", tabName = "visualizations"),
                
                # filtered data using in visualizations
                menuItem("Filtered data", tabName = "filteredData"),
                
                # raw data used to generate visualizations
                menuItem("Raw data", tabName = "rawData"))
)

# the dashboard body
body <- dashboardBody(
    tabItems(
        tabItem(tabName = "visualizations",
                
                # tab title
                h2("ECC Visualizations"),
                
                # row 1 box for map and bubble plot
                fluidRow(
                    # map
                    box(title = "Map plot", 
                        width = 6, 
                        leafletOutput("map"),
                        collapsible = TRUE,
                        sidebar = boxSidebar(
                            id = "mapsidebar",
                            title = "Map controls",
                            # cluster transparency 
                            sliderInput("centroid_transparency",
                                        "Cluster transparency:",
                                        min = 0,
                                        max = 100,
                                        step = 1,
                                        value = 50),
                            
                            # strain transparency
                            sliderInput("strain_transparency",
                                        "Strain transparency:",
                                        min = 0,
                                        max = 100,
                                        step = 1,
                                        value = 50),
                            
                            # legend checkbox for map 
                            checkboxInput("legend", "Show legend", TRUE))),
                    
                    #bubble plot
                    box(title = "Bubble plot", 
                        width = 6, 
                        plotlyOutput("bubbleplot"),
                        collapsible = TRUE)
                ),
                
                # row 2 for change vectors and ridgle plot
                fluidRow(
                    # change vector plot
                    box(title = "Change vector", 
                        width = 6,
                        plotlyOutput("change_vector", width = "100%", height = "100%"),
                        collapsible = TRUE), 
                    
                    # ridgeline plot 
                    box(title = "Ridgeline plot", 
                        width = 6, 
                        plotlyOutput("ridgeplot"),
                        collapsible = TRUE)
                ),
                
                # row 3 for cardinal cluster movement strain histogram
                fluidRow(
                    # cardinal movement of clusters 
                    box(title = "Cardinal movement of clusters", 
                        width = 6, 
                        plotlyOutput("cardinal_polar", width = "100%", height = "100%"),
                        collapsible = TRUE),
                    
                    #strain histogram 
                    box(title = "Strain histogram", 
                        width = 6,
                        plotlyOutput("strain_histogram", width = "100%", height = "100%"),
                        collapsible = TRUE)
                ),
                
                # row 4 for cluster data and filters
                fluidRow(
                    
                    # cluster filters 
                    box(title = "Cluster filters", 
                        width = 3,
                        
             
                        # delta filters
                        sliderInput(inputId = "delta_cluster_size", label = "Delta cluster size", min = min(clusters$delta_cluster_size), max = max(clusters$delta_cluster_size), value = c(min(clusters$delta_cluster_size), max(clusters$delta_cluster_size))),
                        sliderInput(inputId = "num_novel_tp2_strains", label = "Novel TP2 strains", min = min(clusters$num_novel_tp2_strains), max = max(clusters$num_novel_tp2_strains), value = c(min(clusters$num_novel_tp2_strains), max(clusters$num_novel_tp2_strains))),
                        sliderInput(inputId = "overall_cluster_growth_rate", label = "Overall growth rate", min = min(clusters$overall_cluster_growth_rate), max = max(clusters$overall_cluster_growth_rate), value = c(min(clusters$overall_cluster_growth_rate), max(clusters$overall_cluster_growth_rate))),
                        sliderInput(inputId = "cluster_novel_growth_rate", label = "Novel growth rate", min = min(clusters$delta_cluster_size), max = max(clusters$delta_cluster_size), value = c(min(clusters$delta_cluster_size), max(clusters$delta_cluster_size))),
                        sliderInput(inputId = "delta_ecc_0.0.1", label = "delta ecc_0.0.1", min = min(clusters$delta_ecc_0.0.1, na.rm = T), max = max(clusters$delta_ecc_0.0.1, na.rm = T), value = c(min(clusters$delta_ecc_0.0.1, na.rm = T), max(clusters$delta_ecc_0.0.1, na.rm = T))),
                        sliderInput(inputId = "delta_ecc_0.1.0", label = "delta ecc_0.1.0", min = min(clusters$delta_ecc_0.1.0, na.rm = T), max = max(clusters$delta_ecc_0.1.0, na.rm = T), value = c(min(clusters$delta_ecc_0.1.0, na.rm = T), max(clusters$delta_ecc_0.1.0, na.rm = T))),
                        selectizeInput("cardinal", "Avg cluster movement direction", unique(clusters$cardinal), selected = NULL, multiple = T),
                        
                        # make collapsible 
                        collapsible = TRUE),
                    
                    # cluster data
                    box(title = "Cluster data", 
                        width = 9, 
                        dataTableOutput("clusters_dt"),
                        #reactableOutput("clusters_dt"),
                        collapsible = TRUE)   
                ),
                
                # fluid row for strain data and filters
                fluidRow(
                    box(title = "Strain filters", 
                        width = 3,
                        
                        # strain filters
                        selectizeInput("country", "Country", unique(strains$country),  selected = NULL, multiple = T),
                        selectizeInput("province", "Province", unique(strains$province),  selected = NULL, multiple = T),
                        selectizeInput("present_at_tp1", "Present at TP1", unique(strains$present_at_tp1),  selected = NULL, multiple = T),
                        sliderInput(inputId = "strain_date", label = "Strain date", min = min(strains$strain_date), max = max(strains$strain_date), value = c(min(strains$strain_date), max(strains$strain_date)))
                    ),
                    
                    # strain data
                    box(title = "Strain data", 
                        width = 9, 
                        dataTableOutput("strains_dt"),
                        collapsible = TRUE)   
                    
                )
        ),
        
        # tab for filtered data
        tabItem(tabName = "filteredData",
                h2("Filtered data"),
                fluidRow(
                    box(width = 12, 
                        dataTableOutput("filtered_data"))
                )
        ),
        
        # table for raw data
        tabItem(tabName = "rawData",
                h2("Raw data"),
                fluidRow(
                    box(width = 12,
                        dataTableOutput("raw_data"))
                )
        )
    )
)

ui <- dashboardPage(header, sidebar, body)


###############
# SERVER SIDE #
###############

server <- function(input, output, session) { 
    
    ###########################################
    # filter cluster data based on user input #
    ###########################################
    
    # all cluster reactive filtering
    clusters_rall <- reactive({
        
        clusters %>% 
            
            # delta filters
            filter(delta_cluster_size >= input$delta_cluster_size[1] & delta_cluster_size <= input$delta_cluster_size[2]) %>%
            filter(num_novel_tp2_strains >= input$num_novel_tp2_strains[1] & num_novel_tp2_strains <= input$num_novel_tp2_strains[2]) %>%
            filter(overall_cluster_growth_rate >= input$overall_cluster_growth_rate[1] & overall_cluster_growth_rate <= input$overall_cluster_growth_rate[2]) %>%
            filter(cluster_novel_growth_rate >= input$cluster_novel_growth_rate[1] & cluster_novel_growth_rate <= input$cluster_novel_growth_rate[2]) %>%
            filter(delta_ecc_0.0.1 >= input$delta_ecc_0.0.1[1] & delta_ecc_0.0.1 <= input$delta_ecc_0.0.1[2]) %>%
            filter(delta_ecc_0.1.0 >= input$delta_ecc_0.1.0[1] & delta_ecc_0.1.0 <= input$delta_ecc_0.1.0[2]) %>%
            {if (!is.null(input$cardinal)) filter(., cardinal %in% input$cardinal)  else . }  
    })
    
    
    # all cluster reactive filtering
    eccdata_r <- reactive({
        
        eccdata %>%
            
            # delta filters
            filter(delta_cluster_size >= input$delta_cluster_size[1] & delta_cluster_size <= input$delta_cluster_size[2]) %>%
            filter(num_novel_tp2_strains >= input$num_novel_tp2_strains[1] & num_novel_tp2_strains <= input$num_novel_tp2_strains[2]) %>%
            filter(overall_cluster_growth_rate >= input$overall_cluster_growth_rate[1] & overall_cluster_growth_rate <= input$overall_cluster_growth_rate[2]) %>%
            filter(cluster_novel_growth_rate >= input$cluster_novel_growth_rate[1] & cluster_novel_growth_rate <= input$cluster_novel_growth_rate[2]) %>%
            filter(delta_ecc_0.0.1 >= input$delta_ecc_0.0.1[1] & delta_ecc_0.0.1 <= input$delta_ecc_0.0.1[2]) %>%
            filter(delta_ecc_0.1.0 >= input$delta_ecc_0.1.0[1] & delta_ecc_0.1.0 <= input$delta_ecc_0.1.0[2]) %>%
            {if (!is.null(input$cardinal)) filter(., cardinal %in% input$cardinal)  else . }  
    })
    
    # shared cluster data frames
    clusters_sh <- SharedData$new(clusters_rall, key = ~key, group = "clusters_long")
    
    
    ##########################################
    # filter strain data based on user input #
    ##########################################
    
    # strain reactive filtering
    strains_rall <- reactive({
        strains %>%
            {if (!is.null(input$country)) filter(., country %in% input$country)  else . } %>%
            {if (!is.null(input$province)) filter(., province %in% input$province)  else . } %>%
            {if (!is.null(input$present_at_tp1)) filter(., present_at_tp1 %in% input$province)  else . } %>%
            # filter by cluster info
            filter(tp1_cluster %in% clusters_rall()$tp1_cluster) 
    })
    
    # tp1 strain reactive filtering
    strains_r1 <- reactive({
        strains %>%
            {if (!is.null(input$country)) filter(., country %in% input$country)  else . } %>%
            {if (!is.null(input$province)) filter(., province %in% input$province)  else . } %>%
            {if (!is.null(input$present_at_tp1)) filter(., present_at_tp1 %in% input$province)  else . } %>%
            filter(present_at_tp1==1) %>%
            filter(tp1_cluster %in% clusters_rall()$tp1_cluster)
    })
    
    # tp2 strain reactive filtering
    strains_r2 <- reactive({
        strains %>%
            {if (!is.null(input$country)) filter(., country %in% input$country)  else . } %>%
            {if (!is.null(input$province)) filter(., province %in% input$province)  else . } %>%
            {if (!is.null(input$present_at_tp1)) filter(., present_at_tp1 %in% input$province)  else . } %>%
            filter(present_at_tp1==0) %>%
            filter(tp1_cluster %in% clusters_rall()$tp1_cluster) 
    })
    
    
    # #########################################################
    # # create shared data frames for cluster and strain info #
    # #########################################################
    
    # strain data
    strains_sh <- SharedData$new(strains_rall, key = ~key, group = "strains")
    strains_sh1 <- SharedData$new(strains_r1, key = ~key, group = "strains")
    strains_sh2 <- SharedData$new(strains_r2, key = ~key, group = "strains")
    
    ################################
    # cluster and strain DT tables #
    ################################
    
    # clusters
    output$clusters_dt <- renderDataTable({
        datatable(clusters_sh,
                  extensions = c('Select', 'Buttons','Scroller'),
                  options = list(
                      select = list(style = 'os', items = 'row'),
                      dom = 'Blfrtip',
                      rowId = 0,
                      buttons = list('selectRows', 'selectAll', 'selectNone', 'csv', 'excel'),
                      deferRender = TRUE,
                      scrollY = 500,
                      scrollX = 600,
                      scroller = TRUE),
                  selection = 'none')
    }, server=F)
    
    
    # strains
    output$strains_dt <- renderDataTable({
        datatable(data = strains_sh,
                  extensions = c('Select', 'Buttons','Scroller'),
                  options = list(
                      select = list(style = 'os', items = 'row'),
                      dom = 'Blfrtip',
                      rowId = 0,
                      buttons = list('selectRows', 'selectAll', 'selectNone', 'csv', 'excel'),
                      deferRender = TRUE,
                      scrollY = 500,
                      scrollX = 600,
                      scroller = TRUE),
                  selection = 'none',
        )
    }, server=F)
    
    
    ####################
    # base leaflet map #
    ####################
    
    # leaflet built in color function
    pal <-  colorFactor(
        palette = rep(cols25(n=25), 
                      length.out=length(unique(clusters$tp1_cluster))),
        domain = unique(clusters$tp1_cluster))
    
    # leaflet map
    output$map <- renderLeaflet({
        # leaflet map
        leaflet() %>%
            # load tile for selected region
            addTiles("http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}") %>%
            # set base zoom over mid asia/europe
            setView(lng = 60, lat = 50, zoom = 2) %>%
            # control layer for toggling strains and centroids on/off
            addLayersControl(overlayGroups = c("TP1 clusters", "TP2 clusters", "TP1 strains", "TP2 strains"),
                             options = layersControlOptions(collapsed = TRUE)) %>%
            # hide strains until user says otherwise
            hideGroup(c("TP1 strains", "TP2 strains")) %>%
            # tp1 cluster markers
            addCircleMarkers(data = clusters_sh,
                             lat = ~avg_tp1_latitude,
                             lng = ~avg_tp1_longitude,
                             radius = ~log10(tp1_cluster_size_2)*10,
                             fillColor = ~pal(tp1_cluster),
                             fillOpacity = input$centroid_transparency/100,
                             stroke = T,
                             opacity = input$centroid_transparency/100,
                             weight = 1,
                             color = ~pal(tp1_cluster),
                             group = "TP1 clusters") %>%
            
            addCircleMarkers(data = clusters_sh,
                             lat = ~avg_tp2_latitude,
                             lng = ~avg_tp2_longitude,
                             radius = ~log10(tp2_cluster_size_2)*10,
                             fillColor = ~pal(tp1_cluster),
                             fillOpacity = input$centroid_transparency/100,
                             stroke = T,
                             opacity = input$centroid_transparency/100,
                             weight = 1,
                             color = ~pal(tp1_cluster),
                             group = "TP2 clusters") %>%
 
        # tp1 strains
        addCircleMarkers(data = strains_sh1,
                         lat = ~strain_latitude_jit,
                         lng = ~strain_longitude_jit,
                         radius = 5,
                         fillColor = ~pal(tp1_cluster),
                         fillOpacity = input$strain_transparency/100,
                         stroke = T,
                         opacity = input$strain_transparency/100,
                         weight = 1,
                         color = "white",
                         group="TP1 strains") %>%
            
            # tp2 strains
            addCircleMarkers(data = strains_sh2,
                             lat = ~strain_latitude_jit,
                             lng = ~strain_longitude_jit,
                             radius = 5,
                             fillColor = ~pal(tp1_cluster),
                             fillOpacity = input$strain_transparency/100,
                             stroke = T,
                             opacity = input$strain_transparency/100,
                             weight = 1,
                             color = "black",
                             group="TP2 strains")
    })
    
    
    ###################
    # ridgeline plots #
    ###################
    
    output$ridgeplot <- renderPlotly({
        # base plot
        plot_ly(type = 'violin') %>%
            # tp2 traces
            add_trace(data = strains_sh1,
                      x = ~tp2_stain_time_diff,
                      y = ~tp1_cluster,
                      split = ~tp1_cluster,
                      type = 'violin',
                      side = 'positive',
                      orientation = "h",
                      scalemode = "count",
                      color = ~I(pal(tp1_cluster)),
                      name =  ~tp2_cluster,
                      points = "all",
                      line = list(color = "black",
                                  width = 1),
                      marker = list(opacity = 0.4,
                                    size = 10,
                                    line = list(color = "black",
                                                width = 1)),
                      hoveron = "violins+points") %>%
            # tp1 traces
            add_trace(data = strains_sh2,
                      x = ~tp1_stain_time_diff,
                      y = ~tp1_cluster,
                      split = ~tp1_cluster,
                      type = 'violin',
                      side = 'positive',
                      orientation = "h",
                      scalemode = "count",
                      color = ~I(pal(tp1_cluster)),
                      points = "all",
                      marker = list(opacity = 0.4,
                                    size = 10),
                      hoveron = "violins+points")
    })
    
    
    #############################
    # bubble plots for ECC data #
    #############################
    
    output$bubbleplot <- renderPlotly({
        plot_ly() %>% 
            #add tp1 trace
            add_trace(data = clusters_sh,
                      x = ~tp1_t0_ecc_0.0.1,
                      y = ~tp1_t0_ecc_0.1.0,
                      type = 'scatter',
                      mode = 'markers',
                      legendgroup = ~tp1_cluster,
                      color = ~tp1_cluster,
                      colors = ~pal(tp1_cluster),
                      name = ~tp1_cluster,
                      size = ~tp1_cluster_size_2,
                      sizes = c(1, 1000),
                      marker = list(sizemode = "area")) %>%
            #tp2 traces
            add_trace(data = clusters_sh,
                      x = ~tp2_t0_ecc_0.0.1,
                      y = ~tp2_t0_ecc_0.1.0,
                      type = 'scatter',
                      mode = 'markers',
                      legendgroup = ~tp1_cluster,
                      color = ~tp1_cluster,
                      colors = ~pal(tp1_cluster),
                      name = ~tp2_cluster, 
                      size = ~tp2_cluster_size_2,
                      sizes = c(1, 1000),
                      marker = list(sizemode = "area")) %>%
            highlight(on = "plotly_click",
                      off = "plotly_doubleclick")
        
    })
    
    
    ##############################
    # histogram for strain counts#
    ##############################
    
    output$strain_histogram <- renderPlotly({
        # strain count by country histogram
        plot_ly(type = "histogram",
                data = strains_sh,
                histfunc = "count",
                x = ~month,
                split = ~country,
                colors = "Set1",
                hovertemplate = ~paste('<b>', country, '</b><br>',
                                       'Count: %{y}', '<extra></extra>', sep=" ")) %>%
            layout(barmode = "stack")
    })
    
    
    ######################################
    # polar plot for change in direction #
    ######################################
    
    # color pal for polar plot
    polarpal <-  colorFactor(
        palette = ocean.phase(n=16),
        domain = unique(clusters$cardinal))
    
    # render polar plot
    output$cardinal_polar <- renderPlotly({
        # format data
        clusters_sh$data(withSelection = TRUE) %>%
            filter(selected_ | is.na(selected_)) %>%
            count(cardinal, .drop=FALSE) %>%
            mutate(theta = (as.numeric(rownames(.))-1) * 22.5,
                   direction = c("North", "North/Northeast", "Northeast",
                                 "East/Northeast", "East", "East/Southeast",
                                 "Southeast", "South/Southeast", "South",
                                 "South/Southwest", "Southwest",
                                 "West/Southwest", "West",
                                 "West/Northwest", "Northwest",
                                 "North/Northwest")) %>%
            # initialize plot
            plot_ly(type="barpolar",
                    r = ~n,
                    theta = ~theta,
                    opacity = 0.7,
                    name = ~cardinal,
                    hovertemplate = ~paste('<b>', direction, '</b><br>',
                                           'Count: ', n, '<br>',
                                           '<extra></extra>', sep=""),
                    marker = list(color = ~polarpal(cardinal),
                                  line = list(color="black"))) %>%
            layout(showlegend = F,
                   polar = list(
                       angularaxis = list(
                           rotation = 90,
                           direction = 'clockwise',
                           tickmode = 'array',
                           tickvals = c(0, 22.5, 45, 67.5, 90, 112.5, 135, 157.5,180,
                                        202.5, 225, 247.5, 270, 292.5, 315, 337.5),
                           ticktext = c("N", "NNE", "NE", "ENE", "E", "ESE", "SE",
                                        "SSE", "S", "SSW", "SW", "WSW", "W", "WNW",
                                        "NW", "NNW")))) %>%
            # highlight specifications
            highlight(on="plotly_click",
                      off="plotly_doubleclick")
    })
    
    

    
    ###########################################
    # filtered table to be displayed in a tab #
    ###########################################
    
    # filtered data
    output$filtered_data <- renderDataTable({
        datatable(eccdata_r(),
                  extensions = c('Buttons','Scroller'),
                  options = list(
                      dom = 'Blfrtip',
                      rowId = 0,
                      buttons = list('csv', 'excel'),
                      deferRender = TRUE,
                      scrollY = 500,
                      scrollX = 600,
                      scroller = TRUE),
                  selection = 'none')
    })
    
    
    #########################################
    # raw data to be displayed in a new tab #
    #########################################
    
    # input/raw data
    output$raw_data <- renderDataTable({
        datatable(eccdata,
                  extensions = c('Buttons','Scroller'),
                  options = list(
                      dom = 'Blfrtip',
                      rowId = 0,
                      buttons = list('csv', 'excel'),
                      deferRender = TRUE,
                      scrollY = 500,
                      scrollX = 600,
                      scroller = TRUE),
                  selection = 'none')
    })
    
    
    #######################
    # change vector graph #
    #######################
    
    output$change_vector <- renderPlotly({
        
        # change vector plot
        plot_ly(type = 'scatter',
                mode="lines+markers",
                data = clusters,
                x = c(0, ~delta_ecc_0.0.1),
                y = c(0, ~delta_ecc_0.1.0))
    })
    
    
}

shinyApp(ui, server)


######################################
# brushing problems that need fixing #
######################################

# when no filters or selections applied, can click a point on the bubble plot (or change vector plot) and the corresponding point
# will be brushed in the map, change vector plot, and data table. however, if filters/selections are applied
# the user is no longer able to do this

# if brushing is applied and then filters are applied, then removed, brushing seems to be slow
# can only remove brushing if double clicking on bubble or change vector plot

# clicking on a map cluster doesn't result in the brushing of other plots

# there is no brushing associated with the radial plot as it is aggregated data
# and has lost "key" information required for plotting 

# change vector plot only responds to brushing of time point 1 

# selecting a row in data table on applies brushing to map (and cardinal plot)

