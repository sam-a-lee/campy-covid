################ 
# General info #
################

# LONG FORMAT
# COVID VIS SHINY DASHBOARD
# July 06, 2021
# Samantha Lee

# The purpose of this script is to generate a map of epi-cluster cohesian data
# data plots data at time point 1 and time point 2 simulateanously 
# to show how the geographic centroid of clusters changes 


###############################
# load the required libraries #
###############################

library(shinydashboard) # the shiny dashboard
library(readxl) # for reading xlsx files 
library(shiny) # for running shiny apps
library(shinyWidgets) # for better slider/dropdown options
library(here) # for clear dir calling
library(tidyverse) # tidy data/ kind of used
library(pals) # for large colour palettes (similar to brewer)
library(plotly) # for interactive ggplots
library(crosstalk) # for shared data
library(DT) # data table
library(htmltools) # for html input
library(lubridate) # for dates 
library(geosphere) # for directions between points 
library(shinydashboardPlus) # for collapsable boxes
library(data.table) # for melting
library(rvest) # for downloading cardinal table 
# library(shinyjqui) # for resizable/movable boxes


############# 
# functions #
#############

angle <- function(x,y) { 
    z <- x + 1i * y
    res <- 90 - Arg(z) / pi * 180
    res %% 360
}

page <- read_html('http://snowfence.umn.edu/Components/winddirectionanddegreeswithouttable3.htm')
directions_raw <- page %>% html_node('td table') %>% html_table(header = TRUE)

directions <- directions_raw %>% 
    set_names(~tolower(sub(' Direction', '', .x))) %>% 
    slice(-1) %>% 
    separate(degree, c('degree_min', 'degree_max'), sep = '\\s+-\\s+', convert = TRUE)

# create a new column and assign a "direction"
# F = faster spread, S = slower reader, D = dispersed spread, C = concentrated spread
directions$ecc <- c("S", "SSC", "SC", "CSC",
                    "C", "CFC", "FC", "FFC",
                    "F", "FFD", "FD", "DFD", 
                    "D", "DSD", "SD", "SSD")




#############################
# USER INTERFACE/CLINT SIDE #
#############################

header <- dashboardHeader(title = "ECC Vis")

# the dashboard side bar
sidebar <- dashboardSidebar(
    sidebarMenu(id = 'sidebarmenu',
                
                # Cluster visualization tab
                menuItem("Cluster visualizations",tabName = "clusterVis"),
                
                # Strain visualization tab
                menuItem("Strain visualizations", tabName = "strainVis"),
                
                # Data exploration tab
                menuItem("Explore the data", tabName = "exploreData",
                         radioButtons("number", "Number of clusters",
                                      choices = list("5" = 5, "10" = 10,
                                                     "20" = 20, "All" = 99), selected = 10),
                         radioButtons("subsets", "Top 'n' data subsets",
                                      choices = list("Largest cluster size" = 1, "Largest delta geospatial ECC" = 2,
                                                     "Largest delta temporal ECC" = 3, "None" = 4), selected = 1),
                         radioButtons("region", "Regional granularity",
                                      choices = list("No faceting" = 1, "By country" = 2,
                                                     "By province" = 3), selected = 1)
                         # conditionalPanel(
                         #     condition = "input.region == 3",
                         #     selectizeInput("regionProvince", "View provinces in: ", unique(strains$country),
                         #                    selected = NULL, multiple = FALSE))
                ),
                
                # Data filters
                menuItem("Data filters", tabName = "dataFilters",
                         
                         # cluster filters
                         menuItem('Cluster filters', tabName = "clusterFilters",
                                  #cluster filters
                                  selectizeInput("tp1_cluster", "TP1 cluster", c(1,2,3,4,5),  selected = NULL, multiple = T),
                                  selectizeInput("timepoint", "Timepoint", c(1,2), selected = NULL, multiple = T),
                                  selectizeInput("type", "Type", c(1,2,3,4), selected = NULL, multiple = T),
                                  sliderInput(inputId = "cluster_size_2", label = "Cluster size", min = 0, max = 10000, value = c(1, 10000)),
                                  sliderInput(inputId = "avg_date", label = "Avg date", min = as.Date("2020-01-01"), max = as.Date("2025-01-01"), value = c(as.Date("2020-01-01"), as.Date("2025-01-01"))),
                                  sliderInput(inputId = "ecc_0.0.1", label = "ecc_0.0.1", min = 0, max = 1, value = c(0, 1)),
                                  sliderInput(inputId = "ecc_0.1.0", label = "ecc_0.1.0", min = 0, max = 1, value = c(0, 1)),
                                  sliderInput(inputId = "avg_latitude", label = "Avg latitude", min = -90, max = 90, value = c(-90, 90)),
                                  sliderInput(inputId = "avg_longitude", label = "Avg longitude", min = -180, max = 180, value = c(-180, 180)),
                                  # delta filters
                                  sliderInput(inputId = "delta_cluster_size", label = "Delta cluster size", min = -10000, max = 10000, value = c(-10000, 10000)),
                                  sliderInput(inputId = "num_novel_tp2_strains", label = "Novel TP2 strains", min = 0, max = 10000, value = c(0, 10000)),
                                  sliderInput(inputId = "cluster_novel_growth_rate", label = "Novel growth rate", min = 0, max = 100, value = c(0, 100)),
                                  sliderInput(inputId = "overall_cluster_growth_rate", label = "Overall growth rate", min = 0, max = 100, value = c(0, 100)),
                                  sliderInput(inputId = "delta_ecc_0.0.1", label = "delta ecc_0.0.1", min = -1, max = 1, value = c(-1,1)),
                                  sliderInput(inputId = "delta_ecc_0.1.0", label = "delta ecc_0.1.0", min = -1, max = 1, value = c(-1,1))
                         ),
                         
                         #strain filters
                         menuItem('Strain filters', tabName = "strainFilters",
                                  selectizeInput("country", "Country", c("China", "Italy", "France"),  selected = NULL, multiple = T),
                                  selectizeInput("province", "Province", c("China", "Italy", "France"),  selected = NULL, multiple = T)
                         )
                ),
                
                # Cluster data tab
                menuItem("Cluster data", tabName = "clusterData"),
                
                # Strain data tab
                menuItem("Strain data", tabName = "strainData"),
                
                # filtered data used in visualizations in wide format
                menuItem("Filtered raw data", tabName = "filteredData"),
                
                # raw data used to generate visualizations
                menuItem("Raw data", tabName = "rawData"),
                
                # upload user data
                menuItem("Upload data", tabName = "userData", selected = TRUE))
)

# the dashboard body
body <- dashboardBody(
    tabItems(
        tabItem(tabName = "clusterVis",
                # tab title
                h2("Cluster visualizations"),
                
                # row 1 for map of clusters and cardinal movement
                fluidRow(
                    box(title = HTML("<b>", "Cluster map", "</b>"),
                        width = 12,
                        # map
                        plotlyOutput("cluster_map"),
                        collapsible = TRUE,
                        sidebar = boxSidebar(
                            id = "mapsidebar",
                            title = "Map controls",
                            # cluster transparency 
                            sliderInput("cluster_transparency",
                                        "Cluster transparency:",
                                        min = 0,
                                        max = 100,
                                        step = 1,
                                        value = 50),
                            # legend checkbox for map 
                            checkboxInput("legend", "Show legend", TRUE))),
                    collapsible = T),
                
                fluidRow(
                    box(title = HTML("<b>", "Cluster movement", "</b>"),
                        width = 12,
                        
                        # cardinal movement of clusters 
                        box(title = "Cardinal movement of clusters",
                            solidHeader=TRUE,
                            width = 6, 
                            plotlyOutput("cardinal_polar", width = "100%", height = "100%")),
                        
                        # ecc descriptive spread
                        box(title = "ECC description of cluster spread", 
                            solidHeader=TRUE, 
                            width = 6, 
                            plotlyOutput("ecc_radar", width = "100%", height = "100%"))
                    )
                ),
                
                # row 2 for bubble plot and change vector plot 
                fluidRow(
                    box(title = HTML("<b>", "ECC value plots", "</b>"),
                        width = 12,
                        #bubble plot
                        box(title = "Bubble plot", 
                            width = 6, 
                            solidHeader=TRUE,
                            plotlyOutput("bubbleplot", width = "100%", height = "100%")),
                        # change vector plot
                        box(title = "Change vector", 
                            width = 6,
                            solidHeader=TRUE,
                            plotlyOutput("change_vector", width = "100%", height = "100%")),
                        collapsible = TRUE
                    )
                ),
                
                # row 3 for histograms
                fluidRow(
                    box(title = HTML("<b>", "Geospatial and temporal ECC indices", "</b>"),
                        width = 12,
                        # geo ecc histogram by time point
                        box(title = "Geospatial ECC histogram", 
                            width = 6, 
                            solidHeader=TRUE,
                            plotlyOutput("geo_histogram")),
                        # temp ecc histogram by time point
                        box(title = "Temporal ECC histogram", 
                            width = 6, 
                            solidHeader=TRUE,
                            plotlyOutput("temp_histogram")),
                        collapsible = TRUE
                    )
                ), 
                
                # row 4 for delta histograms
                fluidRow(
                    box(title = HTML("<b>", "Delta geospatial and temporal ECC indices", "</b>"),
                        width = 12,
                        # delta geo ecc histogram 
                        box(title = "Delta geospatial ECC", 
                            width = 6, 
                            solidHeader=TRUE,
                            plotlyOutput("delta_geo_histogram")),
                        # delta temp ecc histogram 
                        box(title = "Delta temporal ECC ", 
                            width = 6, 
                            solidHeader=TRUE,
                            plotlyOutput("delta_temp_histogram")),
                        collapsible = TRUE
                    )
                ),
                
                # fluid row 5
                fluidRow(
                    # single vs multi strain cluster histogram 
                    box(title = HTML("<b>", "Number of single and multistrain clusters by date",  "</b>"),
                        width = 12,
                        box(width = 6,
                            title = "Time point 1",
                            solidHeader = TRUE, 
                            plotlyOutput("singlemultclust_tp1", width = "100%", height = "100%")),
                        box(width = 6,
                            solidHeader = TRUE, 
                            title = "Time point 2",
                            plotlyOutput("singlemultclust_tp2", width = "100%", height = "100%")),
                        collapsible = TRUE)
                )
        ),
        
        
        # strain visualizations 
        tabItem(tabName = "strainVis",
                h2("Strain visualizations"),
                
                # row 1 for map
                fluidRow(
                    # map
                    box(title = HTML("<b>", "Strain map",  "</b>"),
                        width = 12, 
                        plotlyOutput("strain_map"),
                        collapsible = TRUE,
                        sidebar = boxSidebar(
                            id = "mapsidebar",
                            title = "Map controls",
                            # strain transparency
                            sliderInput("strain_transparency",
                                        "Strain transparency:",
                                        min = 0,
                                        max = 100,
                                        step = 1,
                                        value = 50),
                            # legend checkbox for map 
                            checkboxInput("legend", "Show legend", TRUE)))
                ),
                
                # row 2 for strain ecc data 
                fluidRow(
                    box(title = HTML("<b>", "Geospatial and temporal ECC indices", "</b>"),
                        width = 12,
                        
                        # geo ecc histogram by time point
                        box(title = "Geospatial ECC histogram", 
                            width = 6, 
                            solidHeader = TRUE, 
                            plotlyOutput("strain_geo_histogram")),
                        
                        # temp ecc histogram by time point
                        box(title = "Temporal ECC histogram", 
                            width = 6, 
                            solidHeader = TRUE, 
                            plotlyOutput("strain_temp_histogram")),
                        collapsible = TRUE
                    )
                ), 
                
                # row 2 for delta strain ecc data 
                fluidRow(
                    box(title = HTML("<b>", "Delta geospatial and temporal ECC indices", "</b>"),
                        width = 12,
                        # geo ecc histogram by time point
                        box(title = "Delta geospatial ECC histogram", 
                            width = 6, 
                            solidHeader = TRUE, 
                            plotlyOutput("strain_delta_geo_histogram")),
                        # temp ecc histogram by time point
                        box(title = "Delta temporal ECC histogram", 
                            width = 6, 
                            solidHeader = TRUE, 
                            plotlyOutput("strain_delta_temp_histogram")),
                        collapsible = TRUE
                    )
                ), 
                
                # row 4 ridgeline and cumulative count
                fluidRow( 
                    # strain ridgeline plot (denisty by date)
                    box(title = HTML("<b>", "Cluster strain identification density by date",  "</b>"),
                        width = 12, 
                        plotlyOutput("ridgeplot", width = "100%", height = "100%"),
                        collapsible = TRUE)
                ),
                
                # fluid row 5
                fluidRow(
                    # single vs multi strain histogram 
                    box(title = HTML("<b>", "Number of strains single part of single and multi strain clusters",  "</b>"),
                        width = 12,
                        box(width = 6,
                            title = "Time point 1",
                            solidHeader = TRUE, 
                            plotlyOutput("straintypes_tp1", width = "100%", height = "100%")),
                        box(width = 6,
                            solidHeader = TRUE, 
                            title = "Time point 2",
                            plotlyOutput("straintypes_tp2", width = "100%", height = "100%")),
                        collapsible = TRUE)
                ),
                
                # fluid row 6
                fluidRow(
                    #strain histogram 
                    box(title = HTML("<b>", "Number of new strains identified per month",  "</b>"),
                        width = 12,
                        box(width = 6,
                            title = "Time point 1",
                            solidHeader = TRUE, 
                            plotlyOutput("strain_histogram_tp1", width = "100%", height = "100%")),
                        box(width = 6,
                            solidHeader = TRUE, 
                            title = "Time point 2",
                            plotlyOutput("strain_histogram_tp2", width = "100%", height = "100%")),
                        collapsible = TRUE)
                ),
                
                # row 7
                fluidRow(
                    # cumulative strain identification by time point 
                    box(title = HTML("<b>", "Cumulative strain identification by cluster ",  "</b>"),
                        width = 12, 
                        box(width = 6, 
                            solidHeader = TRUE,
                            title = "Time point 1",
                            plotlyOutput("cum_strains_tp1")),
                        box(width = 6, 
                            solidHeader = TRUE, 
                            title = "Time point 2",
                            plotlyOutput("cum_strains_tp2")),
                        collapsible = TRUE)
                )
        ),
        
        # tab for filtered data
        tabItem(tabName = "filteredData",
                h2("Filtered data"),
                fluidRow(
                    box(width = 12, 
                        title = "Filtered raw data",
                        dataTableOutput("filtered_data"))
                )
        ),
        
        # table for raw data
        tabItem(tabName = "rawData",
                h2("Raw data"),
                fluidRow(
                    box(width = 12,
                        title = "Unfiltered raw data",
                        dataTableOutput("raw_data"))
                )
        ),
        
        # table for cluster data
        tabItem(tabName = "clusterData",
                h2("Cluster data"),
                fluidRow(
                    box(width = 12,
                        title = "Filtered cluster data used for plotting", 
                        dataTableOutput("clusters_dt"))
                )
        ),
        
        # table for strain data
        tabItem(tabName = "strainData",
                h2("Strain data"),
                fluidRow(
                    box(width = 12,
                        title = "Filtered strain data used for plotting", 
                        dataTableOutput("strains_dt"))
                )
        ),
        
        # user input data
        tabItem(tabName = "userData",
                
                fluidRow(
                    box(width = 12,
                        title = "Select file for upload",
                        # prompt to upload file 
                        fileInput("userFile", "Choose file",
                                  multiple = TRUE,
                                  accept = c("text/csv/xlsx",
                                             "text/comma-separated-values,text/plain",
                                             ".csv", ".txt", ".xlsx")),
                        # check box for header
                        checkboxInput("header", "Header", TRUE),
                        
                        # check box for separator 
                        radioButtons("separator", "Separator",
                                     inline = TRUE,
                                     choices = c(Comma = ",",
                                                 Semicolon = ";",
                                                 Tab = "\t"),
                                     selected = ","),
                        # check box for quotes, if applicable
                        radioButtons("quote", "Quote",
                                     inline = TRUE,
                                     choices = c(None = "",
                                                 "Double Quote" = '"',
                                                 "Single Quote" = "'"),
                                     selected = "")
                    )
                )
        )
    )
)

ui <- dashboardPage(header, sidebar, body)


##########
# server #
##########

server <- function(input, output, session) { 
    
    
    #########################
    # get data to visualize #
    #########################
    
    # create reactive values
    vals <- reactiveValues(data_raw = NULL, data_proc = NULL, clusters = NULL, strains = NULL)
    
    # read data frame into reactive values
    observeEvent(input$userFile, {
        vals$data_raw <- read_xlsx(input$userFile$datapath)
    }, ignoreNULL = T)
    
    
    # process data
    observeEvent(vals$data_raw, {
        
        print("new data!")
        
        eccdata <- vals$data_raw
        
        print("ecc data class")
        print(class(eccdata))
        
        print("col class")
        print(lapply(eccdata, class))
        
        
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
        
        vals$data_proc <- eccdata
        
        
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
                      cluster_novel_growth_rate = mean(cluster_novel_growth_rate),
                      type = mean(type))
        
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
        
        # convert to factor
        clusters$cardinal <- as.factor(clusters$cardinal)
        
        # set levels 
        levels(clusters$cardinal) <- c("N", "NNE","NE", "ENE", "E", "ESE", "SE", "SSE", "S",  
                                       "SSW", "SW", "WSW", "W", "WNW",  "NW","NNW")
        
        # calculate the angle based that delta ECCs form
        clusters <- clusters %>% 
            mutate(ecc_angle = angle(delta_ecc_0.1.0, delta_ecc_0.0.1))
        
        # use new column to assign directions
        clusters <- clusters %>%
            mutate(ecc_direction = cut(ecc_angle, breaks = c(0, directions$degree_max, 360), labels = c(directions$ecc, 'F')))
        
        # convert to factor
        clusters$ecc_direction <- as.factor(clusters$ecc_direction)
        
        # set levels 
        levels(clusters$ecc_direction) <-  c("S", "SSC", "SC", "CSC",
                                             "C", "CFC", "FC", "FFC",
                                             "F", "FFD", "FD", "DFD", 
                                             "D", "DSD", "SD", "SSD")
        # convert to dataframe
        clusters <- as.data.frame(clusters)
        
        # select only complete cases
        clusters_cc <- na.omit(clusters)
        
        # now reshape the data for tp1 and tp2 info
        clusters_long <- data.table::melt(setDT(clusters_cc), 
                                          measure.vars=list(c("avg_tp1_longitude", "avg_tp2_longitude"), 
                                                            c("avg_tp1_latitude", "avg_tp2_latitude"),
                                                            c("tp1_t0_ecc_0.0.1", "tp2_t0_ecc_0.0.1"),
                                                            c("tp1_t0_ecc_0.1.0", "tp2_t0_ecc_0.1.0"),
                                                            c("avg_tp1_date", "avg_tp2_date"),
                                                            c("tp1_cluster_size_2", "tp2_cluster_size_2"),
                                                            c("avg_tp1_temporal_dist_days", "avg_tp2_temporal_dist_days"),
                                                            c("avg_tp1_geo_dist_km", "avg_tp2_geo_dist_km"),
                                                            c("tp1_cluster_size", "tp2_cluster_size")),
                                          variable.name='timepoint', value.name=c('avg_longitude', 'avg_latitude', 
                                                                                  "ecc_0.0.1", "ecc_0.1.0",
                                                                                  "avg_date", "cluster_size_2", 
                                                                                  "avg_temporal_dist", "avg_geo_dist",
                                                                                  "cluster_size"))
        
        # add in pop text for clusters in map
        clusters_long$maptext <-
            paste0('<strong>', clusters_long$tp1_cluster, '</strong>','<br/>',
                   'Timepoint: ', '<strong>', clusters_long$timepoint, '</strong>', '<br/>',
                   'Cluster size: ', clusters_long$cluster_size_2, '<br/>',
                   'Temporal ECC: ', clusters_long$ecc_0.0.1, '<br/>',
                   'Geospatial ECC: ', clusters_long$ecc_0.1.0, '<br/>') %>%
            lapply(htmltools::HTML) 
        
        clusters_long$key <- as.numeric(rownames(clusters_long))
        
        vals$clusters <- clusters_long
        
        
        strains <- eccdata %>%
            select(strain, country, province, city, year, month, day,
                   strain_latitude, strain_longitude, present_at_tp1, 
                   tp1_cluster,tp1_cluster_size_2, present_at_tp2, 
                   tp2_cluster, tp2_cluster_size_2, tp1_t0_ecc_0.1.0, 
                   tp2_t0_ecc_0.1.0, tp1_t0_ecc_0.0.1, tp2_t0_ecc_0.0.1,
                   delta_ecc_0.1.0, delta_ecc_0.0.1, type)
        
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
        strains <- strains %>% mutate(stain_time_diff = as.numeric(abs(as.Date("01-01-2020", format = "%d-%m-%y") - as.Date(strain_date, format = "%d-%m-%y"))))
        
        # match timepoint annotation in clusters long
        strains$timepoint <- ifelse(strains$present_at_tp1==1, 1, 2)
        
        # hard code text
        strains$maptext <-
            paste0('<strong>', strains$strain, '</strong>','<br/>',
                   'Cluster: ', '<strong>', strains$tp1_cluster, '</strong>', '<br/>',
                   'Timepoint: ', '<strong>', strains$timepoint, '</strong>', '<br/>',
                   'Country: ', strains$country, '<br/>',
                   'Province: ', strains$province, '<br/>',
                   'Date: ', strains$strain_date, '<br/>') %>%
            lapply(htmltools::HTML) 
        
        # create unique row ids for shared cluster and strain data
        strains$key <- as.numeric(rownames(strains))
        
        vals$strains <- strains
        
        
        # update cluster sliders
        updateSelectInput(inputId = "tp1_cluster", label = "TP1 cluster", choices = unique(vals$clusters$tp1_cluster),  selected = NULL)
        updateSelectInput(inputId = "timepoint", label = "Timepoint", choices = unique(vals$clusters$timepoint), selected = NULL)
        updateSelectInput(inputId = "type", label = "Type", choices = unique(vals$clusters$type),  selected = NULL)
        updateSliderInput(inputId = "cluster_size_2", label = "Cluster size", min = min(vals$clusters$cluster_size_2), max =  max(vals$clusters$cluster_size_2), value = c(min(vals$clusters$cluster_size_2),  max(vals$clusters$cluster_size_2)))
        updateSliderInput(inputId = "avg_date", label = "Avg date", min = min(vals$clusters$avg_date), max = max(vals$clusters$avg_date), value = c(min(vals$clusters$avg_date), max(vals$clusters$avg_date)))
        updateSliderInput(inputId = "ecc_0.0.1", label = "ecc_0.0.1", min = min(vals$clusters$ecc_0.0.1, na.rm = T), max = max(vals$clusters$ecc_0.0.1, na.rm = T), value = c(min(vals$clusters$ecc_0.0.1, na.rm = T), max(vals$clusters$ecc_0.0.1, na.rm = T)))
        updateSliderInput(inputId = "ecc_0.1.0", label = "ecc_0.1.0", min = min(vals$clusters$ecc_0.1.0, na.rm = T), max = max(vals$clusters$ecc_0.1.0, na.rm = T), value = c(min(vals$clusters$ecc_0.1.0, na.rm = T), max(vals$clusters$ecc_0.1.0, na.rm = T)))
        updateSliderInput(inputId = "avg_latitude", label = "Avg latitude", min = min(vals$clusters$avg_latitude), max = max(vals$clusters$avg_latitude), value = c(min(vals$clusters$avg_latitude), max(vals$clusters$avg_latitude)))
        updateSliderInput(inputId = "avg_longitude", label = "Avg longitude", min = min(vals$clusters$avg_longitude), max = max(vals$clusters$avg_longitude), value = c(min(vals$clusters$avg_longitude), max(vals$clusters$avg_longitude)))
        # update cluster delta sliders 
        sliderInput(inputId = "delta_cluster_size", label = "Delta cluster size", min = min(vals$clusters$delta_cluster_size), max = max(vals$clusters$delta_cluster_size), value = c(min(vals$clusters$delta_cluster_size), max(vals$clusters$delta_cluster_size)))
        sliderInput(inputId = "num_novel_tp2_strains", label = "Novel TP2 strains", min = min(vals$clusters$num_novel_tp2_strains), max = max(vals$clusters$num_novel_tp2_strains), value = c(min(vals$clusters$num_novel_tp2_strains), max(vals$clusters$num_novel_tp2_strains)))
        sliderInput(inputId = "overall_cluster_growth_rate", label = "Overall growth rate", min = min(vals$clusters$overall_cluster_growth_rate), max = max(vals$clusters$overall_cluster_growth_rate), value = c(min(vals$clusters$overall_cluster_growth_rate), max(vals$clusters$overall_cluster_growth_rate)))
        sliderInput(inputId = "cluster_novel_growth_rate", label = "Novel growth rate", min = min(vals$clusters$delta_cluster_size), max = max(vals$clusters$delta_cluster_size), value = c(min(vals$clusters$delta_cluster_size), max(vals$clusters$delta_cluster_size)))
        sliderInput(inputId = "delta_ecc_0.0.1", label = "delta ecc_0.0.1", min = min(vals$clusters$delta_ecc_0.0.1, na.rm = T), max = max(vals$clusters$delta_ecc_0.0.1, na.rm = T), value = c(min(vals$clusters$delta_ecc_0.0.1, na.rm = T), max(vals$clusters$delta_ecc_0.0.1, na.rm = T)))
        sliderInput(inputId = "delta_ecc_0.1.0", label = "delta ecc_0.1.0", min = min(vals$clusters$delta_ecc_0.1.0, na.rm = T), max = max(vals$clusters$delta_ecc_0.1.0, na.rm = T), value = c(min(vals$clusters$delta_ecc_0.1.0, na.rm = T), max(vals$clusters$delta_ecc_0.1.0, na.rm = T)))
        #update stain filters
        # strain filters
        updateSelectInput(inputId = "country", label = "Country", choices = unique(vals$strains$country),  selected = NULL)
        updateSelectInput(inputId = "province", label = "Province", choices = unique(vals$strains$province),  selected = NULL)
    })
    
    
    ###########################
    # reactive data filtering #
    ###########################
    
    clusters_r <- reactive({
        
        req(vals$clusters)
        data <- vals$clusters 
        
        # get top n
        topn <- {if (input$number != "all") as.numeric(input$number) else nrow(clusters_long)}
        
        filtered <- data %>%
            {if (!is.null(input$tp1_cluster)) filter(., tp1_cluster %in% input$tp1_cluster)  else . } %>%
            {if (!is.null(input$timepoint)) filter(., timepoint %in% input$timepoint)  else . } %>%
            {if (!is.null(input$type)) filter(., type %in% input$type)  else . } %>%
            filter(cluster_size_2 >= input$cluster_size_2[1]+1 & cluster_size_2 <= input$cluster_size_2[2]+1 ) %>%
            filter(avg_date >= input$avg_date[1]) %>% filter(avg_date <= input$avg_date[2]) %>%
            filter(ecc_0.0.1 >= input$ecc_0.0.1[1] & ecc_0.0.1 <= input$ecc_0.0.1[2]) %>%
            filter(ecc_0.1.0 >= input$ecc_0.1.0[1] & ecc_0.1.0 <= input$ecc_0.1.0[2]) %>%
            filter(avg_latitude >= input$avg_latitude[1] & avg_latitude <= input$avg_latitude[2]) %>%
            filter(avg_longitude >= input$avg_longitude[1] & avg_longitude <= input$avg_longitude[2])  %>%
            # delta filters
            filter(delta_cluster_size >= input$delta_cluster_size[1] & delta_cluster_size <= input$delta_cluster_size[2]) %>%
            filter(num_novel_tp2_strains >= input$num_novel_tp2_strains[1] & num_novel_tp2_strains <= input$num_novel_tp2_strains[2]) %>%
            filter(overall_cluster_growth_rate >= input$overall_cluster_growth_rate[1] & overall_cluster_growth_rate <= input$overall_cluster_growth_rate[2]) %>%
            filter(cluster_novel_growth_rate >= input$cluster_novel_growth_rate[1] & cluster_novel_growth_rate <= input$cluster_novel_growth_rate[2]) %>%
            filter(delta_ecc_0.0.1 >= input$delta_ecc_0.0.1[1] & delta_ecc_0.0.1 <= input$delta_ecc_0.0.1[2]) %>%
            filter(delta_ecc_0.1.0 >= input$delta_ecc_0.1.0[1] & delta_ecc_0.1.0 <= input$delta_ecc_0.1.0[2])%>%
            
            # data subsetting 
            {if (input$subsets==1) arrange(., desc(abs(cluster_size_2))) else . }  %>%
            {if (input$subsets==2) arrange(., desc(abs(delta_ecc_0.0.1))) else . }  %>%
            {if (input$subsets==3) arrange(., desc(abs(delta_ecc_0.1.0))) else . } 
        
        # grab n rows according to input
        if (input$number != 99){
            topn <- as.numeric(input$number)
            unique_clust <- head(unique(filtered$tp1_cluster),topn)
            return(filtered %>% subset(tp1_cluster %in% unique_clust))
        } else{  
            return(filtered)
        }
    })
    
    # cluster shared data frame 
    clusters_sh <- SharedData$new(clusters_r, key = ~key, group = "clusters")
    
    # all strain filtering 
    strains_r <- reactive({
        req(vals$strains,clusters_sh)
        vals$strains %>%
            {if (!is.null(input$country)) filter(., country %in% input$country)  else . } %>%
            {if (!is.null(input$province)) filter(., province %in% input$province)  else . } %>%
            # filter by shared cluster selections
            filter(tp1_cluster %in% (clusters_sh$data(withSelection = TRUE) %>% filter(selected_ | is.na(selected_)) %>% pull(tp1_cluster)))
    })
    
    # tp1 strain reactive filtering
    strains_r1 <- reactive({
        req(vals$strains,clusters_sh)
        vals$strains %>%
            {if (!is.null(input$country)) filter(., country %in% input$country)  else . } %>%
            {if (!is.null(input$province)) filter(., province %in% input$province)  else . } %>%
            filter(present_at_tp1==1) %>%
            filter(tp1_cluster %in% (clusters_sh$data(withSelection = TRUE) %>% filter(selected_ | is.na(selected_)) %>% pull(tp1_cluster)))
    })
    
    # tp2 strain reactive filtering
    strains_r2 <- reactive({
        req(vals$strains,clusters_sh)
        vals$strains %>%
            {if (!is.null(input$country)) filter(., country %in% input$country)  else . } %>%
            {if (!is.null(input$province)) filter(., province %in% input$province)  else . } %>%
            filter(present_at_tp1==0) %>%
            filter(tp1_cluster %in% (clusters_sh$data(withSelection = TRUE) %>% filter(selected_ | is.na(selected_)) %>% pull(tp1_cluster)))
    })
    
    # strain shared data
    strains_sh <- SharedData$new(strains_r, key = ~key, group = "strains")
    strains_sh1 <- SharedData$new(strains_r1, key = ~key, group = "strains")
    strains_sh2 <- SharedData$new(strains_r2, key = ~key, group = "strains")
    
    
    #################################
    # data frames displayed in tabs #
    #################################
    
    # clusters
    output$clusters_dt <- renderDataTable({
        datatable(clusters_sh,
                  extensions = c('Select', 'Buttons','Scroller'),
                  #extension = 'Scroller', 
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
    
    # filtered data table
    output$filtered_data <- renderDataTable({
        datatable(vals$data_proc,
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
    
    # raw data table
    output$raw_data <- renderDataTable({
        datatable(vals$data_raw,
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
    
    
    ################################
    # cluster based visualizations #
    ################################
    
    # mapbox token
    mapboxToken <- "pk.eyJ1Ijoic2FtLWEtbGVlIiwiYSI6ImNrb2s0bXVpbzFhMGkybm5zeHI1dHR1aTgifQ.1K8o7OaSOWo_y5UdVH348w"    # You need your own token
    Sys.setenv("MAPBOX_TOKEN" = mapboxToken)
    
    # cluster map
    # plotly using mapbox
    output$cluster_map <- renderPlotly({
        
        # leaflet built in color function
        pal <-  leaflet::colorFactor(
            palette = rep(cols25(n=25),
                          length.out=length(unique(vals$clusters$tp1_cluster))),
            domain = unique(vals$clusters$tp1_cluster))
        
        plot_mapbox(mode = 'scattermapbox') %>%
            add_markers(data = clusters_sh,
                        x = ~avg_longitude,
                        y = ~avg_latitude,
                        legendgroup = ~tp1_cluster,
                        color= ~I(pal(tp1_cluster)),
                        name = ~paste(tp1_cluster, timepoint, sep="_"),
                        marker =list(size = ~log10(cluster_size_2)*10,
                                     opacity = 0.5),
                        hovertext = ~maptext) %>%
            config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN")) %>%
            layout(mapbox = list(zoom =1.5,
                                 center = list(lon = 80, lat = 40)))
    })
    
    # polar plot describing cardinal direction of change
    output$cardinal_polar <- renderPlotly({
        
        # color pal for polar plot
        polarpal <-  leaflet::colorFactor(
            palette = ocean.phase(n=16),
            domain = unique(vals$clusters$cardinal))
        
        # format data
        clusters_sh$data(withSelection = TRUE) %>%
            filter(selected_ | is.na(selected_)) %>%
            distinct(tp1_cluster, .keep_all = T) %>%
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
                                        "NW", "NNW"))))
    })
    
    
    # radio plot for ecc explanation of cluster spread
    output$ecc_radar <- renderPlotly({
        
        clusters_sh$data(withSelection = T) %>%
            filter(selected_ | is.na(selected_)) %>% 
            distinct(tp1_cluster, .keep_all = T) %>%
            count(ecc_direction, .drop=FALSE) %>%
            mutate(theta = (as.numeric(rownames(.))-1) * 22.5,
                   direction = c("Slower spread", "Slower spread/Slower spread, more concentrated", 
                                 "Slower spread,  more concentrated",
                                 "More concentrated/Slower spread, more concentrated", 
                                 "More concentrated", 
                                 "More concentrated/Faster spread, more concentrated",
                                 "Faster spread, more concentrated", 
                                 "Faster spread/Faster spread, more concentrated ", 
                                 "Faster spread",
                                 # FIX BELOW
                                 "Faster spread/Faster spread, more disperse", 
                                 "Slower spread, more disperse",
                                 "More disperse/Faster spread, more disperse", 
                                 "More disperse",
                                 "More disperse/Slower spread, more disperse", 
                                 "Slower spread, more disperse",
                                 "Slower spread/Slower spread, more concentrated"))  %>%
            # initialize plot
            plot_ly(type="scatterpolar",
                    r = ~n, 
                    theta = ~theta,
                    fill = 'toself') %>%
            layout(showlegend = F,
                   margin = list(l = 100, r = 100),
                   polar = list(
                       angularaxis = list(
                           rotation = 90,
                           direction = 'clockwise',
                           tickmode = 'array',
                           tickvals = c(0, 45,  90, 135, 180,
                                        225, 270, 315),
                           ticktext = c("Slower spread", 
                                        HTML(paste("Slower spread,", "<br>", "more concentrated")), 
                                        HTML(paste("More ", "<br>", "concentrated", sep="")), 
                                        HTML(paste("Faster spread,", "<br>", "more concentrated")),
                                        "Faster spread",   
                                        HTML(paste("Faster spread,", "<br>", "more disperse")),
                                        HTML(paste("More ", "<br>", "disperse", sep="")),
                                        HTML(paste("Slower spread,", "<br>", "more disperse"))))))
    })
    
    
    # change vector plot describing delta ECC value changes between tp1 and tp2
    # these values are translated into human-interpretable changes in radio plot 
    output$change_vector <- renderPlotly({
        
        # leaflet built in color function
        pal <-  leaflet::colorFactor(
            palette = rep(cols25(n=25),
                          length.out=length(unique(vals$clusters$tp1_cluster))),
            domain = unique(vals$clusters$tp1_cluster))
        
        op <- clusters_sh$data(withSelection = T)
        op$opacity <- ifelse(op$selected_ | is.na(op$selected_), 1, 0.1)
        
        plot_ly(type = "scatter", mode="markers") %>% 
            #add segment to connect each point to origin
            add_trace(data = clusters_sh,
                      x = ~delta_ecc_0.1.0, # geographical
                      y = ~delta_ecc_0.0.1, # temporal 
                      legendgroup = ~tp1_cluster,
                      opacity = 1,
                      size=I(0), 
                      name = ~paste(tp1_cluster, timepoint, sep="_"),
                      color = ~I(pal(tp1_cluster)),
                      hovertemplate = ~paste('<b>', tp1_cluster, '</b><br>',
                                             'Delta temporal ECC: ', delta_ecc_0.0.1, '<br>',
                                             'Delta geospatial ECC: ', delta_ecc_0.1.0, '<extra></extra>', sep="")) %>%
            # set range and overlay arrow annotations
            layout(xaxis = list(range = c(-1, 1.5), 
                                title = "Delta geospatial ECC value"),
                   yaxis = list(range = c(-1, 1.5),
                                title = "Delta temporal ECC value"),
                   annotations = list(ax = 0, ay = 0,
                                      axref='x', ayref='y',
                                      x = ~delta_ecc_0.1.0, # geospatial
                                      y = ~delta_ecc_0.0.1, # temporal
                                      opacity = I(op$opacity),
                                      name = ~paste(tp1_cluster, timepoint, sep="_"),
                                      arrowcolor = ~I(pal(tp1_cluster)),
                                      xref='x', yref='y', text = "")) %>%
            # add inset plot for average vector
            add_trace(data = clusters_sh,
                      x = ~sum(delta_ecc_0.1.0), # geospatial
                      y = ~sum(delta_ecc_0.0.1), # temporal
                      name = "Avg change vector",
                      xaxis='x2',
                      yaxis='y2',
                      opacity = 0.3,
                      size=I(0.1),
                      #inherit = FALSE,
                      color = I("black"),
                      hovertemplate = ~paste('<b>', 'Average change vector', '</b><br>',
                                             'Avg delta geospatial ECC: ', sum(delta_ecc_0.1.0), '<br>',
                                             'Avg delta temporal ECC: ', sum(delta_ecc_0.0.1), '<extra></extra>', sep=""))  %>%
            #add arrow to inset
            layout(annotations = list(ax = 0, ay = 0,
                                      axref='x2', ayref='y2',
                                      x = ~sum(delta_ecc_0.1.0), # sum geospatial
                                      y = ~sum(delta_ecc_0.0.1), # sum temporal
                                      arrowcolor = I("black"),
                                      arrowwidth = I(4),
                                      arrowsize = I(0.6),
                                      xref='x2', yref='y2',
                                      text = "")) %>%
            
            # specify the domain (size) and range of inset plot
            layout(yaxis2 = list(domain = c(0.75, 0.95),
                                 # temporal
                                 range = ~c(-abs(sum(delta_ecc_0.0.1)) - 0.1*abs(sum(delta_ecc_0.0.1)),
                                            abs(sum(delta_ecc_0.0.1)) + 0.1*abs(sum(delta_ecc_0.0.1))),
                                 anchor='x2'),
                   xaxis2 = list(domain = c(0.75, 0.95),
                                 # geospatial
                                 range = ~c(-abs(sum(delta_ecc_0.1.0)) - 0.1*abs(sum(delta_ecc_0.1.0)),
                                            abs(sum(delta_ecc_0.1.0)) + 0.1*abs(sum(delta_ecc_0.1.0))),
                                 anchor='y2'))
    })
    
    # bubble plots for temporal vs ecc values at tp1 and tp2
    # area of bubbles is proportional to size of cluster
    output$bubbleplot <- renderPlotly({
        
        # leaflet built in color function
        pal <-  leaflet::colorFactor(
            palette = rep(cols25(n=25),
                          length.out=length(unique(vals$clusters$tp1_cluster))),
            domain = unique(vals$clusters$tp1_cluster))
        
        if(input$region == 3) {
            # clusters faceting by province 
            strains_sh1_bub <- strains_sh1$data() %>%
                subset(country == input$regionProvince) %>% 
                group_by(tp1_cluster, province, timepoint) %>%
                summarize(size = sum(!is.na(strain)),
                          tp1_temp_ecc = mean(tp1_t0_ecc_0.0.1),
                          tp1_geo_ecc = mean(tp1_t0_ecc_0.1.0),
                          tp2_temp_ecc = mean(tp2_t0_ecc_0.0.1),
                          tp2_geo_ecc = mean(tp2_t0_ecc_0.1.0))
            
            strains_sh2_bub <- strains_sh2$data() %>%
                subset(country == input$regionProvince) %>% 
                group_by(tp1_cluster, province,  timepoint) %>%
                summarize(size = sum(!is.na(strain)),
                          tp1_temp_ecc = mean(tp1_t0_ecc_0.0.1),
                          tp1_geo_ecc = mean(tp1_t0_ecc_0.1.0),
                          tp2_temp_ecc = mean(tp2_t0_ecc_0.0.1),
                          tp2_geo_ecc = mean(tp2_t0_ecc_0.1.0))
            
            ggplotly(ggplot() +
                         geom_point(data = strains_sh1_bub, 
                                    aes(y=tp1_temp_ecc, x=tp1_geo_ecc , fill  = tp1_cluster, col = tp1_cluster, size = size), 
                                    alpha = 0.7) +
                         geom_point(data = strains_sh2_bub, 
                                    aes(y=tp2_temp_ecc, x=tp2_geo_ecc , fill  = tp1_cluster, col = tp1_cluster, size = size), 
                                    alpha = 0.7) +
                         scale_fill_manual(values =  pal(vals$clusters$tp1_cluster)) +
                         scale_color_manual(values =  pal(vals$clusters$tp1_cluster)) +
                         scale_size_area() + 
                         xlim(0,1) +
                         ylim(0,1) +
                         labs(x="Geospatial ECC value", y = "Temporal ECC value") +
                         facet_wrap(~province) + 
                         theme_bw() +
                         theme(panel.border = element_blank())) %>%
                layout(margin = list(l = 100, r = 100))
            
            
        } else if (input$region == 2) {
            
            # clusters faceting by country 
            strains_sh1_bub <- strains_sh1$data() %>%
                group_by(tp1_cluster, country, timepoint) %>%
                summarize(size = sum(!is.na(strain)),
                          tp1_temp_ecc = mean(tp1_t0_ecc_0.0.1),
                          tp1_geo_ecc = mean(tp1_t0_ecc_0.1.0),
                          tp2_temp_ecc = mean(tp2_t0_ecc_0.0.1),
                          tp2_geo_ecc = mean(tp2_t0_ecc_0.1.0))
            
            strains_sh2_bub <- strains_sh2$data() %>%
                group_by(tp1_cluster, country,  timepoint) %>%
                summarize(size = sum(!is.na(strain)),
                          tp1_temp_ecc = mean(tp1_t0_ecc_0.0.1),
                          tp1_geo_ecc = mean(tp1_t0_ecc_0.1.0),
                          tp2_temp_ecc = mean(tp2_t0_ecc_0.0.1),
                          tp2_geo_ecc = mean(tp2_t0_ecc_0.1.0))
            
            ggplotly(ggplot() +
                         geom_point(data = strains_sh1_bub, 
                                    aes(y=tp1_temp_ecc, x=tp1_geo_ecc , fill  = tp1_cluster, col = tp1_cluster, size = size), 
                                    alpha = 0.7) +
                         geom_point(data = strains_sh2_bub, 
                                    aes(y=tp2_temp_ecc, x=tp2_geo_ecc , fill  = tp1_cluster, col = tp1_cluster, size = size), 
                                    alpha = 0.7) +
                         scale_fill_manual(values =  pal(vals$clusters$tp1_cluster)) +
                         scale_color_manual(values =  pal(vals$clusters$tp1_cluster)) +
                         scale_size_area() + 
                         xlim(0,1) +
                         ylim(0,1) +
                         labs(x="Geospatial ECC value", y = "Temporal ECC value") +
                         facet_wrap(~country) + 
                         theme_bw() +
                         theme(panel.border = element_blank())) %>%
                layout(margin = list(l = 100, r = 100))
            
            
        } else { 
            
            # clusters no faceting 
            ggplotly(ggplot(clusters_sh, aes(y=ecc_0.0.1, x=ecc_0.1.0 , fill  = tp1_cluster, col = tp1_cluster)) +
                         geom_point(aes(size = cluster_size_2), alpha = 0.7) +
                         scale_fill_manual(values = pal(vals$clusters$tp1_cluster)) +
                         scale_color_manual(values = pal(vals$clusters$tp1_cluster)) +
                         scale_size_area() + 
                         xlim(0,1) +
                         ylim(0,1) + 
                         labs(x="Geospatial ECC value", y = "Temporal ECC value") +
                         theme_bw() +
                         theme(panel.border = element_blank())) %>%
                layout(margin = list(l = 50, r = 50, t = 50, b = 50),
                       width = 500, height = 450)
        }
    })
    
    
    # geo ecc histogram by time point
    output$geo_histogram <- renderPlotly({
        plot_ly(type = "histogram",
                data = clusters_sh,
                histfunc = "count",
                x = ~ecc_0.0.1,
                xbins = list(size = 0.05),
                color = ~timepoint) %>%
            layout(barmode = "group",
                   xaxis = list(range = c(0, 1),
                                title = "Temporal ECC value"),
                   yaxis = list(title = "Cluster count"))
        
    })
    
    # temp ecc histogram by time point
    output$temp_histogram <- renderPlotly({
        plot_ly(type = "histogram",
                data = clusters_sh,
                histfunc = "count",
                x = ~ecc_0.1.0,
                xbins = list(size = 0.05),
                color = ~timepoint) %>%
            layout(barmode = "group",
                   xaxis = list(range = c(0, 1),
                                title = "Geospatial ECC value"),
                   yaxis = list(title = "Cluster count"))
        
        
    })
    
    # delta geo histogram
    output$delta_geo_histogram <- renderPlotly({
        plot_ly(type = "histogram",
                data = clusters_sh,
                histfunc = "count",
                xbins = list(size = 0.05),
                x = ~delta_ecc_0.0.1) %>%
            layout(barmode = "group",
                   xaxis = list(range = c(-1, 1),
                                title = "Delta geospatial ECC value"),
                   yaxis = list(title = "Cluster count"))
        
    })
    
    #delta temp histogram 
    output$delta_temp_histogram <- renderPlotly({
        plot_ly(type = "histogram",
                data = clusters_sh,
                histfunc = "count",
                xbins = list(size = 0.05),
                x = ~delta_ecc_0.1.0) %>%
            layout(barmode = "group",
                   xaxis = list(range = c(-1, 1),
                                title = "Delta temporal ECC value"),
                   yaxis = list(title = "Cluster count"))
    })
    
    #delta temp histogram 
    output$delta_size_histogram <- renderPlotly({
        plot_ly(type = "histogram",
                xbins = list(size = 10),
                data = clusters_sh,
                histfunc = "count",
                x = ~delta_cluster_size) %>%
            layout(barmode = "overlay")
    })
    
    
    # single vs multistrain cluster prevalence at tp1
    output$singlemultclust_tp1 <- renderPlotly({
        
        plot_ly(type = "histogram",
                data = clusters_sh %>%
                    mutate(homo_het = ifelse(cluster_size_2>2,"multi","single")),
                histfunc = "count",
                x = ~avg_date,
                color = ~homo_het,
                xbins = list(size = "M1")) %>%
            layout(barmode = "beside",
                   xaxis = list(title = "Date"),
                   yaxis = list(title = "Number strains"),
                   updatemenus = list( list(
                       active = -1,
                       x= -0.25,
                       type = 'buttons',
                       buttons = list(
                           list(
                               label = "By day",
                               method = "restyle",
                               args = list(list(xbins = list(size = 86400000.0)))),
                           list(
                               label = "By week",
                               method = "restyle",
                               args = list(list(xbins = list(size = 604800000.0)))),
                           list(
                               label = "By month",
                               method = "restyle",
                               args = list(list(xbins = list(size = "M1"))))
                       ))))
    })
    
    
    # single vs multistrain cluster prevalence at tp1
    output$singlemultclust_tp2 <- renderPlotly({
        
        plot_ly(type = "histogram",
                data = clusters_sh$data(withSelection = TRUE)%>%
                    filter(selected_ | is.na(selected_)) %>%
                    subset(timepoint==2) %>%
                    mutate(homo_het = ifelse(cluster_size_2>2,"multi","single")),
                histfunc = "count",
                x = ~avg_date,
                color = ~homo_het,
                xbins = list(size = "M1")) %>%
            layout(barmode = "beside",
                   xaxis = list(title = "Date"),
                   yaxis = list(title = "Number strains"),
                   updatemenus = list( list(
                       active = -1,
                       x= -0.25,
                       type = 'buttons',
                       buttons = list(
                           list(
                               label = "By day",
                               method = "restyle",
                               args = list(list(xbins = list(size = 86400000.0)))),
                           list(
                               label = "By week",
                               method = "restyle",
                               args = list(list(xbins = list(size = 604800000.0)))),
                           list(
                               label = "By month",
                               method = "restyle",
                               args = list(list(xbins = list(size = "M1"))))
                       ))))
    })
    
    
    ###############################
    # strain based visualizations #
    ###############################
    
    # plotly map for strains using mapbox
    output$strain_map <- renderPlotly({
        
        # leaflet built in color function
        pal <-  leaflet::colorFactor(
            palette = rep(cols25(n=25),
                          length.out=length(unique(vals$clusters$tp1_cluster))),
            domain = unique(vals$clusters$tp1_cluster))
        
        plot_mapbox(mode = 'scattermapbox') %>%
            # strain markers
            add_markers(data = strains_sh,
                        x = ~strain_longitude_jit,
                        y = ~strain_latitude_jit,
                        name = ~tp1_cluster,
                        color= ~I(pal(tp1_cluster)),
                        marker =list(size = 10,
                                     opacity = 0.5),
                        hovertext = ~maptext) %>%
            config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN")) %>%
            layout(mapbox = list(zoom =1.5,
                                 center = list(lon = 80, lat = 40)))
    })
    
    # ridgeline plot
    output$ridgeplot <- renderPlotly({
        
        # leaflet built in color function
        pal <-  leaflet::colorFactor(
            palette = rep(cols25(n=25),
                          length.out=length(unique(vals$clusters$tp1_cluster))),
            domain = unique(vals$clusters$tp1_cluster))
        
        if(input$region == 3) {
            
            plot_ly(type = 'violin') %>%
                # tp1 traces
                add_trace(data = strains_sh1$data(withSelection=T) %>% 
                              filter(selected_ | is.na(selected_)) %>%
                              subset(country %in% input$regionProvince),
                          y = ~province,
                          x = ~strain_date,
                          legendgroup = ~tp1_cluster,
                          name =  ~paste(tp1_cluster,1, sep="_"),
                          type = 'violin',
                          side = 'positive',
                          orientation = "h",
                          scalemode = "count",
                          color = ~I(pal(tp1_cluster)),
                          points = "all",
                          line = list(width = 1), 
                          marker = list(opacity = 0.4,
                                        size = 8),
                          hoveron = "violins+points") %>%
                add_trace(data = strains_sh2$data(withSelection=T) %>% 
                              filter(selected_ | is.na(selected_)) %>%
                              subset(country %in% input$regionProvince),
                          y = ~province,
                          x = ~strain_date,
                          legendgroup = ~tp1_cluster,
                          name =  ~paste(tp1_cluster,2, sep="_"),
                          color = ~I(pal(tp1_cluster)),
                          type = 'violin',
                          side = 'positive',
                          orientation = "h",
                          scalemode = "count",
                          points = "all",
                          line = list(color = "black",
                                      width = 1),
                          marker = list(opacity = 0.4,
                                        size = 8,
                                        line = list(color = "black",
                                                    width = 1)),
                          hoveron = "violins+points") %>%
                layout(height = 800,
                       xaxis = list(title = "Date of strain identification"),
                       updatemenus = list( list(
                           active = -1,
                           x= -0.1,
                           type = 'buttons',
                           buttons = list(
                               list(
                                   label = "Show points",
                                   method = "restyle",
                                   args = list(list(points = "all"))),
                               list(
                                   label = "Remove points",
                                   method = "restyle",
                                   args = list(list(points = FALSE)))
                           )))) %>%
                highlight(on= "plotly_selected",
                          off="plotly_deselect")      
        } else if (input$region == 2) {
            
            plot_ly(type = 'violin') %>%
                # tp1 traces
                add_trace(data = strains_sh1, 
                          y = ~country,
                          x = ~strain_date,
                          legendgroup = ~tp1_cluster,
                          name =  ~paste(tp1_cluster,1, sep="_"),
                          type = 'violin',
                          side = 'positive',
                          orientation = "h",
                          scalemode = "count",
                          color = ~I(pal(tp1_cluster)),
                          points = "all",
                          line = list(width = 1), 
                          marker = list(opacity = 0.4,
                                        size = 8),
                          hoveron = "violins+points") %>%
                add_trace(data = strains_sh2, 
                          y = ~country,
                          x = ~strain_date,
                          legendgroup = ~tp1_cluster,
                          name =  ~paste(tp1_cluster,2, sep="_"),
                          color = ~I(pal(tp1_cluster)),
                          type = 'violin',
                          side = 'positive',
                          orientation = "h",
                          scalemode = "count",
                          points = "all",
                          line = list(color = "black",
                                      width = 1),
                          marker = list(opacity = 0.4,
                                        size = 8,
                                        line = list(color = "black",
                                                    width = 1)),
                          hoveron = "violins+points") %>%
                layout(height = 800,
                       xaxis = list(title = "Date of strain identification"),
                       updatemenus = list( list(
                           active = -1,
                           x= -0.1,
                           type = 'buttons',
                           buttons = list(
                               list(
                                   label = "Show points",
                                   method = "restyle",
                                   args = list(list(points = "all"))),
                               list(
                                   label = "Remove points",
                                   method = "restyle",
                                   args = list(list(points = FALSE)))
                           )))) %>%
                highlight(on= "plotly_selected",
                          off="plotly_deselect")
        } else { 
            
            plot_ly(type = 'violin') %>%
                # tp1 traces
                add_trace(data = strains_sh1,
                          y = ~tp1_cluster,
                          x = ~strain_date,
                          legendgroup = ~tp1_cluster,
                          name =  ~paste(tp1_cluster,1, sep="_"),
                          type = 'violin',
                          side = 'positive',
                          orientation = "h",
                          scalemode = "count",
                          color = ~I(pal(tp1_cluster)),
                          points = "all",
                          line = list(width = 1), 
                          marker = list(opacity = 0.4,
                                        size = 8),
                          hoveron = "violins+points") %>%
                add_trace(data = strains_sh2, 
                          y = ~tp1_cluster,
                          x = ~strain_date,
                          legendgroup = ~tp1_cluster,
                          name =  ~paste(tp1_cluster,2, sep="_"),
                          color = ~I(pal(tp1_cluster)),
                          type = 'violin',
                          side = 'positive',
                          orientation = "h",
                          scalemode = "count", 
                          points = "all",
                          line = list(color = "black",
                                      width = 1),
                          marker = list(opacity = 0.4,
                                        size = 8,
                                        line = list(color = "black",
                                                    width = 1)),
                          hoveron = "violins+points") %>%
                layout(height = 800,
                       xaxis = list(title = "Date of strain identification"),
                       updatemenus = list( list(
                           active = -1,
                           x= -0.15,
                           type = 'buttons',
                           buttons = list(
                               list(
                                   label = "Show points",
                                   method = "restyle",
                                   args = list(list(points = "all"))),
                               list(
                                   label = "Remove points",
                                   method = "restyle",
                                   args = list(list(points = FALSE)))
                           )))) %>%
                highlight(on= "plotly_selected",
                          off="plotly_deselect") 
        }
    })
    
    
    # strain geo ecc histogram by time point
    output$strain_geo_histogram <- renderPlotly({
        
        if(input$region == 3) {
            ggplotly(ggplot(strains_sh$data(withSelection=T) %>%
                                filter(selected_ | is.na(selected_)) %>%
                                subset(country %in% input$regionProvince), 
                            aes(x=tp1_t0_ecc_0.1.0, fill=as.factor(timepoint))) +
                         geom_histogram(position="dodge", bins = 20) +
                         #geom_density(alpha=0.5) +
                         facet_wrap(~province) +
                         scale_fill_manual(values = c("#1c74b4", "#fc7c0d"),
                                           name = "Time point") +
                         xlim(0,1) +
                         theme_bw() +
                         theme(axis.text.x = element_text(angle=45, hjust=1),
                               axis.title = element_blank(),
                               panel.border = element_blank(),
                               panel.grid.major.x = element_blank(),
                               panel.spacing.y = unit(2, "lines"))) %>%
                layout(margin = list(l = 55, r = 100)) %>%
                add_annotations(text = "Geospatial ECC value",
                                y = 0,
                                x=0.5, 
                                yshift=-60,
                                yref = "paper",
                                xref = "paper",
                                xanchor = "middle",
                                yanchor = "bottom",
                                showarrow = FALSE,
                                font = list(size = 14)) %>%
                add_annotations(text = "Number of strains",
                                y = 0.5,
                                x=0, 
                                xshift = -60,
                                yref = "paper",
                                xref = "paper",
                                xanchor = "left",
                                yanchor = "middle",
                                textangle = -90,
                                showarrow = FALSE,
                                font = list(size = 14)) 
            
        } else if (input$region == 2) {
            ggplotly(ggplot(strains_sh, 
                            aes(x=tp1_t0_ecc_0.1.0, fill=as.factor(timepoint))) +
                         geom_histogram(position="dodge", bins = 20) +
                         scale_fill_manual(values = c("#1c74b4", "#fc7c0d"),
                                           name = "Time point") +
                         facet_wrap(~country) +
                         xlim(0,1) +
                         theme_bw() +
                         theme(axis.text.x = element_text(angle=45, hjust=1),
                               axis.title = element_blank(),
                               panel.border = element_blank(),
                               panel.grid.major.x = element_blank(),
                               panel.spacing.y = unit(2, "lines"))) %>%
                layout(margin = list(l = 60, r = 100)) %>%
                add_annotations(text = "Geospatial ECC value",
                                y = 0,
                                x=0.5, 
                                yshift=-60,
                                yref = "paper",
                                xref = "paper",
                                xanchor = "middle",
                                yanchor = "bottom",
                                showarrow = FALSE,
                                font = list(size = 14)) %>%
                add_annotations(text = "Number of strains",
                                y = 0.5,
                                x=0, 
                                xshift = -60,
                                yref = "paper",
                                xref = "paper",
                                xanchor = "left",
                                yanchor = "middle",
                                textangle = -90,
                                showarrow = FALSE,
                                font = list(size = 14)) 
        } else { 
            ggplotly(ggplot(strains_sh, 
                            aes(x=tp1_t0_ecc_0.1.0, fill=as.factor(timepoint))) +
                         geom_histogram(position="dodge", bins = 20) +
                         scale_fill_manual(values = c("#1c74b4", "#fc7c0d"),
                                           name = "Time point") +
                         xlim(0,1) +
                         theme_bw() +
                         theme(axis.text.x = element_text(angle=45, hjust=1),
                               axis.title = element_blank(),
                               panel.border = element_blank(),
                               panel.grid.major.x = element_blank())) %>%
                layout(margin = list(l = 60, r = 100)) %>%
                add_annotations(text = "Geospatial ECC value",
                                y = 0,
                                x=0.5, 
                                yshift=-60,
                                yref = "paper",
                                xref = "paper",
                                xanchor = "middle",
                                yanchor = "bottom",
                                showarrow = FALSE,
                                font = list(size = 14)) %>%
                add_annotations(text = "Number of strains",
                                y = 0.5,
                                x=0, 
                                xshift = -60,
                                yref = "paper",
                                xref = "paper",
                                xanchor = "left",
                                yanchor = "middle",
                                textangle = -90,
                                showarrow = FALSE,
                                font = list(size = 14)) 
            
        }
    })
    
    # strain temp histogram by time point
    output$strain_temp_histogram <- renderPlotly({
        
        if(input$region == 3) {
            ggplotly(ggplot(strains_sh$data(withSelection=T) %>%
                                filter(selected_ | is.na(selected_)) %>%
                                subset(country %in% input$regionProvince), 
                            aes(x=tp1_t0_ecc_0.0.1, fill=as.factor(timepoint))) +
                         geom_histogram(position="dodge", bins = 20) +
                         #geom_density(alpha=0.5) +
                         facet_wrap(~province) +
                         scale_fill_manual(values = c("#1c74b4", "#fc7c0d"),
                                           name = "Time point") +
                         xlim(0,1) +
                         theme_bw() +
                         theme(axis.text.x = element_text(angle=45, hjust=1),
                               axis.title = element_blank(),
                               panel.border = element_blank(),
                               panel.grid.major.x = element_blank(),
                               panel.spacing.y = unit(2, "lines"))) %>%
                layout(margin = list(l = 50, r = 100)) %>%
                add_annotations(text = "Temporal ECC value",
                                y = 0,
                                x=0.5, 
                                yshift=-60,
                                yref = "paper",
                                xref = "paper",
                                xanchor = "middle",
                                yanchor = "bottom",
                                showarrow = FALSE,
                                font = list(size = 14)) %>%
                add_annotations(text = "Number of strains",
                                y = 0.5,
                                x=0, 
                                xshift = -55,
                                yref = "paper",
                                xref = "paper",
                                xanchor = "left",
                                yanchor = "middle",
                                textangle = -90,
                                showarrow = FALSE,
                                font = list(size = 14)) 
            
        } else if (input$region == 2) {
            ggplotly(ggplot(strains_sh, 
                            aes(x=tp1_t0_ecc_0.0.1, fill=as.factor(timepoint))) +
                         geom_histogram(position="dodge", bins = 20) +
                         scale_fill_manual(values = c("#1c74b4", "#fc7c0d"),
                                           name = "Time point") +
                         facet_wrap(~country) +
                         xlim(0,1) +
                         theme_bw() +
                         theme(axis.text.x = element_text(angle=45, hjust=1),
                               axis.title = element_blank(),
                               panel.border = element_blank(),
                               panel.grid.major.x = element_blank(),
                               panel.spacing.y = unit(2, "lines"))) %>%
                layout(margin = list(l = 50, r = 100)) %>%
                add_annotations(text = "Temporal ECC value",
                                y = 0,
                                x=0.5, 
                                yshift=-60,
                                yref = "paper",
                                xref = "paper",
                                xanchor = "middle",
                                yanchor = "bottom",
                                showarrow = FALSE,
                                font = list(size = 14)) %>%
                add_annotations(text = "Number of strains",
                                y = 0.5,
                                x=0, 
                                xshift = -55,
                                yref = "paper",
                                xref = "paper",
                                xanchor = "left",
                                yanchor = "middle",
                                textangle = -90,
                                showarrow = FALSE,
                                font = list(size = 14)) 
        } else { 
            ggplotly(ggplot(strains_sh, 
                            aes(x=tp1_t0_ecc_0.0.1, fill=as.factor(timepoint))) +
                         geom_histogram(position="dodge", bins = 20) +
                         scale_fill_manual(values = c("#1c74b4", "#fc7c0d"),
                                           name = "Time point") +
                         xlim(0,1) +
                         theme_bw() +
                         theme(axis.text.x = element_text(angle=45, hjust=1),
                               axis.title = element_blank(),
                               panel.border = element_blank(),
                               panel.grid.major.x = element_blank())) %>%
                layout(margin = list(l = 50, r = 100)) %>%
                add_annotations(text = "Temporal ECC value",
                                y = 0,
                                x=0.5, 
                                yshift=-60,
                                yref = "paper",
                                xref = "paper",
                                xanchor = "middle",
                                yanchor = "bottom",
                                showarrow = FALSE,
                                font = list(size = 14)) %>%
                add_annotations(text = "Number of strains",
                                y = 0.5,
                                x=0, 
                                xshift = -55,
                                yref = "paper",
                                xref = "paper",
                                xanchor = "left",
                                yanchor = "middle",
                                textangle = -90,
                                showarrow = FALSE,
                                font = list(size = 14)) 
        }
    })
    
    
    # strain delta geo ecc histogram by time point
    output$strain_delta_geo_histogram <- renderPlotly({
        
        if(input$region == 3) {
            ggplotly(ggplot(strains_sh$data(withSelection=T) %>%
                                filter(selected_ | is.na(selected_)) %>%
                                subset(country %in% input$regionProvince),
                            aes(x=delta_ecc_0.1.0)) +
                         geom_histogram(position="dodge", bins = 40, fill = "#1c74b4") +
                         facet_wrap(~province) +
                         xlim(-1,1) +
                         theme_bw() +
                         theme(axis.text.x = element_text(angle=45, hjust=1),
                               axis.title = element_blank(),
                               panel.border = element_blank(),
                               panel.grid.major.x = element_blank(),
                               panel.spacing.y = unit(2, "lines"))) %>%
                layout(margin = list(l = 50, r = 100)) %>%
                add_annotations(text = "Delta geospatial ECC value",
                                y = 0,
                                x=0.5,
                                yshift=-60,
                                yref = "paper",
                                xref = "paper",
                                xanchor = "middle",
                                yanchor = "bottom",
                                showarrow = FALSE,
                                font = list(size = 14)) %>%
                add_annotations(text = "Number of strains",
                                y = 0.5,
                                x=0,
                                xshift = -55,
                                yref = "paper",
                                xref = "paper",
                                xanchor = "left",
                                yanchor = "middle",
                                textangle = -90,
                                showarrow = FALSE,
                                font = list(size = 14))
            
        } else if (input$region == 2) {
            ggplotly(ggplot(strains_sh,
                            aes(x=delta_ecc_0.1.0)) +
                         geom_histogram(position="dodge", bins = 40,fill = "#1c74b4") +
                         facet_wrap(~country) +
                         xlim(-1,1) +
                         theme_bw() +
                         theme(axis.text.x = element_text(angle=45, hjust=1),
                               axis.title = element_blank(),
                               panel.border = element_blank(),
                               panel.grid.major.x = element_blank(),
                               panel.spacing.y = unit(2, "lines"))) %>%
                layout(margin = list(l = 50, r = 100)) %>%
                add_annotations(text = "Delta geospatial ECC value",
                                y = 0,
                                x=0.5,
                                yshift=-60,
                                yref = "paper",
                                xref = "paper",
                                xanchor = "middle",
                                yanchor = "bottom",
                                showarrow = FALSE,
                                font = list(size = 14)) %>%
                add_annotations(text = "Number of strains",
                                y = 0.5,
                                x=0,
                                xshift = -55,
                                yref = "paper",
                                xref = "paper",
                                xanchor = "left",
                                yanchor = "middle",
                                textangle = -90,
                                showarrow = FALSE,
                                font = list(size = 14))
        } else {
            ggplotly(ggplot(strains_sh,
                            aes(x=delta_ecc_0.1.0)) +
                         geom_histogram(position="dodge", bins = 40,  fill = "#1c74b4") +
                         xlim(-1,1) +
                         theme_bw() +
                         theme(axis.text.x = element_text(angle=45, hjust=1),
                               axis.title = element_blank(),
                               panel.border = element_blank(),
                               panel.grid.major.x = element_blank())) %>%
                layout(margin = list(l = 50, r = 100)) %>%
                add_annotations(text = "Delta geospatial ECC value",
                                y = 0,
                                x=0.5,
                                yshift=-60,
                                yref = "paper",
                                xref = "paper",
                                xanchor = "middle",
                                yanchor = "bottom",
                                showarrow = FALSE,
                                font = list(size = 14)) %>%
                add_annotations(text = "Number of strains",
                                y = 0.5,
                                x=0,
                                xshift = -55,
                                yref = "paper",
                                xref = "paper",
                                xanchor = "left",
                                yanchor = "middle",
                                textangle = -90,
                                showarrow = FALSE,
                                font = list(size = 14))
        }
    })
    
    
    # strain delta temp ecc histogram by time point
    output$strain_delta_temp_histogram <- renderPlotly({
        
        if(input$region == 3) {
            ggplotly(ggplot(strains_sh$data(withSelection=T) %>%
                                filter(selected_ | is.na(selected_)) %>%
                                subset(country %in% input$regionProvince),
                            aes(x=delta_ecc_0.0.1)) +
                         geom_histogram(position="dodge", bins = 40, fill = "#1c74b4") +
                         facet_wrap(~province) +
                         xlim(-1,1) +
                         theme_bw() +
                         theme(axis.text.x = element_text(angle=45, hjust=1),
                               axis.title = element_blank(),
                               panel.border = element_blank(),
                               panel.grid.major.x = element_blank(),
                               panel.spacing.y = unit(2, "lines"))) %>%
                layout(margin = list(l = 50, r = 100)) %>%
                add_annotations(text = "Delta temporal ECC value",
                                y = 0,
                                x=0.5,
                                yshift=-60,
                                yref = "paper",
                                xref = "paper",
                                xanchor = "middle",
                                yanchor = "bottom",
                                showarrow = FALSE,
                                font = list(size = 14)) %>%
                add_annotations(text = "Number of strains",
                                y = 0.5,
                                x=0,
                                xshift = -55,
                                yref = "paper",
                                xref = "paper",
                                xanchor = "left",
                                yanchor = "middle",
                                textangle = -90,
                                showarrow = FALSE,
                                font = list(size = 14))
            
        } else if (input$region == 2) {
            ggplotly(ggplot(strains_sh,
                            aes(x=delta_ecc_0.0.1)) +
                         geom_histogram(position="dodge", bins = 40,fill = "#1c74b4") +
                         facet_wrap(~country) +
                         xlim(-1,1) +
                         theme_bw() +
                         theme(axis.text.x = element_text(angle=45, hjust=1),
                               axis.title = element_blank(),
                               panel.border = element_blank(),
                               panel.grid.major.x = element_blank(),
                               panel.spacing.y = unit(2, "lines"))) %>%
                layout(margin = list(l = 50, r = 100)) %>%
                add_annotations(text = "Delta temporal ECC value",
                                y = 0,
                                x=0.5,
                                yshift=-60,
                                yref = "paper",
                                xref = "paper",
                                xanchor = "middle",
                                yanchor = "bottom",
                                showarrow = FALSE,
                                font = list(size = 14)) %>%
                add_annotations(text = "Number of strains",
                                y = 0.5,
                                x=0,
                                xshift = -55,
                                yref = "paper",
                                xref = "paper",
                                xanchor = "left",
                                yanchor = "middle",
                                textangle = -90,
                                showarrow = FALSE,
                                font = list(size = 14))
        } else {
            ggplotly(ggplot(strains_sh,
                            aes(x=delta_ecc_0.0.1)) +
                         geom_histogram(position="dodge", bins = 40,  fill = "#1c74b4") +
                         xlim(-1,1) +
                         theme_bw() +
                         theme(axis.text.x = element_text(angle=45, hjust=1),
                               axis.title = element_blank(),
                               panel.border = element_blank(),
                               panel.grid.major.x = element_blank())) %>%
                layout(margin = list(l = 50, r = 100)) %>%
                add_annotations(text = "Delta temporal ECC value",
                                y = 0,
                                x=0.5,
                                yshift=-60,
                                yref = "paper",
                                xref = "paper",
                                xanchor = "middle",
                                yanchor = "bottom",
                                showarrow = FALSE,
                                font = list(size = 14)) %>%
                add_annotations(text = "Number of strains",
                                y = 0.5,
                                x=0,
                                xshift = -55,
                                yref = "paper",
                                xref = "paper",
                                xanchor = "left",
                                yanchor = "middle",
                                textangle = -90,
                                showarrow = FALSE,
                                font = list(size = 14))
        }
    })
    
    # histogram of strain counts for tp1
    output$strain_histogram_tp1 <- renderPlotly({
        if(input$region == 3) {
            # strain count by country histogram
            plot_ly(type = "histogram",
                    data = strains_sh1$data(withSelection = T) %>%
                        filter(selected_ | is.na(selected_)) %>%
                        subset(country %in% input$regionProvince),
                    histfunc = "count",
                    x = ~strain_date,
                    color = ~province,
                    xbins = list(size = "M1"),
                    hovertemplate = ~paste('<b>', province, '</b><br>',
                                           'Count: %{y}', '<br>',
                                           'Date range: %{x}', '<extra></extra>', sep=" ")) %>%
                layout(barmode = "stack",
                       xaxis = list(title = "Date"),
                       yaxis = list(title = "Number of new strains identified"),
                       updatemenus = list( list(
                           active = -1,
                           x= -0.25,
                           type = 'buttons',
                           buttons = list(
                               list(
                                   label = "By day",
                                   method = "restyle",
                                   args = list(list(xbins = list(size = 86400000.0)))),
                               list(
                                   label = "By week",
                                   method = "restyle",
                                   args = list(list(xbins = list(size = 604800000.0)))),
                               list(
                                   label = "By month",
                                   method = "restyle",
                                   args = list(list(xbins = list(size = "M1"))))
                           ))))
        } else if (input$region == 2) {
            # strain count by country histogram
            plot_ly(type = "histogram",
                    data = strains_sh1,
                    histfunc = "count",
                    x = ~strain_date,
                    color = ~country,
                    colors = "Set1",
                    xbins = list(size = "M1"),
                    hovertemplate = ~paste('<b>', country, '</b><br>',
                                           'Count: %{y}', '<br>',
                                           'Date range: %{x}', '<extra></extra>', sep=" ")) %>%
                layout(barmode = "stack",
                       xaxis = list(title = "Date"),
                       yaxis = list(title = "Number of new strains identified"),
                       updatemenus = list( list(
                           active = -1,
                           x= -0.25,
                           type = 'buttons',
                           buttons = list(
                               list(
                                   label = "By day",
                                   method = "restyle",
                                   args = list(list(xbins = list(size = 86400000.0)))),
                               list(
                                   label = "By week",
                                   method = "restyle",
                                   args = list(list(xbins = list(size = 604800000.0)))),
                               list(
                                   label = "By month",
                                   method = "restyle",
                                   args = list(list(xbins = list(size = "M1"))))
                           ))))
        } else {
            # strain count by country histogram
            plot_ly(type = "histogram",
                    data = strains_sh1,
                    histfunc = "count",
                    x = ~strain_date,
                    colors = "Set1",
                    xbins = list(size = "M1"),
                    hovertemplate = ~paste('Count: %{y}', '<br>',
                                           'Date range: %{x}', '<extra></extra>', sep=" ")) %>%
                layout(barmode = "stack",
                       xaxis = list(title = "Date"),
                       yaxis = list(title = "Number of new strains identified"),
                       updatemenus = list( list(
                           active = -1,
                           x= -0.25,
                           type = 'buttons',
                           buttons = list(
                               list(
                                   label = "By day",
                                   method = "restyle",
                                   args = list(list(xbins = list(size = 86400000.0)))),
                               list(
                                   label = "By week",
                                   method = "restyle",
                                   args = list(list(xbins = list(size = 604800000.0)))),
                               list(
                                   label = "By month",
                                   method = "restyle",
                                   args = list(list(xbins = list(size = "M1"))))
                           ))))
        }
    })
    
    # histogram of strain counts for tp2
    output$strain_histogram_tp2 <- renderPlotly({
        if(input$region == 3) {
            # strain count by country histogram
            plot_ly(type = "histogram",
                    data = strains_sh2$data(withSelection = T) %>%
                        filter(selected_ | is.na(selected_)) %>%
                        subset(country %in% input$regionProvince),
                    histfunc = "count",
                    x = ~strain_date,
                    color = ~province,
                    xbins = list(size = "M1"),
                    hovertemplate = ~paste('<b>', province, '</b><br>',
                                           'Count: %{y}', '<br>',
                                           'Date range: %{x}', '<extra></extra>', sep=" ")) %>%
                layout(barmode = "stack",
                       xaxis = list(title = "Date"),
                       yaxis = list(title = "Number of new strains identified"),
                       updatemenus = list( list(
                           active = -1,
                           x= -0.25,
                           type = 'buttons',
                           buttons = list(
                               list(
                                   label = "By day",
                                   method = "restyle",
                                   args = list(list(xbins = list(size = 86400000.0)))),
                               list(
                                   label = "By week",
                                   method = "restyle",
                                   args = list(list(xbins = list(size = 604800000.0)))),
                               list(
                                   label = "By month",
                                   method = "restyle",
                                   args = list(list(xbins = list(size = "M1"))))
                           ))))
        } else if (input$region == 2) {
            # strain count by country histogram
            plot_ly(type = "histogram",
                    data = strains_sh2,
                    histfunc = "count",
                    x = ~strain_date,
                    color = ~country,
                    colors = "Set1",
                    xbins = list(size = "M1"),
                    hovertemplate = ~paste('<b>', country, '</b><br>',
                                           'Count: %{y}', '<br>',
                                           'Date range: %{x}', '<extra></extra>', sep=" ")) %>%
                layout(barmode = "stack",
                       xaxis = list(title = "Date"),
                       yaxis = list(title = "Number of new strains identified"),
                       updatemenus = list( list(
                           active = -1,
                           x= -0.25,
                           type = 'buttons',
                           buttons = list(
                               list(
                                   label = "By day",
                                   method = "restyle",
                                   args = list(list(xbins = list(size = 86400000.0)))),
                               list(
                                   label = "By week",
                                   method = "restyle",
                                   args = list(list(xbins = list(size = 604800000.0)))),
                               list(
                                   label = "By month",
                                   method = "restyle",
                                   args = list(list(xbins = list(size = "M1"))))
                           ))))
        } else {
            # strain count by country histogram
            plot_ly(type = "histogram",
                    data = strains_sh2,
                    histfunc = "count",
                    x = ~strain_date,
                    colors = "Set1",
                    xbins = list(size = "M1"),
                    hovertemplate = ~paste('Count: %{y}', '<br>',
                                           'Date range: %{x}', '<extra></extra>', sep=" ")) %>%
                layout(barmode = "stack",
                       xaxis = list(title = "Date"),
                       yaxis = list(title = "Number of new strains identified"),
                       updatemenus = list( list(
                           active = -1,
                           x= -0.25,
                           type = 'buttons',
                           buttons = list(
                               list(
                                   label = "By day",
                                   method = "restyle",
                                   args = list(list(xbins = list(size = 86400000.0)))),
                               list(
                                   label = "By week",
                                   method = "restyle",
                                   args = list(list(xbins = list(size = 604800000.0)))),
                               list(
                                   label = "By month",
                                   method = "restyle",
                                   args = list(list(xbins = list(size = "M1"))))
                           ))))
        }
    })
    
    # cumulative strain counts for tp1
    output$cum_strains_tp1 <- renderPlotly({
        
        # leaflet built in color function
        pal <-  leaflet::colorFactor(
            palette = rep(cols25(n=25),
                          length.out=length(unique(vals$clusters$tp1_cluster))),
            domain = unique(vals$clusters$tp1_cluster))
        
        if(input$region == 3) {
            
            # need to aggregate data
            strains_sh1_cumsum <- strains_sh$data(withSelection = T) %>%
                subset(timepoint == 1) %>%
                filter(selected_ | is.na(selected_)) %>%
                subset(country == input$regionProvince) %>%
                group_by(tp1_cluster, province, strain_date) %>% 
                tally(!is.na(strain)) %>% 
                mutate(cumsum = cumsum(n))
            
            # time point 1
            ggplotly(ggplot(data = strains_sh1_cumsum,
                            aes(x=strain_date, y = cumsum, color = tp1_cluster)) +
                         scale_color_manual(values = pal(unique(strains_sh1_cumsum$tp1_cluster)),
                                            name = "Cluster") +
                         geom_line() +
                         facet_wrap(~province) +
                         theme_bw() +
                         theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
                               axis.title = element_blank(),
                               panel.grid.major.x = element_blank(),
                               panel.border = element_blank(),
                               panel.spacing.y = unit(2, "lines"))) %>%
                add_annotations( text = "Date",
                                 y = 0,
                                 x=0.5, 
                                 yshift=-70,
                                 yref = "paper",
                                 xref = "paper",
                                 xanchor = "middle",
                                 yanchor = "bottom",
                                 showarrow = FALSE,
                                 font = list(size = 14)) %>%
                add_annotations( text = "Cumulative strain count",
                                 y = 0.5,
                                 x=0, 
                                 xshift = -50,
                                 yref = "paper",
                                 xref = "paper",
                                 xanchor = "left",
                                 yanchor = "middle",
                                 textangle = -90,
                                 showarrow = FALSE,
                                 font = list(size = 14)) 
            
        } else if (input$region == 2) {
            
            # need to aggregate data
            strains_sh1_cumsum <- strains_sh$data(withSelection = T) %>%
                subset(timepoint == 1) %>%
                filter(selected_ | is.na(selected_)) %>%
                group_by(tp1_cluster, country, strain_date) %>% 
                tally(!is.na(strain)) %>% 
                mutate(cumsum = cumsum(n))
            
            # time point 1
            ggplotly(ggplot(data = strains_sh1_cumsum,
                            aes(x=strain_date, y = cumsum, color = tp1_cluster)) +
                         scale_color_manual(values = pal(unique(strains_sh1_cumsum$tp1_cluster)),
                                            name = "Cluster") +
                         geom_line() +
                         facet_wrap(~country) +
                         theme_bw() +
                         theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
                               axis.title = element_blank(),
                               panel.grid.major.x = element_blank(),
                               panel.border = element_blank(),
                               panel.spacing.y = unit(2, "lines"))) %>%
                add_annotations( text = "Date",
                                 y = 0,
                                 x=0.5, 
                                 yshift=-70,
                                 yref = "paper",
                                 xref = "paper",
                                 xanchor = "middle",
                                 yanchor = "bottom",
                                 showarrow = FALSE,
                                 font = list(size = 14)) %>%
                add_annotations( text = "Cumulative strain count",
                                 y = 0.5,
                                 x=0, 
                                 xshift = -50,
                                 yref = "paper",
                                 xref = "paper",
                                 xanchor = "left",
                                 yanchor = "middle",
                                 textangle = -90,
                                 showarrow = FALSE,
                                 font = list(size = 14)) 
        } else { 
            
            # need to aggregate data
            strains_sh1_cumsum <- strains_sh$data(withSelection = T) %>%
                subset(timepoint == 1) %>%
                filter(selected_ | is.na(selected_)) %>%
                group_by(tp1_cluster, strain_date) %>% 
                tally(!is.na(strain)) %>% 
                mutate(cumsum = cumsum(n))
            
            # time point 1
            ggplotly(ggplot(data = strains_sh1_cumsum,
                            aes(x=strain_date, y = cumsum, color = tp1_cluster)) +
                         scale_color_manual(values = pal(unique(strains_sh1_cumsum$tp1_cluster)),
                                            name = "Cluster") +
                         geom_line() +
                         theme_bw() +
                         theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
                               axis.title = element_blank(),
                               panel.grid.major.x = element_blank(),
                               panel.border = element_blank(),
                               panel.spacing.y = unit(2, "lines"))) %>%
                add_annotations( text = "Date",
                                 y = 0,
                                 x=0.5, 
                                 yshift=-70,
                                 yref = "paper",
                                 xref = "paper",
                                 xanchor = "middle",
                                 yanchor = "bottom",
                                 showarrow = FALSE,
                                 font = list(size = 14)) %>%
                add_annotations( text = "Cumulative strain count",
                                 y = 0.5,
                                 x=0, 
                                 xshift = -50,
                                 yref = "paper",
                                 xref = "paper",
                                 xanchor = "left",
                                 yanchor = "middle",
                                 textangle = -90,
                                 showarrow = FALSE,
                                 font = list(size = 14)) 
        }
    })
    
    # cumulative strain counts for tp2
    output$cum_strains_tp2 <- renderPlotly({
        
        # leaflet built in color function
        pal <-  leaflet::colorFactor(
            palette = rep(cols25(n=25),
                          length.out=length(unique(vals$clusters$tp1_cluster))),
            domain = unique(vals$clusters$tp1_cluster))
        
        if(input$region == 3) {
            
            # need to aggregate data
            strains_sh2_cumsum <- strains_sh$data(withSelection = T) %>%
                subset(timepoint == 2) %>%
                filter(selected_ | is.na(selected_)) %>%
                subset(country == input$regionProvince) %>%
                group_by(tp1_cluster, province, strain_date) %>% 
                tally(!is.na(strain)) %>% 
                mutate(cumsum = cumsum(n))
            
            # time point 1
            ggplotly(ggplot(data = strains_sh2_cumsum,
                            aes(x=strain_date, y = cumsum, color = tp1_cluster)) +
                         scale_color_manual(values = pal(unique(strains_sh2_cumsum$tp1_cluster)),
                                            name = "Cluster") +
                         geom_line() +
                         facet_wrap(~province) +
                         theme_bw() +
                         theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
                               axis.title = element_blank(),
                               panel.grid.major.x = element_blank(),
                               panel.border = element_blank(),
                               panel.spacing.y = unit(2, "lines"))) %>%
                add_annotations( text = "Date",
                                 y = 0,
                                 x=0.5, 
                                 yshift=-70,
                                 yref = "paper",
                                 xref = "paper",
                                 xanchor = "middle",
                                 yanchor = "bottom",
                                 showarrow = FALSE,
                                 font = list(size = 14)) %>%
                add_annotations( text = "Cumulative strain count",
                                 y = 0.5,
                                 x=0, 
                                 xshift = -50,
                                 yref = "paper",
                                 xref = "paper",
                                 xanchor = "left",
                                 yanchor = "middle",
                                 textangle = -90,
                                 showarrow = FALSE,
                                 font = list(size = 14)) 
            
        } else if (input$region == 2) {
            
            # need to aggregate data
            strains_sh2_cumsum <- strains_sh$data(withSelection = T) %>%
                subset(timepoint == 2) %>%
                filter(selected_ | is.na(selected_)) %>%
                group_by(tp1_cluster, country, strain_date) %>% 
                tally(!is.na(strain)) %>% 
                mutate(cumsum = cumsum(n))
            
            # time point 1
            ggplotly(ggplot(data = strains_sh2_cumsum,
                            aes(x=strain_date, y = cumsum, color = tp1_cluster)) +
                         scale_color_manual(values = pal(unique(strains_sh2_cumsum$tp1_cluster)),
                                            name = "Cluster") +
                         geom_line() +
                         facet_wrap(~country) +
                         theme_bw() +
                         theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
                               axis.title = element_blank(),
                               panel.grid.major.x = element_blank(),
                               panel.border = element_blank(),
                               panel.spacing.y = unit(2, "lines"))) %>%
                add_annotations( text = "Date",
                                 y = 0,
                                 x=0.5, 
                                 yshift=-70,
                                 yref = "paper",
                                 xref = "paper",
                                 xanchor = "middle",
                                 yanchor = "bottom",
                                 showarrow = FALSE,
                                 font = list(size = 14)) %>%
                add_annotations( text = "Cumulative strain count",
                                 y = 0.5,
                                 x=0, 
                                 xshift = -50,
                                 yref = "paper",
                                 xref = "paper",
                                 xanchor = "left",
                                 yanchor = "middle",
                                 textangle = -90,
                                 showarrow = FALSE,
                                 font = list(size = 14)) 
        } else { 
            
            # need to aggregate data
            strains_sh2_cumsum <- strains_sh$data(withSelection = T) %>%
                subset(timepoint == 2) %>%
                filter(selected_ | is.na(selected_)) %>%
                group_by(tp1_cluster, strain_date) %>% 
                tally(!is.na(strain)) %>% 
                mutate(cumsum = cumsum(n))
            
            # time point 1
            ggplotly(ggplot(data = strains_sh2_cumsum,
                            aes(x=strain_date, y = cumsum, color = tp1_cluster)) +
                         scale_color_manual(values = pal(unique(strains_sh2_cumsum$tp1_cluster)),
                                            name = "Cluster") +
                         geom_line() +
                         theme_bw() +
                         theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
                               axis.title = element_blank(),
                               panel.grid.major.x = element_blank(),
                               panel.border = element_blank(),
                               panel.spacing.y = unit(2, "lines"))) %>%
                add_annotations( text = "Date",
                                 y = 0,
                                 x=0.5, 
                                 yshift=-70,
                                 yref = "paper",
                                 xref = "paper",
                                 xanchor = "middle",
                                 yanchor = "bottom",
                                 showarrow = FALSE,
                                 font = list(size = 14)) %>%
                add_annotations( text = "Cumulative strain count",
                                 y = 0.5,
                                 x=0, 
                                 xshift = -50,
                                 yref = "paper",
                                 xref = "paper",
                                 xanchor = "left",
                                 yanchor = "middle",
                                 textangle = -90,
                                 showarrow = FALSE,
                                 font = list(size = 14)) 
        }
    })
    
    # counts of single vs multi strain clusters at time point 1
    # regional facetting available 
    output$straintypes_tp1 <- renderPlotly({
        if(input$region == 3) {
            # by province 
            ggplotly(ggplot(data = strains_sh$data(withSelection = T) %>%
                                filter(selected_ | is.na(selected_)) %>%
                                subset(timepoint==1) %>%
                                subset(country %in% input$province) %>%
                                mutate(tp1_homo_het = ifelse(tp1_cluster_size_2>2,"Multi-strain","Single-strain")), 
                            aes(x=strain_date,  fill = tp1_homo_het)) +
                         geom_histogram() +
                         scale_fill_manual(values = c("#1c74b4", "#fc7c0d"),
                                           name = "Cluster type") + 
                         facet_wrap(~province) +
                         theme_bw() +
                         theme(panel.border = element_blank(),
                               panel.grid.major.x = element_blank(),
                               panel.grid.minor.x = element_blank()), 
                     dynamicTicks = T)
            
        } else if (input$region == 2) {
            # by country 
            ggplotly(ggplot(data = strains_sh$data(withSelection = T)%>%
                                filter(selected_ | is.na(selected_)) %>%
                                subset(timepoint==1) %>%
                                mutate(tp1_homo_het = ifelse(tp1_cluster_size_2>2,"Multi-strain","Single-strain")), 
                            aes(x=strain_date,  fill = tp1_homo_het)) +
                         geom_histogram() +
                         scale_fill_manual(values = c("#1c74b4", "#fc7c0d"),
                                           name = "Cluster type") + 
                         facet_wrap(~country) +
                         theme_bw() +
                         theme(panel.border = element_blank(),
                               panel.grid.major.x = element_blank(),
                               panel.grid.minor.x = element_blank()), 
                     dynamicTicks = T)
        } else { 
            # no regional facetting 
            ggplotly(ggplot(data = strains_sh$data(withSelection = T) %>%
                                filter(selected_ | is.na(selected_)) %>%
                                subset(timepoint==1) %>%
                                mutate(tp1_homo_het = ifelse(tp1_cluster_size_2>2,"Multi-strain","Single-strain")), 
                            aes(x=strain_date,  fill = tp1_homo_het)) +
                         geom_histogram() +
                         scale_fill_manual(values = c("#1c74b4", "#fc7c0d"),
                                           name = "Cluster type") + 
                         theme_bw() +
                         theme(panel.border = element_blank(),
                               panel.grid.major.x = element_blank(),
                               panel.grid.minor.x = element_blank()),
                     dynamicTicks = T) 
        }
    })
    
    # counts of single vs multi strain clusters at time point 2
    # regional facetting available 
    output$straintypes_tp2 <- renderPlotly({
        if(input$region == 3) {
            # by province 
            ggplotly(ggplot(data = strains_sh$data(withSelection = T) %>%
                                filter(selected_ | is.na(selected_)) %>%
                                subset(country %in% input$province) %>%
                                subset(timepoint==2) %>%
                                mutate(tp2_homo_het = ifelse(tp2_cluster_size_2>2,"Multi-strain","Single-strain")), 
                            aes(x=strain_date,  fill = tp2_homo_het)) +
                         geom_histogram() +
                         scale_fill_manual(values = c("#1c74b4", "#fc7c0d"),
                                           name = "Cluster type") + 
                         facet_wrap(~province) +
                         theme_bw() +
                         theme(panel.border = element_blank(),
                               panel.grid.major.x = element_blank(),
                               panel.grid.minor.x = element_blank()), 
                     dynamicTicks = T)
            
        } else if (input$region == 2) {
            # by country 
            ggplotly(ggplot(data = strains_sh$data(withSelection = T) %>%
                                filter(selected_ | is.na(selected_)) %>%
                                subset(timepoint==2) %>%
                                mutate(tp2_homo_het = ifelse(tp2_cluster_size_2>2,"Multi-strain","Single-strain")), 
                            aes(x=strain_date,  fill = tp2_homo_het)) +
                         geom_histogram() +
                         scale_fill_manual(values = c("#1c74b4", "#fc7c0d"),
                                           name = "Cluster type") + 
                         facet_wrap(~country) +
                         theme_bw() +
                         theme(panel.border = element_blank(),
                               panel.grid.major.x = element_blank(),
                               panel.grid.minor.x = element_blank()), 
                     dynamicTicks = T)
        } else { 
            # no regional facetting 
            ggplotly(ggplot(data = strains_sh$data(withSelection = T)  %>%
                                filter(selected_ | is.na(selected_)) %>%
                                subset(timepoint==2) %>%
                                mutate(tp2_homo_het = ifelse(tp2_cluster_size_2>2,"Multi-strain","Single-strain")), 
                            aes(x=strain_date,  fill = tp2_homo_het)) +
                         geom_histogram() +
                         scale_fill_manual(values = c("#1c74b4", "#fc7c0d"),
                                           name = "Cluster type") + 
                         theme_bw() +
                         theme(panel.border = element_blank(),
                               panel.grid.major.x = element_blank(),
                               panel.grid.minor.x = element_blank()),
                     dynamicTicks = T) 
        }
    })
}

shinyApp(ui, server)


