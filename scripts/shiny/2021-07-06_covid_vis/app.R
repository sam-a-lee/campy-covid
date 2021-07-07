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
                                                     "By province" = 3), selected = 1),
                         conditionalPanel(
                             condition = "input.region == 3",
                             selectizeInput("regionProvince", "View provinces in: ", unique(strains$country),
                                            selected = NULL, multiple = FALSE))
                ),
                
                # Data filters
                menuItem("Data filters", tabName = "dataFilters",
                         
                         # cluster filters
                         menuItem('Cluster filters', tabName = "clusterFilters",
                                  uiOutput("tp1_cluster_select"),
                                  uiOutput("timepoint_select"),
                                  uiOutput("type_select"),
                                  uiOutput("cluster_size_select"),
                                  uiOutput("date_select"),
                                  uiOutput("ecc001_select"),
                                  uiOutput("ecc010_select"),
                                  uiOutput("latitude_select"),
                                  uiOutput("longitude_select")
                         ),
                         
                         #strain filters
                         menuItem('Strain filters', tabName = "strainFilters"
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
                
                # row 6
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
            vals$data_raw <- read_xlsx(input$userFile$datapath, n_max = 10)
    }, ignoreNULL = T)
    
    
    # process data
    observeEvent(vals$data_raw, {
        
        print("new data!")
        
        eccdata <- vals$data_raw
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
        vals$clusters <- data.table::melt(setDT(clusters_cc), 
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
        vals$clusters$maptext <-
            paste0('<strong>', vals$clusters$tp1_cluster, '</strong>','<br/>',
                   'Timepoint: ', '<strong>', vals$clusters$timepoint, '</strong>', '<br/>',
                   'Cluster size: ', vals$clusters$cluster_size_2, '<br/>',
                   'Temporal ECC: ', vals$clusters$ecc_0.0.1, '<br/>',
                   'Geospatial ECC: ', vals$clusters$ecc_0.1.0, '<br/>') %>%
            lapply(htmltools::HTML) 
        
        vals$clusters$key <- as.numeric(rownames(vals$clusters))
        
        vals$clusters <- vals$clusters
        
        
        strains <- eccdata %>%
            select(strain, country, province, city, year, month, day,
                   strain_latitude, strain_longitude, present_at_tp1, 
                   tp1_cluster, present_at_tp2, tp2_cluster, tp1_t0_ecc_0.1.0, 
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
    })
    
    
    ################################# 
    # user input for data filtering #
    #################################
    
    output$tp1_cluster_select <- renderUI({
        selectizeInput("tp1_cluster", "TP1 cluster", unique(vals$clusters$tp1_cluster),  selected = NULL, multiple = T)
    })
    
    output$timepoint_select <- renderUI({
        selectizeInput("timepoint", "Timepoint", unique(vals$clusters$timepoint), selected = NULL, multiple = T)
    })
    
    output$type_select <- renderUI({
        selectizeInput("type", "Type",  unique(vals$clusters$type),  selected = NULL, multiple = T)
    })
    
    output$cluster_size_select <- renderUI({
        sliderInput(inputId = "cluster_size_2", label = "Cluster size", min = min(vals$clusters$cluster_size_2)-1, max = max(vals$clusters$cluster_size_2)-1, value = c(min(vals$clusters$cluster_size_2)-1, max(vals$clusters$cluster_size_2)-1))
    })
    
    output$date_select <- renderUI({
        sliderInput(inputId = "avg_date", label = "Avg date", min = min(vals$clusters$avg_date), max = max(vals$clusters$avg_date), value = c(min(vals$clusters$avg_date), max(vals$clusters$avg_date)))
    })
    
    output$ecc001_select <- renderUI({
        sliderInput(inputId = "ecc_0.0.1", label = "ecc_0.0.1", min = min(vals$clusters$ecc_0.0.1, na.rm = T), max = max(vals$clusters$ecc_0.0.1, na.rm = T), value = c(min(vals$clusters$ecc_0.0.1, na.rm = T), max(vals$clusters$ecc_0.0.1, na.rm = T)))
    })
    
    output$ecc010_select <- renderUI({
        sliderInput(inputId = "ecc_0.1.0", label = "ecc_0.1.0", min = min(vals$clusters$ecc_0.1.0, na.rm = T), max = max(vals$clusters$ecc_0.1.0, na.rm = T), value = c(min(vals$clusters$ecc_0.1.0, na.rm = T), max(vals$clusters$ecc_0.1.0, na.rm = T)))
    })
    
    output$latitude_select <- renderUI({
        sliderInput(inputId = "avg_latitude", label = "Avg latitude", min = min(vals$clusters$avg_latitude), max = max(vals$clusters$avg_latitude), value = c(min(vals$clusters$avg_latitude), max(vals$clusters$avg_latitude)))
    })
    
    output$longitude_select <- renderUI({
        sliderInput(inputId = "avg_longitude", label = "Avg longitude", min = min(vals$clusters$avg_longitude), max = max(vals$clusters$avg_longitude), value = c(min(vals$clusters$avg_longitude), max(vals$clusters$avg_longitude)))
    })

    
    ###########################
    # reactive data filtering #
    ###########################
    
    clusters_r <- reactive({
        vals$clusters 
    })
    
    strains_r <- reactive({
        vals$strains
    })
    
    observeEvent(input$timepoint_select, input$ecc_0.0.1, {
        print("selected time point")
        print(input$timepoint_select)
        
        print("ecc0.0.1 range")
        print(input$ecc_0.0.1)
        
    })
    ######################
    # shared data frames #
    ######################
    
    clusters_sh <- SharedData$new(clusters_r, key = ~key, group = "clusters")
    
    strains_sh <- SharedData$new(strains_r, key = ~key, group = "clusters")
    
    
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
        datatable(data = vals$strains,
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
    
}

shinyApp(ui, server)