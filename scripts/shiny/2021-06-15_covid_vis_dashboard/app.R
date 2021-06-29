################ 
# General info #
################

# LONG FORMAT
# COVID VIS SHINY DASHBOARD
# June 16, 2021
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
# avg tp1 lng and lat is different for present_at_tp1 == 1 and present_at_tp1 == 0 
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
            cluster_novel_growth_rate = mean(cluster_novel_growth_rate),
            type = mean(type))

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
            cluster_novel_growth_rate = mean(cluster_novel_growth_rate),
            type = mean(type))


################################################
# change in latitude and longitude of clusters #
################################################

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


################################################################
# convert delta ECCs to angle that can be used for radial plot #
################################################################

# angle calculation 
# convert radians to degrees
angle <- function(x,y) { 
  z <- x + 1i * y
  res <- 90 - Arg(z) / pi * 180
  res %% 360
}

# calculate the angle based that delta ECCs form
clusters <- clusters %>% 
  mutate(ecc_angle = angle(delta_ecc_0.1.0, delta_ecc_0.0.1))

# get direction table
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

#######################################
# convert cluster info to long format #
#######################################

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





##########################################
# subset out strain specific infomation  #
##########################################

# strain info could still be important
# subset strain info to a seprate data frame

strains <- eccdata %>%
  select(strain, country, province, city, year, month, day,
         strain_latitude, strain_longitude, present_at_tp1, 
         tp1_cluster, present_at_tp2, tp2_cluster, tp1_t0_ecc_0.1.0, 
         tp2_t0_ecc_0.1.0, tp1_t0_ecc_0.0.1, tp2_t0_ecc_0.0.1,
         delta_ecc_0.1.0, delta_ecc_0.0.1, type)


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
strains <- strains %>% mutate(stain_time_diff = as.numeric(abs(as.Date("01-01-2020", format = "%d-%m-%y") - as.Date(strain_date, format = "%d-%m-%y"))))


################################
# create strain labels for map #
################################

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



##################
# crosstalk keys #
##################

# create unique row ids for shared cluster and strain data
clusters_long$key <- as.numeric(rownames(clusters_long))
strains$key <- as.numeric(rownames(strains))

rm(clusters, clusters_0, clusters_cc)


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
                                                   "Largest delta temporal ECC" = 3, "None" = 4), selected = 1)),
              
              # Data filters
              menuItem("Data filters", tabName = "dataFilters",
                       
                       # cluster filters
                       menuItem('Cluster filters', tabName = "clusterFilters",
                                selectizeInput("tp1_cluster", "TP1 cluster", unique(clusters_long$tp1_cluster),  selected = NULL, multiple = T),
                                selectizeInput("timepoint", "Timepoint", unique(clusters_long$timepoint), selected = NULL, multiple = T),
                                selectizeInput("type", "Type", unique(clusters_long$type), selected = NULL, multiple = T),
                                sliderInput(inputId = "cluster_size_2", label = "Cluster size", min = min(clusters_long$cluster_size_2)-1, max = max(clusters_long$cluster_size_2)-1, value = c(min(clusters_long$cluster_size_2)-1, max(clusters_long$cluster_size_2)-1)),
                                sliderInput(inputId = "avg_date", label = "Avg date", min = min(clusters_long$avg_date), max = max(clusters_long$avg_date), value = c(min(clusters_long$avg_date), max(clusters_long$avg_date))),
                                sliderInput(inputId = "ecc_0.0.1", label = "ecc_0.0.1", min = min(clusters_long$ecc_0.0.1, na.rm = T), max = max(clusters_long$ecc_0.0.1, na.rm = T), value = c(min(clusters_long$ecc_0.0.1, na.rm = T), max(clusters_long$ecc_0.0.1, na.rm = T))),
                                sliderInput(inputId = "ecc_0.1.0", label = "ecc_0.1.0", min = min(clusters_long$ecc_0.1.0, na.rm = T), max = max(clusters_long$ecc_0.1.0, na.rm = T), value = c(min(clusters_long$ecc_0.1.0, na.rm = T), max(clusters_long$ecc_0.1.0, na.rm = T))),
                                sliderInput(inputId = "avg_latitude", label = "Avg latitude", min = min(clusters_long$avg_latitude), max = max(clusters_long$avg_latitude), value = c(min(clusters_long$avg_latitude), max(clusters_long$avg_latitude))),
                                sliderInput(inputId = "avg_longitude", label = "Avg longitude", min = min(clusters_long$avg_longitude), max = max(clusters_long$avg_longitude), value = c(min(clusters_long$avg_longitude), max(clusters_long$avg_longitude))),
                                # delta filters
                                sliderInput(inputId = "delta_cluster_size", label = "Delta cluster size", min = min(clusters_long$delta_cluster_size), max = max(clusters_long$delta_cluster_size), value = c(min(clusters_long$delta_cluster_size), max(clusters_long$delta_cluster_size))),
                                sliderInput(inputId = "num_novel_tp2_strains", label = "Novel TP2 strains", min = min(clusters_long$num_novel_tp2_strains), max = max(clusters_long$num_novel_tp2_strains), value = c(min(clusters_long$num_novel_tp2_strains), max(clusters_long$num_novel_tp2_strains))),
                                sliderInput(inputId = "overall_cluster_growth_rate", label = "Overall growth rate", min = min(clusters_long$overall_cluster_growth_rate), max = max(clusters_long$overall_cluster_growth_rate), value = c(min(clusters_long$overall_cluster_growth_rate), max(clusters_long$overall_cluster_growth_rate))),
                                sliderInput(inputId = "cluster_novel_growth_rate", label = "Novel growth rate", min = min(clusters_long$delta_cluster_size), max = max(clusters_long$delta_cluster_size), value = c(min(clusters_long$delta_cluster_size), max(clusters_long$delta_cluster_size))),
                                sliderInput(inputId = "delta_ecc_0.0.1", label = "delta ecc_0.0.1", min = min(clusters_long$delta_ecc_0.0.1, na.rm = T), max = max(clusters_long$delta_ecc_0.0.1, na.rm = T), value = c(min(clusters_long$delta_ecc_0.0.1, na.rm = T), max(clusters_long$delta_ecc_0.0.1, na.rm = T))),
                                sliderInput(inputId = "delta_ecc_0.1.0", label = "delta ecc_0.1.0", min = min(clusters_long$delta_ecc_0.1.0, na.rm = T), max = max(clusters_long$delta_ecc_0.1.0, na.rm = T), value = c(min(clusters_long$delta_ecc_0.1.0, na.rm = T), max(clusters_long$delta_ecc_0.1.0, na.rm = T))),
                                selectizeInput("cardinal", "Avg cluster movement direction", unique(clusters_long$cardinal), selected = NULL, multiple = T)),
                       
                       #strain filters
                       menuItem('Strain filters', tabName = "strainFilters",
                                # strain filters
                                selectizeInput("country", "Country", unique(strains$country),  selected = NULL, multiple = T),
                                selectizeInput("province", "Province", unique(strains$province),  selected = NULL, multiple = T),
                                selectizeInput("present_at_tp1", "Present at TP1", unique(strains$present_at_tp1),  selected = NULL, multiple = T),
                                sliderInput(inputId = "strain_date", label = "Strain date", min = min(strains$strain_date), max = max(strains$strain_date), value = c(min(strains$strain_date), max(strains$strain_date)))
                       )),
              
              # Cluster data tab
              menuItem("Cluster data", tabName = "clusterData"),
              
              # Strain data tab
              menuItem("Strain data", tabName = "strainData"),
              
              # filtered data used in visualizations in wide format
              menuItem("Filtered raw data", tabName = "filteredData"),
              
              # raw data used to generate visualizations
              menuItem("Raw data", tabName = "rawData"))
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
                      plotlyOutput("bubbleplot")),
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
                      selectInput(inputId = "geoSelect", label = "",
                                  choices = list("No faceting" = 1, "By country" = 2, "By province" = 3),
                                  selected = 1),
                      conditionalPanel(
                        condition = "input.geoSelect == 3",
                        selectizeInput("geoProvince", "By provinces in: ", unique(strains$country),
                                       selected = NULL, multiple = FALSE)
                      ),
                      plotlyOutput("strain_geo_histogram")),
                  
                  # temp ecc histogram by time point
                  box(title = "Temporal ECC histogram", 
                      width = 6, 
                      solidHeader = TRUE, 
                      selectInput(inputId = "tempSelect", label = "",
                                  choices = list("No faceting" = 1, "By country" = 2, "By province" = 3),
                                  selected = 1),
                      conditionalPanel(
                        condition = "input.tempSelect == 3",
                        selectizeInput("tempProvince", "By provinces in: ", unique(strains$country),
                                       selected = NULL, multiple = FALSE)
                      ),
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
                  plotlyOutput("ridgeplot"),
                  collapsible = TRUE)
            ),
            
            # fluid row 5
            fluidRow(
              #strain histogram 
              box(title = HTML("<b>", "Number of new strains identified per month by country",  "</b>"),
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
              box(title = HTML("<b>", "Cumulative strain identification by country",  "</b>"),
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
    
    clusters_filt <- clusters_long %>%
      # tp1 filters
      # also filter for tp2 clusters since they are linked?
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
      filter(delta_ecc_0.1.0 >= input$delta_ecc_0.1.0[1] & delta_ecc_0.1.0 <= input$delta_ecc_0.1.0[2]) %>%
      {if (!is.null(input$cardinal)) filter(., cardinal %in% input$cardinal)  else . } %>%
      
      # data subsetting 
      {if (input$subsets==1) arrange(., desc(abs(cluster_size_2))) else . }  %>%
      {if (input$subsets==2) arrange(., desc(abs(delta_ecc_0.0.1))) else . }  %>%
      {if (input$subsets==3) arrange(., desc(abs(delta_ecc_0.1.0))) else . } 
    
    # grab n rows according to input
    if (input$number != 99){
      topn <- as.numeric(input$number)
      unique_clust <- head(unique(clusters_filt$tp1_cluster),topn)
      return(clusters_filt %>% subset(tp1_cluster %in% unique_clust))
    } else{  
      return(clusters_filt)
    }
    
  })
  
  # all cluster reactive filtering
  eccdata_r <- reactive({
    
    # get top n
    topn <- {if (input$number != "all") as.numeric(input$number) else nrow(clusters_long)}
    
    eccdata %>%
      # currently filtering only tp1 data
      # based on filters should also be filtering tp2 data on same criteria?
      {if (!is.null(input$tp1_cluster)) filter(., tp1_cluster %in% input$tp1_cluster)  else . } %>% 
      {if (!is.null(input$timepoint)) filter(., present_at_tp1 %in% as.numeric(input$timepoint)-1)  else . } %>%
      {if (!is.null(input$type)) filter(., type %in% input$type)  else . } %>%
      filter(tp1_cluster_size_2 >= input$cluster_size_2[1] & tp1_cluster_size_2 <= input$cluster_size_2[2]) %>%
      filter(avg_tp1_date >= input$avg_date[1] & avg_tp1_date <= input$avg_date[2]) %>%
      filter(tp1_t0_ecc_0.0.1 >= input$ecc_0.0.1[1] & tp1_t0_ecc_0.0.1 <= input$ecc_0.0.1[2]) %>%
      filter(tp1_t0_ecc_0.1.0 >= input$ecc_0.1.0[1] & tp1_t0_ecc_0.1.0 <= input$ecc_0.1.0[2]) %>%
      filter(avg_tp1_latitude >= input$avg_latitude[1] & avg_tp1_latitude <= input$avg_latitude[2]) %>%
      filter(avg_tp1_longitude >= input$avg_longitude[1] & avg_tp1_longitude <= input$avg_longitude[2]) %>%
      
      # delta filters
      filter(delta_cluster_size >= input$delta_cluster_size[1] & delta_cluster_size <= input$delta_cluster_size[2]) %>%
      filter(num_novel_tp2_strains >= input$num_novel_tp2_strains[1] & num_novel_tp2_strains <= input$num_novel_tp2_strains[2]) %>%
      filter(overall_cluster_growth_rate >= input$overall_cluster_growth_rate[1] & overall_cluster_growth_rate <= input$overall_cluster_growth_rate[2]) %>%
      filter(cluster_novel_growth_rate >= input$cluster_novel_growth_rate[1] & cluster_novel_growth_rate <= input$cluster_novel_growth_rate[2]) %>%
      filter(delta_ecc_0.0.1 >= input$delta_ecc_0.0.1[1] & delta_ecc_0.0.1 <= input$delta_ecc_0.0.1[2]) %>%
      filter(delta_ecc_0.1.0 >= input$delta_ecc_0.1.0[1] & delta_ecc_0.1.0 <= input$delta_ecc_0.1.0[2]) %>%
      {if (!is.null(input$cardinal)) filter(., cardinal %in% input$cardinal)  else . } %>%
      
      # data subsetting 
      {if (input$subsets==1) arrange(., desc(abs(cluster_size_2))) else . }  %>%
      {if (input$subsets==2) arrange(., desc(abs(delta_ecc_0.0.1))) else . }  %>%
      {if (input$subsets==3) arrange(., desc(abs(delta_ecc_0.1.0))) else . } 
    
    # grab n rows according to input
    if (input$number != 99){
      topn <- as.numeric(input$number)
      unique_clust <- head(unique(clusters_filt$tp1_cluster),topn)
      return(clusters_filt %>% subset(tp1_cluster %in% unique_clust))
    } else{  
      return(clusters_filt)
    }
  })
  
  # shared cluster data frames
  clusters_long_sh <- SharedData$new(clusters_rall, key = ~key, group = "clusters_long")
  
  
  ##########################################
  # filter strain data based on user input #
  ##########################################
  
  # strain reactive filtering
  strains_rall <- reactive({
    strains %>%
      {if (!is.null(input$country)) filter(., country %in% input$country)  else . } %>%
      {if (!is.null(input$province)) filter(., province %in% input$province)  else . } %>%
      {if (!is.null(input$present_at_tp1)) filter(., present_at_tp1 %in% input$province)  else . } %>%
      # filter by shared cluster selections
      filter(tp1_cluster %in% (clusters_long_sh$data(withSelection = TRUE) %>% filter(selected_ | is.na(selected_)) %>% pull(tp1_cluster)))
  })
  
  # tp1 strain reactive filtering
  strains_r1 <- reactive({
    strains %>%
      {if (!is.null(input$country)) filter(., country %in% input$country)  else . } %>%
      {if (!is.null(input$province)) filter(., province %in% input$province)  else . } %>%
      {if (!is.null(input$present_at_tp1)) filter(., present_at_tp1 %in% input$province)  else . } %>%
      filter(present_at_tp1==1) %>%
      filter(tp1_cluster %in% (clusters_long_sh$data(withSelection = TRUE) %>% filter(selected_ | is.na(selected_)) %>% pull(tp1_cluster)))
  })
  
  # tp2 strain reactive filtering
  strains_r2 <- reactive({
    strains %>%
      {if (!is.null(input$country)) filter(., country %in% input$country)  else . } %>%
      {if (!is.null(input$province)) filter(., province %in% input$province)  else . } %>%
      {if (!is.null(input$present_at_tp1)) filter(., present_at_tp1 %in% input$province)  else . } %>%
      filter(present_at_tp1==0) %>%
      filter(tp1_cluster %in% (clusters_long_sh$data(withSelection = TRUE) %>% filter(selected_ | is.na(selected_)) %>% pull(tp1_cluster)))
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
    datatable(clusters_long_sh,
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
  
  
  ###############
  # cluster map #
  ###############
  
  # mapbox token 
  mapboxToken <- "pk.eyJ1Ijoic2FtLWEtbGVlIiwiYSI6ImNrb2s0bXVpbzFhMGkybm5zeHI1dHR1aTgifQ.1K8o7OaSOWo_y5UdVH348w"    # You need your own token
  Sys.setenv("MAPBOX_TOKEN" = mapboxToken)
  
  # leaflet built in color function
  pal <-  leaflet::colorFactor(
    palette = rep(cols25(n=25), 
                  length.out=length(unique(clusters_long$tp1_cluster))),
    domain = unique(clusters_long$tp1_cluster))
  
  # plotly map using mapbox
  output$cluster_map <- renderPlotly({
    
    plot_mapbox(mode = 'scattermapbox') %>%
      add_markers(data = clusters_long_sh,
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
  
  
  # plotly map for strains using mapbox
  output$strain_map <- renderPlotly({
    
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
  
  
  ######################################
  # polar plot for change in direction #
  ######################################
  
  # color pal for polar plot
  polarpal <-  leaflet::colorFactor(
    palette = ocean.phase(n=16),
    domain = unique(clusters_long$cardinal))
  
  # render polar plot
  output$cardinal_polar <- renderPlotly({
    
    # format data
    clusters_long_sh$data(withSelection = TRUE) %>%
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
                              "NW", "NNW")))) %>%
      # highlight specifications
      highlight(on="plotly_click",
                off="plotly_doubleclick")
  })
  
  
  #############################################
  # radial plot for describing change vectors #
  #############################################
  
  # render polar plot
  output$ecc_radar <- renderPlotly({
    
    clusters_long_sh$data(withSelection = T) %>%
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
  
  #######################
  # change vector graph #
  #######################
  
  output$change_vector <- renderPlotly({
    
    op <- clusters_long_sh$data(withSelection = T)
    op$opacity <- ifelse(op$selected_ | is.na(op$selected_), 1, 0.1)
    
    plot_ly(type = "scatter", mode="markers") %>% 
      #add segment to connect each point to origin
      add_trace(data = clusters_long_sh,
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
      add_trace(data = clusters_long_sh,
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
  
  
  #############################
  # bubble plots for ECC data #
  #############################
  
  output$bubbleplot <- renderPlotly({
    plot_ly() %>% 
      #add tp1 trace
      add_trace(data = clusters_long_sh,
                x = ~ecc_0.1.0, # geospatial ecc
                y = ~ecc_0.0.1, # temporal ecc
                type = 'scatter',
                mode = 'markers',
                legendgroup = ~tp1_cluster,
                color = ~I(pal(tp1_cluster)),
                name = ~paste(tp1_cluster, timepoint, sep="_"),
                size = ~cluster_size_2,
                sizes = c(1, 1000),
                opacity = 0.8, 
                marker = list(sizemode = "area")) %>%
      layout(xaxis = list(title = "Geospatial ECC value"),
             yaxis = list(title = "Temporal ECC value"))
  })
  
  
  ##########################
  # cluster ECC Histograms #
  ##########################
  
  # geo ecc histogram by time point
  output$geo_histogram <- renderPlotly({
    plot_ly(type = "histogram",
            data = clusters_long_sh,
            histfunc = "count",
            x = ~ecc_0.0.1,
            xbins = list(size = 0.05),
            color = ~timepoint) %>%
      layout(barmode = "group",
             xaxis = list(range = c(0, 1)))
    
  })
  
  # temp ecc histogram by time point
  output$temp_histogram <- renderPlotly({
    plot_ly(type = "histogram",
            data = clusters_long_sh,
            histfunc = "count",
            x = ~ecc_0.1.0,
            xbins = list(size = 0.05),
            color = ~timepoint) %>%
      layout(barmode = "group",
             xaxis = list(range = c(0, 1)))
    
  })
  
  # delta geo histogram
  output$delta_geo_histogram <- renderPlotly({
    plot_ly(type = "histogram",
            data = clusters_long_sh,
            histfunc = "count",
            xbins = list(size = 0.05),
            x = ~delta_ecc_0.0.1) %>%
      layout(barmode = "group",
             xaxis = list(range = c(-1, 1)))
  })
  
  #delta temp histogram 
  output$delta_temp_histogram <- renderPlotly({
    plot_ly(type = "histogram",
            data = clusters_long_sh,
            histfunc = "count",
            xbins = list(size = 0.05),
            x = ~delta_ecc_0.1.0) %>%
      layout(barmode = "group",
             xaxis = list(range = c(-1, 1)))
  })
  
  #delta temp histogram 
  output$delta_size_histogram <- renderPlotly({
    plot_ly(type = "histogram",
            xbins = list(size = 10),
            data = clusters_long_sh,
            histfunc = "count",
            x = ~delta_cluster_size) %>%
      layout(barmode = "overlay")
  })
  
  
  ###################
  # ridgeline plots #
  ###################
  
  output$ridgeplot <- renderPlotly({
    # base plot
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
                points = FALSE,
                marker = list(opacity = 0.4,
                              size = 8),
                hoveron = "violins+points") %>%  
      # tp2 traces
      add_trace(data = strains_sh2,
                y = ~tp1_cluster,
                x = ~strain_date,
                legendgroup = ~tp1_cluster,
                type = 'violin',
                side = 'positive',
                orientation = "h",
                scalemode = "count",
                color = ~I(pal(tp1_cluster)),
                name =  ~paste(tp1_cluster,2, sep="_"),
                points = FALSE,
                line = list(color = "black",
                            width = 1),
                marker = list(opacity = 0.4,
                              size = 8,
                              line = list(color = "black",
                                          width = 1)),
                hoveron = "violins+points") %>%
      layout(yaxis= list(showticklabels = FALSE,
                         title = "Count density"),
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
    
  })
  
  
  #########################
  # strain ECC Histograms #
  #########################
  
  # strain geo ecc histogram by time point
  output$strain_geo_histogram <- renderPlotly({

    if(input$geoSelect == 3) {
      ggplotly(ggplot(strains_sh$data(withSelection=T) %>%
                        filter(selected_ | is.na(selected_)) %>%
                        subset(country %in% input$geoProvince), 
                      aes(x=tp1_t0_ecc_0.1.0, fill=as.factor(timepoint))) +
                 geom_histogram(position="dodge", bins = 20) +
                 #geom_density(alpha=0.5) +
                 facet_wrap(~province) +
                 scale_fill_manual(values = c("#1c74b4", "#fc7c0d"),
                                   name = "Time point") +
                 xlim(0,1) +
                 theme_bw() +
                 theme(axis.text.x = element_text(angle=45, hjust=1),
                       axis.title = element_blank())) %>%
        layout(margin = list(l = 50, r = 100)) %>%
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
                        xshift = -55,
                        yref = "paper",
                        xref = "paper",
                        xanchor = "left",
                        yanchor = "middle",
                        textangle = -90,
                        showarrow = FALSE,
                        font = list(size = 14)) 
      
    } else if (input$geoSelect == 2) {
      ggplotly(ggplot(strains_sh, 
                      aes(x=tp1_t0_ecc_0.1.0, fill=as.factor(timepoint))) +
                 geom_histogram(position="dodge", bins = 20) +
                 scale_fill_manual(values = c("#1c74b4", "#fc7c0d"),
                                   name = "Time point") +
                 facet_wrap(~country) +
                 xlim(0,1) +
                 theme_bw() +
                 theme(axis.text.x = element_text(angle=45, hjust=1),
                       axis.title = element_blank())) %>%
        layout(margin = list(l = 50, r = 100)) %>%
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
                      aes(x=tp1_t0_ecc_0.1.0, fill=as.factor(timepoint))) +
                 geom_histogram(position="dodge", bins = 20) +
                 scale_fill_manual(values = c("#1c74b4", "#fc7c0d"),
                                   name = "Time point") +
                 xlim(0,1) +
                 theme_bw() +
                 theme(axis.text.x = element_text(angle=45, hjust=1),
                       axis.title = element_blank())) %>%
        layout(margin = list(l = 50, r = 100)) %>%
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
  
  # strain temp histogram by time point
  output$strain_temp_histogram <- renderPlotly({

    if(input$tempSelect == 3) {
      ggplotly(ggplot(strains_sh$data(withSelection=T) %>%
                        filter(selected_ | is.na(selected_)) %>%
                        subset(country %in% input$geoProvince), 
                      aes(x=tp1_t0_ecc_0.0.1, fill=as.factor(timepoint))) +
                 geom_histogram(position="dodge", bins = 20) +
                 #geom_density(alpha=0.5) +
                 facet_wrap(~province) +
                 scale_fill_manual(values = c("#1c74b4", "#fc7c0d"),
                                   name = "Time point") +
                 xlim(0,1) +
                 theme_bw() +
                 theme(axis.text.x = element_text(angle=45, hjust=1),
                       axis.title = element_blank())) %>%
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
      
    } else if (input$tempSelect == 2) {
      ggplotly(ggplot(strains_sh, 
                      aes(x=tp1_t0_ecc_0.0.1, fill=as.factor(timepoint))) +
                 geom_histogram(position="dodge", bins = 20) +
                 scale_fill_manual(values = c("#1c74b4", "#fc7c0d"),
                                   name = "Time point") +
                 facet_wrap(~country) +
                 xlim(0,1) +
                 theme_bw() +
                 theme(axis.text.x = element_text(angle=45, hjust=1),
                       axis.title = element_blank())) %>%
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
                       axis.title = element_blank())) %>%
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
    plot_ly(type = "histogram",
            data = strains,
            histfunc = "count",
            x = ~delta_ecc_0.1.0,
            xbins = list(size = 0.05)) %>%
      layout(barmode = "beside",
             xaxis = list(range = c(-1, 1)))
  })
  
  # strain delta temp ecc histogram by time point
  output$strain_delta_temp_histogram <- renderPlotly({
    plot_ly(type = "histogram",
            data = strains,
            histfunc = "count",
            x = ~delta_ecc_0.0.1,
            xbins = list(size = 0.05)) %>%
      layout(barmode = "beside",
             xaxis = list(range = c(-1, 1)))
  })
  
  
  ##############################
  # histogram for strain counts#
  ##############################
  
  output$strain_histogram_tp1 <- renderPlotly({
    
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
    
  })
  
  output$strain_histogram_tp2 <- renderPlotly({
    
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
  })
  
  
  #################################
  # cumulative strains identified #
  #################################
  
  # time point 1
  output$cum_strains_tp1 <- renderPlotly({
    
    # need to aggregate data
    strains_sh1_cumsum <- strains_sh$data(withSelection = T) %>%
      subset(timepoint == 1) %>%
      filter(selected_ | is.na(selected_)) %>%
      group_by(tp1_cluster, timepoint, country, strain_date) %>% 
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
                     axis.title = element_blank())) %>%
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
  })
  
  # time point 2
  output$cum_strains_tp2 <- renderPlotly({
    
    # need to aggregate data
    strains_sh2_cumsum <- strains_sh$data(withSelection = T) %>%
      subset(timepoint == 2) %>% 
      filter(selected_ | is.na(selected_)) %>%
      group_by(tp1_cluster, country, strain_date) %>% 
      tally(!is.na(strain)) %>% 
      mutate(cumsum = cumsum(n))
    
    # time point 2
    ggplotly(ggplot(data = strains_sh2_cumsum,
                    aes(x=strain_date, y = cumsum, color = tp1_cluster)) +
               scale_color_manual(values = pal(unique(strains_sh2_cumsum$tp1_cluster)),
                                  name = "Cluster") +
               geom_line() +
               facet_wrap(~country) +
               theme_bw() +
               theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
                     axis.title = element_blank())) %>%
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
}

shinyApp(ui, server)

#################
# things to add #
#################

# buttons for 3-4 "go-to" situations/problems
# cardinal plot reservered for bearing of clusters
# rename axis for cardinal plot for humans to understand how 
# clusters are spreading 
# heat map (later)

# 
# 
# df <- data.frame(x = c(1,2,3,4,5), y1 = c(3,3,3,3,3), y2 = c(6,6,6,6,6))
# 
# p <- plot_ly(df, type = 'scatter', mode = 'markers') %>%
#   add_trace(x = ~x, y = ~y1) %>%
#   add_trace(x = ~x, y = ~y2)
# 
# p <- p %>% layout(
#   title = "Button Restyle",
#   xaxis = list(domain = c(0.1, 1)),
#   yaxis = list(title = "y", range = c(0,10)),
#   updatemenus = list(
#     list(
#       type = "buttons",
#       y = 0.8,
#       buttons = list(
#         
#         list(method = "restyle",
#              args = list("visible", c(T,T)),
#              label = "All"),
#         
#         list(method = "restyle",
#              args = list("visible", c(F,T)),
#              label = "Trace 0"),
#         
#         
#         list(method = "restyle",
#              args = list("visible", c(T,F)),
#              label = "Trace 1")))
#   ))



# strains %>%
#   #group_by(country) %>%
#   plot_ly(data=.,
#           x = ~tp1_t0_ecc_0.0.1,
#           split = ~country,
#           color = ~timepoint,
#           type = "histogram",
#           nbinsx=10) %>%
#   layout(xaxis=list(range = c(0,1)),
#          barmode = "beside")
# 
# 
# strains %>%
#   #group_by(country) %>%
#   group_by(country) %>%
#   group_map(~ plot_ly(data=.,
#                       x = ~tp1_t0_ecc_0.0.1,
#                       split = ~country,
#                       color = ~timepoint,
#                       type = "histogram",
#                       nbinsx=10),
#             keep=T) %>%
#   subplot(nrows = 3, shareX = T, shareY=TRUE) %>%
#   layout(xaxis=list(range = c(0,1)),
#          barmode = "beside")
# 
# 

# panel <- . %>%
#   plot_ly(x = ~tp1_t0_ecc_0.0.1) %>%
#   add_trace(type = "histogram",
#             color = ~timepoint,
#             x = ~tp1_t0_ecc_0.0.1,
#             nbinsx=10) %>%
#   add_annotations(
#     text = ~unique(country),
#     x = 0.5,
#     y = 1,
#     yref = "paper",
#     xref = "paper",
#     yanchor = "bottom",
#     showarrow = FALSE,
#     font = list(size = 14)
#   ) %>%
#   layout(
#     shapes = list(
#       type = "rect",
#       x0 = 0,
#       x1 = 1,
#       xref = "paper",
#       y0 = 0,
#       y1 = 16,
#       yanchor = 1,
#       yref = "paper",
#       ysizemode = "pixel",
#       fillcolor = toRGB("gray80"),
#       line = list(color = "transparent")
#     )
#   )
# 
# 
# strains %>%
#   group_by(country) %>%
#   do(p = panel(.)) %>%
#   subplot(nrows = 3, shareX = TRUE)