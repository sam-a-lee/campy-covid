

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
library(reactable)
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


#######################################
# convert cluster info to long format #
#######################################

# now reshape the data for tp1 and tp2 info
clusters_long <- melt(setDT(clusters), 
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


# conver to dataframe
clusters_long <- as.data.frame(clusters_long)

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
clusters_long$key <- as.numeric(rownames(clusters_long))
strains$key <- as.numeric(rownames(strains))


############################
# shared data for clusters #
############################

# shared dfs with all data
clusters_long_sh1 <- SharedData$new(clusters_long[clusters_long$timepoint == 1, ], key = ~key, group = "clusters_long")


test <- filter_slider(id = "tp1_cluster_size_2", label = "tp1_cluster_size_2", sharedData = clusters_long_sh1, column = ~cluster_size_2)

# pal
pal <-  colorFactor(
    palette = rep(cols25(n=25), 
                  length.out=length(unique(clusters_long$tp1_cluster))),
    domain = unique(clusters_long$tp1_cluster))

#plot_ly(type = 'scatter', mode = 'markers') %>%
bubbleplot <- plot_ly() %>%
    add_trace(data = clusters_long_sh1,
              x = ~ecc_0.0.1,
              y = ~ecc_0.1.0,
              type = 'scatter',
              mode = 'markers',
              color = ~tp1_cluster,
              colors = ~pal(tp1_cluster),
              name = ~tp1_cluster,
              size = ~cluster_size_2,
              sizes = c(1, 1000),
              marker = list(sizemode = "area")) %>% 
    highlight(on = "plotly_click",
              off = "plotly_doubleclick")


#############################
# USER INTERFACE/CLINT SIDE #
#############################

header <- dashboardHeader(title = "ECC Vis")

# the dashboard side bar
sidebar <- dashboardSidebar(
    sidebarMenu(id = 'sidebarmenu',
                
                # Visualization tab
                menuItem("Visualizations", tabName = "visualizations")
                )
)

# the dashboard body
body <- dashboardBody(
    tabItems(
        tabItem(tabName = "visualizations",
                
                # tab title
                h2("ECC Visualizations"),
                
                # row 1 box for map and bubble plot
                fluidRow(

                    #bubble plot
                    box(title = "Bubble plot", 
                        width = 6, 
                        bubbleplot,
                        collapsible = TRUE)
                ),
                
                # row 4 for cluster data and filters
                fluidRow(
                    
                    # cluster filters 
                    box(title = "Cluster filters", 
                        width = 3,
                        
                         test, 
                        # uiOutput("slider"),
                        # make collapsible 
                        collapsible = TRUE),
                    
                    # cluster data
                    box(title = "Cluster data", 
                        width = 9, 
                        dataTableOutput("clusters_dt"),
                        #reactableOutput("clusters_dt"),
                        collapsible = TRUE)   
                )
                
        )
        
    )
    
)

ui <- dashboardPage(header, sidebar, body)


server <- function(input, output, session) { 
    

    # 
    # output$select <- renderUI({
    #     filter_select(id = "tp1_cluster", label = "tp1_cluster", sharedData = clusters_long_sh1, group = ~tp1_cluster, multiple = T)
    # }, server = F)
    # 
    # output$slider <- renderUI({
    #     filter_slider(id = "tp1_cluster_size_2", label = "tp1_cluster_size_2", sharedData = clusters_long_sh1, column = ~cluster_size_2)
    # }, server = F)
    # 
    # clusters
    output$clusters_dt <- renderDataTable({
        datatable(clusters_long_sh1,
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
    
    
    # pal
    pal <-  colorFactor(
        palette = rep(cols25(n=25), 
                      length.out=length(unique(clusters_long$tp1_cluster))),
        domain = unique(clusters_long$tp1_cluster))
    
    
    #############################
    # bubble plots for ECC data #
    #############################


    
    
    
}

shinyApp(ui, server)

