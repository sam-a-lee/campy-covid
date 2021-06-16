################ 
# General info #
################

# May 26, 2021
# Samantha Lee

# The purpose of this script is to generate a map of epi-cluster cohesian data
# data plots data at time point 1 and time point 2 simulateanously 
# to show how the geographic centroid of clusters changes 

###############################
# load the required libraries #
###############################

library(readxl) # for reading xlsx files 
library(shiny) # for running shiny apps
library(shinyWidgets) # for better slider/dropdown options
library(here) # for clear dir calling
library(tidyverse) # tidy data/ kind of used
library(leaflet) # for map visualizations
library(maps) # for drawing maps/getting map data
library(RColorBrewer) # for prettier colours
library(reactable) # nested interable dables
library(crosstalk) # for talk between reactable and leaflet
library(htmltools)

#######################
# load and clean data #
#######################

# input an input_data/data in the app when uploading
# load pre-processed data with epi-cluster cohesion info.
eccdata <- read_xlsx(here("input_data", "2021-05-03_0.Europe_1st wave.9000_Merged_strain_results.xlsx"))

# these dont have info 
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

# make column for strain date instead of having three separate columns
eccdata$strain_date <- paste(eccdata$day, 
                             eccdata$month, 
                             eccdata$year, 
                             sep = "-")

# remove month day year and city
eccdata <- subset(eccdata, select = -c(month, day, year, city, type))


###############################
# create columsn for plotting #
###############################

# problem with data:
# info is kind of wide and long which makes it difficult to plot 
# clusters and strains without modifying format
# instead of creating a super long data frame (which would not be ideal for 
# data sets with millions of observations)
# create additional latitude and longitude columns for 
# plotting that can be NA'd out as needed

# create additional latitude and longitude for tp1 and tp2 strains and clusters
eccdata$tp2_strain_latitude_plot <- eccdata$strain_latitude
eccdata$tp2_strain_longitude_plot <- eccdata$strain_longitude
eccdata$tp1_strain_latitude_plot <- eccdata$strain_latitude
eccdata$tp1_strain_longitude_plot <- eccdata$strain_longitude
eccdata$avg_tp1_latitude_plot <- eccdata$avg_tp1_latitude
eccdata$avg_tp2_latitude_plot <- eccdata$avg_tp2_latitude
eccdata$avg_tp1_longitude_plot <- eccdata$avg_tp1_longitude
eccdata$avg_tp2_longitude_plot <- eccdata$avg_tp2_longitude


# na out information that does not pertain to tp1 or tp1 strains
eccdata_na <- apply(eccdata, 1, function(x){
    # create NAs in tp1 data for tp2 strains
    if(x["present_at_tp1"]==0){
        x[c("tp1_strain_latitude_plot", "tp1_strain_longitude_plot")] <- NA
    } 
    # create na in tp2 strains for tp1 
    else if(x["present_at_tp1"]==1){
        x[c("tp2_strain_latitude_plot", "tp2_strain_longitude_plot")] <- NA
    } 
    # catch if neither of these conditions exist
    else {print("uh oh")}
    
    # return df
    return(x)
})

# transpose and convert to df
eccdata_nat <- as.data.frame(t(eccdata_na))


##############################################
# create tp1 and tp2 strain specific columns #
##############################################

####### subset out unique tp1 cluster info 
tp1ecc <- eccdata %>% 
    distinct(tp1_cluster, .keep_all = T) 



# na out information that does not pertain to tp1 clusters
tp1ecc_na <- apply(tp1ecc, 1, function(x){
    
    # null out info that is not important 
    x[c("country", "province", 
        "tp1_strain_latitude_plot", "tp1_strain_longitude_plot",
        "tp2_strain_latitude_plot", "tp2_strain_longitude_plot",
        "avg_tp2_latitude_plot", "avg_tp2_longitude_plot")] <- NA
    
    # set strain equal to tp1 cluster name
    x["strain"]  <- x["tp1_cluster"]
    
    # return df
    return(x)
})


# transpose and convert to df
tp1ecc_nat <- as.data.frame(t(tp1ecc_na))


####### subset out unique tp2 cluster info 
tp2ecc <- eccdata %>% 
    distinct(tp2_cluster, .keep_all = T) 

# na out information that does not pertain to tp1 clusters
tp2ecc_na <- apply(tp2ecc, 1, function(x){
    
    if(x["present_at_tp1"]==1){
        # null out info that is not important 
        x[c("country", "province", 
            "tp1_strain_latitude_plot", "tp1_strain_longitude_plot",
            "tp2_strain_latitude_plot", "tp2_strain_longitude_plot",
            "avg_tp1_latitude_plot", "avg_tp1_longitude_plot")] <- NA
        
        # set strain equal to tp1 cluster name
        x["strain"]  <- x["tp2_cluster"]
        
    } else if(x["present_at_tp1"]==0){
        # null out info that is not important 
        x[c("country", "province", 
            "tp1_strain_latitude_plot", "tp1_strain_longitude_plot",
            "tp2_strain_latitude_plot", "tp2_strain_longitude_plot",
            "avg_tp1_latitude_plot", "avg_tp1_longitude_plot")] <- NA
        
        # set strain equal to tp1 cluster name
        x["strain"]  <- x["tp2_cluster"]
        
        
    } else {
        print("ugh oh")
    }

    # return df
    return(x)
})


# transpose and convert to df
tp2ecc_nat <- as.data.frame(t(tp2ecc_na))


###########################
# rbind new data together #
###########################

ecc_all <- rbind(eccdata_nat, tp1ecc_nat, tp2ecc_nat)

################################
# add jitter to lats and longs #
################################

ecc_all <- ecc_all %>%
    mutate(tp1_strain_latitude_jit = jitter(as.numeric(tp1_strain_latitude_plot),10,1)) %>%
    mutate(tp1_strain_longitude_jit = jitter(as.numeric(tp1_strain_longitude_plot),10,1)) %>%
    mutate(tp2_strain_latitude_jit = jitter(as.numeric(tp2_strain_latitude_plot),10,1)) %>%
    mutate(tp2_strain_longitude_jit = jitter(as.numeric(tp2_strain_longitude_plot),10,1))


#################################
# type convert selected columns #
#################################

# # covert appropriate columns to numeric
 ecc_all[,c(4:6, 8:11, 13:17, 19:22, 24:34, 36:47)] <-
     sapply(ecc_all[,c(4:6, 8:11, 13:17, 19:22, 24:34, 36:47)], as.numeric)

ecc_all <- as.data.frame(ecc_all)

ecc_all$tp1_cluster <- as.character(ecc_all$tp1_cluster)

colorpal <- NULL
#######################
# shared data for all #
#######################

# create shared data frame
# everything will use this
sd <- SharedData$new(ecc_all)


###################################
# user interface of the shiny app #
###################################

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Interactive SARS-CoV-2 epi-cluster cohesion (ECC) data"),
    
    
    
    # data is divided among tables
    # the main tab includes a subsettable map
    tabsetPanel(
        # map tab with map and table
        tabPanel("Map",
                 
                 # map at top
                 # map row at top
                 fluidRow(
                     column(3,
                            wellPanel(
                                # select transparency of strains
                                sliderInput("strain_transparency",
                                            "Select transparency for strain plotting:",
                                            min = 0,
                                            max = 100,
                                            step = 1,
                                            value = 50),
                                
                                # select transparency of clusters
                                sliderInput("centroid_transparency",
                                            "Select transparency for cluster plotting:",
                                            min = 0,
                                            max = 100,
                                            step = 1,
                                            value = 40),
                                
                                selectInput("centroid_radius",
                                            "Centroid radius representation:",
                                            choices = c("Avg geo dist",
                                                        "Tot num strain"),
                                            selected = "Average geospatial distance", 
                                            multiple = FALSE),
                                
                                #checkboxInput("legend", "Show legend", TRUE)
                                )),
                     
                     column(9, leafletOutput("map"))
                 ),
                 
                 # data at bottom
                 fluidRow(column(3,
                                 wellPanel(
                                     #cluster selection
                                     filter_select(id = "cluster", 
                                                   label = "TP1 clusters",
                                                   sharedData =sd, 
                                                   group = ~tp1_cluster),
                                     
                                     # cluster selection
                                     filter_select(id = "country", 
                                                   label = "Country",
                                                   sharedData =sd, 
                                                   group = ~country),
                                     
                                     # province selection
                                     filter_select(id = "province",
                                                   label = "Province:",
                                                   sharedData =sd, 
                                                   group = ~province)
                                     
                                 )
                 ),
                 column(9,
                        reactableOutput("ecctable")
                 )
                 )
        ),
        # filtered data tab
        # same as data displayed in map tab, but not grouped
        tabPanel("Filtered data", 
                 reactableOutput("tp1_filtered_data")),
        
        # raw data tab
        # the data frame used to create data
        # outputted after modifications made in app
        tabPanel("Raw data",
                 reactableOutput("raw_data"))
    )
)



####################
# server interface #
####################

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    
    test <- reactive(sd$data(withSelection = TRUE) %>%
        filter(selected_))
    
    
    output$map <- renderLeaflet({

        # leaflet
        leaflet() %>%
            
            # load tile for selected region
            addTiles("http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}") %>%
            
            # set base zoom over mid asia/europe
            setView(lng = 60, lat = 50, zoom = 2) %>%
            
            # control layer for toggling strains and centroids on/off
            # never changes so can be part of base map
            addLayersControl(overlayGroups = c("TP1 centroid", "TP2 centroid",
                                               "TP1 strains", "TP2 strains"),
                             options = layersControlOptions(collapsed = TRUE)) %>%
            
            addCircleMarkers(data = sd,
                             lat =  ~as.numeric(as.character(avg_tp1_latitude_plot)),
                             lng =  ~as.numeric(as.character(avg_tp1_longitude_plot)),
                             #radius = {if(input$centroid_radius=="Avg geo dist") ~log10(as.numeric(avg_tp1_geo_dist_km))*10 else ~log10(as.numeric(tp1_cluster_size))*10},
                             fillColor = "blue",
                             fillOpacity = 50,
                             stroke = T,
                             opacity = 50,
                             weight = 1,
                             color = "white",
                             group = "TP1 centroid") %>%
            
            addCircleMarkers(data = sd,
                             lat =  ~as.numeric(as.character(avg_tp2_latitude_plot)),
                             lng =  ~as.numeric(as.character(avg_tp2_longitude_plot)),
                             #radius = {if(input$centroid_radius=="Avg geo dist") ~log10(as.numeric(avg_tp1_geo_dist_km))*10 else ~log10(as.numeric(tp1_cluster_size))*10},
                             fillColor = "red",
                             fillOpacity = 50,
                             stroke = T,
                             opacity = 50,
                             weight = 1,
                             color = "white",
                             group = "TP2 centroid")
            
      
            
    })
    
    
       
    
    
    ###################################################################
    # reactable that appears below map and updates on user selections #
    ###################################################################
    
    # create a table using the shared data
    output$ecctable <- renderReactable({
        reactable(data = sd,
                  pagination = TRUE,
                  defaultPageSize = 10,
                  showPageSizeOptions = TRUE,
                  pageSizeOptions = c(10, 25, 50, 100),
                  paginationType = "numbers",
                  compact = TRUE,
                  sortable = T,
                  showSortIcon = T,
                  showSortable = T,
                  highlight = T,
                  groupBy = c("tp1_cluster", "country", "province"))
    })
    
    
    ####################################################
    # raw data filtered table to be displayed in a tab #
    #####################################################
    
    output$raw_data <- renderReactable({
        reactable(data = eccdata,
                  filterable = TRUE,
                  searchable = TRUE,
                  sortable = TRUE,
                  showSortIcon = TRUE,
                  showSortable = TRUE,
                  pagination = TRUE,
                  highlight = T,
                  defaultPageSize = 10,
                  showPageSizeOptions = TRUE,
                  pageSizeOptions = c(10, 25, 50, 100),
                  paginationType = "numbers")
    })
    
    
    ##############################################
    # filtered data to be displayed in a new tab #
    ##############################################
    
    # displayed in new tab without grouping
    output$tp1_filtered_data <- renderReactable({
        reactable(data = sd,  
                  filterable = TRUE,
                  searchable = TRUE,
                  sortable = TRUE,
                  highlight = T,
                  showSortIcon = TRUE,
                  showSortable = TRUE,
                  pagination = TRUE,
                  defaultPageSize = 10,
                  showPageSizeOptions = TRUE,
                  pageSizeOptions = c(10, 25, 50, 100),
                  paginationType = "numbers")
    })
    
}
    

#######################
# Run the application #
#######################
shinyApp(ui = ui, server = server)
    

##############
# scrap code #
##############

# callback_js <- JS(
#     "table.on('click', 'tr.dtrg-group', function () {",
#     "  var rowsCollapse = $(this).nextUntil('.dtrg-group');",
#     "  $(rowsCollapse).toggleClass('hidden');",
#     "});"
# )
# 
# output$ecctable <- DT::renderDataTable({
#     datatable(
#         data = sd,
#         extensions = c("RowGroup", "ColReorder"),
#         callback = callback_js,
#         options = list(rowGroup = list(dataSrc = c(7,2)),
#                        pageLength = 20,
#                        colReorder = TRUE),
#         selection = 'none'
#     )
# }, server=F)




