################ 
# General info #
################

# May 7, 2021
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
library(leaflet.minicharts) # for better mini charts on map
library(here) # for clear dir calling
library(tidyverse) # tidy data/ kind of used
library(leaflet) # for map visualizations
library(maps) # for drawing maps


#######################
# load and clean data #
#######################

#Load pre-processed data with epi-cluster cohesion info.
eccdata <- read_xlsx(here("input_data", "2021-05-03_0.Europe_1st wave.9000_Merged_strain_results.xlsx"))

eccdata <- eccdata[,-c(32:35)]
# rename columns
colnames(eccdata) <- c("strain", "country", "province", "city", "latitude", 
                       "longitude", "day", "month", "year", "present_at_tp1", 
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

# remove columns that dont contain useful information
test <- as.data.frame(eccdata %>% select( -c(day, month, year,
                               country, province, city,
                               tp1_t0_ecc_0.1.0, tp1_t0_ecc_0.0.1,
                               tp2_t0_ecc_0.1.0, tp2_t0_ecc_0.0.1,
                               delta_ecc_0.1.0, delta_ecc_0.0.1, 
                               delta_cluster_size,
                               num_additional_tp1_strains_tp2,
                               num_novel_tp2_strains,
                               overall_cluster_growth_rate,
                               cluster_novel_growth_rate)))

###################################
# user interface of the shiny app #
###################################

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Epi-cluster cohesian data"),
    
    # Side bar with a sliders for data selection
    sidebarLayout(
        
        # side bar
        sidebarPanel(
            
            # drop down menu to select cluster 
            pickerInput("cluster",
                        "Select cluster:", 
                        choices = unique(test$tp1_cluster), 
                        selected = "TP1_h0_c001",
                        options = list(
                            "max-options" = 10,
                            "max-options-text" = "Maximum 10 selected clusters!"),
                        multiple = T)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput("map")
        )
    )
)


#################################################################### 
# sever logic: the plots and function that the shiny app will call #
####################################################################


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    #########################################
    # plot a reactive map of covid clusters #
    #########################################
    
    output$map <- renderLeaflet({
        
        # susbet data according for unique strains
        strain <- test %>% 
            # subset by cluster
            subset(tp1_cluster %in% input$cluster) 
        
        # subset data for unique tp1 clusters
        tp1 <- test %>% 
            # subset by cluster
            subset(tp1_cluster %in% input$cluster) %>%
            # select only unique tp1 info
            distinct(avg_tp1_date, .keep_all = T)
        
        # subset data for unique tp1 clusters
        tp2 <- test %>% 
            # subset by cluster
            subset(tp1_cluster %in% input$cluster) %>%
            # subset unique tp2 strains
            distinct(tp2_cluster, .keep_all = T)
    
        
        # set region for map
        # for now, whole world
        region <- map(regions = ".", fill = F, plot = F)
        
        # map the data using leaflet
        leaflet(data=region) %>% 
        
            # set base zoom over mid asia/europesubstr(as.character(test$avg_tp1_date),1,7)
            setView(lng = 60, lat = 50, zoom = 2) %>%
            
            # load tile for selected region
            addTiles("http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}") %>%
            
            #add circles that correspond to tp1 centroids
            addCircleMarkers(lat = tp1$avg_tp1_latitude,
                             lng = tp1$avg_tp1_longitude,
                             radius = log10(as.numeric(tp1$avg_tp1_geo_dist_km))*10,
                             fillColor = "blue",
                             fillOpacity = 0.4,
                             stroke=F) %>%
            
            # add circles that correspond to tp1 strains
            addCircleMarkers(lat = strain$latitude,
                             lng = strain$longitude,
                             radius = 5,
                             fillColor = "yellow",
                             fillOpacity = 0.5,
                             stroke=F) %>%
            
            # add circles that correspond to tp2  cluster centroids
             addCircleMarkers(lat = tp2$avg_tp2_latitude,
                             lng = tp2$avg_tp2_longitude,
                             radius = log10(as.numeric(tp2$avg_tp2_geo_dist_km))*10,
                             fillColor = "green",
                             fillOpacity = 0.4,
                             stroke=F)
        
    })
    
}


#######################
# Run the application #
#######################
shinyApp(ui = ui, server = server)
