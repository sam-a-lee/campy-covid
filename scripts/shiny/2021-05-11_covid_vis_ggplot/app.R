################ 
# General info #
################

# May 10, 2021
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
library(leaflet) # for map visualizations - shitty colors
library(maps) # for drawing maps
library(RColorBrewer) # for prettier colours
library(plotly) # for ggplot compatible maps
library(ggmap) # maps drawn using ggmap - part of ggplot


#######################
# load and clean data #
#######################

# load pre-processed data with epi-cluster cohesion info.
eccdata <- read_xlsx(here("input_data", "2021-05-03_0.Europe_1st wave.9000_Merged_strain_results.xlsx"))

# remove non-informative cols
eccdata <- eccdata[,-c(32:35)]

# rename columns
# remove spaces, all lowercase
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


colorpal <- data.frame(tp1_cluster = rep(NA,9), colour = brewer.pal(9, "Set1"))

###################################
# user interface of the shiny app #
###################################

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("SARS-CoV-2 epi-cluster cohesion (ECC) data"),
    
    # Side bar with a sliders for data selection
    sidebarLayout(
        
        # left side bar panel
        sidebarPanel(
            
            # drop down menu to select cluster 
            pickerInput("cluster",
                        "Select cluster:", 
                        choices = levels(as.factor(eccdata$tp1_cluster)), 
                        selected = "TP1_h0_c001",
                        options = list(
                            "max-options" = 8,
                            "max-options-text" = "Maximum 8 selected clusters!"),
                        multiple = T),
            
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
                        value = 40)
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
    
    ############
    # coloring #
    ############
    
    in_pal <- input$cluster %in% colorpal$tp1_cluster
    
    # if all inputs are new
    if(sum(in_pal)==0) {
        colorpal$tp1_cluster <- NA
        colorpal$tp1_cluster <- 
            ifelse(length(input$cluster) < 9,
                   c(input$cluster,rep(NA, 9-length(input$cluster))), 
                     input$cluster )
    }
    
    # if some inputs are old
    if(sum(in_pal) > 0) {
        
        # find out position of exisiting cluster(s) in colorpal
        pal_pos <-  which(colorpal$tp1_cluster %in% input$cluster)
        
        # set other positions to NA
        colorpal$tp1_cluster[!pal_pos] <- NA
        
        # get NA positions
        na_pos <- which(is.na(colorpal$tp1_cluster))
        
        colorpal$tp1_cluster[na_pos] <- 
            ifelse(length(input$cluster[!input$cluster %in% colorpal$tp1_cluster]) < length(na_pos),
                   c(input$cluster[!input$cluster %in% colorpal$tp1_cluster],
                     rep(NA, length(na_pos)-length(input$cluster[!input$cluster %in% colorpal$tp1_cluster]))),
                   input$cluster[!input$cluster %in% colorpal$tp1_cluster])
    } 
    
    
    
    ############################################
    # reactive subsetting of data for plotting #
    ############################################
    
    # data subset for TP1 strains
    tp1_strains <- reactive({
        # a cluster must be selected
        req(input$cluster)
        
        # subset data for tp1 strains
        eccdata %>% 
            # subset by cluster
            subset(tp1_cluster %in% input$cluster) %>%
            subset(present_at_tp1==1) %>%
            mutate(color = colorpal[colorpal$tp1_cluster==tp1_cluster, colorpal$colour])
    })
    
    
    
    # data subset for TP1 clusters
    tp1_centroid <- reactive({
        
        # a cluster must be selected
        req(input$cluster)
        
        # subset data for tp1 clusters
        eccdata %>% 
            # subset by cluster
            subset(tp1_cluster %in% input$cluster) %>%
            # select only unique tp1 info
            distinct(avg_tp1_date, .keep_all = T)
    })
    
    # data subset for TP2 strains
    tp2_strains <- reactive({
        
        # a cluster must be selected
        req(input$cluster)
        
        # subset data for tp2 strains
        eccdata %>% 
            # subset by cluster
            subset(tp1_cluster %in% input$cluster) %>%
            subset(present_at_tp1==0)
    })
    
    # data subset for TP2 clusters
    tp2_centroid <- reactive({
        
        # a cluster must be selected
        req(input$cluster)
        
        # subset data for tp1 clusters
        eccdata %>% 
            # subset by cluster
            subset(tp1_cluster %in% input$cluster) %>%
            # subset unique tp2 strains
            distinct(tp2_cluster, .keep_all = T)
    })
    
    
    ############ 
    # base map #
    ############
    
    plot_mapbox(maps::world.cities)
}


#######################
# Run the application #
#######################
shinyApp(ui = ui, server = server)
