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
    titlePanel("SARS-CoV-2 epi-cluster cohesion (ECC) data"),
    
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
                            "max-options" = 8,
                            "max-options-text" = "Maximum 8 selected clusters!"),
                        multiple = T),
            
            # drop down menu to select cluster 
            pickerInput("cluster",
                        "Select cluster:", 
                        choices = unique(test$tp1_cluster), 
                        selected = "TP1_h0_c001",
                        options = list(
                            "max-options" = 8,
                            "max-options-text" = "Maximum 8 selected clusters!"),
                        multiple = T)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput("map")
        )
    )
)