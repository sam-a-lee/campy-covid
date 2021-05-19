################ 
# General info #
################

# May 12, 2021
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
# libary(leaflet.extras)
library(maps) # for drawing maps/getting map data
library(RColorBrewer) # for prettier colours
library(reactable) # nested interable dables
#library(shinyBS)
library(crosstalk) # for talk between reactable and leaflet




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

eccdata$timepoint <- ifelse(eccdata$present_at_tp1==1, "tp1", "tp2")

# make column for strain date
eccdata$strain_date <- paste(eccdata$day, eccdata$month, eccdata$year, sep = "-")

# remove month, day, year and city
eccdata <- subset(eccdata,select = -c(month, day, year, city))

# initialize colorpal as null
colorpal <- NULL

###################################
# user interface of the shiny app #
###################################

# Define UI for application that draws a histogram
ui <- fluidPage(
    

    
)






#################################################################### 
# sever logic: the plots and function that the shiny app will call #
####################################################################


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    
    
    
    ############################
    # create shared data frame #
    ############################
    
    eccdata_sd <- SharedData$new(eccdata)

    wr_table <- reactable(eccdata_sd,
                          pagination = FALSE,
                          compact = TRUE,
                          # Group by for the aggregation
                          groupBy = c("tp1_cluster", "country"),
                          
                          theme = reactableTheme(
                              # set a default theme for font across table
                              style = list(fontFamily = "Fira Mono")
                          )
    )
    
    
    wr_table
    
    bscols(
        # bootstrap is built off a 12 wide grid system,
        # so we have 1/6 and 5/6 width below
        widths = c(2, 10),
        list(
            # Create shiny-esque filters
            # Note that we are defining:
            # a name for the filter
            # a display name for the filter
            # a shared data object
            # and a column of interest (w/ a ~)
            filter_select("tp1_cluster", "TP1_cluster", eccdata_sd, ~tp1_cluster),
            filter_select("coountry", "Total Country", eccdata_sd, ~country),
            filter_slider("province", "Province", eccdata_sd, ~province)
        ),
        # add our table next to the filters
        wr_table
    )
    
    
    
    
    
    
}


#######################
# Run the application #
#######################
shinyApp(ui = ui, server = server)

