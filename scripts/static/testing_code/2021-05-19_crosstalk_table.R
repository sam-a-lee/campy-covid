
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




eccshare <- SharedData$new(eccdata)

ecctable <- reactable(eccshare,
                      pagination = FALSE,
                      compact = TRUE,
                      # Group by for the aggregation
                      groupBy = c("tp1_cluster", "country"),
                      
                      theme = reactableTheme(
                        # set a default theme for font across table
                        style = list(fontFamily = "Fira Mono")
                      )
)




bscols(
  # bootstrap is built off a 12 wide grid system,
  widths = c(3, 9),
  # shiny like filters for data
  # can add more as needed
  list(
    filter_select("tp1_cluster", "TP1_cluster", eccshare, ~tp1_cluster),
    filter_select("coountry", "Country", eccshare, ~country),
    filter_select("province", "Province", eccshare, ~province)
  ),
  # add our table next to the filters
  ecctable
)
