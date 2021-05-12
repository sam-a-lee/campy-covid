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
library(leaflet.minicharts) # for better mini charts on map
library(here) # for clear dir calling
library(tidyverse) # tidy data/ kind of used
library(leaflet) # for map visualizations
library(maps) # for drawing maps
library(RColorBrewer) # for prettier colours


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

# initialize colorpal as null
colorpal <- NULL

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
                        value = 40),
            
            checkboxInput("legend", "Show legend", TRUE)
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
    
    ############################################
    # reactive subsetting of data for plotting #
    ############################################
    
    # data subset avg centroid info
    avg_centroid <- reactive({
        
        # a cluster must be selected
        req(input$cluster)
        
        eccdata %>% 
            # subset by cluster
            subset(tp1_cluster %in% input$cluster) %>% 
            distinct(avg_tp1_date, .keep_all = T) %>%
            group_by(present_at_tp1) %>% 
            subset(present_at_tp1==1)
        
    })
    
    # data subset for TP1 strains
    tp1_strains <- reactive({
        
        # a cluster must be selected
        req(input$cluster)
        
        # subset data for tp1 strains
        eccdata %>% 
            # subset by cluster
            subset(tp1_cluster %in% input$cluster) %>%
            subset(present_at_tp1==1)
        
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
            distinct(avg_tp1_date, .keep_all = T) %>%
            subset(present_at_tp1==1) 
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
            distinct(tp2_cluster, .keep_all = T) %>%
            subset(present_at_tp2==1)
    })
    
    
    #################### 
    # base leaflet map #
    ####################
    
    output$map <- renderLeaflet({
        
        # set region for map - whole world
        region <- map(regions = ".", fill = F, plot = F)
        
        # leaflet
        leaflet(data=region) %>% 
            
            # load tile for selected region
            addTiles("http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}") %>%
            
            # set base zoom over mid asia/europe
            setView(lng = 60, lat = 50, zoom = 2) %>%
            
            # control layer for toggling strains and centroids on/off
            # never changes so can be part of base map
            addLayersControl(overlayGroups = c("TP1 centroid", "TP2 centroid",
                                               "TP1 strains", "TP2 strains", "Average centroid"),
                             options = layersControlOptions(collapsed = TRUE))
    })
    
    
    ####################################################
    # reactive plotting of clusters and strains on map #
    ####################################################
    
    observe({
        
        ###############################
        # custom colouring of strains #
        ###############################
        
        # if some inputs are old
        if(sum(as.character(input$cluster) %in% as.character(colorpal$tp1_cluster)) > 0) {
            
            # find out position of exisiting cluster(s) in colorpal
            #pal_pos <-  colorpal$tp1_cluster %in% input$cluster
            pal_pos <- colorpal[colorpal$tp1_cluster %in% input$cluster, "rownum"]
            
            # set other positions to NA
            #colorpal$tp1_cluster[!pal_pos] <- as.character(NA)
            colorpal[!colorpal$rownum %in% pal_pos, "tp1_cluster"] <- as.character(NA)
            
            # reorder according to rownum
            colorpal <- colorpal[order(colorpal$rownum),]
            
            # get NA positions
            #na_pos <- is.na(colorpal$tp1_cluster)
            na_pos <- colorpal[is.na(colorpal$tp1_cluster), "rownum"]
            
            colorpal$tp1_cluster[na_pos] <- 
                as.character(c(input$cluster[!input$cluster %in% colorpal$tp1_cluster],
                               as.character(rep(NA, length(na_pos) - length(input$cluster[!input$cluster %in% colorpal$tp1_cluster])))))
            
            colorpal <<- colorpal
            colorpal_nona <<- na.omit(colorpal)
            
            cat(file=stderr(), "modifying old colours", "\n")
        } 
        
        
        # if all inputs are new
        if(is.null(colorpal)) {
            
            # new pal 
            colorpal <- data.frame(tp1_cluster = as.character(rep(NA,9)), colour = brewer.pal(9, "Set1"), rownum = 1:9)
            
            # assign new tp1 clusters
            colorpal$tp1_cluster <- as.character(c(input$cluster, as.character(rep(NA, 9-length(input$cluster)))))
            
            colorpal <<- colorpal
            colorpal_nona <<- na.omit(colorpal)
            
            cat(file=stderr(), "all new colours!", "\n")
        }
        
        # if no colours selected
        if(length(input$cluster)==0){
            colorpal <- NULL
            colorpal <<- colorpal
        }
        
        
        leafletProxy("map") %>%
            
            # clear old shapes
            clearMarkers() %>%
            
            
            # add circles that is the average of tp1 and tp2
            # dashed stroke to indicate average 
            addCircleMarkers(lat = (((avg_centroid()$avg_tp1_latitude) + (avg_centroid()$avg_tp2_latitude))/2),
                             lng = (((avg_centroid()$avg_tp1_longitude) + (avg_centroid()$avg_tp2_longitude))/2),
                             radius = ((log10(as.numeric(avg_centroid()$avg_tp1_geo_dist_km))*10) + (log10(as.numeric(avg_centroid()$avg_tp2_geo_dist_km))*10))/2,
                             fillColor = unname(unlist(sapply(avg_centroid()$tp1_cluster, function(x) {colorpal %>% subset(tp1_cluster==x) %>% select(colour)}, simplify="vector"))),
                             fillOpacity = input$centroid_transparency/100,
                             stroke = T,
                             opacity = input$centroid_transparency/100,
                             weight = 1,
                             color = "black",
                             dashArray = "4",
                             group = "Average centroid") %>%
            
            #add circles that correspond to tp1 centroids
            addCircleMarkers(#data = tp1_centroid(),
                lat =  tp1_centroid()$avg_tp1_latitude,
                lng =  tp1_centroid()$avg_tp1_longitude,
                radius = log10(as.numeric(tp1_centroid()$avg_tp1_geo_dist_km))*10,
                fillColor =  unname(unlist(sapply(tp1_centroid()$tp1_cluster, function(x) {colorpal %>% subset(tp1_cluster==x) %>% select(colour)}, simplify="vector"))),
                fillOpacity = input$centroid_transparency/100,
                stroke = T,
                opacity = input$centroid_transparency/100,
                weight = 1,
                color = "white",
                group = "TP1 centroid") %>%
            
            # add circles that correspond to tp1 strains
            addCircleMarkers(#data = tp1_strains(),
                lat = tp1_strains()$latitude,
                lng = tp1_strains()$longitude,
                radius = 5,
                fillColor = unname(unlist(sapply(tp1_strains()$tp1_cluster, function(x) {colorpal %>% subset(tp1_cluster==x) %>% select(colour)}, simplify="vector"))),
                fillOpacity = input$strain_transparency/100,
                stroke = T,
                opacity = input$strain_transparency/100,
                weight = 1,
                color = "white",
                group = "TP1 strains") %>%
            
            # add circles that correspond to tp2  cluster centroids
            addCircleMarkers(lat = tp2_centroid()$avg_tp2_latitude,
                             lng = tp2_centroid()$avg_tp2_longitude,
                             radius = log10(as.numeric(tp2_centroid()$avg_tp2_geo_dist_km))*10,
                             fillColor = unname(unlist(sapply(tp2_centroid()$tp1_cluster, function(x) {colorpal %>% subset(tp1_cluster==x) %>% select(colour)}, simplify="vector"))),
                             fillOpacity = input$centroid_transparency/100,
                             stroke = T,
                             opacity = input$centroid_transparency/100,
                             weight = 1,
                             color = "black",
                             group = "TP2 centroid") %>%
            
            # add circles that correspond to tp2 strains
            addCircleMarkers(lat = tp2_strains()$latitude,
                             lng = tp2_strains()$longitude,
                             radius = 5,
                             fillColor = unname(unlist(sapply(tp2_strains()$tp1_cluster, function(x) {colorpal %>% subset(tp1_cluster==x) %>% select(colour)}, simplify="vector"))),
                             fillOpacity = input$strain_transparency/100,
                             stroke = T,
                             opacity = input$strain_transparency/100,
                             weight = 1,
                             color = "black",
                             group = "TP2 strains") 
        
        
    })
    
    
    ###############################
    # add reactive legend to plot #
    ###############################
    
    observe({
        
        # require input to change to updat map
        req(input$cluster)
        
        # proxy for leaflet map
        proxy <- leafletProxy("map")
        
        # Remove any existing legend, and only if the legend is
        # enabled, create a new one.
        proxy %>% clearControls()
        if (input$legend) {
            proxy %>% 
                addLegend(group = "legend", 
                          "bottomleft", 
                          colors = as.character(colorpal_nona$colour), 
                          labels = as.character(colorpal_nona$tp1_cluster),
                          opacity = 1) 
        }
        
    })
    
}


#######################
# Run the application #
#######################
shinyApp(ui = ui, server = server)
