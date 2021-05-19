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

ecc_all <- eccdata
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
        tabPanel("Map",
                 
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
                            
                            checkboxInput("legend", "Show legend", TRUE))),
                     
                     column(9,leafletOutput("map"))
                 ),
                 
                 # data at bottom
                 fluidRow(

                     column(3,
                            wellPanel(
                                # cluster selection
                                selectizeInput("cluster",
                                               "TP1 clusters:", 
                                               choices = levels(as.factor(eccdata$tp1_cluster)), 
                                               options = list(maxItems = 8L),
                                               multiple = T),
                                
                                # cluster selection
                                selectizeInput("country",
                                               "Country:", 
                                               choices = levels(as.factor(eccdata$country)), 
                                               multiple = T),
                                
                                #uiOutput("slider")
                                # render ui in interface
                                
                                # province selection
                                selectizeInput("province",
                                               "Province:", 
                                               choices = levels(as.factor(eccdata$province)), 
                                               multiple = T)
                                
                            )),
                     column(9,
                            reactableOutput("ecctable")
                     )
                     
                 )
                 
        ), 
        tabPanel("Raw data",reactableOutput("raw_data")), 
        tabPanel("Filtered data", reactableOutput("tp1_filtered_data"))
    )
    
)


####################
# server interface #
####################

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    ###################################
    # rendering the UI based on input #
    ###################################
    
    observe({
        updateSelectizeInput(session, 
                             "cluster", 
                             selected = isolate(input$cluster), 
                             options = list(maxItems = 8L - length(input$clusters)))
    })
    
  
    #####################
    # data manipulation #
    #####################

    ####################### ecc data for table ########################
    
    # filter based
    ecc_all <- reactive({
        
        eccdata %>% 
            {if (!is.null(input$cluster)) filter(., tp1_cluster %in% input$cluster) else .} %>% 
            {if (!is.null(input$country)) filter(., country %in% input$country) else .} %>%
            {if (!is.null(input$province)) filter(., province %in% input$province) else .}
            #{if (FALSE) select(., cyl) else .}
    })
    
    # using the reactive filtered data frame
    # as a shared object between table and map
    ecc_all_share <- SharedData$new(ecc_all, group="ecc")
    
    
    # filter for tp1 cluster data
    # filter eccdata based on user selections in UI
    tp1_clust <- reactive({
        ecc_all() %>% 
            subset(timepoint=="tp1") %>%
            distinct(avg_tp1_date, .keep_all = T)
    })
    
    # an ecc share for tp1 clusters
    tp1_clust_share <- SharedData$new(tp1_clust, group = "ecc")
    
    
    # filter for tp1 strain data
    # filter eccdata based on user selections in UI
    tp1_strain <- reactive({
        ecc_all() %>% 
            subset(timepoint=="tp1") %>%
            {if (nrow(.) >= 1) aggregate(.~province, data = ., unique) else .} %>% 
            mutate(num_tp1_strains = lengths(strain))
    })
    
    # an ecc share for tp1 clusters
    tp1_strain_share <- SharedData$new(tp1_strain, group = "ecc")
    
    # filter for tp2 cluster data
    # filter eccdata based on user selections in UI
    tp2_clust <- reactive({
        ecc_all() %>% 
            distinct(tp2_cluster, .keep_all = T) %>%
            subset(timepoint=="tp1")
    })
    
    # an ecc share for tp1 clusters
    tp2_clust_share <- SharedData$new(tp2_clust, group = "ecc")
    
    # filter for tp2 cluster data
    # filter eccdata based on user selections in UI
    tp2_strain <- reactive({
        ecc_all() %>% 
            subset(timepoint=="tp2") %>% 
            {if (nrow(.) >= 1) aggregate(.~province, data = ., unique) else .} %>% 
            mutate(num_tp2_strains = lengths(strain)) 
    })
    
    # an ecc share for tp1 clusters
    tp2_strain_share <- SharedData$new(tp2_strain, group = "ecc")
    
    ###################################################################
    # reactable that appears below map and updates on user selections #
    ###################################################################
    
    # create a table using the shared data 
    output$ecctable <- renderReactable({
        reactable(data = ecc_all_share,
                  pagination = FALSE,
                  compact = TRUE,
                  sortable = T,
                  showSortIcon = T,
                  showSortable = T,
                  # Group by for the aggregation
                  groupBy = c("tp1_cluster", "country"),
                  theme = reactableTheme(
                      # set a default theme for font across table
                      style = list(fontFamily = "Fira Mono")))
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
                             options = layersControlOptions(collapsed = TRUE)) %>%
            
            hideGroup("Average centroid")
    })
    
    
    ##########################################
    # add circle markers to base leaflet map #
    ##########################################

    toListen <- reactive({
        list(input$cluster, input$country, input$province)
    })
    
    observeEvent(toListen(),{
        
        req(input$cluster)
        
        ################### color pal function start #################
        
        # this should be moved into a function eventually
        # if some inputs are old
        if(sum(as.character(input$cluster) %in% as.character(colorpal$tp1_cluster)) > 0) {
            
            # find out position of exisiting cluster(s) in colorpal
            pal_pos <- colorpal[colorpal$tp1_cluster %in% input$cluster, "rownum"]
            
            # set other positions to NA
            colorpal[!colorpal$rownum %in% pal_pos, "tp1_cluster"] <- as.character(NA)
            
            # reorder according to rownum
            colorpal <- colorpal[order(colorpal$rownum),]
            
            # get NA positions
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
        
        ################### color pal function end #################
      
        
        # leaflet map with markers
        leafletProxy("map") %>%
            
            # clear old shapes
            clearMarkers() %>%
            
            # add circles that correspond to tp1 centroids
            addCircleMarkers(data = tp1_clust_share,
                             lat =  ~avg_tp1_latitude,
                             lng =  ~avg_tp1_longitude,
                             radius = ~log10(as.numeric(avg_tp2_geo_dist_km))*10,
                             fillColor = ~unname(unlist(sapply(tp1_cluster, function(x) {colorpal %>% subset(tp1_cluster==x) %>% select(colour)}, simplify="vector"))),
                             fillOpacity = input$centroid_transparency/100,
                             stroke = T,
                             opacity = input$centroid_transparency/100,
                             weight = 1,
                             color = "white",
                             group = "TP1 centroid") %>%
            
            # add circles that correspond to tp2  cluster centroids
            addCircleMarkers(data = tp2_clust_share,
                             lat = ~as.numeric(avg_tp2_latitude),
                             lng = ~as.numeric(avg_tp2_longitude),
                             radius = ~log10(as.numeric(avg_tp2_geo_dist_km))*10,
                             fillColor = ~unname(unlist(sapply(tp1_cluster, function(x) {colorpal %>% subset(tp1_cluster==x) %>% select(colour)}, simplify="vector"))),
                             fillOpacity = input$centroid_transparency/100,
                             stroke = T,
                             opacity = input$centroid_transparency/100,
                             weight = 1,
                             color = "black",
                             group = "TP2 centroid") %>%
            
            # add circles that correspond to tp1 strains
            addCircleMarkers(data = tp1_strain_share,
                             lat = ~as.numeric(as.character(latitude)),
                             lng = ~as.numeric(as.character(longitude)),
                             radius = 5,
                             fillColor = ~unname(unlist(sapply(tp1_cluster, function(x) {colorpal %>% subset(tp1_cluster==x) %>% select(colour)}, simplify="vector"))),
                             fillOpacity = input$strain_transparency/100,
                             stroke = T,
                             opacity = input$strain_transparency/100,
                             weight = 1,
                             color = "white",
                             group = "TP1 strains") %>%
            
            addCircleMarkers(data = tp2_strain_share,
                             lat = ~as.numeric(as.character(latitude)),
                             lng = ~as.numeric(as.character(longitude)),
                             radius = 5,
                             fillColor = ~unname(unlist(sapply(tp1_cluster, function(x) {colorpal %>% subset(tp1_cluster==x) %>% select(colour)}, simplify="vector"))),
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
        
        # require input cluster to change to update legend
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
        reactable(data = ecc_all_share,  
                  filterable = TRUE,
                  searchable = TRUE,
                  sortable = TRUE,
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
