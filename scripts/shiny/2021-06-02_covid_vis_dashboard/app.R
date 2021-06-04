# COVID VIS SHINU DASHBOARD


################ 
# General info #
################

# June 2, 2021
# Samantha Lee

# The purpose of this script is to generate a map of epi-cluster cohesian data
# data plots data at time point 1 and time point 2 simulateanously 
# to show how the geographic centroid of clusters changes 


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
library(reactable) # nested interable dables
library(plotly) # for interactive ggplots
library(crosstalk) # for shazred data
library(DT)
library(htmltools) # for reactable searching 


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


##########################################
# create columns for jittered locations  #
##########################################

# create additional latitude and longitude for tp1 and tp2 strains and clusters
# 12-13 decimal places in lats and longs
# too long, round to four for our purposes

eccdata <- eccdata %>%
    mutate(strain_latitude_jit = round(jitter(as.numeric(strain_latitude),10,1),digits=4)) %>%
    mutate(strain_longitude_jit = round(jitter(as.numeric(strain_longitude),10,1),digits=4))

# make column for strain date instead of having three separate columns
eccdata$strain_date <- paste(eccdata$day, 
                             eccdata$month, 
                             eccdata$year, 
                             sep = "-")

eccdata$strain_date <- gsub(" ", "", eccdata$strain_date)

eccdata <- eccdata %>% mutate(tp1_stain_time_diff = as.numeric(abs(as.Date("01-01-2020", format = "%d-%m-%y") - as.Date(strain_date, format = "%d-%m-%y"))))
eccdata <- eccdata %>% mutate(tp2_stain_time_diff = as.numeric(abs(as.Date("01-01-2020", format = "%d-%m-%y") - as.Date(strain_date, format = "%d-%m-%y"))))


#############################
# remove uninformative data #
#############################

# remove month day year and city
eccdata <- subset(eccdata, select = -c(month, day, year, city))

# remove 0 for now as its causing problems
eccdata <- eccdata %>% subset(tp1_cluster != 0 )


#################################
# type convert selected columns #
#################################

# covert appropriate columns to numeric
eccdata[,c(4:6, 8:11, 13:17, 19:22, 24:36, 37:38)] <- 
    sapply(eccdata[,c(4:6, 8:11, 13:17, 19:22, 24:36, 37:38)], as.numeric)

# time diff columns for ridgeline
eccdata$tp1_cluster <- as.factor(eccdata$tp1_cluster)
eccdata$province <- as.factor(eccdata$province)
eccdata$country <- as.factor(eccdata$country)


#################
# crosstalk key #
#################

# create unique row ids for crosstalk
eccdata$key <- as.numeric(rownames(eccdata))


# initialize colorpal as null
colorpal <- NULL


######################
# color pal function #
######################


color.pal <- function(clusters, colorpal) {
    
    uniq_clusters <- unique(as.character(clusters))
    # this should be moved into a function eventually
    # if some inputs are old
    if(sum(as.character(uniq_clusters) %in% as.character(colorpal$tp1_cluster)) > 0) {
        
        
        cat(file=stderr(), "modifying old colours", "\n")
        # find out position of exisiting cluster(s) in colorpal
        pal_pos <- colorpal[colorpal$tp1_cluster %in% uniq_clusters, "rownum"]
        
        # set other positions to NA
        colorpal[!colorpal$rownum %in% pal_pos, "tp1_cluster"] <- as.character(NA)
        
        # reorder according to rownum
        colorpal <- colorpal[order(colorpal$rownum),]
        
        # get NA positions
        na_pos <- colorpal[is.na(colorpal$tp1_cluster), "rownum"]
        
        colorpal$tp1_cluster[na_pos] <- 
            as.character(c(uniq_clusters[!uniq_clusters %in% colorpal$tp1_cluster],
                           as.character(rep(NA, length(na_pos) - length(uniq_clusters[!uniq_clusters %in% colorpal$tp1_cluster])))))
        
        colorpal <<- colorpal
        
        return(colorpal)
        
    } 
    
    
    # if all inputs are new
    if(is.null(colorpal)) {
        
        # new pal 
        colorpal <- data.frame(tp1_cluster = as.character(rep(NA,length(unique(eccdata$tp1_cluster)))), 
                               colour = rep(brewer.pal(9, "Set1"), length.out = length(unique(eccdata$tp1_cluster))), 
                               rownum = 1:length(unique(eccdata$tp1_cluster)))
        
        # assign new tp1 clusters
        colorpal$tp1_cluster <- as.character(c(uniq_clusters, as.character(rep(NA,length(unique(eccdata$tp1_cluster))-length(uniq_clusters)))))
        
        colorpal <<- colorpal
        
        return(colorpal)
        cat(file=stderr(), "all new colours!", "\n")
        
        
    }
    
    # if no colours selected
    if(length(clusters)==0){
        colorpal <- NULL
        colorpal <<- colorpal
        return(colorpal)
    }
    
}





#############################
# USER INTERFACE/CLINT SIDE #
#############################

header <- dashboardHeader(title = "ECC Vis")

sidebar <- dashboardSidebar(
    sidebarMenu(id = 'sidebarmenu',
                
                # Visualization tab
                menuItem("Visualizations", tabName = "visualizations"),
                
                # Visulization controls
                menuItem("Map controls", tabName = "visualizationControls",
                         
                         # cluster transparency 
                         sliderInput("centroid_transparency",
                                     "Cluster transparency:",
                                     min = 0,
                                     max = 100,
                                     step = 1,
                                     value = 50),
                         
                         # strain transparency
                         sliderInput("strain_transparency",
                                     "Strain transparency:",
                                     min = 0,
                                     max = 100,
                                     step = 1,
                                     value = 50),
                         
                         # cluster radius
                         selectInput("centroid_radius",
                                     "Cluster radius:",
                                     choices = c("Avg geo dist",
                                                 "Tot num strain"),
                                     selected = "Avg geo dist", 
                                     multiple = FALSE),
                         
                         checkboxInput("legend", "Show legend", TRUE)
                         
                ),
                
         
                # filtered data using in visualizations
                menuItem("Filtered data", tabName = "filteredData"),
                
                # raw data used to generate visualizations
                menuItem("Raw data", tabName = "rawData"))
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "visualizations",
                
                # tab title
                h2("ECC Visualizations"),
                
                # row 1 box for map and ridgeline plot
                fluidRow(
                    box(title = "Map plot", 
                        width = 6, 
                        leafletOutput("map")),
                    
                    box(title = "Ridgeline plot", 
                        width = 6, 
                        plotlyOutput("ridgeplot"))
                ),
                
                # row 2 for histogram and bubble plot
                fluidRow(
                    box(title = "Bubble plot", 
                        width = 6, 
                        plotlyOutput("bubbleplot")),
                    
                    box(title = "Histogram", 
                        width = 6, 
                        "Box content")
                ),
                
                # row 3 for cluster and strain data
                fluidRow(
                    box(title = "Cluster data", 
                        width = 12, 
                        #reactableOutput("clustertable")
                        dataTableOutput("dt")
                        )
                )
        ),
        
        # tab for filtered data
        tabItem(tabName = "filteredData",
                h2("Filtered data"),
                fluidRow(
                    box(width = 12, 
                        reactableOutput("filtered_data"))
                )
        ),
        
        # table for raw data
        tabItem(tabName = "rawData",
                h2("Raw data"),
                fluidRow(
                    box(width = 12,
                        reactableOutput("raw_data")
                        )
                )
        )
    )
    
)

ui <- dashboardPage(header, sidebar, body)



###############
# SERVER SIDE #
###############

server <- function(input, output, session) { 

    
    #############################
    # create shared data frames #
    #############################

    # all data
    shared_ecc_all <- SharedData$new(eccdata, key = ~key, group = "ecc")
    # tp1 strains
    shared_ecc_tp1 <- SharedData$new(eccdata[eccdata$present_at_tp1 == 1,], key = ~key, group = "ecc")
    # tp2 strains
    shared_ecc_tp2 <- SharedData$new(eccdata[eccdata$present_at_tp1 == 0,], key = ~key, group = "ecc")
    
    
    #################################################################
    # cluster and strain reactable tables that appear on first page #
    #################################################################
   
    
    output$dt <- renderDataTable({

        datatable(
            shared_ecc_all, extensions = c('Select', 'Buttons','Scroller',
                                           'RowGroup'), 
            options = list(
                select = list(style = 'os', items = 'row'),
                dom = 'Blfrtip',
                rowId = 0,
                buttons = c('selectAll', 'selectNone', 'selectRows', 'selectColumns', 'selectCells'),
                deferRender = TRUE,
                scrollY = 500,
                scrollX = 600,
                scroller = TRUE,
                rowGroup = list(dataSrc = 7)
            ),
            selection = 'none',
            filter = "top",
        )
        
        
    }, server=F)

    
    
    ###########
    # COLOURS #
    ###########
    
    
    ####################
    # base leaflet map #
    ####################
    
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
            
            # tp1 strain markers
            addCircleMarkers(data = shared_ecc_tp1$data(withSelection = TRUE, withFilter = TRUE) %>% subset(selected_ == T),
                             lat = ~strain_latitude_jit,
                             lng = ~strain_longitude_jit,
                             radius = 5,
                             fillColor = "blue",
                             fillOpacity = .50,
                             stroke = T,
                             opacity = .50,
                             weight = 1,
                             color = "white",
                             group = "TP1 strains") %>% 
            
            
            # tp2 strain markers
            addCircleMarkers(data = shared_ecc_tp2$data(withSelection = TRUE, withFilter = TRUE) %>% subset(selected_ == T),
                             lat = ~strain_latitude_jit,
                             lng = ~strain_longitude_jit,
                             radius = 5,
                             fillColor = "blue",
                             fillOpacity = .50,
                             stroke = T,
                             opacity = .50,
                             weight = 1,
                             color = "black",
                             group = "TP2 strains") %>%
            
            # tp1 centroids 
            addCircleMarkers(data = shared_ecc_all$data(withSelection = TRUE, withFilter = TRUE) %>% 
                                 subset(selected_ == T) %>% 
                                 group_by(tp1_cluster) %>% 
                                 summarise(avg_tp1_longitude = mean(avg_tp1_longitude), 
                                           avg_tp1_latitude = mean(avg_tp1_latitude)),
                             lat = ~avg_tp1_latitude,
                             lng = ~avg_tp1_longitude,
                             radius = 20,
                             fillColor = "red", 
                             fillOpacity = .50,
                             stroke = T,
                             opacity = .50,
                             weight = 1,
                             color = "white",
                             group = "TP1 centroids") %>% 
            
            # tp2 centroids
            addCircleMarkers(data = shared_ecc_all$data(withSelection = TRUE, withFilter = TRUE) %>% 
                                 subset(selected_ == T) %>% 
                                 group_by(tp2_cluster) %>% 
                                 summarise(avg_tp2_longitude_2 = mean(avg_tp2_longitude), 
                                           avg_tp2_latitude_2 = mean(avg_tp2_latitude)),
                             lat = ~avg_tp2_latitude_2,
                             lng = ~avg_tp2_longitude_2,
                             radius = 20,
                             fillColor = "blue", 
                             fillOpacity = .50,
                             stroke = T,
                             opacity = .50,
                             weight = 1,
                             color = "black",
                             group = "TP2 centroids")
    })
    
    
    ###################
    # ridgeline plots #
    ###################
    
    
    output$ridgeplot <- renderPlotly({
        
        ridge <- plot_ly(type = 'violin')

        ridge <- ridge %>%
            add_trace(data = shared_ecc_tp2$data(withSelection = TRUE, withFilter = TRUE) %>% subset(selected_ == T), 
                      x = ~tp2_stain_time_diff,
                      y = ~tp1_cluster,
                      split = ~tp1_cluster,
                      type = 'violin',
                      side = 'positive',
                      orientation = "h",
                      scalemode = "count",
                      name =  ~tp2_cluster,
                      meanline = list(
                          visible = F
                      ),
                      points = "all",
                      line = list(color = "black",
                                  width = 1),
                      marker = list(opacity = 0.4,
                                    size = 10,
                                    line = list(color = "black",
                                                width = 1))
            )
        
        ridge <- ridge %>%
            add_trace(data = shared_ecc_tp1$data(withSelection = TRUE, withFilter = TRUE) %>% subset(selected_ == T),
                x = ~tp1_stain_time_diff,
                y = ~tp1_cluster,
                split = ~tp1_cluster,
                type = 'violin',
                side = 'positive',
                orientation = "h",
                scalemode = "count",
                meanline = list(
                    visible = F
                ),
                points = "all",
                marker = list(opacity = 0.4,
                              size = 10)
            )

        ridge

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
                  wrap = FALSE, 
                  defaultPageSize = 10,
                  showPageSizeOptions = TRUE,
                  pageSizeOptions = c(10, 25, 50, 100),
                  paginationType = "numbers")
    })
    
    
    ##############################################
    # filtered data to be displayed in a new tab #
    ##############################################
    
    # displayed in new tab without grouping
    output$filtered_data <- renderReactable({
        reactable(data = shared_ecc_all,  
                  filterable = TRUE,
                  searchable = TRUE,
                  sortable = TRUE,
                  highlight = T,
                  showSortIcon = TRUE,
                  showSortable = TRUE,
                  pagination = TRUE,
                  wrap = FALSE,
                  defaultPageSize = 10,
                  showPageSizeOptions = TRUE,
                  pageSizeOptions = c(10, 25, 50, 100),
                  paginationType = "numbers")
    })
    
    
}

shinyApp(ui, server)