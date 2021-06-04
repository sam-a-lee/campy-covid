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
#library(htmltools) # for reactable searching 




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


##############################################
# create tp1 and tp2 strain specific columns #
##############################################

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

# make column for strain date instead of having three separate columns
eccdata$strain_date <- paste(eccdata$day, 
                             eccdata$month, 
                             eccdata$year, 
                             sep = "-")

eccdata$strain_date <- gsub(" ", "", eccdata$strain_date)

eccdata <- eccdata %>% mutate(tp1_stain_time_diff = as.numeric(abs(as.Date("01-01-2020", format = "%d-%m-%y") - as.Date(strain_date, format = "%d-%m-%y"))))
eccdata <- eccdata %>% mutate(tp2_stain_time_diff = as.numeric(abs(as.Date("01-01-2020", format = "%d-%m-%y") - as.Date(strain_date, format = "%d-%m-%y"))))


# na out information that does not pertain to tp1 or tp1 strains
eccdata_na <- apply(eccdata, 1, function(x){
    # create NAs in tp1 data for tp2 strains
    if(x["present_at_tp1"]==0){
        x[c("tp1_strain_latitude_plot", "tp1_strain_longitude_plot",  
            "avg_tp1_longitude_plot", "avg_tp1_latitude_plot", 
            "avg_tp2_latitude_plot", "avg_tp2_longitude_plot", "tp1_stain_time_diff") ] <- NA
    } 
    # create na in tp2 strains for tp1 
    else if(x["present_at_tp1"]==1){
        x[c("tp2_strain_latitude_plot", "tp2_strain_longitude_plot",
            "avg_tp1_longitude_plot", "avg_tp1_latitude_plot", 
            "avg_tp2_latitude_plot", "avg_tp2_longitude_plot", "tp2_stain_time_diff")] <- NA
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

#subset out unique tp2 cluster info 
tp2ecc <- eccdata %>% 
    distinct(tp2_cluster, .keep_all = T) 

# na out information that does not pertain to tp1 clusters
tp2ecc_na <- apply(tp2ecc, 1, function(x){
    x[c("country", "province", 
        "tp1_strain_latitude_plot", "tp1_strain_longitude_plot",
        "tp2_strain_latitude_plot", "tp2_strain_longitude_plot",
        "avg_tp1_latitude_plot", "avg_tp1_longitude_plot")] <- NA
    
    # set strain equal to tp1 cluster name
    x["strain"]  <- x["tp2_cluster"]
    # return df
    return(x)
})

# transpose and convert to df
tp2ecc_nat <- as.data.frame(t(tp2ecc_na))


###########################
# rbind new data together #
###########################

ecc_all <- rbind(eccdata_nat, tp1ecc_nat, tp2ecc_nat)


#############################
# remove uninformative data #
#############################

# remove month day year and city
ecc_all <- subset(ecc_all, select = -c(month, day, year, city))


################################
# add jitter to lats and longs #
################################

ecc_all <- ecc_all %>%
    mutate(tp1_strain_latitude_jit = round(jitter(as.numeric(tp1_strain_latitude_plot),10,1),digits=4)) %>%
    mutate(tp1_strain_longitude_jit = round(jitter(as.numeric(tp1_strain_longitude_plot),10,1),digits=4)) %>%
    mutate(tp2_strain_latitude_jit = round(jitter(as.numeric(tp2_strain_latitude_plot),10,1),digits=4)) %>%
    mutate(tp2_strain_longitude_jit = round(jitter(as.numeric(tp2_strain_longitude_plot),10,1),digits=4))

# 12-13 decimal places in lats and longs
# too long, round to four for our purposes


#################################
# type convert selected columns #
#################################

# covert appropriate columns to numeric
ecc_all[,c(4:6, 8:11, 13:17, 19:22, 24:43, 45:50)] <- 
    sapply(ecc_all[,c(4:6, 8:11, 13:17, 19:22, 24:43, 45:50)], as.numeric)


#################
# crosstalk key #
#################

# create unique row ids for crosstalk
ecc_all$key <- as.numeric(rownames(ecc_all))


# initialize colorpal as null
colorpal <- NULL

# time diff columns for ridgeline
ecc_all$tp1_cluster <- as.factor(ecc_all$tp1_cluster)
ecc_all$province <- as.factor(ecc_all$province)
ecc_all$country <- as.factor(ecc_all$country)

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
                
                # Data filtering options for visualizations
                menuItem("Data filtering", tabName = "dataFiltering",
                         
                         # cluster paramters to filter on 
                         menuItem("Cluster parameters", tabName = "clusterParameters",
                                  
                                  selectizeInput("cluster",
                                                 "TP1 cluster(s):",
                                                 choices = sort(unique(ecc_all$tp1_cluster)),
                                                 options = list(maxItems = 8L),
                                                 multiple = T),
                                  
                                  
                                  sliderInput("tp1_cluster_size",
                                              "TP1 cluster size:",
                                              min = 0,
                                              max = max(na.omit(ecc_all$tp1_cluster_size_2)),
                                              step = 5,
                                              value = c(0,max(na.omit(ecc_all$tp1_cluster_size_2)))),
                                  
                                  sliderInput("tp2_cluster_size",
                                              "TP2 cluster size:",
                                              min = 0,
                                              max = max(na.omit(ecc_all$tp2_cluster_size_2)),
                                              step = 5,
                                              value = c(0,max(na.omit(ecc_all$tp2_cluster_size_2)))),
                                  
                                  # tp1 geospatial ecc
                                  sliderInput("tp1_ecc_0.1.0",
                                              "TP1 geospatial ECC:",
                                              min = 0,
                                              max = 1,
                                              step = 0.01,
                                              value = c(0,1)),
                                  
                                  # tp1 temporal ecc
                                  sliderInput("tp1_ecc_0.0.1",
                                              "TP1 temporal ECC:",
                                              min = 0,
                                              max = 1,
                                              step = 0.01,
                                              value = c(0,1)),
                                  
                                  # tp2 geospatial ecc
                                  sliderInput("tp2_ecc_0.1.0",
                                              "TP2 geospatial ECC:",
                                              min = 0,
                                              max = 1,
                                              step = 0.01,
                                              value = c(0,1)),
                                  
                                  # tp2 temporal ecc
                                  sliderInput("tp2_ecc_0.0.1",
                                              "TP2 temporal ECC:",
                                              min = 0,
                                              max = 1,
                                              step = 0.01,
                                              value = c(0,1)),
                                  
                                  # delta geospatial ecc
                                  sliderInput("delta_ecc_0.1.0",
                                              "Delta geospatial ECC:",
                                              min = -1,
                                              max = 1,
                                              step = 0.01,
                                              value = c(-1,1)),
                                  
                                  # delta temporal ecc
                                  sliderInput("delta_ecc_0.0.1",
                                              "Delta temporal ECC:",
                                              min = -1,
                                              max = 1,
                                              step = 0.01,
                                              value = c(-1,1))
                         ),
                         
                         # strain paramters to filter on 
                         menuItem("Strain parameters", tabName = "strainParameters",
                                  
                                  # cluster selection
                                  selectizeInput("country",
                                                 "Country:",
                                                 choices = sort(unique(ecc_all$country)),
                                                 multiple = T),
                                  
                                  # province selection
                                  selectizeInput("province",
                                                 "Province:",
                                                 choices = sort(unique(ecc_all$province)),
                                                 multiple = T))
                         
                         
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

    
    ###################################
    # rendering the UI based on input #
    ###################################
    
    # update number of available selections for cluster (max 8)
    observe({
        updateSelectizeInput(session,
                             "cluster",
                             selected = isolate(input$cluster),
                             options = list(maxItems = 8L - length(input$clusters)))
    })
    
    
    ######################################
    # data filtering based on selections #
    ######################################
    
    # table of filtered data used for plotting
    # and creation of other dependent tables 
    ecc_all_filtered <- reactive({
        
        ecc_all %>% 
            # if selection is not null then filter on it
            {if (!is.null(input$cluster)) filter(., tp1_cluster %in% input$cluster) else .} %>% 
            {if (!is.null(input$country)) filter(., country %in% input$country) else .} %>%
            {if (!is.null(input$province)) filter(., province %in% input$province) else .} #%>% 
            # filter(.,  tp1_t0_ecc_0.1.0 >= input$tp1_ecc_0.1.0[1] & tp1_t0_ecc_0.1.0 <= input$tp1_ecc_0.1.0[2])  %>%
            # filter(.,  tp1_t0_ecc_0.0.1 >= input$tp1_ecc_0.0.1[1] & tp1_t0_ecc_0.0.1 <= input$tp1_ecc_0.0.1[2])  %>%
            # filter(.,  tp2_t0_ecc_0.1.0 >= input$tp2_ecc_0.1.0[1] & tp2_t0_ecc_0.1.0 <= input$tp2_ecc_0.1.0[2])  %>%
            # filter(.,  tp2_t0_ecc_0.0.1 >= input$tp2_ecc_0.0.1[1] & tp2_t0_ecc_0.0.1 <= input$tp2_ecc_0.0.1[2])  %>%
            # filter(.,  delta_ecc_0.1.0 >= input$delta_ecc_0.1.0[1] & delta_ecc_0.1.0 <= input$delta_ecc_0.1.0[2])  %>%
            # filter(.,  delta_ecc_0.0.1 >= input$delta_ecc_0.0.1[1] & delta_ecc_0.0.1 <= input$delta_ecc_0.0.1[2])  %>%
            # filter(.,  tp1_cluster_size_2 >= input$tp1_cluster_size[1] & tp1_cluster_size_2 <= input$tp1_cluster_size[2])  %>%
            # filter(.,  tp2_cluster_size_2 >= input$tp2_cluster_size[1] & tp2_cluster_size_2 <= input$tp2_cluster_size[2])
    })
    
    #############################
    # create shared data frames #
    #############################
    
    shared_ecc_all_filtered <- SharedData$new(ecc_all_filtered, key = ~key, group = "ecc")

    
    #################################################################
    # cluster and strain reactable tables that appear on first page #
    #################################################################
    
    # table for cluster info  
    # output$clustertable <- renderReactable({
    #     
    #     # this is hacky but gets this table to recognize when filters change
    #     ph <- ecc_all_filtered()
    # 
    #     reactable(data = shared_ecc_all_filtered,
    #               sortable = TRUE, 
    #               compact = TRUE,
    #                         selection = "multiple",
    #                         onClick = "select",
    #                         theme = reactableTheme(rowSelectedStyle = list(boxShadow = "inset 4px 0 0 0 #ffa62d")),
    #               resizable = TRUE,
    #               highlight = TRUE,
    #               showSortIcon = TRUE,
    #               showSortable = TRUE,
    #               pagination = TRUE,
    #               defaultPageSize = 10,
    #               showPageSizeOptions = TRUE,
    #               pageSizeOptions = c(10, 25, 50, 100),
    #               paginationType = "numbers",
    #               showPageInfo = TRUE,
    #               defaultSorted = "tp1_cluster",
    #               groupBy = c("tp1_cluster"), 
    #               columns = list(tp1_cluster = colDef(name = "TP1 cluster"),
    #                              tp2_cluster = colDef(name = "TP2 cluster"),
    #                              avg_tp1_latitude = colDef(name = "TP1 cluster latitude"),
    #                              avg_tp1_longitude = colDef(name = "TP1 cluster longitude" ),
    #                              avg_tp1_geo_dist_km = colDef(name = "Average distance between TP1 strains (km)"),
    #                              avg_tp1_temporal_dist_days = colDef(name = "Average time between TP1 strains (days)"),
    #                              avg_tp2_latitude = colDef(name = "TP2 cluster latitude"),
    #                              avg_tp2_longitude = colDef(name = "TP2 cluster longitude"),
    #                              avg_tp2_geo_dist_km = colDef(name = "Average distance between TP2 strains (km)"),
    #                              avg_tp2_temporal_dist_days = colDef(name = "Average time between TP2 strains (days)"),
    #                              tp1_t0_ecc_0.1.0 = colDef(name = "TP1 cluster geospatial ECC"),
    #                              tp1_t0_ecc_0.0.1 = colDef(name = "TP1 cluster temporal ECC"),
    #                              tp2_t0_ecc_0.1.0 = colDef(name = "TP2 cluster geospatial ECC"),
    #                              tp2_t0_ecc_0.0.1 = colDef(name = "TP2 cluster temporal ECC"),
    #                              delta_ecc_0.1.0 = colDef(name = "Delta geospatial ECC"),
    #                              delta_ecc_0.0.1 = colDef(name = "Delta temporal ECC"),
    #                              tp1_cluster_size_2 = colDef(name = "TP1 cluster size"),
    #                              tp2_cluster_size_2 = colDef(name = "TP2 cluster size"),
    #                              delta_cluster_size = colDef(name = "Delta cluster size"),
    #                              num_additional_tp1_strains_tp2 = colDef(name = "Number of additional TP1 strains in TP2 cluster"),
    #                              num_novel_tp2_strains =  colDef(name = "Number of novel TP2 strains"),
    #                              overall_cluster_growth_rate = colDef(name = "Overall cluster growth rate"),
    #                              cluster_novel_growth_rate = colDef(name = "Novel cluster growth rate"),
    #                              country = colDef(name = "Country"),
    #                              province = colDef(name = "Province"),
    #                              strain = colDef(name = "Strain"),
    #                              present_at_tp1 = colDef(name = "Present at TP1"),
    #                              present_at_tp2 = colDef(name = "Present at TP2"),
    #                              strain_latitude = colDef(name = "Strain latitude"),
    #                              strain_longitude = colDef(name = "Strain longitude"),
    #                              strain_date = colDef(name = "Strain date"),
    #                              avg_tp1_latitude_plot = colDef(show = FALSE), 
    #                              avg_tp1_longitude_plot = colDef(show = FALSE),
    #                              avg_tp2_latitude_plot = colDef(show = FALSE),
    #                              avg_tp2_longitude_plot = colDef(show = FALSE),
    #                              tp1_cluster = colDef(show = FALSE),
    #                              tp1_strain_latitude_jit = colDef(show = FALSE), 
    #                              tp1_strain_longitude_jit = colDef(show = FALSE),
    #                              tp2_strain_latitude_jit = colDef(show = FALSE), 
    #                              tp2_strain_longitude_jit = colDef(show = FALSE)
    #               ))
    # })
    # 
    
    
    output$dt <- renderDataTable({
        datatable(shared_ecc_all_filtered,
                  filter = "top",
                  selection = "multiple",
                  extensions = 'RowGroup',
                  options = list(rowGroup = list(dataSrc = 7)),)
    }, server=F)

    
    

    
    ####################
    # base leaflet map #
    ####################
    
    output$map <- renderLeaflet({
        
        # set region for map - whole world
        #region <- map(regions = ".", fill = F, plot = F)
        
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
            
            hideGroup("Average centroid")
    })
    
    
    ##########################################
    # add circle markers to base leaflet map #
    ##########################################
    
    # list for any change in input
    toListen <- reactive({
        list(input$cluster, input$country, input$province, 
             input$strain_transparency, input$centroid_transparency)
    })
    
    
    observeEvent(toListen(),{
        
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
        proxy <- leafletProxy("map")
        
        # clear old shapes
        proxy %>% clearMarkers()
        
        req(input$cluster)
        
        proxy %>% 
            addCircleMarkers(data = shared_ecc_all_filtered,
                             lat =  ~avg_tp1_latitude_plot,
                             lng =  ~avg_tp1_longitude_plot,
                             radius = {if(input$centroid_radius=="Avg geo dist") ~log10(as.numeric(avg_tp1_geo_dist_km))*10 else ~log10(as.numeric(tp1_cluster_size))*10},
                             fillColor = ~unname(unlist(sapply(tp1_cluster, function(x) {colorpal %>% subset(tp1_cluster==x) %>% select(colour)}, simplify="vector"))),
                             fillOpacity = input$centroid_transparency/100,
                             stroke = T,
                             opacity = input$centroid_transparency/100,
                             weight = 1,
                             color = "white",
                             label = lapply(seq(nrow(ecc_all_filtered())), function(i) {
                                 ~HTML(paste("<b>", tp1_cluster[i],"</b>", "<br/>",
                                             "Num of strains:",tp1_cluster_size_2[i]-1,"<br/>",
                                             "Temporal ECC:", tp1_t0_ecc_0.0.1[i],"<br/>",
                                             "Geo ECC:",tp1_t0_ecc_0.1.0[i],"<br/>",
                                             "Avg date:", as.Date(avg_tp1_date)[i],"<br/>", sep=" ")) 
                             }),
                             group = "TP1 centroid") 
        
        # add tp2 centroids
        proxy %>% 
            addCircleMarkers(data = shared_ecc_all_filtered,
                             lat = ~as.numeric(avg_tp2_latitude_plot),
                             lng = ~as.numeric(avg_tp2_longitude_plot),  
                             radius = {if(input$centroid_radius=="Avg geo dist") ~log10(as.numeric(avg_tp2_geo_dist_km))*10 else ~log10(as.numeric(tp2_cluster_size))*10},
                             fillColor = ~unname(unlist(sapply(tp1_cluster, function(x) {colorpal %>% subset(tp1_cluster==x) %>% select(colour)}, simplify="vector"))),
                             fillOpacity = input$centroid_transparency/100,
                             stroke = T,
                             opacity = input$centroid_transparency/100,
                             weight = 1,
                             color = "black",
                             label = lapply(seq(nrow(ecc_all_filtered())), function(i) {
                                 ~HTML(paste("<b>",  tp2_cluster[i], "</b>", "<br/>",
                                             "Num of strains:",tp2_cluster_size_2[i]-1,"<br/>",
                                             "Num novel strains:",num_novel_tp2_strains[i],"<br/>",
                                             "Temporal ECC:", tp2_t0_ecc_0.0.1[i],"<br/>",
                                             "Geo ECC:",tp2_t0_ecc_0.1.0[i],"<br/>",
                                             "Delta temporal ECC:", delta_ecc_0.0.1[i],"<br/>",
                                             "Delta geo ECC:",delta_ecc_0.1.0[i],"<br/>",
                                             "Avg date:", as.Date(avg_tp2_date)[i],"<br/>", sep=" ")) 
                             }),
                             group = "TP2 centroid")
        
        
        # add tp1 strains
        proxy %>% 
            addCircleMarkers(data = shared_ecc_all_filtered,
                             layerId = ~strain,
                             lat = ~tp1_strain_latitude_jit,
                             lng = ~tp1_strain_longitude_jit,
                             radius = 5,
                             fillColor = ~unname(unlist(sapply(tp1_cluster, function(x) {colorpal %>% subset(tp1_cluster==x) %>% select(colour)}, simplify="vector"))),
                             fillOpacity = input$strain_transparency/100,
                             stroke = T,
                             opacity = input$strain_transparency/100,
                             weight = 1,
                             color = "white",
                             label = lapply(seq(nrow(ecc_all_filtered())), function(i) {
                                 ~HTML(paste("Strain:", "<b>", strain[i], "</b>", "<br/>",
                                             "Country:", country[i], "<br/>",
                                             "Province:",province[i],"<br/>",
                                             "Date:", strain_date[i],"<br/>", sep=" ")) 
                             }),
                             group = "TP1 strains")
        
        
        # add tp2 strains
        proxy %>%
            addCircleMarkers(data = shared_ecc_all_filtered,
                             lat = ~as.numeric(tp2_strain_latitude_jit),
                             lng = ~as.numeric(tp2_strain_longitude_jit),
                             radius = 5,
                             fillColor = ~unname(unlist(sapply(tp1_cluster, function(x) {colorpal %>% subset(tp1_cluster==x) %>% select(colour)}, simplify="vector"))),
                             fillOpacity = input$strain_transparency/100,
                             stroke = T,
                             opacity = input$strain_transparency/100,
                             weight = 1,
                             color = "black",
                             label = lapply(seq(nrow(ecc_all_filtered())), function(i) {
                                 ~HTML(paste("Strain:", "<b>", strain[i], "</b>", "<br/>",
                                             "Country:", country[i], "<br/>",
                                             "Province:",province[i],"<br/>",
                                             "Date:", strain_date[i],"<br/>", sep=" ")) 
                             }),
                             group = "TP2 strains")
        
        
        
    }, ignoreNULL = F) # forces section to run when input is null
    
    
    
    ###############################
    # add reactive legend to plot #
    ###############################
    
    # list for any change in input
    toListenLegend <- reactive({
        list(input$cluster, input$legend)
    })
    
    observeEvent(toListenLegend(), {
        
        # print statement for debugging
        print("updating legend")
        
        # proxy for leaflet map
        proxy <- leafletProxy("map")
        
        # Remove any existing legend, and only if the legend is
        # enabled, and only when the legend is changed (this includes
        # when all selection are removed)
        proxy %>% clearControls()
        
        # require input cluster to not be null to add new selection to legent 
        req(input$cluster)
        
        # update legend
        if (input$legend) {
            proxy %>% 
                addLegend(group = "legend", 
                          "bottomleft", 
                          colors = as.character(colorpal_nona$colour), 
                          labels = as.character(colorpal_nona$tp1_cluster),
                          opacity = 1) 
        }
    }, ignoreNULL = F) # forces section to run when input is null
    
    
    
    
    
    ###################
    # ridgeline plots #
    ###################
    
    
    output$ridgeplot <- renderPlotly({
        
        # dont run until at least one cluster selected 
        req(input$cluster)
        
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


        p <- shared_ecc_all_filtered  %>%
            plot_ly(type = 'violin')

        p <- p %>%
            add_trace(
                x = ~tp2_stain_time_diff,
                y = ~tp1_cluster,
                split = ~tp1_cluster,
                type = 'violin',
                side = 'positive',
                orientation = "h",
                scalemode = "count",
                name =  ~tp2_cluster,
                color = ~I(unname(unlist(sapply(tp1_cluster, function(x) {colorpal %>% subset(tp1_cluster==x) %>% select(colour)}, simplify="vector")))),
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

        p <- p %>%
            add_trace(
                x = ~tp1_stain_time_diff,
                y = ~tp1_cluster,
                split = ~tp1_cluster,
                type = 'violin',
                side = 'positive',
                orientation = "h",
                scalemode = "count",
                color = ~I(unname(unlist(sapply(tp1_cluster, function(x) {colorpal %>% subset(tp1_cluster==x) %>% select(colour)}, simplify="vector")))),
                meanline = list(
                    visible = F
                ),
                points = "all",
                marker = list(opacity = 0.4,
                              size = 10)
            )

        p

        

    })
    
    
    
    ###################
    # Add bubble plot #
    ###################
    
    
    output$bubbleplot <- renderPlotly({
        
        req(input$cluster)
        ggplotly(ggplot(shared_ecc_all_filtered, aes(x=tp1_t0_ecc_0.1.0, y=tp1_t0_ecc_0.0.1)) +
                   geom_point())
        
        
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
        reactable(data = shared_ecc_all_filtered,  
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