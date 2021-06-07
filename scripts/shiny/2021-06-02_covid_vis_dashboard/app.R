# COVID VIS SHINy DASHBOARD

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
library(DT) # data table
library(htmltools) # for html input
library(lubridate) # for dates 

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
eccdata[,c(4:6, 8:11, 13:17, 19:22, 24:37, 39:40)] <- 
    sapply(eccdata[,c(4:6, 8:11, 13:17, 19:22, 24:37, 39:40)], as.numeric)

# time diff columns for ridgeline
eccdata$tp1_cluster <- as.factor(eccdata$tp1_cluster)
eccdata$province <- as.factor(eccdata$province)
eccdata$country <- as.factor(eccdata$country)


# convert to date
eccdata$strain_date <- as.Date(eccdata$strain_date, format = "%d-%m-%y") 
eccdata$avg_tp1_date <- as.Date(eccdata$avg_tp1_date, format = "%d-%m-%y") 
eccdata$avg_tp2_date <- as.Date(eccdata$avg_tp2_date, format = "%d-%m-%y")

# convert to factor
eccdata$present_at_tp1 <- as.factor(eccdata$present_at_tp1)
eccdata$present_at_tp2 <- as.factor(eccdata$present_at_tp2)


#################
# crosstalk key #
#################

# create unique row ids for crosstalk
eccdata$key <- as.numeric(rownames(eccdata))


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
                        plotlyOutput("histogram", width = "100%", height = "100%"))
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

        # for custom button actions
        # https://community.rstudio.com/t/select-only-filtered-rows-using-select-all-button-that-comes-with-select-extension-in-shinys-dt-package/66749/2
        datatable(
            shared_ecc_all, extensions = c('Select', 'Buttons','Scroller','RowGroup'), 
            options = list(
                select = list(style = 'os', items = 'row'),
                dom = 'Blfrtip',
                rowId = 0,
                buttons = list('selectRows',
                               list(extend='selectAll', className='selectAll',
                                 text="Select all filtered rows",
                                 action=DT::JS("function () {
                                var table = $('#DataTables_Table_0').DataTable();
                                table.rows({ search: 'applied'}).deselect();
                                table.rows({ search: 'applied'}).select();}")),
                               'selectAll', 'selectNone'),
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
    
    
    ##################################################
    # color palette using leaflets built in function #
    ##################################################

    # leaflet built in color function
    pal <-  colorFactor(
        palette = rep(brewer.pal(9, "Set1"), 
                      length.out=length(unique(eccdata$tp1_cluster))),
        domain = unique(eccdata$tp1_cluster))


    ####################
    # base leaflet map #
    ####################

    # leaflet map 
    output$map <- renderLeaflet({
        
        # sub-setting shard data for selections and filters for plotting#
        
        # tp1 cluster data
        tp1_clust <- shared_ecc_tp1$data(withSelection = TRUE, 
                                         withFilter = TRUE) %>% 
            subset(selected_ == T) %>%
            group_by(tp1_cluster) %>%
            summarise(avg_tp1_longitude = mean(avg_tp1_longitude),
                      avg_tp1_latitude = mean(avg_tp1_latitude),
                      tp1_cluster_size_2 = mean(tp1_cluster_size_2),
                      tp1_t0_ecc_0.0.1 = mean(tp1_t0_ecc_0.0.1),
                      tp1_t0_ecc_0.1.0 = mean(tp1_t0_ecc_0.1.0),
                      avg_tp1_date = mean(avg_tp1_date))
        
        # tp2 cluster data
        tp2_clust <- shared_ecc_tp2$data(withSelection = TRUE, 
                                         withFilter = TRUE) %>% 
            subset(selected_ == T) %>%
            group_by(tp2_cluster) %>%
            summarise(avg_tp2_longitude = mean(avg_tp2_longitude),
                      avg_tp2_latitude = mean(avg_tp2_latitude),
                      tp2_cluster_size_2 = mean(tp2_cluster_size_2),
                      tp1_cluster = unique(tp1_cluster)[1],
                      num_novel_tp2_strains = mean(num_novel_tp2_strains),
                      tp2_t0_ecc_0.0.1 = mean(tp2_t0_ecc_0.0.1),
                      tp2_t0_ecc_0.1.0 = mean(tp2_t0_ecc_0.1.0),
                      avg_tp2_date = mean(avg_tp2_date))
        
        # tp1 strain 
        tp1_strain <- shared_ecc_tp1$data(withSelection = TRUE, 
                                          withFilter = TRUE) %>% 
            subset(selected_ == T)
        
        # tp2 strain
        tp2_strain <- shared_ecc_tp2$data(withSelection = TRUE,
                                          withFilter = TRUE) %>% 
            subset(selected_ == T)
        
        
        # leaflet map 
        leaflet() %>% 
            
            # load tile for selected region
            addTiles("http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}") %>%
            
            # set base zoom over mid asia/europe
            setView(lng = 60, lat = 50, zoom = 2) %>%
            
            # control layer for toggling strains and centroids on/off
            # never changes so can be part of base map
            addLayersControl(overlayGroups = c("TP1 clusters", "TP1 strains", 
                                            "TP2 clusters", "TP2 strains"),
                             options = layersControlOptions(collapsed = TRUE)) %>% 

            # tp1 cluster markers
            addCircleMarkers(data = tp1_clust,
                             lat = ~avg_tp1_latitude,
                             lng = ~avg_tp1_longitude,
                             radius = ~log10(tp1_cluster_size_2)*10,
                             fillColor = ~pal(tp1_cluster),
                             fillOpacity = input$centroid_transparency/100,
                             stroke = T,
                             opacity = input$centroid_transparency/100,
                             weight = 1,
                             color = "white",
                             label = lapply(seq(nrow(tp1_clust)), function(i) {
                                 ~HTML(paste("<b>", tp1_cluster[i],"</b>", "<br/>",
                                             "Num of strains:",tp1_cluster_size_2[i]-1,"<br/>",
                                             "Temporal ECC:", tp1_t0_ecc_0.0.1[i],"<br/>",
                                             "Geo ECC:",tp1_t0_ecc_0.1.0[i],"<br/>",
                                             "Avg date:", avg_tp1_date[i],"<br/>", sep=" ")) 
                             }),
                             group = "TP1 clusters") %>% 
            
            # tp2 cluster markers
            addCircleMarkers(data = tp2_clust,
                             lat = ~avg_tp2_latitude,
                             lng = ~avg_tp2_longitude,
                             radius = ~log10(tp2_cluster_size_2)*10,
                             fillColor = ~pal(tp1_cluster),
                             fillOpacity = input$centroid_transparency/100,
                             stroke = T,
                             opacity = input$centroid_transparency/100,
                             weight = 1,
                             color = "black",
                             lapply(seq(nrow(tp2_clust)), function(i) {
                                 ~HTML(paste("<b>",  tp2_cluster[i], "</b>", "<br/>",
                                             "Num of strains:",tp2_cluster_size_2[i]-1,"<br/>",
                                             "Num novel strains:",num_novel_tp2_strains[i],"<br/>",
                                             "Temporal ECC:", tp2_t0_ecc_0.0.1[i],"<br/>",
                                             "Geo ECC:",tp2_t0_ecc_0.1.0[i],"<br/>",
                                             "Avg date:", avg_tp2_date,"<br/>", sep=" ")) 
                             }),
                             group = "TP2 clusters") %>%             

            # tp1 strain markers
            addCircleMarkers(data = tp1_strain,
                             lat = ~strain_latitude_jit,
                             lng = ~strain_longitude_jit,
                             radius = 5,
                             fillColor = ~pal(tp1_cluster),
                             fillOpacity = input$strain_transparency/100,
                             stroke = T,
                             opacity = input$strain_transparency/100,
                             weight = 1,
                             color = "white",
                             label = lapply(seq(nrow(tp1_strain)), function(i) {
                                 ~HTML(paste("Strain:", "<b>", strain[i], "</b>", "<br/>",
                                             "Cluster:", tp1_cluster[i], "<br/>",
                                             "Country:", country[i], "<br/>",
                                             "Province:",province[i],"<br/>",
                                             "Date:", strain_date[i],"<br/>", sep=" ")) 
                             }),
                             group = "TP1 strains") %>% 
            
            # tp2 strain markers
            addCircleMarkers(data = tp2_strain,
                             lat = ~strain_latitude_jit,
                             lng = ~strain_longitude_jit,
                             radius = 5,
                             fillColor = ~pal(tp1_cluster),
                             fillOpacity = input$strain_transparency/100,
                             stroke = T,
                             opacity = input$strain_transparency/100,
                             weight = 1,
                             color = "black",
                             label = lapply(seq(nrow(tp2_strain)), function(i) {
                                 ~HTML(paste("Strain:", "<b>", strain[i], "</b>", "<br/>",
                                             "Cluster:", tp2_cluster[i], "<br/>",
                                             "Country:", country[i], "<br/>",
                                             "Province:",province[i],"<br/>",
                                             "Date:", strain_date[i],"<br/>", sep=" ")) 
                             }),
                             group = "TP2 strains",
                             ) 
    })
    
    
    ###################
    # ridgeline plots #
    ###################
    
    output$ridgeplot <- renderPlotly({
       
        # base plot
        ridge <- plot_ly(type = 'violin')

        # tp2 ridges
        ridge <- ridge %>%
            add_trace(data = shared_ecc_tp2$data(withSelection = TRUE, withFilter = TRUE) %>% 
                          subset(selected_ == T),
                      x = ~tp2_stain_time_diff,
                      y = ~tp1_cluster,
                      split = ~tp1_cluster,
                      type = 'violin',
                      side = 'positive',
                      orientation = "h",
                      scalemode = "count",
                      color = ~I(pal(tp1_cluster)),
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
                                                width = 1)))
        
        #tp1 ridges
        ridge <- ridge %>%
            add_trace(data = shared_ecc_tp1$data(withSelection = TRUE, withFilter = TRUE) %>% 
                          subset(selected_ == T),
                x = ~tp1_stain_time_diff,
                y = ~tp1_cluster,
                split = ~tp1_cluster,
                type = 'violin',
                side = 'positive',
                orientation = "h",
                scalemode = "count",
                color = ~I(pal(tp1_cluster)),
                meanline = list(
                    visible = F
                ),
                points = "all",
                marker = list(opacity = 0.4,
                              size = 10))

        # write out plot
        ridge
    })
    
    
    #############################
    # bubble plots for ECC data #
    #############################


    output$bubbleplot <- renderPlotly({

        # sub-setting shard data for selections and filters for plotting#
        
        # tp1 cluster data
        tp1_clust <- shared_ecc_all$data(withSelection = TRUE, 
                                         withFilter = TRUE) %>% 
            subset(selected_ == T) %>%
            group_by(tp1_cluster) %>%
            summarise(avg_tp1_longitude = mean(avg_tp1_longitude),
                      avg_tp1_latitude = mean(avg_tp1_latitude),
                      tp1_cluster_size_2 = mean(tp1_cluster_size_2),
                      tp1_t0_ecc_0.0.1 = mean(tp1_t0_ecc_0.0.1),
                      tp1_t0_ecc_0.1.0 = mean(tp1_t0_ecc_0.1.0),
                      avg_tp1_date = mean(avg_tp1_date))
        
        # tp2 cluster data
        tp2_clust <- shared_ecc_all$data(withSelection = TRUE, 
                                         withFilter = TRUE) %>% 
            subset(selected_ == T) %>%
            group_by(tp2_cluster) %>%
            summarise(avg_tp2_longitude = mean(avg_tp2_longitude),
                      avg_tp2_latitude = mean(avg_tp2_latitude),
                      tp2_cluster_size_2 = mean(tp2_cluster_size_2),
                      tp1_cluster = unique(tp1_cluster)[1],
                      num_novel_tp2_strains = mean(num_novel_tp2_strains),
                      tp2_t0_ecc_0.0.1 = mean(tp2_t0_ecc_0.0.1),
                      tp2_t0_ecc_0.1.0 = mean(tp2_t0_ecc_0.1.0),
                      avg_tp2_date = mean(avg_tp2_date))
        
        bubble <-  ggplot() +
            
            # tp2 clusters
            geom_point(data = tp2_clust,
                       shape = 21,
                       color = "black",
                       alpha = input$centroid_transparency/100,
                       aes(x = tp2_t0_ecc_0.0.1,
                           y = tp2_t0_ecc_0.1.0,
                           size = tp2_cluster_size_2,
                           fill = as.factor(tp1_cluster))) +

            # tp1 clusters
            geom_point(data = tp1_clust,
                       shape=19,
                       alpha = input$centroid_transparency/100,
                       inherit.aes = FALSE, 
                       aes(x = tp1_t0_ecc_0.0.1, 
                                     y=tp1_t0_ecc_0.1.0, 
                           alpha = input$centroid_transparency/100,
                                     size = tp1_cluster_size_2, 
                                     color = as.factor(tp1_cluster))) + 
            
            scale_color_manual(values = pal(tp1_clust$tp1_cluster)) + 
            scale_fill_manual(values = pal(tp1_clust$tp1_cluster)) + 
            labs(y= "Geographical ECC", x = "Temporal ECC") + 
            scale_size_area() + 
            xlim(0,1) +
            ylim(0,1) + 
            theme_bw()
        
        ggplotly(bubble)
    })
    
    
    #############
    # histogram #
    #############
    
    output$histogram <- renderPlotly({ 
        
        histo <- ggplot() +
            geom_bar(data = shared_ecc_all$data(withSelection = TRUE, 
                                                withFilter = TRUE) %>% 
                         subset(selected_ == T) %>% 
                         mutate(month = as.factor(month(strain_date))),
                     aes(x=month,
                         fill=present_at_tp1),
                     colour = "black",
                     position = position_dodge2(preserve = "single")) +
            scale_fill_manual(values = c("grey10", "grey80")) + 
            facet_wrap(~country, ncol = 3) + 
            labs(x="Month", y = "Genome count") +
            theme_bw() +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())
        
        ggplotly(histo)
        
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




