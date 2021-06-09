################ 
# General info #
################

# COVID VIS SHINY DASHBOARD
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
library(geosphere) # for directions between points 
library(shinydashboardPlus) # for collapsable boxes
# library(shinyjqui) # for resizable/movable boxes


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

# remove 0 for now as its causing problems
eccdata <- eccdata %>% subset(tp1_cluster != 0 )


#################################
# type convert selected columns #
#################################

# covert appropriate columns to numeric
eccdata[,c(5:10, 12:15, 17:21, 23:26, 28:39)] <- 
    sapply(eccdata[,c(5:10, 12:15, 17:21, 23:26, 28:39)], as.numeric)

# convert character columns and grouping vars to factor
# e.g. names, whether a strain is present at tp1
eccdata[,c(1:4, 10:11, 15:16)] <- 
    sapply(eccdata[,c(1:4, 10:11, 15:16)], as.factor)

# format date columns
eccdata$avg_tp1_date <- as.Date(eccdata$avg_tp1_date, format = "%d-%m-%y") 
eccdata$avg_tp2_date <- as.Date(eccdata$avg_tp2_date, format = "%d-%m-%y")


###########################################
# subset out cluster specific infomation  #
###########################################

# users of this app more interested in cluster info
# use tidyverse to summarize it (lots of repeats because of strains)

# tp1 cluster data
clusters <- eccdata %>% 
    group_by(tp1_cluster) %>%
    summarise(avg_tp1_longitude = mean(avg_tp1_longitude),
              avg_tp1_latitude = mean(avg_tp1_latitude),
              tp1_cluster_size_2 = mean(tp1_cluster_size_2),
              tp1_t0_ecc_0.0.1 = mean(tp1_t0_ecc_0.0.1, na.rm=T),
              tp1_t0_ecc_0.1.0 = mean(tp1_t0_ecc_0.1.0, na.rm=T),
              avg_tp1_date = mean(avg_tp1_date),
              tp2_cluster = unique(tp2_cluster)[1],
              avg_tp2_longitude = mean(avg_tp2_longitude),
              avg_tp2_latitude = mean(avg_tp2_latitude),
              tp2_cluster_size_2 = mean(tp2_cluster_size_2),
              tp2_t0_ecc_0.1.0 = mean(tp2_t0_ecc_0.1.0),
              tp2_t0_ecc_0.0.1 = mean(tp2_t0_ecc_0.0.1), 
              delta_ecc_0.1.0 = mean(delta_ecc_0.1.0),
              delta_ecc_0.0.1 = mean(delta_ecc_0.0.1),
              avg_tp1_date = mean(avg_tp1_date),
              avg_tp1_temporal_dist_days = mean(avg_tp1_temporal_dist_days, na.rm=T),
              avg_tp1_geo_dist_km = mean(avg_tp1_geo_dist_km, na.rm=T),
              avg_tp2_date = mean(avg_tp2_date),
              avg_tp2_temporal_dist_days = mean(avg_tp2_temporal_dist_days),
              avg_tp2_geo_dist_km = mean(avg_tp2_geo_dist_km), 
              tp1_cluster_size = mean(tp1_cluster_size),
              tp2_cluster_size = mean(tp2_cluster_size),
              delta_cluster_size = mean(delta_cluster_size),
              num_additional_tp1_strains_tp2 = mean(num_additional_tp1_strains_tp2),
              num_novel_tp2_strains = mean(num_novel_tp2_strains),
              overall_cluster_growth_rate = mean(overall_cluster_growth_rate),
              cluster_novel_growth_rate = mean(cluster_novel_growth_rate))


################################################
# change in latitude and longitude of clusters #
################################################

# (North = 0 and 360, East = 90, Sout = 180, and West = 270  degrees).
# negative bearings means being measaured counterclockwise from north
# 8 wind compass rose each direction is 45° from the next. 
# 16 wind compass rose points at a 22+1⁄2° angle from its two neighbours

clusters$delta_avg_longitude <- clusters$avg_tp1_longitude - clusters$avg_tp2_longitude
clusters$delta_avg_latitude <- clusters$avg_tp1_latitude - clusters$avg_tp2_latitude

# get bearing (direction of change)
clusters$bearing <- bearing(as.matrix(clusters[,c("avg_tp1_longitude", "avg_tp1_latitude")]),
                            as.matrix(clusters[,c("avg_tp2_longitude", "avg_tp2_latitude")]))

# convert bearing to compass direction
# centre on directions 
clusters$cardinal <- sapply(clusters$bearing, function(x){
    
    if(x <= 11.25 & x >= 0) {return("N")}
    if(x > 11.25 & x <= 33.75) {return("NNE")}
    if(x > 33.75 & x <= 56.25) {return("NE")}
    if(x > 56.25 & x <= 78.75) {return("ENE")}
    if(x > 78.75 & x <= 101.25) {return("E")}
    if(x > 101.25 & x <= 123.75) {return("ESE")}
    if(x > 123.75 & x <= 146.25) {return("SE")}
    if(x > 146.25 & x <= 168.75) {return("SSE")}
    if(x > 168.5 & x <= 180) {return("S")}
    if(x < -168.5 & x >= -180) {return("S")}
    if(x < -146.25 & x >= -168.5) {return("SSW")}
    if(x < -123.75 & x >= -146.25) {return("SW")}
    if(x < -101.25 & x >= -123.75) {return("WSW")}
    if(x < -78.75 & x >= -101.25) {return("W")}
    if(x < -56.25 & x >= -78.75) {return("WNW")}
    if(x < -33.75 & x >= -56.25) {return("NW")}
    if(x < -11.25 & x >= -33.75) {return("NNW")}
    if(x < 0 & x >= -11.25) {return("N")}
})  


##########################################
# subset out strain specific infomation  #
##########################################

# strain info could still be important
# subset strain info to a seprate data frame

strains <- eccdata %>%
    select(strain, country, province, city, year, month, day,
           strain_latitude, strain_longitude, present_at_tp1, 
           tp1_cluster, present_at_tp2, tp2_cluster)


#################################################
# create columns for jittered strain locations  #
#################################################

# create additional latitude and longitude for tp1 and tp2 strains and clusters
# 12-13 decimal places in lats and longs
# too long, round to four for our purposes

strains <- strains %>%
    mutate(strain_latitude_jit = round(jitter(as.numeric(strain_latitude),10,1),digits=4)) %>%
    mutate(strain_longitude_jit = round(jitter(as.numeric(strain_longitude),10,1),digits=4))

# make column for strain date instead of having three separate columns
strains$strain_date <- paste(strains$day, 
                             strains$month, 
                             strains$year, 
                             sep = "-")

# create time difference for ridgeline plots 
strains <- strains %>% mutate(tp1_stain_time_diff = as.numeric(abs(as.Date("01-01-2020", format = "%d-%m-%y") - as.Date(strain_date, format = "%d-%m-%y"))))
strains <- strains %>% mutate(tp2_stain_time_diff = as.numeric(abs(as.Date("01-01-2020", format = "%d-%m-%y") - as.Date(strain_date, format = "%d-%m-%y"))))


##################
# crosstalk keys #
##################

# create unique row ids for shared cluster and strain data
clusters$key <- as.numeric(rownames(clusters))
strains$key <- as.numeric(rownames(strains))


#########################################################
# create shared data frames for cluster and strain info #
#########################################################

# clusters
clusters_sh <- SharedData$new(clusters, key = ~key, group = "clusters")
strains_sh <- SharedData$new(strains, key = ~key, group = "strains")


#############################
# USER INTERFACE/CLINT SIDE #
#############################

header <- dashboardHeader(title = "ECC Vis")

# the dashboard side bar
sidebar <- dashboardSidebar(
    sidebarMenu(id = 'sidebarmenu',
                
                # Visualization tab
                menuItem("Visualizations", tabName = "visualizations"),
            
                # filtered data using in visualizations
                menuItem("Filtered data", tabName = "filteredData"),
                
                # raw data used to generate visualizations
                menuItem("Raw data", tabName = "rawData"))
)

# the dashboard body
body <- dashboardBody(
    tabItems(
        tabItem(tabName = "visualizations",
                
                # tab title
                h2("ECC Visualizations"),
                
                # row 1 box for map and bubble plot
                fluidRow(
                    box(title = "Map plot", 
                        width = 6, 
                        leafletOutput("map"),
                        collapsible = TRUE,
                        sidebar = boxSidebar(
                            id = "mapsidebar",
                            title = "Map controls",
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
                            
                            # legend checkbox for map 
                            checkboxInput("legend", "Show legend", TRUE)
                        )),
                    
                    box(title = "Bubble plot", 
                        width = 6, 
                        plotlyOutput("bubbleplot"),
                        collapsible = TRUE)
                ),
                
                # row 2 for change vectors and cluster movement
                fluidRow(

                    # change vector plot
                    box(title = "Change vector", 
                        width = 6,
                        plotlyOutput("change_vector", width = "100%", height = "100%"),
                        collapsible = TRUE), 
                    
                    # cardinal movement of clusters 
                    box(title = "Cardinal movement of clusters", 
                        width = 6, 
                        plotlyOutput("cardinal_polar", width = "100%", height = "100%"),
                        collapsible = TRUE)
                ),
                
                # row 3 for strain histogram and ridgeline plot 
                fluidRow(
                    box(title = "Strain histogram", 
                        width = 6,
                        plotlyOutput("strain_histogram", width = "100%", height = "100%"),
                        collapsible = TRUE)

                ),
                
                # row 4 for cluster and strain data
                fluidRow(
                    box(title = "Cluster data", 
                        width = 6, 
                        dataTableOutput("clusters_dt"),
                        collapsible = TRUE),
                    
                    box(title = "Cluster filters", 
                        width = 6, 
                        filter_select("tp1_cluster", "TP1 cluster", clusters_sh, ~tp1_cluster),
                        filter_select("tp2_cluster", "TP2 cluster", clusters_sh, ~tp2_cluster),
                        filter_slider("tp1_t0_ecc_0.0.1", "tp1_t0_ecc_0.0.1", clusters_sh, ~tp1_t0_ecc_0.0.1),
                        
                        collapsible = TRUE)
                )
        ),
        
        # tab for filtered data
        tabItem(tabName = "filteredData",
                h2("Filtered data"),
                fluidRow(
                    box(width = 12, 
                        dataTableOutput("filtered_data"))
                )
        ),
        
        # table for raw data
        tabItem(tabName = "rawData",
                h2("Raw data"),
                fluidRow(
                    box(width = 12,
                        dataTableOutput("raw_data")
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

    ################################
    # cluster and strain DT tables #
    ################################
   
    # clusters 
    output$clusters_dt <- renderDataTable({
        
        # for custom button actions
        # https://community.rstudio.com/t/select-only-filtered-rows-using-select-all-button-that-comes-with-select-extension-in-shinys-dt-package/66749/2
        datatable(clusters_sh, 
                  extensions = c('Select', 'Buttons','Scroller'), 
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
                      scroller = TRUE),
                  selection = 'none',
                  filter = "top",
        )
    }, server=F)
    
    
    # strains 
    output$strains_dt <- renderDataTable({
        
        # for custom button actions
        # https://community.rstudio.com/t/select-only-filtered-rows-using-select-all-button-that-comes-with-select-extension-in-shinys-dt-package/66749/2
        datatable(data = strains_sh, 
                  extensions = c('Select', 'Buttons','Scroller'), 
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
                      scroller = TRUE),
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
                      length.out=length(unique(clusters$tp1_cluster))),
        domain = unique(eccdata$tp1_cluster))


    ####################
    # base leaflet map #
    ####################

    # leaflet map 
    output$map <- renderLeaflet({
        
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
            addCircleMarkers(data = clusters_sh,
                             lat = ~avg_tp1_latitude,
                             lng = ~avg_tp1_longitude,
                             radius = ~log10(tp1_cluster_size_2)*10,
                             fillColor = ~pal(tp1_cluster),
                             fillOpacity = input$centroid_transparency/100,
                             stroke = T,
                             opacity = input$centroid_transparency/100,
                             weight = 1,
                             color = "white",
                             label = lapply(seq(nrow(clusters_sh$data())), function(i) {
                                 ~HTML(paste("<b>", tp1_cluster[i],"</b>", "<br/>",
                                             "Num of strains:",tp1_cluster_size_2[i]-1,"<br/>",
                                             "Temporal ECC:", tp1_t0_ecc_0.0.1[i],"<br/>",
                                             "Geo ECC:",tp1_t0_ecc_0.1.0[i],"<br/>",
                                             "Avg date:", avg_tp1_date[i],"<br/>", sep=" ")) 
                             }),
                             group = "TP1 clusters") %>% 
            
            # tp2 cluster markers
            addCircleMarkers(data = clusters_sh,
                             lat = ~avg_tp2_latitude,
                             lng = ~avg_tp2_longitude,
                             radius = ~log10(tp2_cluster_size_2)*10,
                             fillColor = ~pal(tp1_cluster),
                             fillOpacity = input$centroid_transparency/100,
                             stroke = T,
                             opacity = input$centroid_transparency/100,
                             weight = 1,
                             color = "black",
                             lapply(seq(nrow(clusters_sh$data())), function(i) {
                                 ~HTML(paste("<b>",  tp2_cluster[i], "</b>", "<br/>",
                                             "Num of strains:",tp2_cluster_size_2[i]-1,"<br/>",
                                             "Num novel strains:",num_novel_tp2_strains[i],"<br/>",
                                             "Temporal ECC:", tp2_t0_ecc_0.0.1[i],"<br/>",
                                             "Geo ECC:",tp2_t0_ecc_0.1.0[i],"<br/>",
                                             "Avg date:", avg_tp2_date,"<br/>", sep=" ")) 
                             }),
                             group = "TP2 clusters")          

    })
    
    
    ###################
    # ridgeline plots #
    ###################

    output$ridgeplot <- renderPlotly({

        # # base plot
        # ridge <- plot_ly(type = 'violin')
        # 
        # # tp2 ridges
        # ridge <- ridge %>%
        #     add_trace(data = strains_sh,
        #               x = ~tp2_stain_time_diff,
        #               y = ~tp1_cluster,
        #               split = ~tp1_cluster,
        #               type = 'violin',
        #               side = 'positive',
        #               orientation = "h",
        #               scalemode = "count",
        #               color = ~I(pal(tp1_cluster)),
        #               name =  ~tp2_cluster,
        #               meanline = list(
        #                   visible = F
        #               ),
        #               #points = "all",
        #               line = list(color = "black",
        #                           width = 1)
        #               # marker = list(opacity = 0.4,
        #               #               size = 10,
        #               #               line = list(color = "black",
        #               #                           width = 1)) 
        #               )
        # 
        # #tp1 ridges
        # ridge <- ridge %>%
        #     add_trace(data = strains_sh,
        #         x = ~tp1_stain_time_diff,
        #         y = ~tp1_cluster,
        #         split = ~tp1_cluster,
        #         type = 'violin',
        #         side = 'positive',
        #         orientation = "h",
        #         scalemode = "count",
        #         color = ~I(pal(tp1_cluster)),
        #         meanline = list(
        #             visible = F
        #         )
        #         #points = "all",
        #         # marker = list(opacity = 0.4,
        #         #               size = 10)
        #         )
        # 
        # # remove legend if we dont want it
        # {if(input$ridgelegend==FALSE) ridge <- ridge %>% layout(showlegend = FALSE) else ridge <- ridge}
        # 
        #  # write out plot
        # ridge
    })
    
    
    #############################
    # bubble plots for ECC data #
    #############################
    
    output$bubbleplot <- renderPlotly({
        
        # intialize plot 
        bubble <- plot_ly(type = 'scatter', mode = 'markers') 
        # 
        # #add tp1 clusters
        # bubble <- bubble %>% add_trace(data = clusters_sh,
        #                                x = ~tp1_t0_ecc_0.0.1,
        #                                y = ~tp1_t0_ecc_0.1.0,
        #                                name = "TP1 clusters", 
        #                                marker = list(size = ~tp1_cluster_size_2,
        #                                              sizemode = "area",
        #                                              color = ~pal(tp1_cluster),
        #                                              line = list(color = ~pal(tp1_cluster))))
        # 
        # #add tp1 clusters
        # bubble <- bubble %>% add_trace(data = clusters_sh,
        #                                x = ~tp2_t0_ecc_0.0.1,
        #                                y = ~tp2_t0_ecc_0.1.0,
        #                                name = "TP2 clusters", 
        #                                marker = list(size = ~tp2_cluster_size_2,
        #                                              sizemode = "area",
        #                                              color = ~pal(tp1_cluster),
        #                                              line = list(color = "black")))
        # 
        # 
        # bubble
        

        #add tp1 clusters
        bubble <- bubble %>% add_trace(data = clusters_sh,
                                       x = ~tp1_t0_ecc_0.0.1,
                                       y = ~tp1_t0_ecc_0.0.1,
                                       size = ~log10(tp1_cluster_size_2),
                                       split =  ~tp1_cluster,
                                       sizes = c(1, max(clusters[,c("tp1_cluster_size_2", "tp2_cluster_size_2")])),
                                       colors = ~pal(tp1_cluster),
                                       marker = list(sizemode = 'diameter'),
                                       showlegend = T)

        # add tp2 clusters
        bubble <- bubble %>% add_trace(data = clusters_sh,
                                       x = ~tp2_t0_ecc_0.0.1,
                                       y = ~tp2_t0_ecc_0.1.0,
                                       size = ~log10(tp2_cluster_size_2),
                                       split =  ~tp2_cluster,
                                       sizes = c(1, max(clusters[,c("tp1_cluster_size_2", "tp2_cluster_size_2")])),
                                        colors = ~pal(tp1_cluster),
                                       marker = list(sizemode = 'diameter',
                                                     line = list(color = "black")),
                                       showlegend = T)

        # write out plot
        bubble
    })
    

    ##############################
    # histogram for strain counts#
    ##############################
    
    output$strain_histogram <- renderPlotly({ 
        
        # initialize plotly histogram 
        strain_histo <- plot_ly(type = "histogram")
        
        # create histogram
        strain_histo <- strain_histo %>% add_trace(data = strains_sh,  
                                                   x = ~month,
                                                   split = ~country,
                                                   colors = "Set1")
        
        # specify overlay for bar plot 
        strain_histo <- strain_histo %>% layout(barmode = "stack")
        
        # write out histogram 
        strain_histo
        
        })
    

    #####################################
    # histogram for change in direction #
    #####################################
    
    output$cardinal_polar <- renderPlotly({ 
        
        # initialize plotly histogram 
        cardinal_histo <- plot_ly(type = "histogram")
        
        # histogram 
        cardinal_histo <- cardinal_histo %>% add_trace(data = clusters_sh, 
                                                       x = ~cardinal)

        # remove legend
        cardinal_histo <- cardinal_histo %>% layout(showlegend = FALSE)

        # write out histogram 
        cardinal_histo
        
    #     cardinal_polar <- ggplot(clusters) +
    #         geom_bar(aes(x=cardinal), fill="lightgrey", color="black") +
    #         scale_x_discrete(drop=FALSE) + 
    #         theme_bw() +
    #         coord_polar()
    #     cardinal_polar
    # 
    #     
    #     fig <- plot_ly(clusters %>% group_by(cardinal) %>% summarize(n=n()), 
    #                    r = ~n, t = ~cardinal) %>% 
    #         
    #         #add_area(colors = "grey")
    #     
    #     fig <- fig %>% layout(radialaxis = list(range = c(0, ~max(n))), orientation = 350)
    # 
    #     
    #     fig        
    #     
    #     
    #     fig <- plot_ly(
    #         type = 'scatterpolar',
    #         mode = 'lines'
    #     ) 
    #     
    # 
    #     fig <- fig %>%
    #         add_trace(data = clusters %>% group_by(cardinal) %>% summarize(n=n()), 
    #                   r = ~n, 
    #                   theta = ~cardinal, 
    #                   fill = 'toself',
    #                   fillcolor = 'red',
    #                   line = list(
    #                       color = 'black'
    #                   )
    #         )     
    #     
    #     
    # fig <- fig %>%
    #     add_trace(data = clusters %>% group_by(cardinal) %>% summarize(n=n()) %>% slice(1), 
    #         r = c(0, ~n, ~n, 0), 
    #         theta = c(0,"ENE", "ESE", 0),
    #         fill = 'toself',
    #         fillcolor = 'red',
    #         line = list(
    #             color = 'black'
    #         )
    #     ) 
    #     
    #     fig <- fig %>%
    #         layout(
    #             polar = list(
    #                 radialaxis = list(
    #                     visible = T,
    #                     range = c(0,30)
    #                 )
    #             ),
    #             showlegend = F
    #         )
    #     
    #     fig
    })
    
    
    #######################
    # change vector graph #
    #######################
    
    output$change_vector <- renderPlotly({ 
        
        # initialize plot 
        change_vector <-  plot_ly(type = 'scatter',mode="lines+markers")
        
        # add change vector
        change_vector <- change_vector %>% add_trace(data = clusters_sh,
                                                     x = c(0, ~delta_ecc_0.0.1), 
                                                     y = c(0, ~delta_ecc_0.1.0),
                                                     split = ~tp1_cluster, 
                                                     colors = ~I(pal(tp1_cluster)),
                                                     opacity = 1) 
        
        # set axis limits
        change_vector <- change_vector %>% layout(xaxis = list(title = "delta_ecc_0.0.1", range = c(-1,1)), 
                                                  yaxis = list(title = "delta_ecc_0.1.0", range = c(-1,1)))
        
        # write out plot
        change_vector
        
    })
        
    
    ###########################################
    # filtered table to be displayed in a tab #
    ###########################################
    
    output$filtered_data <- renderDataTable({
        datatable(eccdata,
                  filter = "top")
        },  server=FALSE)

    
    #########################################
    # raw data to be displayed in a new tab #
    #########################################
    
    # displayed in new tab without grouping
    output$raw_data <- renderDataTable({
        datatable(eccdata, 
                  filter = "top"
        )
    })
    
}

shinyApp(ui, server)




