# Title: RShiny interactive CGM visualizations
# Authors: Sam, Guangzhi, Vasena, Dillon, Ed

# load libraries required for dashboard side 
library(shinydashboard) # the shiny dashboard
library(readxl) # for reading xlsx files 
library(shiny) # for running shiny apps
library(shinyWidgets) # for better slider/dropdown options
library(here) # for clear dir calling
library(plotly) # for interactive ggplots
library(tidyverse) # tidy data/ kind of used
library(pals) # for large colour palettes (similar to brewer)
library(crosstalk) # for shared data
library(DT) # data table
library(htmltools) # for html input
library(lubridate) # for dates 
library(geosphere) # for directions between points 
library(shinydashboardPlus) # for collapsable boxes
library(data.table) # for melting
library(rvest) # for downloading cardinal table 
library(shinyalert) # for alters on data loading


############# 
# functions #
#############

# this function calculates the angle between cluster point 1 and point 2
source(here("scripts", "functions", "2021-09-25_point_angle.R"))

# this function imports a table with cardinal directions and angles
# can also be used to describe cluster transmission
source(here("scripts", "functions", "2021-09-25_cluster_movement_direction.R"))

# this function calculates the cardinal direction of cluster movement
source(here("scripts", "functions", "2021-10-10_cardinal_direction.R"))


##########
# server #
##########

# this is the actual service side 
server <- function(input, output, session) { 
    
    ############################
    # set max file upload size #
    ############################
    
    # default size too small; set max file size to 1 gb
    options(shiny.maxRequestSize=1048576000)
    
    #######################################
    # get user inputted data to visualize #
    #######################################
    
    # create reactive values
    vals <- reactiveValues(clusters_raw = NULL, strains_raw = NULL, clusters = NULL, strains = NULL)
    
    # read data frame into reactive values
    observeEvent(input$folder, {
        
        dir <- NULL # set to null for control of steps
        req(input$folder) # req not null
        
        # get directory to read files in from
        shinyDirChoose(input, 'folder', roots= c(home = '~', home = '~'))
        dir <- parseDirPath(roots= c(home = '~'), selection = input$folder)
        
        req(dir) # when dir selected read in data
        
        # read in cluster data
        clusters_raw <- read.delim(file.path(dir,"Wide_merged_cluster_results.tsv"))
        
        # read in strain file names
        file_list <- list.files(file.path(dir,"Merged_strain_results"))
        # create empty dataframe
        strains_raw <- data.frame()
        # read in and concatenate by rows 
        for (i in 1:length(file_list)){
            temp_data <- read.delim(file.path(dir,"Merged_strain_results", file_list[i]))
            strains_raw <- rbind(strains_raw, temp_data) 
        }
        
        # rename columns, remove spaces, all lowercase
        # remove redundant periods and those at end of name
        print("renaming cluster and strain columns nicely...")
        # clusters
        colnames(clusters_raw) <- make.names(colnames(clusters_raw), 
                                             unique=TRUE, 
                                             allow_ = F) %>%
            tolower() %>%
            gsub("([[:punct:]])\\1+", "\\1", .) %>%
            gsub("[.]$", "", .)
        # strains
        colnames(strains_raw) <- make.names(colnames(strains_raw), 
                                            unique=TRUE, 
                                            allow_ = F) %>%
            tolower() %>%
            gsub("([[:punct:]])\\1+", "\\1", .) %>%
            gsub("[.]$", "", .)
        
        # write to reactive val data frame
        vals$clusters_raw <- clusters_raw
        vals$strains_raw <- strains_raw
        
    })
    
    
    #############################
    # process user cluster data #
    #############################
    
    # when user enters new data, run the following chunk
    observeEvent(vals$clusters_raw, {
        
        req(vals$clusters_raw) # must not be null
        clusters_raw <- vals$clusters_raw # pull out for modifying
        
        ##########################
        # clean raw cluster data #
        ##########################
        
        print("converting cluster data classes...")
        # convert columns to appropriate types
        # as.is = T specifies character stays as character
        # type.convert is stable and used in read.table
        clusters_raw <- lapply(clusters_raw, type.convert, as.is = T)
        clusters_raw <- bind_rows(clusters_raw)
        
        ###########################################
        # calculate cluster transmission movement #
        ###########################################
        
        clusters_raw <- clusters_raw %>% 
            mutate(delta.ecc.angle = angle(delta.ecc.0.1.0, delta.ecc.0.0.1))
        
        # use new column to assign ecc values speed, spread, and combined
        clusters_raw <- clusters_raw %>%
            mutate(delta.ecc.direction = cut(delta.ecc.angle,
                                             breaks = c(0, directions$degree_max, 360), 
                                             labels = c(directions$ecc.comb, 'F')),
                   delta.ecc.speed = cut(delta.ecc.angle, 
                                         breaks = c(0, directions$degree_max, 360), 
                                         labels = c(directions$ecc.speed, 'F')),
                   delta.ecc.spread = cut(delta.ecc.angle,
                                          breaks = c(0, directions$degree_max, 360), 
                                          labels = c(directions$ecc.spread, 'F')))
        
        # convert to factor
        clusters_raw$delta.ecc.direction <- as.factor(clusters_raw$delta.ecc.direction)
        
        # set levels 
        clusters_raw$delta.ecc.direction <-  factor(clusters_raw$delta.ecc.direction, 
                                                    levels = c("Slower", "Slower/Slower, isolated", "Slower, Isolated", "Isolated/Slower, isolated",
                                                               "Isolated", "Isolated/Faster, isolated", "Faster, Isolated", "Faster/Faster, isolated",
                                                               "Faster", "Faster/Faster, dispersed", "Faster, dispersed", "Dispersed/Faster, dispersed", 
                                                               "Dispersed", "Dispersed/Slower, dispersed", "Slower, dispersed", "Slower/Slower, dispersed"))
        
        #############################
        # calculate cluster bearing #
        #############################
        
        # get bearing (direction of change)
        clusters_raw$delta.bearing <- 
            bearing(as.matrix(clusters_raw[,c("average.tp1.longitude", 
                                              "average.tp1.latitude")]),
                    as.matrix(clusters_raw[,c("average.tp2.longitude", 
                                              "average.tp2.latitude")]))
        
        # convert bearing to compass direction
        # centre on directions 
        clusters_raw$delta.cardinal <- sapply(clusters_raw$delta.bearing, cardinal)  
        
        # convert to factor
        clusters_raw$delta.cardinal <- 
            factor(clusters_raw$delta.cardinal,
                   levels = c("N", "NNE", "NE", "ENE",
                              "E", "ESE", "SE", "SSE",
                              "S", "SSW", "SW", "WSW", 
                              "W", "WNW", "NW", "NNW"))
        
        # set levels 
        clusters_raw$delta.cardinal.long <- 
            plyr::revalue(clusters_raw$delta.cardinal, 
                          c("N" = "North", "NNE" = "North/Northeast", 
                            "NE" = "Northeast", "ENE" = "East/Northeast",
                            "E"= "East", "ESE" = "East/Southeast", 
                            "SE" = "Southeast", "SSE" = "South/Southeast", 
                            "S" = "South", "SSW" = "South/Southwest", 
                            "SW" = "Southwest", "WSW" = "West/Southwest", 
                            "W" = "West", "WNW" = "West/Northwest",
                            "NW" = "Northwest", "NNW" = "North/Northwest"))
        
        # write processed data to reactive vals 
        vals$clusters <- clusters_raw
    })
    
    
    #############################
    # process user strain data #
    #############################
    
    observeEvent(vals$strains_raw, {
        
        print("formatting strain data...")
        req(vals$strains_raw)
        strains_raw <- vals$strains_raw 
        
        # make column for strain date instead of having three separate columns
        strains_raw$strain.date <- dmy(paste(strains_raw$day, 
                                             strains_raw$month, 
                                             strains_raw$year, 
                                             sep = "-"))
        
        # categorize as single vs multistrain
        # >2 because counts are +1 of actual value 
        strains_raw <- strains_raw %>% 
            mutate(single.mult = ifelse(tp1.cluster.size.1.2>2,
                                        "Multi strain clusters",
                                        "Single strain clusters"))
        
        
        strains_raw <- strains_raw %>% 
            mutate(delta.ecc.angle = angle(delta.ecc.0.1.0, delta.ecc.0.0.1))
        
        # use new column to assign ecc values speed, spread, and combined
        strains_raw <- strains_raw %>%
            mutate(delta.ecc.direction = cut(delta.ecc.angle,
                                             breaks = c(0, directions$degree_max, 360), 
                                             labels = c(directions$ecc.comb, 'F')),
                   delta.ecc.speed = cut(delta.ecc.angle, 
                                         breaks = c(0, directions$degree_max, 360), 
                                         labels = c(directions$ecc.speed, 'F')),
                   delta.ecc.spread = cut(delta.ecc.angle,
                                          breaks = c(0, directions$degree_max, 360), 
                                          labels = c(directions$ecc.spread, 'F')))
        
        # convert to factor
        strains_raw$delta.ecc.direction <- as.factor(strains_raw$delta.ecc.direction)
        
        # set levels 
        strains_raw$delta.ecc.direction <-  factor(strains_raw$delta.ecc.direction, 
                                                    levels = c("Slower", "Slower/Slower, isolated", "Slower, Isolated", "Isolated/Slower, isolated",
                                                               "Isolated", "Isolated/Faster, isolated", "Faster, Isolated", "Faster/Faster, isolated",
                                                               "Faster", "Faster/Faster, dispersed", "Faster, dispersed", "Dispersed/Faster, dispersed", 
                                                               "Dispersed", "Dispersed/Slower, dispersed", "Slower, dispersed", "Slower/Slower, dispersed"))

        
        #############################
        # calculate strain bearing #
        #############################
        
        # get bearing (direction of change)
        strains_raw$delta.bearing <- 
            bearing(as.matrix(strains_raw[,c("average.tp1.longitude", 
                                              "average.tp1.latitude")]),
                    as.matrix(strains_raw[,c("average.tp2.longitude", 
                                              "average.tp2.latitude")]))
        
        # convert bearing to compass direction
        # centre on directions 
        strains_raw$delta.cardinal <- sapply(strains_raw$delta.bearing, cardinal)  
        
        # convert to factor
        strains_raw$delta.cardinal <- 
            factor(strains_raw$delta.cardinal,
                   levels = c("N", "NNE", "NE", "ENE",
                              "E", "ESE", "SE", "SSE",
                              "S", "SSW", "SW", "WSW", 
                              "W", "WNW", "NW", "NNW"))
        
        # set levels 
        strains_raw$delta.cardinal.long <- 
            plyr::revalue(strains_raw$delta.cardinal, 
                          c("N" = "North", "NNE" = "North/Northeast", 
                            "NE" = "Northeast", "ENE" = "East/Northeast",
                            "E"= "East", "ESE" = "East/Southeast", 
                            "SE" = "Southeast", "SSE" = "South/Southeast", 
                            "S" = "South", "SSW" = "South/Southwest", 
                            "SW" = "Southwest", "WSW" = "West/Southwest", 
                            "W" = "West", "WNW" = "West/Northwest",
                            "NW" = "Northwest", "NNW" = "North/Northwest"))
        
        # write out to reactive vals object
        vals$strains <- strains_raw
    })
    
    
    
    ############################################## 
    # update selectize inputs based on user data #
    ##############################################
    
    # listen for changes to these reactive val dataframes 
    toListen <- reactive({
        list(vals$clusters,vals$strains)
    })
    
    observeEvent(toListen(), {
        
        print("updating selectize inputs...")
        req(vals$clusters)
        req(vals$strains)
        
        # update cluster sliders
        updateSelectInput(inputId = "tp1.cluster", label = "Cluster", choices = unique(vals$clusters$tp1.cluster),  selected = NULL)
        updateSelectInput(inputId = "interval", label = "Time point", choices = unique(vals$clusters$interval), selected = NULL)
        updateSelectInput(inputId = "type", label = "Type", choices = unique(vals$clusters$type),  selected = NULL)
        updateSliderInput(inputId = "cluster.size.1.2", label = "Cluster size", min = min(vals$clusters$cluster.size.1.2, na.rm = T), max =  max(vals$clusters$cluster.size.1.2, na.rm = T), value = c(min(vals$clusters$cluster.size.1.2, na.rm = T),  max(vals$clusters$cluster.size.1.2, na.rm = T)))
        updateSliderInput(inputId = "average.date", label = "Cluster date", min = min(vals$clusters$average.date, na.rm = T), max = max(vals$clusters$average.date, na.rm = T), value = c(min(vals$clusters$average.date, na.rm = T), max(vals$clusters$average.date, na.rm = T)))
        updateSliderInput(inputId = "ecc.0.0.1", label = "Temporal ECC", min = floor(min(vals$clusters$ecc.0.0.1, na.rm = T)), max = ceiling(max(vals$clusters$ecc.0.0.1, na.rm = T)), value = c(floor(min(vals$clusters$ecc.0.0.1, na.rm = T)), ceiling(max(vals$clusters$ecc.0.0.1, na.rm = T))))
        updateSliderInput(inputId = "ecc.0.1.0", label = "Geospatial ECC", min = floor(min(vals$clusters$ecc.0.1.0, na.rm = T)), max = ceiling(max(vals$clusters$ecc.0.1.0, na.rm = T)), value = c(floor(min(vals$clusters$ecc.0.1.0, na.rm = T)), ceiling(max(vals$clusters$ecc.0.1.0, na.rm = T))))
        updateSliderInput(inputId = "average.latitude", label = "Average latitude", min = floor(min(vals$clusters$average.latitude, na.rm = T)), max = ceiling(max(vals$clusters$average.latitude, na.rm = T)), value = c(floor(min(vals$clusters$average.latitude, na.rm = T)), ceiling(max(vals$clusters$average.latitude, na.rm = T))))
        updateSliderInput(inputId = "average.longitude", label = "Average longitude", min = floor(min(vals$clusters$average.longitude, na.rm = T)), max = ceiling(max(vals$clusters$average.longitude, na.rm = T)), value = c(floor(min(vals$clusters$average.longitude, na.rm = T)), ceiling(max(vals$clusters$average.longitude, na.rm = T))))
        # update cluster delta sliders 
        updateSliderInput(inputId = "actual.cluster.size.tp2.size.tp1.size", label = "Delta cluster size", min = floor(min(vals$clusters$actual.cluster.size.tp2.size.tp1.size, na.rm = T)), max = ceiling(max(vals$clusters$actual.cluster.size.tp2.size.tp1.size, na.rm = T)), value = c(floor(min(vals$clusters$actual.cluster.size.tp2.size.tp1.size, na.rm = T)), ceiling(max(vals$clusters$actual.cluster.size.tp2.size.tp1.size, na.rm = T))))
        updateSliderInput(inputId = "number.of.novels.in.the.tp2.match", label = "Number of new strains", min = floor(min(vals$clusters$number.of.novels.in.the.tp2.match, na.rm = T)), max = ceiling(max(vals$clusters$number.of.novels.in.the.tp2.match, na.rm = T)), value = c(floor(min(vals$clusters$number.of.novels.in.the.tp2.match, na.rm = T)), ceiling(max(vals$clusters$number.of.novels.in.the.tp2.match, na.rm = T))))
        updateSliderInput(inputId = "actual.growth.rate.tp2.size.tp1.size.tp1.size", label = "Overall growth rate", min = floor(min(vals$clusters$actual.growth.rate.tp2.size.tp1.size.tp1.size, na.rm = T)), max = ceiling(max(vals$clusters$actual.growth.rate.tp2.size.tp1.size.tp1.size, na.rm = T)), value = c(floor(min(vals$clusters$actual.growth.rate.tp2.size.tp1.size.tp1.size, na.rm = T)), ceiling(max(vals$clusters$actual.growth.rate.tp2.size.tp1.size.tp1.size, na.rm = T))))
        updateSliderInput(inputId = "novel.growth.tp2.size.tp2.size.number.of.novels", label = "Novel growth rate", min = floor(min(vals$clusters$novel.growth.tp2.size.tp2.size.number.of.novels, na.rm = T)), max = ceiling(max(vals$clusters$novel.growth.tp2.size.tp2.size.number.of.novels, na.rm = T)), value = c(floor(min(vals$clusters$novel.growth.tp2.size.tp2.size.number.of.novels, na.rm = T)), ceiling(max(vals$clusters$novel.growth.tp2.size.tp2.size.number.of.novels, na.rm = T))))
        updateSliderInput(inputId = "delta.ecc.0.0.1", label = "Delta temporal ECC", min = floor(min(vals$clusters$delta.ecc.0.0.1, na.rm = T)), max = ceiling(max(vals$clusters$delta.ecc.0.0.1, na.rm = T)), value = c(floor(min(vals$clusters$delta.ecc.0.0.1, na.rm = T)), ceiling(max(vals$clusters$delta.ecc.0.0.1, na.rm = T))))
        updateSliderInput(inputId = "delta.ecc.0.1.0", label = "Delta geospatial ECC", min = floor(min(vals$clusters$delta.ecc.0.1.0, na.rm = T)), max = ceiling(max(vals$clusters$delta.ecc.0.1.0, na.rm = T)), value = c(floor(min(vals$clusters$delta.ecc.0.1.0, na.rm = T)), ceiling(max(vals$clusters$delta.ecc.0.1.0, na.rm = T))))
        #update stain filters
        updateSelectInput(inputId = "country", label = "Country", choices = unique(vals$strains$country),  selected = NULL)
        updateSelectInput(inputId = "province", label = "Province", choices = unique(vals$strains$province),  selected = NULL)
        # regional facet inputs
        updateSelectizeInput(inputId = "regionProvince", label = "View provinces in: ", choices = unique(vals$strains$country), selected = NULL)
    })
    
    
    ###########################
    # reactive data filtering #
    ###########################
    
    clusters_r <- reactive({
        
        req(vals$clusters)
        data <- vals$clusters 
        data
        
        # 
        # # get top n
        # topn <- {if (input$number != "all") as.numeric(input$number) else nrow(data)}
        # 
        # filtered <- data %>%
        #     {if (!is.null(input$tp1.cluster)) filter(., tp1.cluster %in% input$tp1.cluster)  else . } %>%
        #     {if (!is.null(input$interval)) filter(., interval %in% input$interval)  else . } %>%
        #     {if (!is.null(input$type)) filter(., type %in% input$type)  else . } %>%
        #     filter(tp1.cluster.size.1.2 >= input$cluster.size.1.2[1] & tp1.cluster.size.1.2 <= input$cluster.size.1.2[2]) %>%
        #     filter(average.tp1.date >= input$average.date[1] & average.tp1.date <= input$average.date[2]) %>%
        #     filter(tp1.ecc.0.0.1 >= input$ecc.0.0.1[1] & tp1.ecc.0.0.1 <= input$ecc.0.0.1[2]) %>%
        #     filter(tp1.ecc.0.1.0 >= input$ecc.0.1.0[1] & tp1.ecc.0.1.0 <= input$ecc.0.1.0[2]) %>%
        #     filter(average.tp1.latitude >= input$average.latitude[1] & average.tp1.latitude <= input$average.latitude[2]) %>%
        #     filter(average.tp1.longitude >= input$average.longitude[1] & average.tp1.longitude <= input$average.longitude[2]) %>%
        #     # delta filters
        #     filter(actual.cluster.growth.tp2.size.tp1.size >= input$actual.cluster.size.tp2.size.tp1.size[1] & actual.cluster.growth.tp2.size.tp1.size <= input$actual.cluster.size.tp2.size.tp1.size[2]) %>%
        #     filter(number.of.novels.in.the.tp2.match >= input$number.of.novels.in.the.tp2.match[1] & number.of.novels.in.the.tp2.match <= input$number.of.novels.in.the.tp2.match[2]) %>%
        #     filter(actual.growth.rate.tp2.size.tp1.size.tp1.size >= input$actual.growth.rate.tp2.size.tp1.size.tp1.size[1] & actual.growth.rate.tp2.size.tp1.size.tp1.size <= input$actual.growth.rate.tp2.size.tp1.size.tp1.size[2]) %>%
        #     filter(novel.growth.tp2.size.tp2.size.number.of.novels >= input$novel.growth.tp2.size.tp2.size.number.of.novels[1] & novel.growth.tp2.size.tp2.size.number.of.novels <= input$novel.growth.tp2.size.tp2.size.number.of.novels[2]) %>%
        #     filter(delta.ecc.0.0.1 >= input$delta.ecc.0.0.1[1] & delta.ecc.0.0.1 <= input$delta.ecc.0.0.1[2]) %>%
        #     filter(delta.ecc.0.1.0 >= input$delta.ecc.0.1.0[1] & delta.ecc.0.1.0 <= input$delta.ecc.0.1.0[2])%>%
        #     
        #     # data subsetting 
        #     {if (input$subsets==1) arrange(., desc(abs(tp1.cluster.size.1.2))) else . }  %>%
        #     {if (input$subsets==2) arrange(., desc(abs(delta.ecc.0.0.1))) else . }  %>%
        #     {if (input$subsets==3) arrange(., desc(abs(delta.ecc.0.1.0))) else . } 
        # 
        # # grab n rows according to input
        # if (input$number != 99){
        #     topn <- as.numeric(input$number)
        #     unique.clust <- head(unique(filtered$tp1.cluster),topn)
        #     return(filtered %>% subset(tp1.cluster %in% unique.clust))
        # } else{  
        #     return(filtered)
        # }
        
    })
    
    # cluster shared data frame 
    clusters.sh <- SharedData$new(clusters_r, group = "clusters")
    
    # all strain filtering 
    strains_r <- reactive({
        req(vals$strains,clusters.sh)
        vals$strains %>%
            {if (!is.null(input$country)) filter(., country %in% input$country)  else . } %>%
            {if (!is.null(input$province)) filter(., province %in% input$province)  else . } %>%
            # filter by shared cluster selections
            filter(tp1.cluster %in% (clusters.sh$data(withSelection = TRUE) %>% filter(selected_ | is.na(selected_)) %>% pull(tp1.cluster)))
    })
    
    # strain shared data
    strains.sh <- SharedData$new(strains_r, group = "strains")
    
    #################################
    # data frames displayed in tabs #
    #################################
    
    # renaming of columns in datatable output is based on two time points
    # will need to be modified/generalized for additional time points 
    
    # clusters
    output$clusters_dt <- renderDataTable({
        datatable(clusters.sh,
                  extensions = c('Select', 'Buttons','Scroller'),
                  options = list(
                      columnDefs = list(
                          list(visible=FALSE, 
                               targets=c(7:11, 19:23, 26:27, 29, 36,37,40,41))),
                      select = list(
                          style = 'os', 
                          items = 'row'),
                      dom = 'Blfrtip',
                      rowId = 0,
                      buttons = list('selectRows', 'selectAll', 'selectNone', 'csv', 'excel'),
                      deferRender = TRUE,
                      scrollY = 500,
                      scrollX = 600,
                      scroller = TRUE),
                  colnames = c("Cluster" = "tp1.cluster",
                               "Cluster name" = "actual.tp1.cluster",
                               "Type" = "type",
                               "Transmission speed" = "delta.ecc.speed",
                               "Transmission spread" = "delta.ecc.spread",
                               "Movement direction" = "delta.cardinal.long",
                               "Number of additonal strains" = "actual.cluster.growth.tp2.size.tp1.size",
                               "Number of additional previously identified strains" = "number.of.additional.tp1.strains.in.the.tp2.match",
                               "Number of additional novel strains" = "number.of.novels.in.the.tp2.match",
                               "Delta geosptial ECC" = "delta.ecc.0.1.0",
                               "Delta temporal ECC" = "delta.ecc.0.0.1",
                               "Overall growth rate" = "actual.growth.rate.tp2.size.tp1.size.tp1.size",
                               "Novel growth rate" = "novel.growth.tp2.size.tp2.size.number.of.novels",
                               "Time point" = "interval",
                               "Centroid longitude" = "average.tp1.longitude",
                               "Centroid latitude" = "average.tp1.latitude",
                               "Temporal ECC" = "tp1.ecc.0.0.1",
                               "Geospatial ECC" = "tp1.ecc.0.1.0",
                               "Average cluster date" = "average.tp1.date",
                               "Cluster size +1" = "tp1.cluster.size.1.2",
                               "Average time (days) between strains" = "tp1.temp.avg.dist",
                               "Average distance (km) between strains" = "tp1.geo.avg.dist",
                               "Cluster size" = "tp1.cluster.size.1",
                               "First time cluster seen" = "first.time.this.cluster.was.seen.in.tp1",
                               "Last time cluster seen" = "last.time.this.cluster.was.seen.in.tp1"),
                  selection = 'none')
    }, server=F)
    
    # strains
    output$strains_dt <- renderDataTable({
        datatable(data = strains,
                  extensions = c('Select', 'Buttons','Scroller'),
                  options = list(
                      columnDefs = list(
                          list(visible=TRUE, 
                               targets=c(1:7, 11:12,45:46))),
                      select = list(
                          style = 'os', 
                          items = 'row'),
                      dom = 'Blfrtip',
                      rowId = 0,
                      buttons = list('selectRows', 'selectAll', 'selectNone', 'csv', 'excel'),
                      deferRender = TRUE,
                      scrollY = 500,
                      scrollX = 600,
                      scroller = TRUE),              
                  colnames = c("Time point" = "interval",
                                "Strain" = "strain",
                               "Country" = "country",
                               "Province" = "province",
                               "City" = "city",
                               "Latitude" = "latitude",
                               "Longitude" = "longitude",
                               "Cluster name" = "actual.tp1.cluster",
                               "Cluster" = "tp1.cluster",
                               "Date" = "strain.date",
                               "Single or multi strain cluster" = "single.mult"),
                  selection = 'none')
    }, server=F)
    
    
    ################################
    # cluster movement value boxes #
    ################################
    
    # these values were arbitrarily selected
    # in future, should be selected empirically
    # thought: will these cutoffs differ based on weekly, monthly, user defined time points?
    
    # active growth defined as 3x growth
    output$activegrowth <- renderValueBox({
        valueBox(
            subtitle = "clusters actively growing",
            value = clusters_r() %>% 
                subset(as.numeric(interval) == max(as.numeric(interval))) %>%
                subset(actual.growth.rate.tp2.size.tp1.size.tp1.size >= 3) %>% 
                nrow()
        )
    })
    
    # active spread defined as just a value of dispersed
    # perhaps more appropriate to select a range of delta ecc values?
    output$activespread <- renderValueBox({
        valueBox(
            subtitle = "clusters actively spreading",
            value = clusters_r() %>% 
                subset(as.numeric(interval) == max(as.numeric(interval))) %>%
                subset(delta.ecc.spread == "Dispersed") %>% 
                nrow()
        )
    })
    
    # significant local transmission defined based on isolated spread and 
    # cluster growth rate less than 3
    # better to select based on range of ECC values? 
    output$sigtransmission <- renderValueBox({
        valueBox(
            subtitle = "clusters with local transmission",
            value = clusters_r() %>% 
                slice(which(clusters_r()$ecc.spread=="Isolated")) %>%                 
                subset(as.numeric(interval) == max(as.numeric(interval))) %>%
                subset(actual.growth.rate.tp2.size.tp1.size.tp1.size >= 3) %>%
                nrow()
        )
    })
    
    
    ############################
    # reactive vals for facets #
    ############################
    
    # store "mini" data frames in reactiveValues
    # allows these data frames to be pulled and modified for manual brushing
    # then auto updating of graphs
    
    # initialize reactive values holder
    plotvals <- reactiveValues()
    
    # populate new every time shared data changes 
    observeEvent(strains.sh$data(), {
        
        # bubble plot faceted by country
        plotvals$bubble_c <- 
            strains.sh$data() %>%
            group_by(country, tp1.cluster, interval) %>%
            summarize(n = n(),
                      ecc.0.1.0 = mean(tp1.ecc.0.1.0),
                      ecc.0.0.1 = mean(tp1.ecc.0.0.1),
                      color = "#898a8c",
                      selected = F) %>%
            split(.$country)
        
        # bubble plot faceted by province
        plotvals$bubble_p <- strains.sh$data() %>%
            subset(country %in% input$regionProvince) %>%
            group_by(province, tp1.cluster, interval) %>%
            summarize(n = n(),
                      ecc.0.1.0 = mean(tp1.ecc.0.1.0),
                      ecc.0.0.1 = mean(tp1.ecc.0.0.1),
                      color = "#898a8c",
                      selected = F) %>%
            split(.$province)
        
        # # cluster growth rate by country 
        # bycountry <- strains %>%
        #     mutate(country = as.factor(country),
        #            interval = as.factor(interval),
        #            tp1 = as.factor(tp1)) %>%
        #     group_by(country, tp1.cluster, interval, tp1) %>%
        #     count() %>%
        #     ungroup() %>%
        #     complete(expand(.,interval, tp1, country, tp1.cluster), fill = list(n = NA)) 
        # 
        # growth = data.frame(expand(bycountry, country, tp1.cluster))
        # 
        # for(i in seq(length(levels(bycountry$interval))-1)){
        #     
        #     tp2 <- bycountry %>% 
        #         group_by(country) %>%
        #         subset(interval == i+1) %>%
        #         group_by(country, tp1.cluster) %>%
        #         summarize(n=sum(na.omit(n)))
        #     
        #     tp2novels <- bycountry %>% 
        #         group_by(country) %>%
        #         subset(interval == i+1) %>%
        #         group_by(country, tp1.cluster, tp1) %>%
        #         summarize(n=sum(n)) %>%
        #         subset(tp1==0)
        #     
        #     tp1 <- bycountry %>% 
        #         group_by(country) %>%
        #         subset(interval == i) %>%
        #         subset(tp1==1) %>%
        #         group_by(country, tp1.cluster) %>%
        #         summarize(n=sum(na.omit(n)))
        #     
        #     overall_name <- paste("overall", i, sep="_")
        #     novel_name <- paste("novel", i, sep="_")
        #     
        #     growth[[overall_name]] <- (tp2$n - tp1$n)/tp1$n
        #     growth[[novel_name]] <- tp2$n/(tp2$n -tp2novels$n)
        # }
        # 
        # # for more time points, use this method
        # plotvals$growth_c <- data.table::melt(setDT(growth),
        #                                       id.vars = c("country", "tp1.cluster"),
        #                                       measure = patterns(overall = "^overall_*",
        #                                                          novel = "^novel_*"),
        #                                       variable.name='interval') %>% 
        #     mutate(color = "#898a8c") %>%
        #     split(.$country)
        # 
        # 
        # # cluster growthp rate by province      
        # byprovince <- 
        #     strains.sh$data() %>%
        #     subset(country %in% input$regionProvince) %>%
        #     mutate(province = as.factor(province),
        #            interval = as.factor(interval),
        #            tp1 = as.factor(tp1),
        #            tp1.cluster = as.character(tp1.cluster)) %>%
        #     group_by(province, tp1.cluster, interval, tp1) %>%
        #     count() %>%
        #     ungroup() %>%
        #     complete(expand(.,interval, tp1, province, tp1.cluster), fill = list(n = NA)) 
        # 
        # growthp = data.frame(expand(byprovince, province, tp1.cluster))
        # 
        # for(i in seq(length(levels(byprovince$interval))-1)){
        #     
        #     tp2 <- byprovince %>% 
        #         group_by(province) %>%
        #         subset(interval == i+1) %>%
        #         group_by(province, tp1.cluster) %>%
        #         summarize(n=sum(na.omit(n)))
        #     
        #     tp2novels <- byprovince %>% 
        #         group_by(province) %>%
        #         subset(interval == i+1) %>%
        #         group_by(province, tp1.cluster, tp1) %>%
        #         summarize(n=sum(n)) %>%
        #         subset(tp1==0)
        #     
        #     tp1 <- byprovince %>% 
        #         group_by(province) %>%
        #         subset(interval == i) %>%
        #         subset(tp1==1) %>%
        #         group_by(province, tp1.cluster) %>%
        #         summarize(n=sum(na.omit(n)))
        #     
        #     overall_name <- paste("overall", i, sep="_")
        #     novel_name <- paste("novel", i, sep="_")
        #     
        #     growthp[[overall_name]] <- (tp2$n - tp1$n)/tp1$n
        #     growthp[[novel_name]] <- tp2$n/(tp2$n -tp2novels$n)
        # }
        # 
        # # for more time points, use this method
        # plotvals$growth_p <- as.data.frame(data.table::melt(setDT(growthp),
        #                                                     id.vars = c("province", "tp1.cluster"),
        #                                                     measure = patterns(overall = "^overall_*",
        #                                                                        novel = "^novel_*"),
        #                                                     variable.name='interval')) %>%
        #     mutate(color = "#898a8c") %>%
        #     split(.$province)
        # 
        # 
        # new cluster strains identified by country
        plotvals$strainbycluster_c <- strains.sh$data() %>%
            group_by(country, tp1.cluster, strain.date) %>%
            tally() %>%
            mutate(color = "#898a8c") %>%
            split(.$country)
        
        # new cluster strains identified by province
        plotvals$strainbycluster_p <- strains.sh$data() %>%
            subset(country %in% input$regionProvince) %>%
            group_by(province, tp1.cluster, strain.date) %>%
            tally() %>%
            mutate(color = "#898a8c") %>%
            split(.$province)
        
        # cumulative strain count per cluster by country
        plotvals$cumsum_c <- strains.sh$data() %>%
            group_by(country, tp1.cluster, strain.date) %>% 
            tally(!is.na(strain)) %>% 
            mutate(cumsum = cumsum(n),
                   color = "#898a8c") %>%
            split(.$country) 
        
        # cumulative strain count per cluster by province
        plotvals$cumsum_p <- strains.sh$data() %>%
            subset(country %in% input$regionProvince) %>%
            group_by(province, tp1.cluster, strain.date) %>% 
            tally(!is.na(strain)) %>% 
            mutate(cumsum = cumsum(n),
                   color = "#898a8c") %>%
            split(.$province) 
        
        # strains by date by country
        plotvals$strainsbydate_c <- strains.sh$data() 
        
        # strains by date by province
        plotvals$strainsbydate_p <- strains.sh$data() %>%
            subset(country %in% input$regionProvince)
        
        # radar plot for cluster spread and speed by country
        plotvals$radar_c <- strains_r() %>%
            group_by(country, tp1.cluster, interval) %>%
            summarize(n = n(),
                      ecc.0.0.1 = mean(tp1.ecc.0.0.1),
                      ecc.0.1.0 = mean(tp1.ecc.0.1.0),
                      ecc.comb = unique(as.character(delta.ecc.direction))) %>%
            rowid_to_column(., var = "rowid") 
        
        # radar plot for cluster spread and speed by province
        plotvals$radar_p <- strains_r() %>%
            subset(country %in% input$regionProvince) %>%
            group_by(province, tp1.cluster, interval) %>%
            summarize(n = n(),
                      ecc.0.0.1 = mean(tp1.ecc.0.0.1),
                      ecc.0.1.0 = mean(tp1.ecc.0.1.0),
                      ecc.comb = unique(as.character(delta.ecc.direction))) %>%
            rowid_to_column(., var = "rowid") 
        
    })
    
    
    #########
    # plots #
    #########
    
    # ECC bubble plots 
    # size of bubbles proportial to cluster size 
    
    output$bubble <- renderPlotly({
        
        # no faceting
        if(input$region == 1) {
            
            # geospatial bubble plot
            geo <- plot_ly() %>%
                add_trace(data = clusters, 
                          type = "scatter",
                          mode = "markers", 
                          x = ~tp1.ecc.0.1.0,
                          y = 1,
                          frame = ~interval, 
                          size = ~I(tp1.cluster.size.1),
                          color = ~I("#898a8c"), 
                          hovertemplate = ~paste('<b>', tp1.cluster,'</b>', '<br>',
                                                 'Geospatial ECC:', tp1.ecc.0.1.0,
                                                 '<extra></extra>',
                                                 sep = " "),
                          showlegend = FALSE,
                          marker = list(sizemode = "area", 
                                        opacity = 0.5)) %>%
                layout(xaxis = list(title = "Geospatial epicluster cohesion index",
                                    range = c(0, 1.05)),
                       yaxis = list(showticklabels = F),
                       annotations = list(
                           text= "Geospatial epicluster cohesion index",
                           xref = "paper",
                           yref = "paper",
                           yanchor = "bottom",
                           xanchor = "center",
                           align = "center",
                           x = 0.5,
                           y = -0.3,
                           showarrow = FALSE)) %>%
                highlight(color = "#1F78C8") 
            
            # temporal bubble plot 
            temp <- plot_ly() %>%
                add_trace(data = clusters, 
                          type = "scatter",
                          mode = "markers", 
                          x = ~tp1.ecc.0.0.1,
                          y = 1,
                          size = ~I(tp1.cluster.size.1),
                          color = ~I("#898a8c"), 
                          frame = ~interval, 
                          hovertemplate = ~paste('<b>', tp1.cluster,'</b>','<br>',
                                                 'Temporal ECC:', tp1.ecc.0.0.1,
                                                 '<extra></extra>',
                                                 sep = " "),
                          showlegend = FALSE,
                          marker = list(sizemode = "area",
                                        opacity = 0.5)) %>%
                layout(xaxis = list(title = "Temporal epicluster cohesion index",
                                    range = c(0, 1.05)),
                       yaxis = list(showticklabels = F),
                       annotations = list(text= "Temporal epicluster cohesion index",
                                          xref = "paper",
                                          yref = "paper",
                                          yanchor = "bottom",
                                          xanchor = "center",
                                          align = "center",
                                          x = 0.5,
                                          y = -0.3,
                                          showarrow = FALSE)) %>%
                highlight(color = "#1F78C8") 
            
            # geosptial and temporal ECC values plotted against one another bubble plot 
            both <- plot_ly() %>%
                add_trace(data = clusters,
                          type = "scatter",
                          mode = "markers", 
                          x = ~tp1.ecc.0.1.0,
                          y = ~tp1.ecc.0.0.1,
                          frame = ~interval,  
                          size = ~I(tp1.cluster.size.1),
                          color = ~I("#898a8c"), 
                          hovertemplate = ~paste('<b>', tp1.cluster,'</b>', '<br>',
                                                 'Geospatial ECC:', tp1.ecc.0.1.0, '<br>',
                                                 'Temporal ECC:', tp1.ecc.0.0.1,
                                                 '<extra></extra>',
                                                 sep = " "),
                          showlegend = FALSE, 
                          marker = list(sizemode = "area", 
                                        opacity = 0.5)) %>%
                layout(xaxis = list(title = "Geospatial epicluster cohesion index",
                                    range = c(0,1.05)),
                       yaxis = list(title = "Temporal epicluster cohesion index",
                                    range = c(0,1.05)),
                       annotations = list(
                           list(text= "Geospatial epicluster cohesion index",
                                xref = "paper",
                                yref = "paper",
                                yanchor = "bottom",
                                xanchor = "center",
                                align = "center",
                                x = 0.5,
                                y = -0.15,
                                showarrow = FALSE),
                           list(text= "Temporal epicluster cohesion index",
                                xref = "paper",
                                yref = "paper",
                                yanchor = "center",
                                xanchor = "center",
                                align = "center",
                                x = -0.075,
                                y = 0.5,
                                textangle = -90,
                                showarrow = FALSE))) %>%
                highlight(color = "#1F78C8")
            
            # Link Animated views
            subplot(both, subplot(geo, temp, nrows = 2, margin = 0.075), 
                    nrows = 1, 
                    margin = 0.025)
            
        } else if (input$region == 2){
            
            df <- plotvals$bubble_c
            
            # geospatial bubble plot
            geo <- df %>%
                lapply(function(d) plot_ly(d, # group and summarize here?
                                           # catch statement for less than 3 groups?
                                           type = "scatter",
                                           mode = "markers",
                                           x = ~ecc.0.1.0,
                                           y = 1,
                                           color = ~I(color),
                                           frame = ~interval,
                                           hovertemplate = ~paste('<b>', tp1.cluster,'</b>', '<br>',
                                                                  'Geospatial ECC:', ecc.0.1.0, '<br>',
                                                                  '<extra></extra>',
                                                                  sep = " "),                                           size = ~I(n),
                                           marker = list(sizemode = "area", 
                                                         opacity = 0.5),
                                           showlegend = FALSE,
                                           source = "bubble_c") %>%
                           layout(xaxis = list(range = c(-0.05,1.05), 
                                               title = "",
                                               zeroline = F),
                                  yaxis = list(showticklabels = F),
                                  annotations = list(text= ~unique(country),
                                                     xref = "paper",
                                                     yref = "paper",
                                                     yanchor = "bottom",
                                                     xanchor = "center",
                                                     align = "center",
                                                     x = 0.5,
                                                     y = 0.9,
                                                     showarrow = FALSE)) %>%
                           highlight(color = "#1F78C8")) %>%
                subplot(nrows = ceiling(length(unique(strains.sh$data() %>% 
                                                          pull(country)))/4),   
                        shareX = T) %>%
                layout(annotations = list(text= "Gesbospatial epicluster cohesion index",
                                          xref = "paper",
                                          yref = "paper",
                                          yanchor = "bottom",
                                          xanchor = "center",
                                          align = "center",
                                          x = 0.5,
                                          y = -0.175,
                                          font = list(size = 14),
                                          showarrow = FALSE))  %>% 
                event_register(event = 'plotly_click') %>%
                event_register(event = 'plotly_doubleclick')
            
            # temporal bubble plot
            temp <- df %>%
                lapply(function(d) plot_ly(d,                          
                                           type = "scatter",
                                           mode = "markers", 
                                           x = ~ecc.0.0.1,
                                           y = 1,
                                           color = ~I(color),
                                           frame = ~interval, 
                                           hovertemplate = ~paste('<b>', tp1.cluster,'</b>', '<br>',
                                                                  'Temporal ECC:', ecc.0.0.1,
                                                                  '<extra></extra>',
                                                                  sep = " "),                                           size = ~I(n),
                                           marker = list(sizemode = "area", 
                                                         opacity = 0.5),
                                           showlegend = FALSE,
                                           source = "bubble_c") %>%
                           layout(xaxis = list(range = c(-0.05,1.05), 
                                               title = "", 
                                               zeroline = F),
                                  yaxis = list(showticklabels = F),
                                  annotations = list(text= ~unique(country),
                                                     xref = "paper",
                                                     yref = "paper",
                                                     yanchor = "bottom",
                                                     xanchor = "center",
                                                     align = "center",
                                                     x = 0.5,
                                                     y = 0.9,
                                                     showarrow = FALSE)) %>%
                           highlight(color = "#1F78C8")) %>% 
                subplot(nrows = ceiling(length(unique(strains.sh$data() %>% 
                                                          pull(country)))/4), 
                        shareX = T) %>%
                layout(annotations = list(text= "Temporal epicluster cohesion index",
                                          xref = "paper",
                                          yref = "paper",
                                          yanchor = "bottom",
                                          xanchor = "center",
                                          align = "center",
                                          x = 0.5,
                                          y = -0.175,
                                          font = list(size = 14),
                                          showarrow = FALSE))  %>% 
                event_register(event = 'plotly_click') %>%
                event_register(event = 'plotly_doubleclick')
            
            # bubble plot of temporal ECC plotted against geospatial ECC
            both <- df %>%
                lapply(function(d) plot_ly(d,                          
                                           type = "scatter",
                                           mode = "markers", 
                                           y = ~ecc.0.0.1,
                                           x = ~ecc.0.1.0,
                                           color = ~I(color),
                                           frame = ~interval, 
                                           hovertemplate = ~paste('<b>', tp1.cluster,'</b>', '<br>',
                                                                  'Geospatial ECC:', ecc.0.1.0, '<br>',
                                                                  'Temporal ECC:', ecc.0.0.1,
                                                                  '<extra></extra>',
                                                                  sep = " "),                                           size = ~I(n),
                                           marker = list(sizemode = "area", 
                                                         opacity = 0.5),
                                           showlegend = FALSE,
                                           source = "bubble_c") %>%
                           layout(xaxis = list(range = c(-0.05,1.05),
                                               title = ""),
                                  yaxis = list(range = c(-0.05,1.05)),
                                  annotations = list(text= ~unique(country),
                                                     xref = "paper",
                                                     yref = "paper",
                                                     yanchor = "bottom",
                                                     xanchor = "center",
                                                     align = "center",
                                                     x = 0.5,
                                                     y = 0.95,
                                                     showarrow = FALSE)) %>%
                           highlight(color = "#1F78C8")) %>% 
                subplot(nrows = ceiling(length(unique(strains.sh$data() %>% 
                                                          pull(country)))/4), 
                        shareX = T) %>%
                layout(annotations = list( 
                    list(text= "Geospatial epicluster cohesion index",
                         xref = "paper",
                         yref = "paper",
                         yanchor = "bottom",
                         xanchor = "center",
                         align = "center",
                         x = 0.5,
                         y = -0.075,
                         font = list(size = 14),
                         showarrow = FALSE),
                    list(text= "Temporal epicluster cohesion index",
                         xref = "paper",
                         yref = "paper",
                         yanchor = "center",
                         xanchor = "center",
                         align = "center",
                         x = -0.1,
                         y = 0.3,
                         textangle = -90,
                         font = list(size = 14),
                         showarrow = FALSE))) %>% 
                event_register(event = 'plotly_click') %>%
                event_register(event = 'plotly_doubleclick')
            
            # Link Animated views
            subplot(both, 
                    subplot(geo, temp, nrows = 2, margin = 0.075), 
                    nrows = 1, 
                    margin = 0.02) %>%
                layout(height = ceiling(length(unique(strains.sh$data() %>%
                                                          pull(country)))/4)*100)
            
        } else if (input$region == 3) {
            
            df <- plotvals$bubble_p
            
            # geospatial bubble plot 
            geo <- df %>%
                lapply(function(d) plot_ly(d,                          
                                           type = "scatter",
                                           mode = "markers", 
                                           x = ~ecc.0.1.0,
                                           y = 1,
                                           color = ~I(color),
                                           frame = ~interval, 
                                           hovertemplate = ~paste('<b>', tp1.cluster,'</b>', '<br>',
                                                                  'Geospatial ECC:', ecc.0.1.0, '<br>',
                                                                  '<extra></extra>',
                                                                  sep = " "),                                           size = ~I(n),
                                           marker = list(sizemode = "area",
                                                         opacity = 0.5),
                                           showlegend = FALSE,
                                           source = "bubble_p") %>%
                           layout(xaxis = list(range = c(-0.05,1.05), 
                                               title = "", 
                                               zeroline = F),
                                  yaxis = list(showticklabels = F),
                                  annotations = list(text= ~unique(province),
                                                     xref = "paper",
                                                     yref = "paper",
                                                     yanchor = "bottom",
                                                     xanchor = "center",
                                                     align = "center",
                                                     x = 0.5,
                                                     y = 0.9,
                                                     showarrow = FALSE)) %>%
                           highlight(color = "#1F78C8")) %>% 
                subplot(nrows = ceiling(length(unique(strains.sh$data(withSelection=T)  %>%
                                                          subset(country %in% input$regionProvince) %>%
                                                          pull(province)))/4),
                        shareX = T) %>%
                layout(annotations = list(text= "Geospatial epicluster cohesion index",
                                          xref = "paper",
                                          yref = "paper",
                                          yanchor = "bottom",
                                          xanchor = "center",
                                          align = "center",
                                          x = 0.5,
                                          y = -0.175,
                                          font = list(size = 14),
                                          showarrow = FALSE)) %>% 
                event_register(event = 'plotly_click') %>%
                event_register(event = 'plotly_doubleclick')
            
            # temporal bubble plot
            temp <- df %>%
                lapply(function(d) plot_ly(d,                          
                                           type = "scatter",
                                           mode = "markers", 
                                           x = ~ecc.0.0.1,
                                           y = 1,
                                           color = ~I(color),
                                           frame = ~interval, 
                                           hovertemplate = ~paste('<b>', tp1.cluster,'</b>', '<br>',
                                                                  'Temporal ECC:', ecc.0.0.1,
                                                                  '<extra></extra>',
                                                                  sep = " "),                                           size = ~I(n),
                                           marker = list(sizemode = "area", 
                                                         opacity = 0.5),
                                           showlegend = FALSE,
                                           source = "bubble_p") %>%
                           layout(xaxis = list(range = c(-0.05,1.05), 
                                               title = "",
                                               zeroline = F),
                                  yaxis = list(showticklabels = F),
                                  annotations = list(text= ~unique(province),
                                                     xref = "paper",
                                                     yref = "paper",
                                                     yanchor = "bottom",
                                                     xanchor = "center",
                                                     align = "center",
                                                     x = 0.5,
                                                     y = 0.9,
                                                     showarrow = FALSE)) %>%
                           highlight(color = "#1F78C8")) %>% 
                subplot(nrows = ceiling(length(unique(strains.sh$data(withSelection=T)  %>%
                                                          subset(country %in% input$regionProvince) %>%
                                                          pull(province)))/4),
                        shareX = T) %>%
                layout(annotations = list(text= "Temporal epicluster cohesion index",
                                          xref = "paper",
                                          yref = "paper",
                                          yanchor = "bottom",
                                          xanchor = "center",
                                          align = "center",
                                          x = 0.5,
                                          y = -0.175,
                                          font = list(size = 14),
                                          showarrow = FALSE)) %>% 
                event_register(event = 'plotly_click') %>%
                event_register(event = 'plotly_doubleclick')
            
            # bubble plot of temporal ECC plotted against geospatial ECC
            both <- df %>%
                lapply(function(d) plot_ly(d,                          
                                           type = "scatter",
                                           mode = "markers", 
                                           y = ~ecc.0.0.1,
                                           x = ~ecc.0.1.0,
                                           color = ~I(color),
                                           frame = ~interval, 
                                           hovertemplate = ~paste('<b>', tp1.cluster,'</b>', '<br>',
                                                                  'Geospatial ECC:', ecc.0.1.0, '<br>',
                                                                  'Temporal ECC:', ecc.0.0.1,
                                                                  '<extra></extra>',
                                                                  sep = " "),                                           size = ~I(n),
                                           marker = list(sizemode = "area", 
                                                         opacity = 0.5),
                                           showlegend = FALSE,
                                           source = "bubble_p") %>%
                           layout(xaxis = list(range = c(-0.05,1.05), 
                                               title = ""),
                                  yaxis = list(range = c(-0.05,1.05)),
                                  annotations = list(text= ~unique(province),
                                                     xref = "paper",
                                                     yref = "paper",
                                                     yanchor = "bottom",
                                                     xanchor = "center",
                                                     align = "center",
                                                     x = 0.5,
                                                     y = 0.95,
                                                     showarrow = FALSE)) %>%
                           highlight(color = "#1F78C8")) %>% 
                subplot(nrows = ceiling(length(unique(strains.sh$data(withSelection=T)  %>%
                                                          subset(country %in% input$regionProvince) %>%
                                                          pull(province)))/4), 
                        shareX = T) %>%
                layout(annotations = list( 
                    list(text= "Geospatial epicluster cohesion index",
                         xref = "paper",
                         yref = "paper",
                         yanchor = "bottom",
                         xanchor = "center",
                         align = "center",
                         x = 0.5,
                         y = -0.075,
                         font = list(size = 14),
                         showarrow = FALSE),
                    list(text= "Temporal epicluster cohesion index",
                         xref = "paper",
                         yref = "paper",
                         yanchor = "center",
                         xanchor = "center",
                         align = "center",
                         x = -0.1,
                         y = 0.3,
                         textangle = -90,
                         font = list(size = 14),
                         showarrow = FALSE))) %>% 
                event_register(event = 'plotly_click') %>%
                event_register(event = 'plotly_doubleclick')
            
            # Link Animated views
            subplot(both, 
                    subplot(geo, temp, nrows = 2, margin = 0.075), 
                    nrows = 1, 
                    margin = 0.02) %>%
                layout(height = ceiling(length(unique(strains.sh$data()  %>%
                                                          subset(country %in% input$regionProvince) %>%
                                                          pull(province)))/4)*200)
        }
        
    })
    
    # histogram plots of ECC values
    # bin size = 0.01 
    
    output$ecc_histograms <- renderPlotly({
        
        # no faceting 
        if(input$region == 1){
            
            # geospatial ECC histogram
            geo <- plot_ly() %>%
                add_trace(data = strains.sh,
                          type = "histogram",
                          x = ~tp1.ecc.0.1.0,
                          frame = ~interval,
                          color = I("#898a8c"),
                          xbins = list(size = 0.01),
                          hovertemplate = ~paste('Bin range:', '%{x}', '<br>',
                                                 'Count:', '%{y}' ,
                                                 '<extra></extra>',
                                                 sep = " "),
                          showlegend = FALSE,
                          source = "histogram") %>%
                layout(xaxis = list(range = c(0,1),
                                    title = "Geospatial epicluster cohesion index"),
                       yaxis = list(range = c(0, strains.sh$data(withSelection = T) %>%
                                                          filter(selected_ | is.na(selected_)) %>%
                                                          group_by(interval) %>% 
                                                          count() %>% 
                                                          pull(n) %>% 
                                                          max())),
                       annotations = list(
                           list(text= "Geospatial epicluster cohesion index",
                                xref = "paper",
                                yref = "paper",
                                yanchor = "bottom",
                                xanchor = "center",
                                align = "center",
                                x = 0.5,
                                y = -0.3,
                                showarrow = FALSE),
                           list(text= "Count",
                                xref = "paper",
                                yref = "paper",
                                yanchor = "bottom",
                                xanchor = "center",
                                align = "center",
                                x = -0.075,
                                y = 0.3,
                                textangle = -90,
                                showarrow = FALSE))) %>%
                highlight(color = "#1F78C8") %>%
                event_register("plotly_click") %>%
                event_register("plotly_doubleclick")
            
            # delta geospatial ecc histogram
            delta_geo <- plot_ly() %>%
                add_trace(data = strains.sh,
                          type = "histogram",
                          x = ~delta.ecc.0.1.0,
                          frame = ~interval,
                          color = I("#898a8c"),
                          xbins = list(size = 0.01),
                          hovertemplate = ~paste('Bin range:', '%{x}', '<br>',
                                                 'Count:', '%{y}' ,
                                                 '<extra></extra>',
                                                 sep = " "),
                          showlegend = FALSE,
                          source = "histogram") %>%
                layout(xaxis = list(range = c(-1,1),
                                    title = "Delta geospatial epicluster cohesion index"),
                       yaxis = list(range = c(0, strains.sh$data(withSelection = T) %>%
                                                          filter(selected_ | is.na(selected_)) %>%
                                                          group_by(interval) %>% 
                                                          count() %>% 
                                                          pull(n) %>% 
                                                          max())),
                       annotations = list(
                           list(text= "Delta geospatial epicluster cohesion index",
                                xref = "paper",
                                yref = "paper",
                                yanchor = "bottom",
                                xanchor = "center",
                                align = "center",
                                x = 0.5,
                                y = -0.3,
                                showarrow = FALSE),
                           list(text= "Count",
                                xref = "paper",
                                yref = "paper",
                                yanchor = "bottom",
                                xanchor = "center",
                                align = "center",
                                x = -0.075,
                                y = 0.3,
                                textangle = -90,
                                showarrow = FALSE))) %>%
                highlight(color = "#1F78C8") %>%
                event_register("plotly_click") %>%
                event_register("plotly_doubleclick")
            
            # temporal histogram
            temp <- plot_ly() %>%
                add_trace(data = strains.sh,
                          type = "histogram",
                          x = ~tp1.ecc.0.0.1,
                          frame = ~interval,
                          color = I("#898a8c"), 
                          xbins = list(size = 0.01),
                          hovertemplate = ~paste('Bin range:', '%{x}', '<br>',
                                                 'Count:', '%{y}' ,
                                                 '<extra></extra>',
                                                 sep = " "),
                          showlegend = FALSE,
                          source = "histogram") %>%
                layout(xaxis = list(range = c(0,1),
                                    title = "Temporal epicluster cohesion index"),
                       yaxis = list(range = c(0, strains.sh$data(withSelection = T) %>%
                                                          filter(selected_ | is.na(selected_)) %>%
                                                          group_by(interval) %>% 
                                                          count() %>% 
                                                          pull(n) %>% 
                                                          max())),
                       annotations = list(
                           list(text= "Temporal epicluster cohesion index",
                                xref = "paper",
                                yref = "paper",
                                yanchor = "bottom",
                                xanchor = "center",
                                align = "center",
                                x = 0.5,
                                y = -0.3,
                                showarrow = FALSE),
                           list(text= "Count",
                                xref = "paper",
                                yref = "paper",
                                yanchor = "bottom",
                                xanchor = "center",
                                align = "center",
                                x = -0.075,
                                y = 0.3,
                                textangle = -90,
                                showarrow = FALSE))) %>%
                highlight(color = "#1F78C8") %>%
                event_register("plotly_click") %>%
                event_register("plotly_doubleclick")
            
            # delta temporal ECC histogram
            delta_temp <- plot_ly() %>%
                add_trace(data = strains.sh,
                          type = "histogram",
                          x = ~delta.ecc.0.0.1,
                          frame = ~interval,
                          color = I("#898a8c"), 
                          xbins = list(size = 0.01),
                          hovertemplate = ~paste('Bin range:', '%{x}', '<br>',
                                                 'Count:', '%{y}' ,
                                                 '<extra></extra>',
                                                 sep = " "),
                          showlegend = FALSE,
                          source = "histogram") %>%
                layout(xaxis = list(range = c(-1,1),
                                    title = "Delta temporal epicluster cohesion index"),
                       yaxis = list(range = c(0, strains.sh$data(withSelection = T) %>%
                                                          filter(selected_ | is.na(selected_)) %>% 
                                                          group_by(interval) %>% 
                                                          count() %>% 
                                                          pull(n) %>% 
                                                          max())),
                       annotations = list(
                           list(text= "Delta temporal epicluster cohesion index",
                                xref = "paper",
                                yref = "paper",
                                yanchor = "bottom",
                                xanchor = "center",
                                align = "center",
                                x = 0.5,
                                y = -0.3,
                                showarrow = FALSE),
                           list(text= "Count",
                                xref = "paper",
                                yref = "paper",
                                yanchor = "centre",
                                xanchor = "center",
                                align = "center",
                                x = -0.075,
                                y = 0.3,
                                textangle = -90,
                                showarrow = FALSE))) %>%
                highlight(color = "#1F78C8") %>%
                event_register("plotly_click") %>%
                event_register("plotly_doubleclick")
            
            subplot(subplot(geo, temp, nrows=1,  margin = 0.025),
                    subplot(delta_geo, delta_temp, nrows=1, margin = 0.025), 
                    nrows=2, 
                    margin = 0.075) 
            
            # faceted by country
        } else if (input$region == 2) {
            
            # geosptial histogram
            geo <- strains.sh$data() %>%
                split(.$country) %>%
                lapply(function(d) plot_ly(d, 
                                           type = "histogram",
                                           x = ~tp1.ecc.0.1.0,
                                           color = I("#898a8c"),
                                           xbins = list(size = 0.01),
                                           showlegend = FALSE,
                                           frame = ~interval,
                                           source = "histogram_c",
                                           hovertemplate = ~paste('Bin range:', '%{x}', '<br>',
                                                                  'Count:', '%{y}' ,
                                                                  '<extra></extra>',
                                                                  sep = " ")) %>%
                           layout(xaxis = list(range = c(0,1), 
                                               title = ""),
                                  yaxis = list(range = c(0, max(strains.sh$data() %>%
                                                                    group_by(country, interval) %>%
                                                                    count(cut_width(tp1.ecc.0.1.0, width=0.01)) %>%
                                                                    pull(n))*1.1)),
                                  annotations = list(text= ~unique(country),
                                                     xref = "paper",
                                                     yref = "paper",
                                                     yanchor = "bottom",
                                                     xanchor = "center",
                                                     align = "center",
                                                     x = 0.5,
                                                     y = 1,
                                                     showarrow = FALSE))) %>%
                subplot(nrows = ceiling(length(unique(strains.sh$data() %>% 
                                                          distinct(country) %>% 
                                                          pull(country)))/5), 
                        shareY = TRUE,
                        shareX = TRUE) %>%
                layout(annotations = list(
                    list(text= "Geospatial epicluster cohesion index",
                         xref = "paper",
                         yref = "paper",
                         yanchor = "bottom",
                         xanchor = "center",
                         align = "center",
                         x = 0.5,
                         y = -0.05,
                         font = list(size = 14),
                         showarrow = FALSE),
                    list(text= "Count",
                         xref = "paper",
                         yref = "paper",
                         yanchor = "bottom",
                         xanchor = "center",
                         align = "center",
                         x = -0.1,
                         y = 0.5,
                         textangle = -90,
                         font = list(size = 14),
                         showarrow = FALSE))) %>%
                event_register(event = 'plotly_click') %>%
                event_register(event = 'plotly_doubleclick')
            
            # temporal ecc histogram
            temp <- strains.sh$data() %>%
                split(.$country) %>%
                lapply(function(d) plot_ly(d, 
                                           type = "histogram",
                                           x = ~tp1.ecc.0.0.1,
                                           color = I("#898a8c"),
                                           xbins = list(size = 0.01),
                                           showlegend = FALSE,
                                           frame = ~interval,
                                           source = "histogram_c",
                                           hovertemplate = ~paste('Bin range:', '%{x}', '<br>',
                                                                  'Count:', '%{y}' ,
                                                                  '<extra></extra>',
                                                                  sep = " ")) %>%
                           layout(xaxis = list(range = c(0,1), 
                                               title = ""),
                                  yaxis = list(range = c(0, max(strains.sh$data() %>%
                                                                    group_by(country, interval) %>%
                                                                    count(cut_width(tp1.ecc.0.0.1, width=0.01)) %>%
                                                                    pull(n))*1.1)),
                                  annotations = list(text= ~unique(country),
                                                     xref = "paper",
                                                     yref = "paper",
                                                     yanchor = "bottom",
                                                     xanchor = "center",
                                                     align = "center",
                                                     x = 0.5,
                                                     y = 1,
                                                     showarrow = FALSE))) %>%
                subplot(nrows = ceiling(length(unique(strains.sh$data() %>% 
                                                          distinct(country) %>% 
                                                          pull(country)))/5), 
                        shareY = TRUE,
                        shareX = TRUE) %>%
                layout(annotations = list(
                    list(text= "Temporal epicluster cohesion index",
                         xref = "paper",
                         yref = "paper",
                         yanchor = "bottom",
                         xanchor = "center",
                         align = "center",
                         x = 0.5,
                         y = -0.05,
                         font = list(size = 14),
                         showarrow = FALSE),
                    list(text= "Count",
                         xref = "paper",
                         yref = "paper",
                         yanchor = "bottom",
                         xanchor = "center",
                         align = "center",
                         x = -0.1,
                         y = 0.5,
                         textangle = -90,
                         font = list(size = 14),
                         showarrow = FALSE))) %>%
                event_register(event = 'plotly_click') %>%
                event_register(event = 'plotly_doubleclick')
            
            # delta geospatial ecc
            delta_geo <-strains.sh$data() %>%
                split(.$country) %>%
                lapply(function(d) plot_ly(d, 
                                           type = "histogram",
                                           x = ~delta.ecc.0.1.0,
                                           color = I("#898a8c"),
                                           xbins = list(size = 0.01),
                                           showlegend = FALSE,
                                           frame = ~interval,
                                           source = "histogram_c",
                                           hovertemplate = ~paste('Bin range:', '%{x}', '<br>',
                                                                  'Count:', '%{y}' ,
                                                                  '<extra></extra>',
                                                                  sep = " ")) %>%
                           layout(xaxis = list(range = c(0,1), 
                                               title = ""),
                                  yaxis = list(range = c(0, max(strains.sh$data() %>%
                                                                    group_by(country, interval) %>%
                                                                    count(cut_width(delta.ecc.0.1.0, width=0.01)) %>%
                                                                    pull(n))*1.1)),
                                  annotations = list(text= ~unique(country),
                                                     xref = "paper",
                                                     yref = "paper",
                                                     yanchor = "bottom",
                                                     xanchor = "center",
                                                     align = "center",
                                                     x = 0.5,
                                                     y = 1,
                                                     showarrow = FALSE))) %>%
                subplot(nrows = ceiling(length(unique(strains.sh$data() %>% 
                                                          distinct(country) %>% 
                                                          pull(country)))/5), 
                        shareY = TRUE,
                        shareX = TRUE) %>%
                layout(annotations = list(
                    list(text= "Delta geospatial epicluster cohesion index",
                         xref = "paper",
                         yref = "paper",
                         yanchor = "bottom",
                         xanchor = "center",
                         align = "center",
                         x = 0.5,
                         y = -0.05,
                         font = list(size = 14),
                         showarrow = FALSE),
                    list(text= "Count",
                         xref = "paper",
                         yref = "paper",
                         yanchor = "bottom",
                         xanchor = "center",
                         align = "center",
                         x = -0.1,
                         y = 0.5,
                         textangle = -90,
                         font = list(size = 14),
                         showarrow = FALSE))) %>%
                event_register(event = 'plotly_click') %>%
                event_register(event = 'plotly_doubleclick')
            
            # delta temporal ecc histogram
            delta_temp <- strains.sh$data() %>%
                split(.$country) %>%
                lapply(function(d) plot_ly(d, 
                                           type = "histogram",
                                           x = ~delta.ecc.0.0.1,
                                           color = I("#898a8c"),
                                           xbins = list(size = 0.01),
                                           showlegend = FALSE,
                                           frame = ~interval,
                                           source = "histogram_c",
                                           hovertemplate = ~paste('Bin range:', '%{x}', '<br>',
                                                                  'Count:', '%{y}' ,
                                                                  '<extra></extra>',
                                                                  sep = " ")) %>%
                           layout(xaxis = list(range = c(0,1), 
                                               title = ""),
                                  yaxis = list(range = c(0, max(strains.sh$data() %>%
                                                                    group_by(country, interval) %>%
                                                                    count(cut_width(delta.ecc.0.0.1, width=0.01)) %>%
                                                                    pull(n))*1.1)),
                                  annotations = list(text= ~unique(country),
                                                     xref = "paper",
                                                     yref = "paper",
                                                     yanchor = "bottom",
                                                     xanchor = "center",
                                                     align = "center",
                                                     x = 0.5,
                                                     y = 1,
                                                     showarrow = FALSE))) %>%
                subplot(nrows = ceiling(length(unique(strains.sh$data() %>% 
                                                          distinct(country) %>% 
                                                          pull(country)))/5), 
                        shareY = TRUE,
                        shareX = TRUE) %>%
                layout(annotations = list(
                    list(text= "Temporal epicluster cohesion index",
                         xref = "paper",
                         yref = "paper",
                         yanchor = "bottom",
                         xanchor = "center",
                         align = "center",
                         x = 0.5,
                         y = -0.05,
                         font = list(size = 14),
                         showarrow = FALSE),
                    list(text= "Count",
                         xref = "paper",
                         yref = "paper",
                         yanchor = "bottom",
                         xanchor = "center",
                         align = "center",
                         x = -0.1,
                         y = 0.5,
                         textangle = -90,
                         font = list(size = 14),
                         showarrow = FALSE))) %>%
                event_register(event = 'plotly_click') %>%
                event_register(event = 'plotly_doubleclick')
            
            subplot(subplot(geo, temp, nrows=1, margin = 0.05), 
                    subplot(delta_geo, delta_temp, nrows=1, margin = 0.05), 
                    nrows=2, 
                    margin = 0.03)  %>%
                layout(height = ceiling(length(unique(strains.sh$data() %>% 
                                                          pull(country)))/4)*600)
            
            # facet by province
        } else if (input$region == 3) {
            
            df <- strains.sh$data(withSelection=T)  %>% 
                subset(country %in% input$regionProvince) %>%
                split(.$province)
            
            # geospatial ecc histogram
            geo <- df %>%
                lapply(function(d) plot_ly(d, 
                                           type = "histogram",
                                           x = ~tp1.ecc.0.1.0,
                                           color = I("#898a8c"),
                                           xbins = list(size = 0.01),
                                           showlegend = FALSE,
                                           frame = ~interval,
                                           source = "histogram_p",
                                           hovertemplate = ~paste('Bin range:', '%{x}', '<br>',
                                                                  'Count:', '%{y}' ,
                                                                  '<extra></extra>',
                                                                  sep = " ")) %>%
                           layout(xaxis = list(range = c(0,1), 
                                               title = ""),
                                  yaxis = list(range = c(0, max(strains.sh$data() %>%
                                                                    subset(country %in% input$regionProvince) %>%
                                                                    group_by(province, interval) %>%
                                                                    count(cut_width(tp1.ecc.0.1.0, width=0.01)) %>%
                                                                    pull(n))*1.1)),
                                  annotations = list(text= ~unique(province),
                                                     xref = "paper",
                                                     yref = "paper",
                                                     yanchor = "bottom",
                                                     xanchor = "center",
                                                     align = "center",
                                                     x = 0.5,
                                                     y = 1,
                                                     showarrow = FALSE))) %>%
                subplot(nrows = ceiling(length(unique(strains.sh$data() %>% 
                                                          subset(country %in% input$regionProvince) %>%
                                                          distinct(province) %>% 
                                                          pull(country)))/5), 
                        shareY = TRUE,
                        shareX = TRUE) %>%
                layout(annotations = list(
                    list(text= "Geospatial epicluster cohesion index",
                         xref = "paper",
                         yref = "paper",
                         yanchor = "bottom",
                         xanchor = "center",
                         align = "center",
                         x = 0.5,
                         y = -0.05,
                         font = list(size = 14),
                         showarrow = FALSE),
                    list(text= "Count",
                         xref = "paper",
                         yref = "paper",
                         yanchor = "bottom",
                         xanchor = "center",
                         align = "center",
                         x = -0.1,
                         y = 0.5,
                         textangle = -90,
                         font = list(size = 14),
                         showarrow = FALSE))) %>%
                event_register(event = 'plotly_click') %>%
                event_register(event = 'plotly_doubleclick')
            
            # temporal ecc histogram
            temp <- df %>%
                lapply(function(d) plot_ly(d, 
                                           type = "histogram",
                                           x = ~tp1.ecc.0.0.1,
                                           color = I("#898a8c"),
                                           xbins = list(size = 0.01),
                                           showlegend = FALSE,
                                           frame = ~interval,
                                           source = "histogram_p",
                                           hovertemplate = ~paste('Bin range:', '%{x}', '<br>',
                                                                  'Count:', '%{y}' ,
                                                                  '<extra></extra>',
                                                                  sep = " ")) %>%
                           layout(xaxis = list(range = c(0,1), 
                                               title = ""),
                                  yaxis = list(range = c(0, max(strains.sh$data() %>%
                                                                    subset(country %in% input$regionProvince) %>%
                                                                    group_by(province, interval) %>%
                                                                    count(cut_width(tp1.ecc.0.0.1, width=0.01)) %>%
                                                                    pull(n))*1.1)),
                                  annotations = list(text= ~unique(province),
                                                     xref = "paper",
                                                     yref = "paper",
                                                     yanchor = "bottom",
                                                     xanchor = "center",
                                                     align = "center",
                                                     x = 0.5,
                                                     y = 1,
                                                     showarrow = FALSE))) %>%
                subplot(nrows = ceiling(length(unique(strains.sh$data() %>% 
                                                          subset(country %in% input$regionProvince) %>%
                                                          distinct(province) %>% 
                                                          pull(country)))/5), 
                        shareY = TRUE,
                        shareX = TRUE) %>%
                layout(annotations = list(
                    list(text= "Temporal epicluster cohesion index",
                         xref = "paper",
                         yref = "paper",
                         yanchor = "bottom",
                         xanchor = "center",
                         align = "center",
                         x = 0.5,
                         y = -0.05,
                         font = list(size = 14),
                         showarrow = FALSE),
                    list(text= "Count",
                         xref = "paper",
                         yref = "paper",
                         yanchor = "bottom",
                         xanchor = "center",
                         align = "center",
                         x = -0.1,
                         y = 0.5,
                         textangle = -90,
                         font = list(size = 14),
                         showarrow = FALSE))) %>%
                event_register(event = 'plotly_click') %>%
                event_register(event = 'plotly_doubleclick')
            
            # delta geospatial ecc histogram
            delta_geo <- df %>%
                lapply(function(d) plot_ly(d, 
                                           type = "histogram",
                                           x = ~delta.ecc.0.1.0,
                                           color = I("#898a8c"),
                                           xbins = list(size = 0.01),
                                           showlegend = FALSE,
                                           frame = ~interval,
                                           source = "histogram_p",
                                           hovertemplate = ~paste('Bin range:', '%{x}', '<br>',
                                                                  'Count:', '%{y}' ,
                                                                  '<extra></extra>',
                                                                  sep = " ")) %>%
                           layout(xaxis = list(range = c(0,1), 
                                               title = ""),
                                  yaxis = list(range = c(0, max(strains.sh$data() %>%
                                                                    subset(country %in% input$regionProvince) %>%
                                                                    group_by(province, interval) %>%
                                                                    count(cut_width(delta.ecc.0.1.0, width=0.01)) %>%
                                                                    pull(n))*1.1)),
                                  annotations = list(text= ~unique(province),
                                                     xref = "paper",
                                                     yref = "paper",
                                                     yanchor = "bottom",
                                                     xanchor = "center",
                                                     align = "center",
                                                     x = 0.5,
                                                     y = 1,
                                                     showarrow = FALSE))) %>%
                subplot(nrows = ceiling(length(unique(strains.sh$data() %>% 
                                                          subset(country %in% input$regionProvince) %>%
                                                          distinct(province) %>% 
                                                          pull(country)))/5), 
                        shareY = TRUE,
                        shareX = TRUE) %>%
                layout(annotations = list(
                    list(text= "Delta geospatial epicluster cohesion index",
                         xref = "paper",
                         yref = "paper",
                         yanchor = "bottom",
                         xanchor = "center",
                         align = "center",
                         x = 0.5,
                         y = -0.05,
                         font = list(size = 14),
                         showarrow = FALSE),
                    list(text= "Count",
                         xref = "paper",
                         yref = "paper",
                         yanchor = "bottom",
                         xanchor = "center",
                         align = "center",
                         x = -0.1,
                         y = 0.5,
                         textangle = -90,
                         font = list(size = 14),
                         showarrow = FALSE))) %>%
                event_register(event = 'plotly_click') %>%
                event_register(event = 'plotly_doubleclick')
            
            # delta temporal ecc histogram
            delta_temp <- df %>%
                lapply(function(d) plot_ly(d, 
                                           type = "histogram",
                                           x = ~delta.ecc.0.0.1,
                                           color = I("#898a8c"),
                                           xbins = list(size = 0.01),
                                           showlegend = FALSE,
                                           frame = ~interval,
                                           source = "histogram_p",
                                           hovertemplate = ~paste('Bin range:', '%{x}', '<br>',
                                                                  'Count:', '%{y}' ,
                                                                  '<extra></extra>',
                                                                  sep = " ")) %>%
                           layout(xaxis = list(range = c(0,1), 
                                               title = ""),
                                  yaxis = list(range = c(0, max(strains.sh$data() %>%
                                                                    subset(country %in% input$regionProvince) %>%
                                                                    group_by(province, interval) %>%
                                                                    count(cut_width(delta.ecc.0.0.1, width=0.01)) %>%
                                                                    pull(n))*1.1)),
                                  annotations = list(text= ~unique(province),
                                                     xref = "paper",
                                                     yref = "paper",
                                                     yanchor = "bottom",
                                                     xanchor = "center",
                                                     align = "center",
                                                     x = 0.5,
                                                     y = 1,
                                                     showarrow = FALSE))) %>%
                subplot(nrows = ceiling(length(unique(strains.sh$data() %>% 
                                                          subset(country %in% input$regionProvince) %>%
                                                          distinct(province) %>% 
                                                          pull(country)))/5), 
                        shareY = TRUE,
                        shareX = TRUE) %>%
                layout(annotations = list(
                    list(text= "Delta temporal epicluster cohesion index",
                         xref = "paper",
                         yref = "paper",
                         yanchor = "bottom",
                         xanchor = "center",
                         align = "center",
                         x = 0.5,
                         y = -0.05,
                         font = list(size = 14),
                         showarrow = FALSE),
                    list(text= "Count",
                         xref = "paper",
                         yref = "paper",
                         yanchor = "bottom",
                         xanchor = "center",
                         align = "center",
                         x = -0.1,
                         y = 0.5,
                         textangle = -90,
                         font = list(size = 14),
                         showarrow = FALSE))) %>%
                event_register(event = 'plotly_click') %>%
                event_register(event = 'plotly_doubleclick')
            
            subplot(subplot(geo, temp, nrows=1, margin = 0.05), 
                    subplot(delta_geo, delta_emp, nrows=1, margin = 0.05), 
                    nrows=2,
                    margin = 0.03)  %>%
                layout(height = ceiling(length(unique(strains.sh$data() %>% 
                                                          pull(country)))/5)*200)
        }
    })
    
    # radar plots to describe speed and spread of cluster transmission
    output$radar <- renderPlotly({
        
        # no faceting 
        if (input$region ==1) {
            
            clusters %>%
                mutate(ecc.comb = delta.ecc.direction) %>%
                group_by(interval) %>%
                count(ecc.comb, .drop=FALSE) %>%
                full_join(directions, by = c('ecc.comb')) %>% 
                subset(n>0) %>%
                plot_ly(type="scatterpolar",
                        r = ~n,
                        theta = ~degree.mid,
                        color  = I("#898a8c"),
                        frame = ~interval, 
                        hovertemplate = ~paste('<b>', ecc.comb, '</b>', '<br>',
                                               'Count:', '%{r}', 
                                               '<extra></extra>',
                                               sep = " "),
                        fill = 'toself',
                        source = "radar") %>%
                layout(showlegend = F,
                       margin = list(l = 100, 
                                     r = 100),
                       polar = list(angularaxis = list(rotation = 90,
                                                       direction = 'clockwise',
                                                       tickmode = 'array',
                                                       tickvals = c(0, 45,  90, 135, 180,
                                                                    225, 270, 315),
                                                       ticktext = c("Slower spread",
                                                                    HTML(paste("Slower spread,", "<br>", "more concentrated")),
                                                                    HTML(paste("More ", "<br>", "concentrated", sep="")),
                                                                    HTML(paste("Faster spread,", "<br>", "more concentrated")),
                                                                    "Faster spread",
                                                                    HTML(paste("Faster spread,", "<br>", "more disperse")),
                                                                    HTML(paste("More ", "<br>", "disperse", sep="")),
                                                                    HTML(paste("Slower spread,", "<br>", "more disperse")))))) 
            
            # facet by country 
        } else if (input$region == 2) {
            
            plotvals$radar_c %>%
                group_by(country, interval) %>%
                count(ecc.comb, .drop=FALSE) %>%
                full_join(directions, by = c('ecc.comb')) %>%
                na.omit() %>%
                subset(n>0) %>%
                arrange(cardinal) %>%
                plot_ly(type="scatterpolar",
                        frame = ~interval,
                        r = ~n,
                        split = ~country,
                        theta = ~degree.mid,
                        name = ~country,
                        hovertemplate = ~paste('<b>', ecc.comb, '</b>', '<br>',
                                               'Count:', '%{r}', 
                                               '<extra></extra>',
                                               sep = " "),
                        color  = I("#898a8c"),
                        fill = "tonext") %>%
                layout(margin = list(l = 100, 
                                     r = 100),
                       polar = list(angularaxis = list(rotation = 90,
                                                       direction = 'clockwise',
                                                       tickmode = 'array',
                                                       tickvals = c(0, 45,  90, 135, 180,
                                                                    225, 270, 315),
                                                       ticktext = c("Slower spread",
                                                                    HTML(paste("Slower spread,", "<br>", "more concentrated")),
                                                                    HTML(paste("More ", "<br>", "concentrated", sep="")),
                                                                    HTML(paste("Faster spread,", "<br>", "more concentrated")),
                                                                    "Faster spread",
                                                                    HTML(paste("Faster spread,", "<br>", "more disperse")),
                                                                    HTML(paste("More ", "<br>", "disperse", sep="")),
                                                                    HTML(paste("Slower spread,", "<br>", "more disperse"))))))
            
            # facet by province
        } else if (input$region == 3){
            
            plotvals$radar_p %>%
                group_by(province, interval) %>%
                count(ecc.comb, .drop=FALSE) %>%
                full_join(directions, by = c('ecc.comb')) %>%
                na.omit() %>%
                arrange(cardinal) %>%
                plot_ly(type="scatterpolar",
                        frame = ~interval,
                        r = ~n,
                        split = ~province,
                        theta = ~degree.mid,
                        name = ~province,
                        hovertemplate = ~paste('<b>', ecc.comb, '</b>', '<br>',
                                               'Count:', '%{r}', 
                                               '<extra></extra>',
                                               sep = " "),
                        color  = I("#898a8c"),
                        fill = "tonext") %>%
                layout(margin = list(l = 100, 
                                     r = 100),
                       polar = list(angularaxis = list(rotation = 90,
                                                       direction = 'clockwise',
                                                       tickmode = 'array',
                                                       tickvals = c(0, 45,  90, 135, 180,
                                                                    225, 270, 315),
                                                       ticktext = c("Slower spread",
                                                                    HTML(paste("Slower spread,", "<br>", "more concentrated")),
                                                                    HTML(paste("More ", "<br>", "concentrated", sep="")),
                                                                    HTML(paste("Faster spread,", "<br>", "more concentrated")),
                                                                    "Faster spread",
                                                                    HTML(paste("Faster spread,", "<br>", "more disperse")),
                                                                    HTML(paste("More ", "<br>", "disperse", sep="")),
                                                                    HTML(paste("Slower spread,", "<br>", "more disperse"))))))
        }
    })
    
    # change vector plot from which the cluster transmission speed and spread is defined
    output$changevector <- renderPlotly({
        
        # no faceting 
        if (input$region == 1) {
            
            # color settings settings
            # color wont change automatically because 
            # data is split by cluster
            op <- clusters.sh$data(withSelection = T)
            op$colour <- ifelse(is.na(op$selected_) | !op$selected_ , "#898a8c", "#1F78C8")
            
            plot_ly(type = "scatter", mode="markers") %>% 
                #add segment to connect each point to origin
                add_trace(data = clusters.sh,
                          x = ~delta.ecc.0.1.0, # geographical
                          y = ~delta.ecc.0.0.1, # temporal 
                          frame = ~interval,
                          color = I(op$colour),
                          opacity = 0.7,
                          showlegend = F,
                          hovertemplate = ~paste('<b>', tp1.cluster, '</b>', '<br>',
                                                 'Delta geospatial ECC:', '%{x}', '<br>',
                                                 'Delta temporal ECC:', '%{y}', 
                                                 '<extra></extra>',
                                                 sepx = " ")) %>%
                # set range and overlay arrow annotations
                layout(xaxis = list(range = c(-1.05, 1.05), 
                                    title = "Delta geospatial ECC value"),
                       yaxis = list(range = c(-1.05, 1.05),
                                    title = "Delta temporal ECC value")) %>%
                highlight(color = NULL)
            
            # facet by country  
        } else if (input$region == 2) {
            
            #strains.sh$data() %>%
            strains %>%  
               distinct(country, tp1.cluster, .keep_all = T) %>%
                split(.$country) %>%
                lapply(function(d) plot_ly(d,
                                           type = "scatter", 
                                           mode='markers',
                                           x = ~delta.ecc.0.1.0, # geographical
                                           y = ~delta.ecc.0.0.1, # temporal 
                                           color = I("#898a8c"),
                                           showlegend = F,
                                           opacity = 0.5,
                                           frame = ~interval, 
                                           hovertemplate = ~paste('<b>', tp1.cluster, '</b>', '<br>',
                                                                  'Delta geospatial ECC:', '%{x}', '<br>',
                                                                  'Delta temporal ECC:', '%{y}', 
                                                                  '<extra></extra>',
                                                                  sep = " ")) %>%
                           layout(xaxis = list(range = c(-1.05, 1.05),
                                               title = ""),
                                  yaxis = list(range = c(-1.05, 1.05),
                                               title = "")) %>%
                           add_annotations(text= ~unique(country),
                                           xref = "paper",
                                           yref = "paper",
                                           yanchor = "center",
                                           xanchor = "center",
                                           align = "center",
                                           x = 0.5,
                                           y = 1.1,
                                           showarrow = FALSE)) %>%
                subplot(nrows = ceiling(length(unique(strains #.sh$data() 
                                                      %>%
                                                          distinct(country, tp1.cluster, .keep_all = T) %>% 
                                                          pull(country)))/4), 
                        shareX = T, 
                        shareY = T) %>%
                layout(height = ceiling(length(unique(strains#.sh$data() 
                                                      %>%
                                                          distinct(country, tp1.cluster, .keep_all = T) %>% 
                                                          pull(country)))/4)*600,
                       annotations = list(
                           list(text= "Delta geospatial epicluster cohesion index",
                                xref = "paper",
                                yref = "paper",
                                yanchor = "center",
                                xanchor = "center",
                                align = "center",
                                x = 0.5,
                                y = -0.1,
                                font = list(size = 14),
                                showarrow = FALSE),
                           list(text= "Delta temporal epicluster cohesion index",
                                xref = "paper",
                                yref = "paper",
                                yanchor = "center",
                                xanchor = "center",
                                align = "center",
                                x = -0.1,
                                y = 0.5,
                                textangle = -90,
                                font = list(size = 14),
                                showarrow = FALSE)))
            
            # facet by province
        } else if (input$region == 3) {
            
            strains.sh$data(withSelection = T) %>%
                filter(selected_ | is.na(selected_)) %>%
                subset(country %in% input$regionProvince) %>% 
                distinct(province, tp1.cluster, .keep_all = T) %>%
                split(.$province) %>%
                lapply(function(d) plot_ly(d, 
                                           type = "scatter", 
                                           mode='markers',
                                           x = ~delta.ecc.0.1.0, # geographical
                                           y = ~delta.ecc.0.0.1, # temporal 
                                           color = I("#898a8c"),
                                           showlegend = F,
                                           opacity = 1,
                                           frame = ~interval, 
                                           hovertemplate = ~paste('<b>', tp1.cluster, '</b>', '<br>',
                                                                  'Delta geospatial ECC:', '%{x}', '<br>',
                                                                  'Delta temporal ECC:', '%{y}', 
                                                                  '<extra></extra>',
                                                                  sep = " ")) %>%
                           layout(xaxis = list(range = c(-1.05, 1.05),
                                               title = ""),
                                  yaxis = list(range = c(-1.05, 1.05),
                                               title = ""),
                                  annotations = list(
                                      list(text= ~unique(province),
                                           xref = "paper",
                                           yref = "paper",
                                           yanchor = "center",
                                           xanchor = "center",
                                           align = "center",
                                           x = 0.5,
                                           y = 1.1,
                                           showarrow = FALSE)))) %>%
                subplot(nrows = ceiling(length(unique(strains.sh$data() %>%
                                                          subset(country %in% input$regionProvince) %>%
                                                          distinct(province, tp1.cluster, .keep_all = T) %>% 
                                                          pull(province)))/4), 
                        shareX = T, 
                        shareY = T) %>%
                layout(height = ceiling(length(unique(strains.sh$data() %>%
                                                          subset(country %in% input$regionProvince) %>%
                                                          distinct(province, tp1.cluster, .keep_all = T) %>% 
                                                          pull(province)))/4)*150, 
                       annotations = list(
                           list(text= "Delta geospatial epicluster cohesion index",
                                xref = "paper",
                                yref = "paper",
                                yanchor = "center",
                                xanchor = "center",
                                align = "center",
                                x = 0.5,
                                y = -0.1,
                                font = list(size = 14),
                                showarrow = FALSE),
                           list(text= "Delta temporal epicluster cohesion index",
                                xref = "paper",
                                yref = "paper",
                                yanchor = "center",
                                xanchor = "center",
                                align = "center",
                                x = -0.1,
                                y = 0.5,
                                textangle = -90,
                                font = list(size = 14),
                                showarrow = FALSE)))
        }
    })
    
    # cluster growth
    # presently a single point 
    # when more time points added, this will be a line graph animation
    # (for now just a marker)
    
    output$cluster_growth <- renderPlotly({
        
    #     # no faceting 
    #     if (input$region == 1) {
    #         
    #         
    #         accumulate_by <- function(dat, var) {
    #             var <- lazyeval::f_eval(var, dat)
    #             lvls <- plotly:::getLevels(var)
    #             dats <- lapply(seq_along(lvls), function(x) {
    #                 cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
    #             })
    #             dplyr::bind_rows(dats)
    #         }
    #         
    #         
    #         # overall growth rate
    #         overall <- plot_ly(data = test) %>%
    #             add_trace(type = "scatter",
    #                       mode = "markers",
    #                       x = ~interval,
    #                       y = ~actual.growth.rate.tp2.size.tp1.size.tp1.size,
    #                       color = I("#898a8c"),
    #                       showlegend = F,
    #                       hovertemplate = ~paste('<b>', tp1.cluster, '</b>', '<br>',
    #                                              'Overall growth rate:', '%{y}', '<br>', 
    #                                              'Time point:', '%{x}', 
    #                                              '<extra></extra>',
    #                                              sep = " "),
    #                       frame=~interval) %>%
    #             layout(yaxis = list(rangemode = "tozero", 
    #                                 title = "Overall growth rate"),
    #                    xaxis = list(title = "Time point"),
    #                    annotations = list(
    #                        list(text= "Overall cluster growth rate",
    #                             xref = "paper",
    #                             yref = "paper",
    #                             yanchor = "center",
    #                             xanchor = "center",
    #                             align = "center",
    #                             textangle = -90,
    #                             x = -0.05,
    #                             y = 0.5,
    #                             showarrow = FALSE),
    #                        list(text= "Time point",
    #                             xref = "paper",
    #                             yref = "paper",
    #                             yanchor = "center",
    #                             xanchor = "center",
    #                             align = "center",
    #                             x = 0.5,
    #                             y = -0.1,
    #                             showarrow = FALSE)))%>%
    #             highlight(color = "#1F78C8")
    #         
    #         # novel growth rate
    #         novel <- plot_ly(data = clusters.sh) %>%
    #             add_trace(type = "scatter",
    #                       mode = "markers",
    #                       x = ~interval,
    #                       y = ~novel.growth.tp2.size.tp2.size.number.of.novels,
    #                       color = I("#898a8c"),
    #                       showlegend = F,
    #                       hovertemplate = ~paste('<b>', tp1.cluster, '</b>', '<br>',
    #                                              'Novel growth rate:', '%{y}', '<br>', 
    #                                              'Time point:', '%{x}', 
    #                                              '<extra></extra>',
    #                                              sep = " "),
    #                       frame=~interval) %>%
    #             layout(yaxis = list(rangemode = "tozero",
    #                                 title = "Novel growth rate"),
    #                    xaxis = list(title = "Time point"),
    #                    annotations = list(
    #                        list(text= "Novel cluster growth rate",
    #                             xref = "paper",
    #                             yref = "paper",
    #                             yanchor = "center",
    #                             xanchor = "center",
    #                             align = "center",
    #                             textangle = -90,
    #                             x = -0.05,
    #                             y = 0.5,
    #                             showarrow = FALSE),
    #                        list(text= "Time point",
    #                             xref = "paper",
    #                             yref = "paper",
    #                             yanchor = "center",
    #                             xanchor = "center",
    #                             align = "center",
    #                             x = 0.5,
    #                             y = -0.1,
    #                             showarrow = FALSE))) %>%
    #             highlight(color = "#1F78C8")
    #         
    #         subplot(overall, novel, nrows = 1, margin = 0.025)
    #         
    #         # facet by country
    #     } else if (input$region == 2) {
    #         
    #         # these calculations are repeated to grab some values for setting axes
    #         # could be cleaned up by putting everything in reactiveValues?
    #         
    #         bycountry <- 
    #             strains.sh$data(withSelection=T) %>%
    #             filter(selected_ | is.na(selected_)) %>%
    #             mutate(country = as.factor(country),
    #                    interval = as.factor(interval),
    #                    tp1 = as.factor(tp1)) %>%
    #             group_by(country, tp1.cluster, interval, tp1) %>%
    #             count() %>%
    #             ungroup() %>%
    #             complete(expand(.,interval, tp1, country, tp1.cluster), fill = list(n = NA)) 
    #         
    #         growth = data.frame(expand(bycountry, country, tp1.cluster))
    #         
    #         for(i in seq(length(levels(bycountry$interval))-1)){
    #             
    #             tp2 <- bycountry %>% 
    #                 group_by(country) %>%
    #                 subset(interval == i+1) %>%
    #                 group_by(country, tp1.cluster) %>%
    #                 summarize(n=sum(na.omit(n)))
    #             
    #             tp2novels <- bycountry %>% 
    #                 group_by(country) %>%
    #                 subset(interval == i+1) %>%
    #                 group_by(country, tp1.cluster, tp1) %>%
    #                 summarize(n=sum(n)) %>%
    #                 subset(tp1==0)
    #             
    #             tp1 <- bycountry %>% 
    #                 group_by(country) %>%
    #                 subset(interval == i) %>%
    #                 subset(tp1==1) %>%
    #                 group_by(country, tp1.cluster) %>%
    #                 summarize(n=sum(na.omit(n)))
    #             
    #             overall_name <- paste("overall", i, sep="_")
    #             novel_name <- paste("novel", i, sep="_")
    #             
    #             growth[[overall_name]] <- (tp2$n - tp1$n)/tp1$n
    #             growth[[novel_name]] <- tp2$n/(tp2$n -tp2novels$n)
    #         }
    #         
    #         # for more time points, use this method
    #         long_df <- as.data.frame(data.table::melt(setDT(growth),
    #                                                   id.vars = c("country", "tp1.cluster"),
    #                                                   measure = patterns(overall = "^overall_*",
    #                                                                      novel = "^novel_*"),
    #                                                   variable.name='interval'))
    #         
    #         overall_max <- max(na.omit(long_df$overall)[!is.infinite(na.omit(long_df$overall))])
    #         novel_max <- max(na.omit(long_df$novel)[!is.infinite(na.omit(long_df$novel))])
    #         
    #         # overall growth rate
    #         overall <- plotvals$growth_c  %>%
    #             lapply(function(d) plot_ly(d,
    #                                        type = "scatter",
    #                                        y = ~overall,
    #                                        x = ~interval, 
    #                                        frame = ~interval,
    #                                        hovertext = ~tp1.cluster,
    #                                        color = ~I(color),
    #                                        hovertemplate = ~paste('<b>', tp1.cluster, '</b>', '<br>',
    #                                                               'Ovwerall growth rate:', '%{y}', '<br>', 
    #                                                               'Time point:', '%{x}', 
    #                                                               '<extra></extra>',
    #                                                               sep = " "),
    #                                        showlegend = FALSE,
    #                                        source = "growth_c") %>%
    #                        layout(xaxis = list(title = ""), 
    #                               yaxis = list(title = "",
    #                                            range = c(0,overall_max)),
    #                               annotations = list(
    #                                   list(text= ~unique(country),
    #                                        xref = "paper",
    #                                        yref = "paper",
    #                                        yanchor = "center",
    #                                        xanchor = "center",
    #                                        align = "center",
    #                                        x = 0.5,
    #                                        y = 1.1,
    #                                        showarrow = FALSE)))) %>%
    #             subplot(nrows = ceiling(length(unique(long_df %>% 
    #                                                       pull(country)))/4),
    #                     shareX = T,
    #                     shareY = T) %>%
    #             layout(annotations = list(
    #                 list(text= "Time point",
    #                      xref = "paper",
    #                      yref = "paper",
    #                      yanchor = "center",
    #                      xanchor = "center",
    #                      align = "center",
    #                      x = 0.5,
    #                      y = -0.1,
    #                      font = list(size = 14),
    #                      showarrow = FALSE),
    #                 list(text= "Overall cluster growth",
    #                      xref = "paper",
    #                      yref = "paper",
    #                      yanchor = "center",
    #                      xanchor = "center",
    #                      align = "center",
    #                      x = -0.1,
    #                      y = 0.5,
    #                      textangle = -90,
    #                      font = list(size = 14),
    #                      showarrow = FALSE))) %>%
    #             event_register(event = 'plotly_click') %>%
    #             event_register(event = 'plotly_doubleclick')
    #         
    #         # novel growth rate
    #         novel <- plotvals$growth_c %>%
    #             lapply(function(d) plot_ly(d,
    #                                        type = "scatter",
    #                                        y = ~novel,
    #                                        x = ~interval, 
    #                                        frame = ~interval,
    #                                        hovertext = ~tp1.cluster,
    #                                        color = ~I(color),
    #                                        hovertemplate = ~paste('<b>', tp1.cluster, '</b>', '<br>',
    #                                                               'Novel growth rate:', '%{y}', '<br>', 
    #                                                               'Time point:', '%{x}', 
    #                                                               '<extra></extra>',
    #                                                               sep = " "),
    #                                        showlegend = FALSE,
    #                                        source = "growth_c") %>%
    #                        layout(xaxis = list(title = ""), 
    #                               yaxis = list(title = "",
    #                                            range = c(0,novel_max)),
    #                               annotations = list(
    #                                   list(text= ~unique(country),
    #                                        xref = "paper",
    #                                        yref = "paper",
    #                                        yanchor = "center",
    #                                        xanchor = "center",
    #                                        align = "center",
    #                                        x = 0.5,
    #                                        y = 1.1,
    #                                        showarrow = FALSE)))) %>%
    #             subplot(nrows = ceiling(length(unique(long_df %>% 
    #                                                       pull(country)))/4),
    #                     shareX = T,
    #                     shareY = T) %>%
    #             layout(annotations = list(
    #                 list(text= "Time point",
    #                      xref = "paper",
    #                      yref = "paper",
    #                      yanchor = "center",
    #                      xanchor = "center",
    #                      align = "center",
    #                      x = 0.5,
    #                      y = -0.1,
    #                      font = list(size = 14),
    #                      showarrow = FALSE),
    #                 list(text= "Novel cluster growth",
    #                      xref = "paper",
    #                      yref = "paper",
    #                      yanchor = "center",
    #                      xanchor = "center",
    #                      align = "center",
    #                      x = -0.1,
    #                      y = 0.5,
    #                      textangle = -90,
    #                      font = list(size = 14),
    #                      showarrow = FALSE))) %>%
    #             event_register(event = 'plotly_click') %>%
    #             event_register(event = 'plotly_doubleclick')
    #         
    #         subplot(overall, novel, margin = 0.05) %>%
    #             layout(height = ceiling(length(unique(long_df %>% 
    #                                                       pull(country)))/4)*100)
    #         
    #         # facet by province
    #     } else if (input$region == 3) {
    #         
    #         # these calculations are repeated to grab some values for setting axes
    #         # could be cleaned up by putting everything in reactiveValues?
    #         
    #         byprovince <- 
    #             strains.sh$data(withSelection=T) %>%
    #             filter(selected_ | is.na(selected_)) %>%
    #             subset(country %in% input$regionProvince) %>%
    #             mutate(province = as.factor(province),
    #                    interval = as.factor(interval),
    #                    tp1 = as.factor(tp1)) %>%
    #             group_by(province, tp1.cluster, interval, tp1) %>%
    #             count() %>%
    #             ungroup() %>%
    #             complete(expand(.,interval, tp1, province, tp1.cluster), fill = list(n = NA)) 
    #         
    #         growth = data.frame(expand(byprovince, province, tp1.cluster))
    #         
    #         for(i in seq(length(levels(byprovince$interval))-1)){
    #             
    #             tp2 <- byprovince %>% 
    #                 group_by(province) %>%
    #                 subset(interval == i+1) %>%
    #                 group_by(province, tp1.cluster) %>%
    #                 summarize(n=sum(na.omit(n)))
    #             
    #             tp2novels <- byprovince %>% 
    #                 group_by(province) %>%
    #                 subset(interval == i+1) %>%
    #                 group_by(province, tp1.cluster, tp1) %>%
    #                 summarize(n=sum(n)) %>%
    #                 subset(tp1==0)
    #             
    #             tp1 <- byprovince %>% 
    #                 group_by(province) %>%
    #                 subset(interval == i) %>%
    #                 subset(tp1==1) %>%
    #                 group_by(province, tp1.cluster) %>%
    #                 summarize(n=sum(na.omit(n)))
    #             
    #             overall_name <- paste("overall", i, sep="_")
    #             novel_name <- paste("novel", i, sep="_")
    #             
    #             growth[[overall_name]] <- (tp2$n - tp1$n)/tp1$n
    #             growth[[novel_name]] <- tp2$n/(tp2$n -tp2novels$n)
    #         }
    #         
    #         # for more time points, use this method
    #         long_df <- as.data.frame(data.table::melt(setDT(growth),
    #                                                   id.vars = c("province", "tp1.cluster"),
    #                                                   measure = patterns(overall = "^overall_*",
    #                                                                      novel = "^novel_*"),
    #                                                   variable.name='interval'))
    #         
    #         overall_max <- max(na.omit(long_df$overall)[!is.infinite(na.omit(long_df$overall))])
    #         novel_max <- max(na.omit(long_df$novel)[!is.infinite(na.omit(long_df$novel))])
    #         
    #         # overall growth rate
    #         overall <-  plotvals$growth_p %>%
    #             lapply(function(d) plot_ly(d,
    #                                        type = "scatter",
    #                                        y = ~overall,
    #                                        x = ~interval, 
    #                                        frame = ~interval,
    #                                        hovertext = ~tp1.cluster,
    #                                        color = ~I(color),
    #                                        hovertemplate = ~paste('<b>', tp1.cluster, '</b>', '<br>',
    #                                                               'Overall growth rate:', '%{y}', '<br>', 
    #                                                               'Time point:', '%{x}', 
    #                                                               '<extra></extra>',
    #                                                               sep = " "),
    #                                        showlegend = FALSE,
    #                                        source = "growth_p") %>%
    #                        layout(xaxis = list(title = ""), 
    #                               yaxis = list(title = "",
    #                                            range = c(0,overall_max)),
    #                               annotations = list(
    #                                   list(text= ~unique(province),
    #                                        xref = "paper",
    #                                        yref = "paper",
    #                                        yanchor = "center",
    #                                        xanchor = "center",
    #                                        align = "center",
    #                                        x = 0.5,
    #                                        y = 1.1,
    #                                        showarrow = FALSE)))) %>%
    #             subplot(nrows = ceiling(length(unique(long_df %>% 
    #                                                       pull(province)))/4),
    #                     shareX = T, 
    #                     shareY = T) %>%
    #             layout(annotations = list(
    #                 list(text= "Time point",
    #                      xref = "paper",
    #                      yref = "paper",
    #                      yanchor = "center",
    #                      xanchor = "center",
    #                      align = "center",
    #                      x = 0.5,
    #                      y = -0.1,
    #                      font = list(size = 14),
    #                      showarrow = FALSE),
    #                 list(text= "Overall cluster growth",
    #                      xref = "paper",
    #                      yref = "paper",
    #                      yanchor = "center",
    #                      xanchor = "center",
    #                      align = "center",
    #                      x = -0.1,
    #                      y = 0.5,
    #                      textangle = -90,
    #                      font = list(size = 14),
    #                      showarrow = FALSE))) %>%         
    #             event_register(event = 'plotly_click') %>%
    #             event_register(event = 'plotly_doubleclick')
    #         
    #         # novel growth rate
    #         novel <- plotvals$growth_p %>%
    #             lapply(function(d) plot_ly(d,
    #                                        type = "scatter",
    #                                        y = ~novel,
    #                                        x = ~interval, 
    #                                        frame = ~interval,
    #                                        hovertext = ~tp1.cluster,
    #                                        color = ~I(color),
    #                                        hovertemplate = ~paste('<b>', tp1.cluster, '</b>', '<br>',
    #                                                               'Novel growth rate:', '%{y}', '<br>', 
    #                                                               'Time point:', '%{x}', 
    #                                                               '<extra></extra>',
    #                                                               sep = " "),
    #                                        showlegend = FALSE,
    #                                        source = "growth_p") %>%
    #                        layout(xaxis = list(title = ""), 
    #                               yaxis = list(title = "",
    #                                            range = c(0, novel_max)),
    #                               annotations = list(
    #                                   list(text= ~unique(province),
    #                                        xref = "paper",
    #                                        yref = "paper",
    #                                        yanchor = "center",
    #                                        xanchor = "center",
    #                                        align = "center",
    #                                        x = 0.5,
    #                                        y = 1.1,
    #                                        showarrow = FALSE)))) %>%
    #             subplot(nrows = ceiling(length(unique(long_df %>% 
    #                                                       pull(province)))/4), 
    #                     shareX = T, 
    #                     shareY = T) %>%
    #             layout(annotations = list(
    #                 list(text= "Time point",
    #                      xref = "paper",
    #                      yref = "paper",
    #                      yanchor = "center",
    #                      xanchor = "center",
    #                      align = "center",
    #                      x = 0.5,
    #                      y = -0.1,
    #                      font = list(size = 14),
    #                      showarrow = FALSE),
    #                 list(text= "Novel cluster growth",
    #                      xref = "paper",
    #                      yref = "paper",
    #                      yanchor = "center",
    #                      xanchor = "center",
    #                      align = "center",
    #                      x = -0.1,
    #                      y = 0.5,
    #                      textangle = -90,
    #                      font = list(size = 14),
    #                      showarrow = FALSE))) %>%
    #             event_register(event = 'plotly_click') %>%
    #             event_register(event = 'plotly_doubleclick')
    #         
    #         subplot(overall, novel, margin = 0.05) %>%
    #             layout(height = ceiling(length(unique(long_df %>% 
    #                                                       pull(province)))/4)*150)
    #     }
    })

    # strain by cluster plots include number of novel strains identified by cluster
    # and cumulative strains identified bu cluster

    output$strainsbycluster <- renderPlotly({

        # no faceting
        if (input$region == 1) {

            # tally counts by cluster and strain date
            counts <- strains.sh$data(withSelection = T) %>%
                filter(selected_ | is.na(selected_)) %>%
                group_by(tp1.cluster, strain.date) %>%
                tally()

            # mountain plot
            # novel strains identifed
            mountain <- plot_ly(data = counts) %>%
                add_trace(type = "scatter",
                          mode = "line",
                          color = I("#898a8c"),
                          x = ~strain.date,
                          y = ~n,
                          name = ~tp1.cluster,
                          hovertemplate = ~paste('<b>', tp1.cluster, '</b>', '<br>',
                                                 'Date:', '%{x}', '<br>',
                                                 'Count:', '%{y}',
                                                 '<extra></extra>',
                                                 sep = " "),
                          showlegend = F) %>%
                layout(yaxis = list(rangemode = "tozero"),
                       xaxis = list(tickvals = as.list(strains.sh$data() %>%
                                                           group_by(tp1) %>%
                                                           filter(strain.date == min(strain.date)) %>%
                                                           distinct(strain.date) %>%
                                                           pull(strain.date) %>%
                                                           sort()),
                                    ticktext = as.list(as.character(seq(length(strains.sh$data() %>%
                                                                                   group_by(tp1) %>%
                                                                                   filter(strain.date == min(strain.date)) %>%
                                                                                   distinct(strain.date) %>%
                                                                                   pull(strain.date) %>%
                                                                                   order())))),
                                    tickmode = "array")) %>%
                add_annotations(text= "interval",
                                xref = "paper",
                                yref = "paper",
                                yanchor = "center",
                                xanchor = "center",
                                align = "center",
                                x = 0.5,
                                y = -0.1,
                                font = list(size = 14),
                                showarrow = FALSE) %>%
                add_annotations(text= "Number of novel strains identified",
                                xref = "paper",
                                yref = "paper",
                                yanchor = "center",
                                xanchor = "center",
                                align = "center",
                                x = -0.1,
                                y = 0.5,
                                textangle = -90,
                                font = list(size = 14),
                                showarrow = FALSE) %>%
                highlight(color = "#1F78C8")

            # need to aggregate data for cumsum
            cumsum <- strains.sh$data(withSelection = T) %>%
                filter(selected_ | is.na(selected_)) %>%
                group_by(tp1.cluster, strain.date) %>%
                tally(!is.na(strain)) %>%
                mutate(cumsum = cumsum(n))

            # cumsum plot
            # cumulative strains identified
            cumplot <- plot_ly(data = cumsum) %>%
                add_trace(type = "scatter",
                          mode = "line",
                          color = I("#898a8c"),
                          x = ~strain.date,
                          y = ~cumsum,
                          name = ~tp1.cluster,
                          hovertemplate = ~paste('<b>', tp1.cluster, '</b>', '<br>',
                                                 'Date:', '%{x}', '<br>',
                                                 'Count:', '%{y}',
                                                 '<extra></extra>',
                                                 sep = " "),
                          showlegend = F) %>%
                layout(yaxis = list(rangemode = "tozero"),
                       xaxis = list(tickvals = as.list(strains.sh$data() %>%
                                                           group_by(tp1) %>%
                                                           filter(strain.date == min(strain.date)) %>%
                                                           distinct(strain.date) %>%
                                                           pull(strain.date) %>%
                                                           sort()),
                                    ticktext = as.list( as.character(seq(length(strains.sh$data() %>%
                                                                                    group_by(tp1) %>%
                                                                                    filter(strain.date == min(strain.date)) %>%
                                                                                    distinct(strain.date) %>%
                                                                                    pull(strain.date) %>%
                                                                                    order())))),
                                    tickmode = "array")) %>%
                add_annotations(text= "interval",
                                xref = "paper",
                                yref = "paper",
                                yanchor = "center",
                                xanchor = "center",
                                align = "center",
                                x = 0.5,
                                y = -0.1,
                                font = list(size = 14),
                                showarrow = FALSE) %>%
                add_annotations(text= "Cumulative number of strains identified",
                                xref = "paper",
                                yref = "paper",
                                yanchor = "center",
                                xanchor = "center",
                                align = "center",
                                x = -0.1,
                                y = 0.5,
                                textangle = -90,
                                font = list(size = 14),
                                showarrow = FALSE) %>%
                highlight(color = "#1F78C8")

            # subplot mountain and cumsum together
            subplot(mountain, cumplot, nrows = 1, margin = 0.05)

            # facet by country
        } else if (input$region == 2) {

            counts<- strains.sh$data() %>%
                group_by(country, tp1.cluster, strain.date) %>%
                tally()

            # aggregate data by country, cluster, and strain date
            # take cumsum of counts
            cumsum <- strains.sh$data() %>%
                group_by(country, tp1.cluster, strain.date) %>%
                tally(!is.na(strain)) %>%
                mutate(cumsum = cumsum(n))

            # mountain plot
            # novel strains identified
            mountain <- plotvals$strainbycluster_c %>%
                lapply(function(d) plot_ly(d,
                                           type = "scatter",
                                           mode = "line",
                                           color = ~I(color),
                                           x = ~strain.date,
                                           y = ~n,
                                           name = ~tp1.cluster,
                                           hovertemplate = ~paste('<b>', tp1.cluster, '</b>', '<br>',
                                                                  'Date:', '%{x}', '<br>',
                                                                  'Count:', '%{y}',
                                                                  '<extra></extra>',
                                                                  sep = " "),
                                           showlegend = F,
                                           source = "strainsbycluster_c") %>%
                           layout(xaxis = list(title = "",
                                               tickvals = as.list( strains.sh$data(withSelection = T) %>%
                                                                       group_by(tp1) %>%
                                                                       filter(strain.date == min(strain.date)) %>%
                                                                       distinct(strain.date) %>%
                                                                       pull(strain.date) %>%
                                                                       sort()),
                                               ticktext = as.list( as.character(seq(length(strains.sh$data(withSelection = T) %>%
                                                                                               group_by(tp1) %>%
                                                                                               filter(strain.date == min(strain.date)) %>%
                                                                                               distinct(strain.date) %>%
                                                                                               pull(strain.date) %>%
                                                                                               order())))),
                                               tickmode = "array"),
                                  yaxis = list(title = "",
                                               range = c(0, max(na.omit(counts$n))*1.1)),
                                  annotations = list(
                                      list(text= ~unique(country),
                                           xref = "paper",
                                           yref = "paper",
                                           yanchor = "center",
                                           xanchor = "center",
                                           align = "center",
                                           x = 0.5,
                                           y = 1.1,
                                           showarrow = FALSE))) %>%
                           highlight(color = "#1F78C8")) %>%
                subplot(nrows = ceiling(length(unique(counts %>%
                                                          pull(country)))/4),
                        shareX = T,
                        shareY=T,
                        margin = 0.025) %>%
                layout(annotations = list(
                    list(text= "Date",
                         xref = "paper",
                         yref = "paper",
                         yanchor = "center",
                         xanchor = "center",
                         align = "center",
                         x = 0.5,
                         y = -0.22,
                         font = list(size = 14),
                         showarrow = FALSE),
                    list(text= "Novel strain count",
                         xref = "paper",
                         yref = "paper",
                         yanchor = "center",
                         xanchor = "center",
                         align = "center",
                         x = -0.1,
                         y = 0.5,
                         textangle = -90,
                         font = list(size = 14),
                         showarrow = FALSE))) %>%
                event_register(event = 'plotly_click') %>%
                event_register(event = 'plotly_doubleclick')

            # cumsum plot
            # cumulative strains identified
            cumplot <- plotvals$cumsum_c %>%
                lapply(function(d) plot_ly(d,
                                           type = "scatter",
                                           mode = "line",
                                           color = ~I(color),
                                           x = ~strain.date,
                                           y = ~cumsum,
                                           name = ~tp1.cluster,
                                           hovertemplate = ~paste('<b>', tp1.cluster, '</b>', '<br>',
                                                                  'Date:', '%{x}', '<br>',
                                                                  'Count:', '%{y}',
                                                                  '<extra></extra>',
                                                                  sep = " "),
                                           showlegend = F,
                                           source = "strainsbycluster_c") %>%
                           layout(xaxis = list(title = "",
                                               tickvals = as.list( strains.sh$data() %>%
                                                                       group_by(tp1) %>%
                                                                       filter(strain.date == min(strain.date)) %>%
                                                                       distinct(strain.date) %>%
                                                                       pull(strain.date) %>%
                                                                       sort()),
                                               ticktext = as.list( as.character(seq(length(strains.sh$data() %>%
                                                                                               group_by(tp1) %>%
                                                                                               filter(strain.date == min(strain.date)) %>%
                                                                                               distinct(strain.date) %>%
                                                                                               pull(strain.date) %>%
                                                                                               order())))),
                                               tickmode = "array"),
                                  yaxis = list(title = "",
                                               range = c(0, max(na.omit(cumsum$cumsum))*1.1)),
                                  annotations = list(
                                      list(text= ~unique(country),
                                           xref = "paper",
                                           yref = "paper",
                                           yanchor = "center",
                                           xanchor = "center",
                                           align = "center",
                                           x = 0.5,
                                           y = 1.1,
                                           showarrow = FALSE))) %>%
                           highlight(color = "#1F78C8")) %>%
                subplot(nrows = ceiling(length(unique(cumsum %>%
                                                          pull(country)))/4),
                        shareX = T,
                        shareY=T,
                        margin = 0.025) %>%
                layout(annotations = list(
                    list(text= "Time point",
                         xref = "paper",
                         yref = "paper",
                         yanchor = "center",
                         xanchor = "center",
                         align = "center",
                         x = 0.5,
                         y = -0.22,
                         font = list(size = 14),
                         showarrow = FALSE),
                    list(text= "Cumulative strain count",
                         xref = "paper",
                         yref = "paper",
                         yanchor = "center",
                         xanchor = "center",
                         align = "center",
                         x = -0.1,
                         y = 0.5,
                         textangle = -90,
                         font = list(size = 14),
                         showarrow = FALSE))) %>%
                event_register(event = 'plotly_click') %>%
                event_register(event = 'plotly_doubleclick')

            # subplot mountain and cumsum together
            subplot(mountain, cumplot, margin = 0.05) %>%
                layout(height = ceiling(length(unique(counts %>% pull(country)))/4)*100)

            # facet by province
        } else if (input$region == 3) {

            # tally counts by province, cluster, and date
            counts <- strains.sh$data() %>%
                subset(country %in% input$regionProvince) %>%
                group_by(province, tp1.cluster, strain.date) %>%
                tally()

            # mountain plot
            # novel strains identified
            mountain <- plotvals$strainbycluster_p %>%
                lapply(function(d) plot_ly(d,
                                           type = "scatter",
                                           mode = "line",
                                           color = ~I(color),
                                           x = ~strain.date,
                                           y = ~n,
                                           name = ~tp1.cluster,
                                           hovertemplate = ~paste('<b>', tp1.cluster, '</b>', '<br>',
                                                                  'Date:', '%{x}', '<br>',
                                                                  'Count:', '%{y}',
                                                                  '<extra></extra>',
                                                                  sep = " "),
                                           showlegend = F,
                                           source = "strainsbycluster_p") %>%
                           layout(xaxis = list(title = "",
                                               tickvals = as.list( strains.sh$data(withSelection = T) %>%
                                                                       group_by(tp1) %>%
                                                                       filter(strain.date == min(strain.date)) %>%
                                                                       distinct(strain.date) %>%
                                                                       pull(strain.date) %>%
                                                                       sort()),
                                               ticktext = as.list( as.character(seq(length(strains.sh$data(withSelection = T) %>%
                                                                                               group_by(tp1) %>%
                                                                                               filter(strain.date == min(strain.date)) %>%
                                                                                               distinct(strain.date) %>%
                                                                                               pull(strain.date) %>%
                                                                                               order())))),
                                               tickmode = "array"),
                                  yaxis = list(title = "",
                                               range = c(0, max(na.omit(counts$n))*1.1)),
                                  annotations = list(list(text= ~unique(province),
                                                          xref = "paper",
                                                          yref = "paper",
                                                          yanchor = "center",
                                                          xanchor = "center",
                                                          align = "center",
                                                          x = 0.5,
                                                          y = 1.1,
                                                          showarrow = FALSE))) %>%
                           highlight(color = "#1F78C8")) %>%
                subplot(nrows = ceiling(length(unique(counts %>%
                                                          pull(province)))/4),
                        shareX = T,
                        shareY=T,
                        margin = 0.025) %>%
                layout(annotations = list(list(text= "Date",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "center",
                                               xanchor = "center",
                                               align = "center",
                                               x = 0.5,
                                               y = -0.22,
                                               font = list(size = 14),
                                               showarrow = FALSE),
                                          list(text= "Novel strain count",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "center",
                                               xanchor = "center",
                                               align = "center",
                                               x = -0.1,
                                               y = 0.5,
                                               textangle = -90,
                                               font = list(size = 14),
                                               showarrow = FALSE))) %>%
                event_register(event = 'plotly_click') %>%
                event_register(event = 'plotly_doubleclick')

            # aggregate data by province, cluster, and strain date
            # take cumsum of counts
            cumsum <- strains.sh$data() %>%
                subset(country %in% input$regionProvince) %>%
                group_by(province, tp1.cluster, strain.date) %>%
                tally(!is.na(strain)) %>%
                mutate(cumsum = cumsum(n))

            # cumsum plot
            # cumulative strains identified
            cumplot <- plotvals$cumsum_p %>%
                lapply(function(d) plot_ly(d,
                                           type = "scatter",
                                           mode = "line",
                                           color = ~I(color),
                                           x = ~strain.date,
                                           y = ~cumsum,
                                           name = ~tp1.cluster,
                                           hovertemplate = ~paste('<b>', tp1.cluster, '</b>', '<br>',
                                                                  'Date:', '%{x}', '<br>',
                                                                  'Count:', '%{y}',
                                                                  '<extra></extra>',
                                                                  sep = " "),
                                           showlegend = F,
                                           source = "strainsbycluster_p") %>%
                           layout(xaxis = list(title = "",
                                               tickvals = as.list(strains.sh$data(withSelection = T) %>%
                                                                      group_by(tp1) %>%
                                                                      filter(strain.date == min(strain.date)) %>%
                                                                      distinct(strain.date) %>%
                                                                      pull(strain.date) %>%
                                                                      sort()),
                                               ticktext = as.list(as.character(seq(length(strains.sh$data(withSelection = T) %>%
                                                                                              group_by(tp1) %>%
                                                                                              filter(strain.date == min(strain.date)) %>%
                                                                                              distinct(strain.date) %>%
                                                                                              pull(strain.date) %>%
                                                                                              order())))),
                                               tickmode = "array"),
                                  yaxis = list(title = "",
                                               range = c(0, max(na.omit(cumsum$cumsum))*1.1)),
                                  annotations = list(list(text= ~unique(province),
                                                          xref = "paper",
                                                          yref = "paper",
                                                          yanchor = "center",
                                                          xanchor = "center",
                                                          align = "center",
                                                          x = 0.5,
                                                          y = 1.1,
                                                          showarrow = FALSE))) %>%
                           highlight(color = "#1F78C8")) %>%
                subplot(nrows = ceiling(length(unique(cumsum %>%
                                                          pull(province)))/4),
                        shareX = T,
                        shareY=T,
                        margin = 0.025) %>%
                layout(annotations = list(
                    list(text= "Time point",
                         xref = "paper",
                         yref = "paper",
                         yanchor = "center",
                         xanchor = "center",
                         align = "center",
                         x = 0.5,
                         y = -0.22,
                         font = list(size = 14),
                         showarrow = FALSE),
                    list(text= "Cumulative strain count",
                         xref = "paper",
                         yref = "paper",
                         yanchor = "center",
                         xanchor = "center",
                         align = "center",
                         x = -0.1,
                         y = 0.5,
                         textangle = -90,
                         font = list(size = 14),
                         showarrow = FALSE))) %>%
                event_register(event = 'plotly_click') %>%
                event_register(event = 'plotly_doubleclick')

            # subplot mountain and cumsum together
            subplot(mountain, cumplot, nrows = 1, margin = 0.05) %>%
                layout(height = ceiling(length(unique(counts %>%
                                                          pull(province)))/4)*100)
        }
    })

    # number of new strains identified by date
    # not separated by cluster

    output$newstrainsbydate <- renderPlotly({

        # no faceting
        if (input$region == 1) {

            plot_ly(type = "histogram",
                    data = strains.sh,
                    histfunc = "count",
                    x = ~strain.date,
                    color = I("#898a8c"),
                    xbins = list(size = 86400000.0),
                    hovertemplate = ~paste('Count: %{y}', '<br>',
                                           'Date range: %{x}', '<extra></extra>', sep=" ")) %>%
                layout(barmode = "stack",
                       xaxis = list(title = "Time point",
                                    tickvals = as.list(strains.sh$data() %>%
                                                           group_by(tp1) %>%
                                                           filter(strain.date == min(strain.date)) %>%
                                                           distinct(strain.date) %>%
                                                           pull(strain.date) %>%
                                                           sort()),
                                    ticktext = as.list( as.character(seq(length(strains.sh$data() %>%
                                                                                    group_by(tp1) %>%
                                                                                    filter(strain.date == min(strain.date)) %>%
                                                                                    distinct(strain.date) %>%
                                                                                    pull(strain.date) %>%
                                                                                    order())))),
                                    tickmode = "array"),
                       yaxis = list(title = "Number of new strains identified"),
                       updatemenus = list(
                           list(active = -1,
                                x= -0.1,
                                type = 'buttons',
                                buttons = list(
                                    list(label = "By day",
                                         method = "restyle",
                                         args = list(list(xbins = list(size = 86400000.0)))),
                                    list(label = "By week",
                                         method = "restyle",
                                         args = list(list(xbins = list(size = 604800000.0)))),
                                    list(label = "By month",
                                         method = "restyle",
                                         args = list(list(xbins = list(size = "M1"))))
                                )))) %>%
                highlight(color = "#1F78C8")

            # facet by country
        } else if (input$region == 2) {

            # calc maximum count and use to set yaxis max
            counts <- plotvals$strainsbydate_c %>%
                group_by(country, strain.date) %>%
                tally()

            plotvals$strainsbydate_c %>%
                split(.$country) %>%
                lapply(function(d) plot_ly(d,
                                           type = "histogram",
                                           histfunc = "count",
                                           x = ~strain.date,
                                           color = I("#898a8c"),
                                           xbins = list(size = 86400000.0),
                                           showlegend = F,
                                           hovertemplate = ~paste('Count: %{y}', '<br>',
                                                                  'Date range: %{x}',
                                                                  '<extra></extra>',
                                                                  sep=" ")) %>%
                           layout(xaxis = list(title = "Time point",
                                               tickvals = as.list(strains.sh$data() %>%
                                                                      group_by(tp1) %>%
                                                                      filter(strain.date == min(strain.date)) %>%
                                                                      distinct(strain.date) %>%
                                                                      pull(strain.date) %>%
                                                                      sort()),
                                               ticktext = as.list( as.character(seq(length(strains.sh$data() %>%
                                                                                               group_by(tp1) %>%
                                                                                               filter(strain.date == min(strain.date)) %>%
                                                                                               distinct(strain.date) %>%
                                                                                               pull(strain.date) %>%
                                                                                               order())))),
                                               tickmode = "array"),
                                  yaxis = list(title = "",
                                               range = c(0, max(counts$n)*1.1)),
                                  annotations = list(
                                      list(text= ~unique(country),
                                           xref = "paper",
                                           yref = "paper",
                                           yanchor = "center",
                                           xanchor = "center",
                                           align = "center",
                                           x = 0.5,
                                           y = 1.1,
                                           showarrow = FALSE)),
                                  updatemenus = list(
                                      list(active = -1,
                                           x= -0.1,
                                           type = 'buttons',
                                           buttons = list(
                                               list(label = "By day",
                                                    method = "restyle",
                                                    args = list(list(xbins = list(size = 86400000.0)))),
                                               list(label = "By week",
                                                    method = "restyle",
                                                    args = list(list(xbins = list(size = 604800000.0)))),
                                               list(label = "By month",
                                                    method = "restyle",
                                                    args = list(list(xbins = list(size = "M1"))))
                                           ))))) %>%
                subplot(nrows = ceiling(length(unique(strains.sh$data(withSelection = T) %>%
                                                          pull(country)))/5),
                        shareX = T,
                        shareY = T) %>%
                layout(height = ceiling(length(unique(strains.sh$data(withSelection = T) %>%
                                                          pull(country)))/5)*100,
                       annotations = list(list(text= "Time point",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "center",
                                               xanchor = "center",
                                               align = "center",
                                               x = 0.5,
                                               y = -0.22,
                                               font = list(size = 14),
                                               showarrow = FALSE),
                                          list(text= "Novel strain count",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "center",
                                               xanchor = "center",
                                               align = "center",
                                               x = -0.05,
                                               y = 0.5,
                                               textangle = -90,
                                               font = list(size = 14),
                                               showarrow = FALSE)))

            # facet by province
        } else if (input$region == 3) {

            # calc maximum count and use to set yaxis max
            counts <- strains.sh$data() %>%
                subset(country %in% input$regionProvince) %>%
                group_by(province, strain.date) %>%
                tally()

            plotvals$strainsbydate_p %>%
                split(.$province) %>%
                lapply(function(d) plot_ly(d,
                                           type = "histogram",
                                           histfunc = "count",
                                           x = ~strain.date,
                                           color = I("#898a8c"),
                                           xbins = list(size = 86400000.0),
                                           showlegend = F,
                                           hovertemplate = ~paste('Count: %{y}', '<br>',
                                                                  'Date range: %{x}',
                                                                  '<extra></extra>',
                                                                  sep=" ")) %>%
                           layout(xaxis = list(title = "Time point",
                                               tickvals = as.list(strains.sh$data() %>%
                                                                      group_by(tp1) %>%
                                                                      filter(strain.date == min(strain.date)) %>%
                                                                      distinct(strain.date) %>%
                                                                      pull(strain.date) %>%
                                                                      sort()),
                                               ticktext = as.list( as.character(seq(length(strains.sh$data() %>%
                                                                                               group_by(tp1) %>%
                                                                                               filter(strain.date == min(strain.date)) %>%
                                                                                               distinct(strain.date) %>%
                                                                                               pull(strain.date) %>%
                                                                                               order())))),
                                               tickmode = "array"),
                                  yaxis = list(title = "",
                                               range = c(0, max(counts$n)*1.1)),
                                  annotations = list(
                                      list(text= ~unique(province),
                                           xref = "paper",
                                           yref = "paper",
                                           yanchor = "center",
                                           xanchor = "center",
                                           align = "center",
                                           x = 0.5,
                                           y = 1.1,
                                           showarrow = FALSE)),
                                  updatemenus = list(
                                      list(active = -1,
                                           x= -0.1,
                                           type = 'buttons',
                                           buttons = list(
                                               list(label = "By day",
                                                    method = "restyle",
                                                    args = list(list(xbins = list(size = 86400000.0)))),
                                               list(label = "By week",
                                                    method = "restyle",
                                                    args = list(list(xbins = list(size = 604800000.0)))),
                                               list(label = "By month",
                                                    method = "restyle",
                                                    args = list(list(xbins = list(size = "M1"))))))))) %>%
                subplot(nrows = ceiling(length(unique(strains.sh$data() %>%
                                                          subset(country %in% input$regionProvince) %>%
                                                          pull(province)))/5),
                        shareX = T,
                        shareY = T) %>%
                layout(height = ceiling(length(unique(strains.sh$data() %>%
                                                          subset(country %in% input$regionProvince) %>%
                                                          pull(province)))/5)*100,
                       annotations = list(
                           list(text= "Time point",
                                xref = "paper",
                                yref = "paper",
                                yanchor = "center",
                                xanchor = "center",
                                align = "center",
                                x = 0.5,
                                y = -0.22,
                                font = list(size = 14),
                                showarrow = FALSE),
                           list(text= "Novel strain count",
                                xref = "paper",
                                yref = "paper",
                                yanchor = "center",
                                xanchor = "center",
                                align = "center",
                                x = -0.05,
                                y = 0.5,
                                textangle = -90,
                                font = list(size = 14),
                                showarrow = FALSE)))
        }
    })
    
    # single vs multstrains by date
    # no regional faceting as overall very small number of single strain clusters
    
    output$singlevsmulti <- renderPlotly({
        
        # multi strain count
        multiplot <- plot_ly(type = "histogram") %>%
            add_trace(data = strains.sh$data(withSelection = T) %>%
                          filter(selected_ | is.na(selected_)) %>%
                          subset(single.mult == "Multi strain clusters"),
                      histfunc = "count",
                      x = ~strain.date,
                      color = I("#898a8c"),    
                      showlegend = F, 
                      hovertemplate = ~paste('Count:', '%{y}', '<br>',
                                             'Date:', '%{x}', '<br>', 
                                             '<extra></extra>',
                                             sep = " "),
                      xbins = list(size = 86400000.0)) %>%
            layout(xaxis = list(title = "Time point",
                                tickvals = as.list(strains.sh$data() %>%
                                                       group_by(tp1) %>%
                                                       filter(strain.date == min(strain.date)) %>%
                                                       distinct(strain.date) %>%
                                                       pull(strain.date) %>%
                                                       sort()),
                                ticktext = as.list(as.character(seq(length(strains.sh$data() %>% 
                                                                               group_by(tp1) %>% 
                                                                               filter(strain.date == min(strain.date)) %>% 
                                                                               distinct(strain.date) %>% 
                                                                               pull(strain.date) %>% 
                                                                               order())))),
                                tickmode = "array"),
                   annotations = list(
                       list(text= "Count",
                            xref = "paper",
                            yref = "paper",
                            yanchor = "center",
                            xanchor = "center",
                            align = "center",
                            textangle = -90,
                            x = -0.05,
                            y = 0.3,
                            font = list(size = 14),
                            showarrow = FALSE),
                       list(text= "New strains identified as part of a multi-strain cluster",
                            xref = "paper",
                            yref = "paper",
                            yanchor = "center",
                            xanchor = "left",
                            align = "left",
                            font = list(size = 14),
                            x = 0,
                            y = 1,
                            showarrow = FALSE)))
        
        # single strain counts
        singleplot <- plot_ly(type = "histogram") %>%
            add_trace(data = strains.sh$data(withSelection = T) %>%
                          filter(selected_ | is.na(selected_)) %>%
                          subset(single.mult == "Single strain clusters"),
                      histfunc = "count",
                      x = ~strain.date,
                      color = I("#898a8c"),     
                      showlegend = F,
                      hovertemplate = ~paste('Count:', '%{y}', '<br>',
                                             'Date:', '%{x}', '<br>', 
                                             '<extra></extra>',
                                             sep = " "),
                      xbins = list(size = 86400000.0)) %>%
            layout(xaxis = list(title = "Date"),
                   yaxis = list(range = c(0, max(strains.sh$data(withSelection = T) %>%
                                                     filter(selected_ | is.na(selected_)) %>%
                                                     subset(single.mult == "Multi strain clusters") %>%
                                                     group_by(strain.date) %>%
                                                     count() %>%
                                                     pull(n)))),
                   annotations = list(
                       list(text= "Count",
                            xref = "paper",
                            yref = "paper",
                            yanchor = "center",
                            xanchor = "center",
                            align = "center",
                            textangle = -90,
                            x = -0.05,
                            y = 0.5,
                            font = list(size = 14),
                            showarrow = FALSE),
                       list(text= "New strains identified as part of single-strain cluster",
                            xref = "paper",
                            yref = "paper",
                            yanchor = "center",
                            xanchor = "left",
                            align = "left",
                            font = list(size = 14),
                            x = 0,
                            y = 1,
                            showarrow = FALSE)))
        
        subplot(singleplot, multiplot, nrows = 2, shareX = T, margin = 0.075)
    })
    
    # mapbox token 
    mapboxToken <- "pk.eyJ1Ijoic2FtLWEtbGVlIiwiYSI6ImNrb2s0bXVpbzFhMGkybm5zeHI1dHR1aTgifQ.1K8o7OaSOWo_y5UdVH348w"    # You need your own token
    Sys.setenv("MAPBOX_TOKEN" = mapboxToken)
    
    output$cluster_map <- renderPlotly({
        plot_mapbox(mode = 'scattermapbox') %>%
            add_markers(data = clusters.sh,
                        name = "Clusters",
                        x = ~average.longitude,
                        y = ~average.latitude,
                        frame = ~interval,
                        color= I("#646566"),
                        size = ~log10(cluster.size.1.2)*10,
                        opacity = 0.6,
                        hovertemplate = ~paste(tp1.cluster, '<br>',
                                               'Avg distance between strains (km):', geo.average.cluster.distance.km,
                                               '<extra></extra>',
                                               sep = " ")) %>%
            config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN")) %>%
            layout(mapbox = list(zoom = 1,
                                 style = 'light',
                                 center = list(lon = 50, lat = 40))) %>%
            highlight(color = I("#1F78C8")) %>%            
            animation_opts(1000, redraw = T, transition = 100) %>% 
            animation_button(visible = T) %>%
            animation_slider(active = 0)
    })
    
    
    # polar plot describing cardinal direction of change
    output$cardinal_movement <- renderPlotly({
        # format data
        clusters.sh$data(withSelection = TRUE) %>%
            mutate(selfact = selection_factor(.),
                   cardinal = delta.cardinal) %>%
            group_by(selfact) %>%
            distinct(tp1.cluster, .keep_all = T) %>%
            count(cardinal, .drop=FALSE) %>%
            full_join(directions, by = c('cardinal')) %>%
            # initialize plot
            plot_ly(type="barpolar",
                    r = ~n,
                    theta = ~degree.mid,
                    opacity = 0.7,
                    name = ~cardinal,
                    hovertemplate = ~paste('<b>', cardinal, '</b><br>',
                                           'Count: ', n, '<br>',
                                           '<extra></extra>', sep=""),
                    color = ~selfact, 
                    colors = c("#1F78C8", "red")) %>%
            layout(showlegend = F,
                   polar = list(
                       angularaxis = list(
                           rotation = 90,
                           direction = 'clockwise',
                           tickmode = 'array',
                           tickvals = c(0, 22.5, 45, 67.5, 90, 112.5, 135, 157.5,180,
                                        202.5, 225, 247.5, 270, 292.5, 315, 337.5),
                           ticktext = c("N", "NNE", "NE", "ENE", "E", "ESE", "SE",
                                        "SSE", "S", "SSW", "SW", "WSW", "W", "WNW",
                                        "NW", "NNW"))))
    })
    
    ###################################################
    # plotly click events for regional faceted graphs #
    ###################################################
    
    # if clicking on faceted bubble plot 
    observeEvent(event_data("plotly_click", source = "bubble_c"), {
        
        bubble_c <- plotvals$bubble_c
        growth_c <- plotvals$growth_c
        strainbycluster_c <- plotvals$strainbycluster_c
        cumsum_c <- plotvals$cumsum_c
        
        # pull vars needed to get info for subsetting 
        d <- event_data("plotly_click", source = "bubble_c")
        x <- d %>% pull(x)
        y <- d %>% pull(y)
        i <- d %>% mutate(curveNumber = curveNumber + 1) %>% pull(curveNumber) 
        l <-  length(unique(strains.sh$data(withSelection = T) %>%
                                pull(country)))
        j <- NULL
        
        # deal with subplots 
        # i does not go back to one for subplots - keeps adding past actual
        if (i <= l ) {j = i}  # if the user clicks on bubble plot
        if (i > l & i <= l*2) {j = i - l} # if the user clicks on geospatial subplot  
        if (i > l*2) {j = i - 2*l} # if user clicks on temporal subplot
        
        # get cluster id of selected point
        clust <- bubble_c[[j]] %>%
            { if (i <= l)  subset(., ecc.0.1.0 == x) %>% subset(., ecc.0.0.1 == y)  else . } %>%
            { if (i > l & i <= l*2)  subset(., ecc.0.1.0 == x)  else . } %>%
            { if (i > l*2)  subset(., ecc.0.0.1 == x)  else . } %>%
            pull(tp1.cluster)
        
        # set select vals
        bubble_c <- lapply(bubble_c, function(x){
            x <- x %>% mutate(color = ifelse(tp1.cluster == clust, "#1F78C8", "#898a8c"))
            return(x)
        })
        
        growth_c <- lapply(growth_c, function(x){
            x <- x %>% mutate(color = ifelse(tp1.cluster == clust, "#1F78C8", "#898a8c"))
            return(x)
        })
        
        strainbycluster_c <- lapply(strainbycluster_c, function(x){
            x <- x %>% mutate(color = ifelse(tp1.cluster == clust, "#1F78C8", "#898a8c"))
            return(x)
        })
        
        cumsum_c <- lapply(cumsum_c, function(x){
            x <- x %>% mutate(color = ifelse(tp1.cluster == clust, "#1F78C8", "#898a8c"))
            return(x)
        })
        
        # subset strains by date
        plotvals$strainsbydate_c <- strains.sh$data(withSelection = T)  %>%
            filter(selected_ | is.na(selected_)) %>%
            subset(tp1.cluster == clust)
        
        # subset data used in radar plot for cluster transmission
        plotvals$radar_c <- strains_r() %>%
            subset(tp1.cluster == clust) %>%
            group_by(country, tp1.cluster, interval) %>%
            summarize(n = n(),
                      ecc.0.0.1 = mean(ecc.0.0.1),
                      ecc.0.1.0 = mean(ecc.0.1.0),
                      ecc.comb = unique(as.character(delta.ecc.direction))) %>%
            rowid_to_column(., var = "rowid")
        
        # return df in reactiveValues
        plotvals$bubble_c <- bubble_c
        plotvals$growth_c <- growth_c
        plotvals$strainbycluster_c <- strainbycluster_c
        plotvals$cumsum_c <- cumsum_c
    })
    
    
    # if clicking on faceted bubble plot 
    observeEvent(event_data("plotly_click", source = "bubble_p"), {
        
        bubble_p <- plotvals$bubble_p
        growth_p <- plotvals$growth_p
        strainbycluster_p <- plotvals$strainbycluster_p
        cumsum_p <- plotvals$cumsum_p
        
        # pull vars needed to get info for subsetting 
        d <- event_data("plotly_click", source = "bubble_p")
        x <- d %>% pull(x)
        y <- d %>% pull(y)
        i <- d %>% mutate(curveNumber = curveNumber + 1) %>% pull(curveNumber) 
        l <-  length(unique(strains.sh$data() %>%
                                subset(country %in% input$regionProvince) %>%
                                pull(province)))
        j <- NULL
        
        # deal with subplots
        # i does not go back to one for subplots - keeps adding past actual
        if (i <= l ) {j = i}  # if the user clicks on bubble plot
        if (i > l & i <= l*2) {j = i - l} # if the user clicks on geospatial subplot
        if (i > l*2) {j = i - 2*l} # if user clicks on temporal subplot
        
        # get cluster id of selected point
        clust <- bubble_p[[j]] %>%
            subset(ecc.0.1.0 == x) %>% 
            subset(ecc.0.0.1 == y) %>%
            pull(tp1.cluster)
        
        # set select vals
        bubble_p <- lapply(bubble_p, function(x){
            x <- x %>% mutate(color = ifelse(tp1.cluster == clust, "#1F78C8", "#898a8c"))
            return(x)
        })
        
        growth_p <- lapply(growth_p, function(x){
            x <- x %>% mutate(color = ifelse(tp1.cluster == clust, "#1F78C8", "#898a8c"))
            return(x)
        })
        
        strainbycluster_p <- lapply(strainbycluster_p, function(x){
            x <- x %>% mutate(color = ifelse(tp1.cluster == clust, "#1F78C8", "#898a8c"))
            return(x)
        })
        
        cumsum_p <- lapply(cumsum_p, function(x){
            x <- x %>% mutate(color = ifelse(tp1.cluster == clust, "#1F78C8", "#898a8c"))
            return(x)
        })
        
        # subset strains by date
        plotvals$strainsbydate_p <- strains.sh$data() %>%
            subset(tp1.cluster == clust)
        
        # subset data used in radar plot for cluster transmission
        plotvals$radar_p <- strains_r() %>%
            subset(country %in% input$regionProvince) %>%
            subset(tp1.cluster == clust) %>%
            group_by(province, tp1.cluster, interval) %>%
            summarize(n = n(),
                      ecc.0.0.1 = mean(ecc.0.0.1),
                      ecc.0.1.0 = mean(ecc.0.1.0),
                      ecc.comb = unique(as.character(delta.ecc.direction))) %>%
            rowid_to_column(., var = "rowid")
        
        # return df in reactiveValues
        plotvals$bubble_p <- bubble_p
        plotvals$growth_p <- growth_p
        plotvals$strainbycluster_p <- strainbycluster_p
        plotvals$cumsum_p <- cumsum_p
    })
    
    
    # if clicking on cluster growth rate faceted by country
    observeEvent(event_data("plotly_click", source = "growth_c"), {
        
        bubble_c <- plotvals$bubble_c
        growth_c <- plotvals$growth_c
        strainbycluster_c <- plotvals$strainbycluster_c
        cumsum_c <- plotvals$cumsum_c
        
        # pull vars needed to get info for subsetting 
        d <- event_data("plotly_click", source = "growth_c")
        x <- d %>% pull(x)
        y <- d %>% pull(y)
        i <- d %>% mutate(curveNumber = curveNumber + 1) %>% pull(curveNumber) 
        l <-  length(unique(strains.sh$data(withSelection = T) %>%
                                pull(country)))
        j <- NULL
        
        if (i <= l ) {j = i}  # if the user clicks on bubble plot
        if (i > l) {j = i - l} # if the user clicks on geospatial subplot
        
        clust <- growth_c[[j]] %>%
            { if (i <= l)  subset(., round(overall,3) == round(y, 3))   else . } %>%
            { if (i > l)  subset(., round(novel,3) == round(y,3)) else . } %>%
            slice(n=1) %>%
            pull(tp1.cluster)   
        
        # set select vals
        bubble_c <- lapply(bubble_c, function(x){
            x <- x %>% mutate(color = ifelse(tp1.cluster == clust, "#1F78C8", "#898a8c"))
            return(x)
        })
        
        growth_c <- lapply(growth_c, function(x){
            x <- x %>% mutate(color = ifelse(tp1.cluster == clust, "#1F78C8", "#898a8c"))
            return(x)
        })
        
        strainbycluster_c <- lapply(strainbycluster_c, function(x){
            x <- x %>% mutate(color = ifelse(tp1.cluster == clust, "#1F78C8", "#898a8c"))
            return(x)
        })
        
        cumsum_c <- lapply(cumsum_c, function(x){
            x <- x %>% mutate(color = ifelse(tp1.cluster == clust, "#1F78C8", "#898a8c"))
            return(x)
        })
        
        # subset strains by date
        plotvals$strainsbydate_c <- strains.sh$data(withSelection = T)  %>%
            filter(selected_ | is.na(selected_)) %>%
            subset(tp1.cluster == clust)
        
        # subset data used in radar plot for cluster transmission
        plotvals$radar_c <- strains_r() %>%
            subset(tp1.cluster == clust) %>%
            group_by(country, tp1.cluster, interval) %>%
            summarize(n = n(),
                      ecc.0.0.1 = mean(ecc.0.0.1),
                      ecc.0.1.0 = mean(ecc.0.1.0),
                      ecc.comb = unique(as.character(delta.ecc.direction))) %>%
            rowid_to_column(., var = "rowid")
        
        # return df in reactiveValues
        plotvals$bubble_c <- bubble_c
        plotvals$growth_c <- growth_c
        plotvals$strainbycluster_c <- strainbycluster_c
        plotvals$cumsum_c <- cumsum_c
    })
    
    # if clicking on cluster growth rate faceted by country
    observeEvent(event_data("plotly_click", source = "growth_p"), {
        
        bubble_p <- plotvals$bubble_p
        growth_p <- plotvals$growth_p
        strainbycluster_p <- plotvals$strainbycluster_p
        cumsum_p <- plotvals$cumsum_p
        
        # pull vars needed to get info for subsetting 
        d <- event_data("plotly_plick", source = "growth_p")
        x <- d %>% pull(x)
        y <- d %>% pull(y)
        i <- d %>% mutate(curveNumber = curveNumber + 1) %>% pull(curveNumber) 
        l <-  length(unique(strains.sh$data(withSelection = T) %>%
                                pull(country)))
        j <- NULL
        
        if (i <= l ) {j = i}  # if the user clicks on bubble plot
        if (i > l) {j = i - l} # if the user clicks on geospatial subplot
        
        clust <- growth_p[[j]] %>%
            { if (i <= l)  subset(., round(overall,3) == round(y, 3))   else . } %>%
            { if (i > l)  subset(., round(novel,3) == round(y,3)) else . } %>%
            slice(n=1) %>%
            pull(tp1.cluster)   
        
        # set select vals
        bubble_p <- lapply(bubble_p, function(x){
            x <- x %>% mutate(color = ifelse(tp1.cluster == clust, "#1F78C8", "#898a8c"))
            return(x)
        })
        
        growth_p <- lapply(growth_p, function(x){
            x <- x %>% mutate(color = ifelse(tp1.cluster == clust, "#1F78C8", "#898a8c"))
            return(x)
        })
        
        strainbycluster_p <- lapply(strainbycluster_p, function(x){
            x <- x %>% mutate(color = ifelse(tp1.cluster == clust, "#1F78C8", "#898a8c"))
            return(x)
        })
        
        cumsum_p <- lapply(cumsum_p, function(x){
            x <- x %>% mutate(color = ifelse(tp1.cluster == clust, "#1F78C8", "#898a8c"))
            return(x)
        })
        
        # subset strains by date
        plotvals$strainsbydate_p <- strains.sh$data()  %>%
            subset(tp1.cluster == clust)
        
        # subset data used in radar plot for cluster transmission
        plotvals$radar_p <- strains_r() %>%
            subset(tp1.cluster == clust) %>%
            group_by(country, tp1.cluster, interval) %>%
            summarize(n = n(),
                      ecc.0.0.1 = mean(ecc.0.0.1),
                      ecc.0.1.0 = mean(ecc.0.1.0),
                      ecc.comb = unique(as.character(delta.ecc.direction))) %>%
            rowid_to_polumn(., var = "rowid")
        
        # return df in reactiveValues
        plotvals$bubble_p <- bubble_p
        plotvals$growth_p <- growth_p
        plotvals$strainbycluster_p <- strainbycluster_p
        plotvals$cumsum_p <- cumsum_p
    })
    
    # if clicking on histogram faceted by country
    observeEvent(event_data("plotly_click", source = "histogram_c"), {
        
        # pull vars needed to get info for subsetting
        d <- event_data("plotly_click", source = "histogram_c")
        x <- d %>% pull(x)
        y <- d %>% pull(y)
        i <- d %>% mutate(curveNumber = curveNumber + 1) %>% pull(curveNumber)
        l <-  length(unique(strains.sh$data(withSelection = T) %>%
                                pull(country)))
        j <- NULL
        
        print(d)
    })
    
    
    # if clicking on faceted strains by cluster
    observeEvent(event_data("plotly_click", source = "strainsbycluster_c"), {
        
        bubble_c <- plotvals$bubble_c
        growth_c <- plotvals$growth_c
        strainbycluster_c <- plotvals$strainbycluster_c
        cumsum_c <- plotvals$cumsum_c
        
        # pull vars needed to get info for subsetting
        d <- event_data("plotly_click", source = "strainsbycluster_c")
        x <- d %>% pull(x)
        y <- d %>% pull(y)
        i <- d %>% mutate(curveNumber = curveNumber + 1) %>% pull(curveNumber)
        l <-  length(unique(strains.sh$data(withSelection = T) %>%
                                pull(country)))
        j <- NULL
        
        print(d)
    })
    
    
    ##################################################
    # plotly double click events to reset everything #
    ##################################################
    
    # listen for all different double click events 
    doubleclickevents <- reactive({
        list(event_data("plotly_doubleclick", source = "bubble_c"),
             event_data("plotly_doubleclick", source = "bubble_p"),
             event_data("plotly_doubleclick", source = "growth_c"),
             event_data("plotly_doubleclick", source = "growth_p"),
             event_data("plotly_doubleclick", source = "strainsbycluster_c"),
             event_data("plotly_doubleclick", source = "strainsbycluster_p"))
    })
    
    # double click to deselect all points
    observeEvent(doubleclickevents(), {
        
        bubble_c <- plotvals$bubble_c
        bubble_p <- plotvals$bubble_p
        growth_c <- plotvals$growth_c
        growth_p <- plotvals$growth_p
        strainbycluster_c <- plotvals$strainbycluster_c
        strainbycluster_p <- plotvals$strainbycluster_p
        cumsum_c <- plotvals$cumsum_c
        cumsum_p <- plotvals$cumsum_p
        
        bubble_c <- lapply(bubble_c, function(x){
            x <- x %>% mutate(color = "#898a8c")
            return(x)
        })
        
        bubble_p <- lapply(bubble_p, function(x){
            x <- x %>% mutate(color = "#898a8c")
            return(x)
        })
        
        growth_c <- lapply(growth_c, function(x){
            x <- x %>% mutate(color = "#898a8c")
            return(x)
        })
        
        growth_p <- lapply(growth_p, function(x){
            x <- x %>% mutate(color = "#898a8c")
            return(x)
        })
        
        strainbycluster_c <- lapply(strainbycluster_c, function(x){
            x <- x %>% mutate(color = "#898a8c")
            return(x)
        })
        
        strainbycluster_p <- lapply(strainbycluster_p, function(x){
            x <- x %>% mutate(color = "#898a8c")
            return(x)
        })
        
        cumsum_c <- lapply(cumsum_c, function(x){
            x <- x %>% mutate(color = "#898a8c")
            return(x)
        })
        
        cumsum_p <- lapply(cumsum_p, function(x){
            x <- x %>% mutate(color = "#898a8c")
            return(x)
        })
        
        plotvals$strainsbydate_c <- strains.sh$data() 
        
        plotvals$strainsbydate_p <- strains.sh$data() %>%
            subset(country %in% input$regionProvince)
        
        plotvals$radar_c <- strains_r() %>%
            group_by(country, tp1.cluster, interval) %>%
            summarize(n = n(),
                      ecc.0.0.1 = mean(ecc.0.0.1),
                      ecc.0.1.0 = mean(ecc.0.1.0),
                      ecc.comb = unique(as.character(delta.ecc.direction))) %>%
            rowid_to_column(., var = "rowid") %>%
            ungroup()
        
        plotvals$radar_p <- strains_r() %>%
            subset(country %in% input$regionProvince) %>%
            group_by(province, tp1.cluster, interval) %>%
            summarize(n = n(),
                      ecc.0.0.1 = mean(ecc.0.0.1),
                      ecc.0.1.0 = mean(ecc.0.1.0),
                      ecc.comb = unique(as.character(delta.ecc.direction))) %>%
            rowid_to_column(., var = "rowid") %>%
            ungroup()
        
        plotvals$bubble_c <- bubble_c
        plotvals$bubble_p <- bubble_p
        plotvals$growth_c <- growth_c
        plotvals$growth_p <- growth_p
        plotvals$strainbycluster_c <- strainbycluster_c
        plotvals$strainbycluster_p <- strainbycluster_p
        plotvals$cumsum_c <- cumsum_c
        plotvals$cumsum_p <- cumsum_p
        
    })
    
}


