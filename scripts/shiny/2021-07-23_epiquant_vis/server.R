##########
# server #
##########

library(shinydashboard) # the shiny dashboard
library(readxl) # for reading xlsx files 
library(shiny) # for running shiny apps
library(shinyWidgets) # for better slider/dropdown options
library(here) # for clear dir calling
library(tidyverse) # tidy data/ kind of used
library(pals) # for large colour palettes (similar to brewer)
library(plotly) # for interactive ggplots
library(crosstalk) # for shared data
library(DT) # data table
library(htmltools) # for html input
library(lubridate) # for dates 
library(geosphere) # for directions between points 
library(shinydashboardPlus) # for collapsable boxes
library(data.table) # for melting
library(rvest) # for downloading cardinal table 



############# 
# functions #
#############

angle <- function(x,y) { 
    z <- x + 1i * y
    res <- 90 - Arg(z) / pi * 180
    res %% 360
}

page <- read_html('http://snowfence.umn.edu/Components/winddirectionanddegreeswithouttable3.htm')
directions_raw <- page %>% html_node('td table') %>% html_table(header = TRUE)

directions <- directions_raw %>% 
    set_names(~tolower(sub(' Direction', '', .x))) %>% 
    slice(-1) %>% 
    separate(degree, c('degree_min', 'degree_max'), sep = '\\s+-\\s+', convert = TRUE)

# create a new column and assign a "speed" and a "spread"
# F = faster spread, S = slower reader, D = dispersed spread, C = concentrated spread

directions$ecc_speed <- c("Slower", "Slower", "Slower", "Slower",
                          "No change", "Faster", "Faster", "Faster",
                          "Faster", "Faster", "Faster", "Faster", 
                          "No change", "Slower", "Slower", "Slower")

directions$ecc_spread <- c("No change", "Isolated", "Isolated", "Isolated",
                           "Isolated", "Isolated", "Isolated", "Isolated",
                           "No change", "Dispersed", "Dispersed", "Dispersed", 
                           "Dispersed", "Dispersed", "Dispersed", "Dispersed")

directions$ecc_comb <- c("Slower", "Slower/Slower, isolated", "Slower, Isolated", "Isolated/Slower, isolated",
                         "Isolated", "Isolated/Faster, isolated", "Faster, Isolated", "Faster/Faster, isolated",
                         "Faster", "Faster/Faster, dispersed", "Faster, dispersed", "Dispersed/Faster, dispersed", 
                         "Dispersed", "Dispersed/Slower, dispersed", "Slower, dispersed", "Slower/Slower, dispersed")

# add midpoints
directions$degree_mid <- c(0,22.5, 45, 67.5, 90, 112.5, 135, 157.5, 180, 202.5,225, 247.5, 270, 292.5, 315, 337.5)



server <- function(input, output, session) { 
    
    
    #######################################
    # get user inputted data to visualize #
    #######################################
    
    # create reactive values
    vals <- reactiveValues(data_raw = NULL, data_proc = NULL, clusters = NULL, strains = NULL)
    
    # read data frame into reactive values
    observeEvent(input$userFile, {
        vals$data_raw <- read_xlsx(input$userFile$datapath) #n_max=1000
    }, ignoreNULL = T)
    
    #####################
    # process user data #
    #####################
    
    observeEvent(vals$data_raw, {
        
        print("new data!")
        
        # get data 
        eccdata <- vals$data_raw
        
        #################
        # clean eccdata #
        #################
        
        # rename columns, remove spaces, all lowercase
        print("naming nicely...")
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
                               "first_obs_tp1", "last_obs_tp1", 
                               "first_obs_tp2", "last_obs_tp2",
                               "tp1_cluster_size", "tp2_cluster_size", 
                               "delta_cluster_size", "num_additional_tp1_strains_tp2", 
                               "num_novel_tp2_strains", "overall_cluster_growth_rate", 
                               "cluster_novel_growth_rate", "type")
        
        # remove 0 for now as its causing problems
        eccdata <- eccdata %>% subset(tp1_cluster != 0 )
        
        print("coverting classes...")
        # convert columns to appropriate types
        # as.is = T specifies character stays as character
        # type.convert is table and used in read.table
        eccdata <- lapply(eccdata, type.convert, as.is = T)
        eccdata <- bind_rows(eccdata)
        
        # format date columns
        # these should be in a single column in new epiquant format
        eccdata$avg_tp1_date <- as.Date(eccdata$avg_tp1_date, format = "%d-%m-%y") 
        eccdata$avg_tp2_date <- as.Date(eccdata$avg_tp2_date, format = "%d-%m-%y")
        
        ##################################
        # calculate cluster transmission #
        ##################################
        
        eccdata <- eccdata %>% 
            mutate(ecc_angle_delta = angle(delta_ecc_0.1.0, delta_ecc_0.0.1))
        
        # use new column to assign ecc values speed, spread, and combined
        eccdata <- eccdata %>%
            mutate(ecc_direction_delta = cut(ecc_angle_delta,
                                             breaks = c(0, directions$degree_max, 360), 
                                             labels = c(directions$ecc_comb, 'F')),
                   ecc_speed = cut(ecc_angle_delta, 
                                   breaks = c(0, directions$degree_max, 360), 
                                   labels = c(directions$ecc_speed, 'F')),
                   ecc_spread = cut(ecc_angle_delta,
                                    breaks = c(0, directions$degree_max, 360), 
                                    labels = c(directions$ecc_spread, 'F')))
        
        # convert to factor
        eccdata$ecc_direction_delta <- as.factor(eccdata$ecc_direction_delta)
        
        # set levels 
        eccdata$ecc_direction_delta <-  factor(eccdata$ecc_direction_delta, 
                                               levels = c("Slower", "Slower/Slower, isolated", "Slower, Isolated", "Isolated/Slower, isolated",
                                                          "Isolated", "Isolated/Faster, isolated", "Faster, Isolated", "Faster/Faster, isolated",
                                                          "Faster", "Faster/Faster, dispersed", "Faster, dispersed", "Dispersed/Faster, dispersed", 
                                                          "Dispersed", "Dispersed/Slower, dispersed", "Slower, dispersed", "Slower/Slower, dispersed"))
        
        #############################
        # calculate cluster bearing #
        #############################
        
        # get bearing (direction of change)
        eccdata$bearing_delta <- bearing(as.matrix(eccdata[,c("avg_tp1_longitude", "avg_tp1_latitude")]),
                                         as.matrix(eccdata[,c("avg_tp2_longitude", "avg_tp2_latitude")]))
        
        # convert bearing to compass direction
        # centre on directions 
        eccdata$cardinal_delta <- sapply(eccdata$bearing_delta, function(x){
            
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
        
        
        # convert to factor
        eccdata$cardinal_delta <- as.factor(eccdata$cardinal_delta)
        
        # set levels
        levels(eccdata$cardinal_delta) <-  c("N", "NNE", "NE", "ENE",
                                             "E", "ESE", "SE", "SSE",
                                             "S", "SSW", "SW", "WSW", 
                                             "W", "WNW", "NW", "NNW")
        
        # set levels 
        eccdata$cardinal_delta_long <- plyr::revalue(eccdata$cardinal_delta, 
                                                     c("N" = "North", "NNE" = "North/Northeast", "NE" = "Northeast", "ENE" = "East/Northeast",
                                                       "E"= "East", "ESE" = "East/Southeast", "SE" = "Southeast", "SSE" = "South/Southeast", 
                                                       "S" = "South", "SSW" = "South/Southwest", "SW" = "Southwest", "WSW" = "West/Southwest", 
                                                       "W" = "West", "WNW" = "West/Northwest", "NW" = "Northwest", "NNW" = "North/Northwest"))
        
        # write processed data to reactive vals 
        # will show to user in table later
        vals$data_proc <- eccdata
        
        ##################################
        # pull out cluster specific info #
        ##################################
        
        print("pulling cluster data...")
        
        # tp1 cluster data
        clusters <- eccdata %>% 
            group_by(tp1_cluster) %>%
            subset(present_at_tp1==1) %>%
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
                      cluster_novel_growth_rate = mean(cluster_novel_growth_rate),
                      ecc_direction_delta = unique(ecc_direction_delta),
                      ecc_speed = unique(ecc_speed),
                      ecc_spread = unique(ecc_spread),
                      bearing_delta = mean(bearing_delta),
                      cardinal_delta = unique(cardinal_delta),
                      cardinal_delta_long = unique(cardinal_delta_long),
                      type = mean(type))
        
        
        # select only complete cases
        clusters_cc <- na.omit(clusters)
        
        # now reshape the data for tp1 and tp2 info
        clusters_long <- data.table::melt(setDT(clusters_cc), 
                                          measure.vars=list(c("avg_tp1_longitude", "avg_tp2_longitude"), 
                                                            c("avg_tp1_latitude", "avg_tp2_latitude"),
                                                            c("tp1_t0_ecc_0.0.1", "tp2_t0_ecc_0.0.1"),
                                                            c("tp1_t0_ecc_0.1.0", "tp2_t0_ecc_0.1.0"),
                                                            c("avg_tp1_date", "avg_tp2_date"),
                                                            c("tp1_cluster_size_2", "tp2_cluster_size_2"),
                                                            c("avg_tp1_temporal_dist_days", "avg_tp2_temporal_dist_days"),
                                                            c("avg_tp1_geo_dist_km", "avg_tp2_geo_dist_km"),
                                                            c("tp1_cluster_size", "tp2_cluster_size")),
                                          variable.name='timepoint', 
                                          value.name=c('avg_longitude', 'avg_latitude', 
                                                       "ecc_0.0.1", "ecc_0.1.0",
                                                       "avg_date", "cluster_size_2", 
                                                       "avg_temporal_dist", "avg_geo_dist",
                                                       "cluster_size"))
        
        # specify key for crosstalk shareddata
        clusters_long$key <- as.numeric(rownames(clusters_long))
        
        # write to reactive vals df 
        # to be displayed in table later
        vals$clusters <- clusters_long
        
        print("formatting strain data...")
        
        #################################
        # pull out strain specific info #
        #################################
        
        # grab pertinent columns
        strains <- eccdata %>%
            select(strain, country, province, city, year, month, day,
                   strain_latitude, strain_longitude, present_at_tp1, 
                   tp1_cluster,tp1_cluster_size_2, present_at_tp2, 
                   tp2_cluster, tp2_cluster_size_2, tp1_t0_ecc_0.1.0, 
                   tp2_t0_ecc_0.1.0, tp1_t0_ecc_0.0.1, tp2_t0_ecc_0.0.1,
                   delta_ecc_0.1.0, delta_ecc_0.0.1, type, ecc_angle_delta,
                   ecc_direction_delta, ecc_speed, ecc_spread, bearing_delta,
                   cardinal_delta, cardinal_delta_long)
        
        # hit 
        strains <- strains %>%
            mutate(latitude_jit = round(jitter(as.numeric(strain_latitude),10,1),digits=4)) %>%
            mutate(longitude_jit = round(jitter(as.numeric(strain_longitude),10,1),digits=4))
        
        # make column for strain date instead of having three separate columns
        strains$strain_date <- as.Date(paste(strains$day, strains$month, strains$year, sep = "-"), format = "%d-%m-%y")
        
        # create time difference for ridgeline plots 
        strains <- strains %>% mutate(stain_time_diff = as.numeric(abs(as.Date("01-01-2020", format = "%d-%m-%y") - as.Date(strain_date, format = "%d-%m-%y"))))
        
        # match timepoint annotation in clusters long
        strains$timepoint <- ifelse(strains$present_at_tp1==1, 1, 2)
        
        # categorize as single vs multistrain
        # >2 because counts are +1 of actual value 
        strains <- strains %>% 
            mutate(single_mult = ifelse(tp1_cluster_size_2>2 & timepoint ==1 ,"Multi strain clusters",
                                        ifelse(tp1_cluster_size_2<=2 & timepoint ==1, "Single strain clusters",
                                               ifelse(tp2_cluster_size_2>2 & timepoint ==2, "Multi strain clusters", "Single strain clusters"))))
        
        levels(strains$single_mult) <- c("Multi strain clusters", "Single strain clusters")
        
        # convert to long form
        strains_long <- data.table::melt(setDT(strains),
                                         measure.vars=list(c("tp1_t0_ecc_0.0.1", "tp2_t0_ecc_0.0.1"),
                                                           c("tp1_t0_ecc_0.1.0", "tp2_t0_ecc_0.1.0"),
                                                           c("tp1_cluster_size_2", "tp2_cluster_size_2")),
                                         variable.name='timepoint2', value.name=c("ecc_0.0.1", "ecc_0.1.0",
                                                                                  "cluster_size_2"))
        
        # create unique row ids for shared data
        strains_long$key <- as.numeric(rownames(strains_long))
        
        # write out to reactive vals object
        vals$strains_long <- strains_long
        
        ############################################## 
        # update selectize inputs based on user data #
        ##############################################
        
        print("updating selectize inputs...")
        
        # update cluster sliders
        updateSelectInput(inputId = "tp1_cluster", label = "TP1 cluster", choices = unique(vals$clusters$tp1_cluster),  selected = NULL)
        updateSelectInput(inputId = "timepoint", label = "Timepoint", choices = unique(vals$clusters$timepoint), selected = NULL)
        updateSelectInput(inputId = "type", label = "Type", choices = unique(vals$clusters$type),  selected = NULL)
        updateSliderInput(inputId = "cluster_size_2", label = "Cluster size", min = min(vals$clusters$cluster_size_2), max =  max(vals$clusters$cluster_size_2), value = c(min(vals$clusters$cluster_size_2),  max(vals$clusters$cluster_size_2)))
        updateSliderInput(inputId = "avg_date", label = "Avg date", min = min(vals$clusters$avg_date), max = max(vals$clusters$avg_date), value = c(min(vals$clusters$avg_date), max(vals$clusters$avg_date)))
        updateSliderInput(inputId = "ecc_0.0.1", label = "ecc_0.0.1", min = min(vals$clusters$ecc_0.0.1, na.rm = T), max = max(vals$clusters$ecc_0.0.1, na.rm = T), value = c(min(vals$clusters$ecc_0.0.1, na.rm = T), max(vals$clusters$ecc_0.0.1, na.rm = T)))
        updateSliderInput(inputId = "ecc_0.1.0", label = "ecc_0.1.0", min = min(vals$clusters$ecc_0.1.0, na.rm = T), max = max(vals$clusters$ecc_0.1.0, na.rm = T), value = c(min(vals$clusters$ecc_0.1.0, na.rm = T), max(vals$clusters$ecc_0.1.0, na.rm = T)))
        updateSliderInput(inputId = "avg_latitude", label = "Avg latitude", min = min(vals$clusters$avg_latitude), max = max(vals$clusters$avg_latitude), value = c(min(vals$clusters$avg_latitude), max(vals$clusters$avg_latitude)))
        updateSliderInput(inputId = "avg_longitude", label = "Avg longitude", min = min(vals$clusters$avg_longitude), max = max(vals$clusters$avg_longitude), value = c(min(vals$clusters$avg_longitude), max(vals$clusters$avg_longitude)))
        # update cluster delta sliders 
        sliderInput(inputId = "delta_cluster_size", label = "Delta cluster size", min = min(vals$clusters$delta_cluster_size), max = max(vals$clusters$delta_cluster_size), value = c(min(vals$clusters$delta_cluster_size), max(vals$clusters$delta_cluster_size)))
        sliderInput(inputId = "num_novel_tp2_strains", label = "Novel TP2 strains", min = min(vals$clusters$num_novel_tp2_strains), max = max(vals$clusters$num_novel_tp2_strains), value = c(min(vals$clusters$num_novel_tp2_strains), max(vals$clusters$num_novel_tp2_strains)))
        sliderInput(inputId = "overall_cluster_growth_rate", label = "Overall growth rate", min = min(vals$clusters$overall_cluster_growth_rate), max = max(vals$clusters$overall_cluster_growth_rate), value = c(min(vals$clusters$overall_cluster_growth_rate), max(vals$clusters$overall_cluster_growth_rate)))
        sliderInput(inputId = "cluster_novel_growth_rate", label = "Novel growth rate", min = min(vals$clusters$delta_cluster_size), max = max(vals$clusters$delta_cluster_size), value = c(min(vals$clusters$delta_cluster_size), max(vals$clusters$delta_cluster_size)))
        sliderInput(inputId = "delta_ecc_0.0.1", label = "delta ecc_0.0.1", min = min(vals$clusters$delta_ecc_0.0.1, na.rm = T), max = max(vals$clusters$delta_ecc_0.0.1, na.rm = T), value = c(min(vals$clusters$delta_ecc_0.0.1, na.rm = T), max(vals$clusters$delta_ecc_0.0.1, na.rm = T)))
        sliderInput(inputId = "delta_ecc_0.1.0", label = "delta ecc_0.1.0", min = min(vals$clusters$delta_ecc_0.1.0, na.rm = T), max = max(vals$clusters$delta_ecc_0.1.0, na.rm = T), value = c(min(vals$clusters$delta_ecc_0.1.0, na.rm = T), max(vals$clusters$delta_ecc_0.1.0, na.rm = T)))
        #update stain filters
        updateSelectInput(inputId = "country", label = "Country", choices = unique(vals$strains_long$country),  selected = NULL)
        updateSelectInput(inputId = "province", label = "Province", choices = unique(vals$strains_long$province),  selected = NULL)
    })
    
    
    ###########################
    # reactive data filtering #
    ###########################
    
    clusters_r <- reactive({
        
        print("creating reactive cluster data...")
        print(vals$clusters)
        
        req(vals$clusters)
        data <- vals$clusters 
        
        # get top n
        topn <- {if (input$number != "all") as.numeric(input$number) else nrow(clusters_long)}
        
        filtered <- data %>%
            {if (!is.null(input$tp1_cluster)) filter(., tp1_cluster %in% input$tp1_cluster)  else . } %>%
            {if (!is.null(input$timepoint)) filter(., timepoint %in% input$timepoint)  else . } %>%
            {if (!is.null(input$type)) filter(., type %in% input$type)  else . } %>%
            filter(cluster_size_2 >= input$cluster_size_2[1]+1 & cluster_size_2 <= input$cluster_size_2[2]+1 ) %>%
            filter(avg_date >= input$avg_date[1]) %>% filter(avg_date <= input$avg_date[2]) %>%
            filter(ecc_0.0.1 >= input$ecc_0.0.1[1] & ecc_0.0.1 <= input$ecc_0.0.1[2]) %>%
            filter(ecc_0.1.0 >= input$ecc_0.1.0[1] & ecc_0.1.0 <= input$ecc_0.1.0[2]) %>%
            filter(avg_latitude >= input$avg_latitude[1] & avg_latitude <= input$avg_latitude[2]) %>%
            filter(avg_longitude >= input$avg_longitude[1] & avg_longitude <= input$avg_longitude[2])  %>%
            # delta filters
            filter(delta_cluster_size >= input$delta_cluster_size[1] & delta_cluster_size <= input$delta_cluster_size[2]) %>%
            filter(num_novel_tp2_strains >= input$num_novel_tp2_strains[1] & num_novel_tp2_strains <= input$num_novel_tp2_strains[2]) %>%
            filter(overall_cluster_growth_rate >= input$overall_cluster_growth_rate[1] & overall_cluster_growth_rate <= input$overall_cluster_growth_rate[2]) %>%
            filter(cluster_novel_growth_rate >= input$cluster_novel_growth_rate[1] & cluster_novel_growth_rate <= input$cluster_novel_growth_rate[2]) %>%
            filter(delta_ecc_0.0.1 >= input$delta_ecc_0.0.1[1] & delta_ecc_0.0.1 <= input$delta_ecc_0.0.1[2]) %>%
            filter(delta_ecc_0.1.0 >= input$delta_ecc_0.1.0[1] & delta_ecc_0.1.0 <= input$delta_ecc_0.1.0[2])%>%
            
            # data subsetting 
            {if (input$subsets==1) arrange(., desc(abs(cluster_size_2))) else . }  %>%
            {if (input$subsets==2) arrange(., desc(abs(delta_ecc_0.0.1))) else . }  %>%
            {if (input$subsets==3) arrange(., desc(abs(delta_ecc_0.1.0))) else . } 
        
        # grab n rows according to input
        if (input$number != 99){
            topn <- as.numeric(input$number)
            unique_clust <- head(unique(filtered$tp1_cluster),topn)
            return(filtered %>% subset(tp1_cluster %in% unique_clust))
        } else{  
            return(filtered)
        }
    })
    
    
    # cluster shared data frame 
    clusters_sh <- SharedData$new(clusters_r, key = ~key, group = "clusters")
    
    
    # all strain filtering 
    strains_r <- reactive({
        print("creating reactive strain data...")
        
        req(vals$strains_long,clusters_sh)
        vals$strains_long %>%
            {if (!is.null(input$country)) filter(., country %in% input$country)  else . } %>%
            {if (!is.null(input$province)) filter(., province %in% input$province)  else . } %>%
            # filter by shared cluster selections
            filter(tp1_cluster %in% (clusters_sh$data(withSelection = TRUE) %>% filter(selected_ | is.na(selected_)) %>% pull(tp1_cluster)))
    })
    
    strains_r_group <- reactive({
        
        ungrouped <- strains_r()
        if (input$region == 2) {
            ungrouped %>%
                group_by(country, tp1_cluster, timepoint) %>%
                summarize(n = n(),
                          ecc_0.0.1 = mean(ecc_0.0.1),
                          ecc_0.1.0 = mean(ecc_0.1.0),
                          ecc_comb = unique(as.character(ecc_direction_delta))) %>%
                rowid_to_column(., var = "rowid") %>%
                group_by(country) 
        } else if (input$region == 3){
            ungrouped %>%
                subset(country %in% input$regionProvince) %>% 
                group_by(province, tp1_cluster, timepoint) %>%
                summarize(n = n(),
                          ecc_0.0.1 = mean(ecc_0.0.1),
                          ecc_0.1.0 = mean(ecc_0.1.0),
                          ecc_comb = unique(as.character(ecc_direction_delta))) %>%
                rowid_to_column(., var = "rowid") %>%
                group_by(province) 
        } else {
            ungrouped
        }
        
        
    })
    
    # strain shared data
    strains_sh <- SharedData$new(strains_r, key = ~key, group = "strains")
    strains_sh_gr <- SharedData$new(strains_r_group, key = ~rowid, group = "strains_grouped")
    
    #################################
    # data frames displayed in tabs #
    #################################
    
    # clusters
    output$clusters_dt <- renderDataTable({
        datatable(clusters_sh,
                  extensions = c('Select', 'Buttons','Scroller'),
                  #extension = 'Scroller', 
                  options = list(
                      select = list(style = 'os', items = 'row'),
                      dom = 'Blfrtip',
                      rowId = 0,
                      buttons = list('selectRows', 'selectAll', 'selectNone', 'csv', 'excel'),
                      deferRender = TRUE,
                      scrollY = 500,
                      scrollX = 600,
                      scroller = TRUE),
                  selection = 'none')
    }, server=F)
    
    # strains
    output$strains_dt <- renderDataTable({
        datatable(data = strains_sh,
                  extensions = c('Select', 'Buttons','Scroller'),
                  options = list(
                      select = list(style = 'os', items = 'row'),
                      dom = 'Blfrtip',
                      rowId = 0,
                      buttons = list('selectRows', 'selectAll', 'selectNone', 'csv', 'excel'),
                      deferRender = TRUE,
                      scrollY = 500,
                      scrollX = 600,
                      scroller = TRUE),
                  selection = 'none',
        )
    }, server=F)
    
    # filtered data table
    output$filtered_data <- renderDataTable({
        datatable(vals$data_proc,
                  extensions = c('Buttons','Scroller'),
                  options = list(
                      dom = 'Blfrtip',
                      rowId = 0,
                      buttons = list('csv', 'excel'),
                      deferRender = TRUE,
                      scrollY = 500,
                      scrollX = 600,
                      scroller = TRUE),
                  selection = 'none')
    })
    
    # raw data table
    output$raw_data <- renderDataTable({
        datatable(vals$data_raw,
                  extensions = c('Buttons','Scroller'),
                  options = list(
                      dom = 'Blfrtip',
                      rowId = 0,
                      buttons = list('csv', 'excel'),
                      deferRender = TRUE,
                      scrollY = 500,
                      scrollX = 600,
                      scroller = TRUE),
                  selection = 'none')
    })
    
    
    ################################
    # cluster movement value boxes #
    ################################
    
    output$activegrowth <- renderValueBox({
        
        valueBox(
            subtitle = "clusters actively growing",
            value = sum(clusters_r()$overall_cluster_growth_rate >= 3)
        )
    })
    
    output$activespread <- renderValueBox({
        valueBox(
            subtitle = "clusters actively spreading",
            value = sum(na.omit(clusters_r()$ecc_spread)=="Dispersed")
        )
    })
    
    output$sigtransmission <- renderValueBox({
        valueBox(
            subtitle = "clusters with significant local transmission",
            value = nrow(clusters_r() %>% 
                             slice(which(clusters_r()$ecc_spread=="Isolated")) %>% 
                             subset(overall_cluster_growth_rate >= 3))
        )
    })
    
    
    
    # radio plot for ecc explanation of cluster spread
    output$ecc_radar <- renderPlotly({
        
        
        # #time point to time point
        # plot_ly(type="scatterpolar",
        #         data = clusters_long %>%
        #             mutate(ecc_comb = ecc_direction) %>%
        #             group_by(timepoint) %>%
        #             count(ecc_comb, .drop=FALSE) %>%
        #             full_join(directions, by = c('ecc_comb')),
        #         r = ~n,
        #         theta = ~degree_mid,
        #         frame = ~timepoint,
        #         showlegend = F,
        #         connectgaps = T,
        #         color =  I("#1F78C8"),
        #         fill = 'toself') %>%
        #     layout(showlegend = F,
        #            margin = list(
        #                l = 100, r = 100),
        #            polar = list(
        #                radialaxis = list(
        #                    range =c(0,max(clusters_long %>%
        #                                       mutate(ecc_comb = ecc_direction) %>%
        #                                       group_by(timepoint) %>%
        #                                       count(ecc_comb, .drop=FALSE) %>%
        #                                       full_join(directions, by = c('ecc_comb')) %>% 
        #                                       pull(n)))),
        #                angularaxis = list(
        #                    rotation = 90,
        #                    direction = 'clockwise',
        #                    tickmode = 'array',
        #                    tickvals = c(0, 45,  90, 135, 180,
        #                                 225, 270, 315),
        #                    ticktext = c("Slower spread", 
        #                                 HTML(paste("Slower spread,", "<br>", "more concentrated")), 
        #                                 HTML(paste("More ", "<br>", "concentrated", sep="")), 
        #                                 HTML(paste("Faster spread,", "<br>", "more concentrated")),
        #                                 "Faster spread",   
        #                                 HTML(paste("Faster spread,", "<br>", "more disperse")),
        #                                 HTML(paste("More ", "<br>", "disperse", sep="")),
        #                                 HTML(paste("Slower spread,", "<br>", "more disperse"))))))
        
        
        # change to change
        plot_ly(type="scatterpolar",
                data = clusters_sh$data(withSelection = TRUE) %>%
                    mutate(selfact = selection_factor(.),
                           ecc_comb = ecc_direction_delta) %>%
                    group_by(selfact) %>%
                    distinct(tp1_cluster, .keep_all = T) %>%
                    count(ecc_comb, .drop=FALSE) %>%
                    full_join(directions, by = c('ecc_comb')),
                r = ~n,
                theta = ~degree_mid,
                color = ~selfact,
                colors = c("#1F78C8", "red"),
                fill = 'toself') %>%
            layout(showlegend = F,
                   margin = list(l = 100, r = 100),
                   polar = list(
                       angularaxis = list(
                           rotation = 90,
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
    })
    
    output$bubble <- renderPlotly({
        
        # no faceting
        if(input$region == 1) {
            a <- plot_ly() %>%
                add_trace(data = clusters_sh, 
                          type = "scatter",
                          mode = "markers", 
                          x = ~ecc_0.1.0,
                          frame = ~timepoint, 
                          size = ~I(cluster_size_2),
                          color = ~I("#898a8c"), 
                          hovertemplate = ~tp1_cluster,
                          showlegend = FALSE,
                          marker = list(sizemode = "area", opacity = 0.5)) %>%
                layout(xaxis = list(title = "Geospatial epicluster cohesion index",
                                    range = c(0, 1.05)),
                       yaxis = list(showticklabels = F),
                       annotations = list(text= "Geospatial epicluster cohesion index",
                                          xref = "paper",
                                          yref = "paper",
                                          yanchor = "bottom",
                                          xanchor = "center",
                                          align = "center",
                                          x = 0.5,
                                          y = -0.3,
                                          #font = list(size = 14),
                                          showarrow = FALSE)) %>%
                highlight(color = "#1F78C8")
            
            b <- plot_ly() %>%
                add_trace(data = clusters_sh, 
                          type = "scatter",
                          mode = "markers", 
                          x = ~ecc_0.0.1,
                          size = ~I(cluster_size_2),
                          color = ~I("#898a8c"), 
                          frame = ~timepoint, 
                          hovertemplate = ~tp1_cluster,
                          showlegend = FALSE,
                          marker = list(sizemode = "area", opacity = 0.5)) %>%
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
            
            c <- plot_ly() %>%
                add_trace(data = clusters_sh,
                          type = "scatter",
                          mode = "markers", 
                          x = ~ecc_0.1.0,
                          y = ~ecc_0.0.1,
                          frame = ~timepoint,  
                          size = ~I(cluster_size_2),
                          color = ~I("#898a8c"), 
                          hovertemplate = ~tp1_cluster,
                          showlegend = FALSE, 
                          marker = list(sizemode = "area", opacity = 0.5)) %>%
                layout(xaxis = list(title = "Geospatial epicluster cohesion index",
                                    range = c(0,1.05)),
                       yaxis = list(title = "Temporal epicluster cohesion index",
                                    range = c(0,1.05)),
                       annotations = list(list(text= "Geospatial epicluster cohesion index",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "bottom",
                                               xanchor = "center",
                                               align = "center",
                                               x = 0.5,
                                               y = -0.15,
                                               #font = list(size = 14),
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
            subplot(c, subplot(a, b, nrows = 2, margin = 0.075), nrows = 1, margin = 0.025)
        } else if (input$region == 2 ){
            
            a <- ggplotly(ggplot(strains_sh_gr, aes(x = ecc_0.0.1, y=1, size = n)) +
                              geom_point(aes(frame=timepoint), alpha=0.5, col ="#898a8c") +
                              scale_x_continuous(breaks=seq(0, 1, 0.5),
                                                 limits=c(0,1)) +
                              facet_wrap(~country, scales = "free_y") +
                              theme_minimal() +
                              theme(axis.text.y = element_blank(),
                                    axis.ticks.y = element_blank())) %>%
                layout(xaxis = list(title = "Geospatial epicluster cohesion index",
                                    range = c(0, 1.05)),
                       yaxis = list(showticklabels = F),
                       annotations = list(text= "Geospatial epicluster cohesion index",
                                          xref = "paper",
                                          yref = "paper",
                                          yanchor = "bottom",
                                          xanchor = "center",
                                          align = "center",
                                          x = 0.5,
                                          y = -0.3,
                                          font = list(size = 12),
                                          showarrow = FALSE)) %>%
                highlight(color = "#1F78C8")
            
            
            
            b <- ggplotly(ggplot(strains_sh_gr, aes(x = ecc_0.1.0, y=1, size = n)) +
                              geom_point(aes(frame=timepoint), alpha=0.5, col ="#898a8c") +
                              scale_x_continuous(breaks=seq(0, 1, 0.5),
                                                 limits=c(0,1)) +
                              facet_wrap(~country, scales = "free_y") +
                              theme_minimal() +
                              theme(axis.text.y = element_blank(),
                                    axis.ticks.y = element_blank())) %>%
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
                                          font = list(size = 12),
                                          showarrow = FALSE)) %>%
                highlight(color = "#1F78C8")
            
            
            
            c <- ggplotly(ggplot(strains_sh_gr, aes(x = ecc_0.0.1, y=ecc_0.1.0, size = n)) +
                              geom_point(aes(frame=timepoint), alpha=0.5, col ="#898a8c") +
                              scale_x_continuous(breaks=seq(0, 1, 0.5),
                                                 limits=c(0,1)) +
                              scale_y_continuous(breaks=seq(0, 1, 0.5),
                                                 limits = c(0,1)) +
                              facet_wrap(~country) +
                              theme_minimal()) %>%
                layout(xaxis = list(title = "Geospatial epicluster cohesion index",
                                    range = c(0,1.05)),
                       yaxis = list(title = "Temporal epicluster cohesion index",
                                    range = c(0,1.05)),
                       annotations = list(list(text= "Geospatial epicluster cohesion index",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "bottom",
                                               xanchor = "center",
                                               align = "center",
                                               x = 0.5,
                                               y = -0.12,
                                               font = list(size = 12),
                                               showarrow = FALSE),
                                          list(text= "Temporal epicluster cohesion index",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "center",
                                               xanchor = "center",
                                               align = "center",
                                               x = -0.075,
                                               y = 0.5,
                                               font = list(size = 12),
                                               textangle = -90,
                                               showarrow = FALSE))) %>%
                highlight(color = "#1F78C8")
            
            # Link Animated views
            subplot(c, subplot(a, b, nrows = 2, margin = 0.1), nrows = 1, margin = 0.02)
            
        } else if (input$region == 3) {
            
            a <- ggplotly(ggplot(strains_sh_gr, aes(x = ecc_0.0.1, y=1, size = n)) +
                              geom_point(aes(frame=timepoint), alpha=0.5, col ="#898a8c") +
                              scale_x_continuous(breaks=seq(0, 1, 0.5),
                                                 limits=c(0,1)) +
                              facet_wrap(~province, scales = "free_y") +
                              theme_minimal() +
                              theme(axis.text.y = element_blank(),
                                    axis.ticks.y = element_blank())) %>%
                layout(xaxis = list(title = "Geospatial epicluster cohesion index",
                                    range = c(0, 1.05)),
                       yaxis = list(showticklabels = F),
                       annotations = list(text= "Geospatial epicluster cohesion index",
                                          xref = "paper",
                                          yref = "paper",
                                          yanchor = "bottom",
                                          xanchor = "center",
                                          align = "center",
                                          x = 0.5,
                                          y = -0.3,
                                          font = list(size = 12),
                                          showarrow = FALSE)) %>%
                highlight(color = "#1F78C8")
            
            
            
            b <- ggplotly(ggplot(strains_sh_gr, aes(x = ecc_0.1.0, y=1, size = n)) +
                              geom_point(aes(frame=timepoint), alpha=0.5, col ="#898a8c") +
                              scale_x_continuous(breaks=seq(0, 1, 0.5),
                                                 limits=c(0,1)) +
                              facet_wrap(~province, scales = "free_y") +
                              theme_minimal() +
                              theme(axis.text.y = element_blank(),
                                    axis.ticks.y = element_blank())) %>%
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
                                          font = list(size = 12),
                                          showarrow = FALSE)) %>%
                highlight(color = "#1F78C8")
            
            
            
            c <- ggplotly(ggplot(strains_sh_gr, aes(x = ecc_0.0.1, y=ecc_0.1.0, size = n)) +
                              geom_point(aes(frame=timepoint), alpha=0.5, col ="#898a8c") +
                              scale_x_continuous(breaks=seq(0, 1, 0.5),
                                                 limits=c(0,1)) +
                              scale_y_continuous(breaks=seq(0, 1, 0.5),
                                                 limits = c(0,1)) +
                              facet_wrap(~province) +
                              theme_minimal()) %>%
                layout(xaxis = list(title = "Geospatial epicluster cohesion index",
                                    range = c(0,1.05)),
                       yaxis = list(title = "Temporal epicluster cohesion index",
                                    range = c(0,1.05)),
                       annotations = list(list(text= "Geospatial epicluster cohesion index",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "bottom",
                                               xanchor = "center",
                                               align = "center",
                                               x = 0.5,
                                               y = -0.12,
                                               font = list(size = 12),
                                               showarrow = FALSE),
                                          list(text= "Temporal epicluster cohesion index",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "center",
                                               xanchor = "center",
                                               align = "center",
                                               x = -0.075,
                                               y = 0.5,
                                               font = list(size = 12),
                                               textangle = -90,
                                               showarrow = FALSE))) %>%
                highlight(color = "#1F78C8")
            
            # Link Animated views
            subplot(c, subplot(a, b, nrows = 2, margin = 0.1), nrows = 1, margin = 0.02)
        }
        
    })
    
    output$ecc_histograms <- renderPlotly({
        
        # no faceting 
        geo <- plot_ly() %>%
            add_trace(data = strains_sh,
                      type = "histogram",
                      x = ~ecc_0.1.0,
                      frame = ~timepoint2,
                      color = I("#898a8c"),
                      xbins = list(size = 0.01),
                      showlegend = FALSE) %>%
            layout(xaxis = list(range = c(0,1),
                                title = "Geospatial epicluster cohesion index"),
                   yaxis = list(range = c(0, nrow(strains_sh$data(withSelection = T) %>%
                                                      filter(selected_ | is.na(selected_))))),
                   annotations = list(text= "Geospatial epicluster cohesion index",
                                      xref = "paper",
                                      yref = "paper",
                                      yanchor = "bottom",
                                      xanchor = "center",
                                      align = "center",
                                      x = 0.5,
                                      y = -0.3,
                                      showarrow = FALSE)) %>%
            highlight(color = "#1F78C8")
        
        delta_geo <- plot_ly() %>%
            add_trace(data = strains_sh,
                      type = "histogram",
                      x = ~delta_ecc_0.1.0,
                      frame = ~timepoint2,
                      color = I("#898a8c"),
                      xbins = list(size = 0.01),
                      showlegend = FALSE) %>%
            layout(xaxis = list(range = c(-1,1),
                                title = "Delta geospatial epicluster cohesion index"),
                   yaxis = list(range = c(0, nrow(strains_sh$data(withSelection = T) %>%
                                                      filter(selected_ | is.na(selected_))))),
                   annotations = list(text= "Delta geospatial epicluster cohesion index",
                                      xref = "paper",
                                      yref = "paper",
                                      yanchor = "bottom",
                                      xanchor = "center",
                                      align = "center",
                                      x = 0.5,
                                      y = -0.3,
                                      showarrow = FALSE)) %>%
            highlight(color = "#1F78C8")
        
        
        temp <- plot_ly() %>%
            add_trace(data = strains_sh,
                      type = "histogram",
                      x = ~ecc_0.0.1,
                      frame = ~timepoint2,
                      color = I("#898a8c"), 
                      xbins = list(size = 0.01),
                      showlegend = FALSE) %>%
            layout(xaxis = list(range = c(0,1),
                                title = "Temporal epicluster cohesion index"),
                   yaxis = list(range = c(0, nrow(strains_sh$data(withSelection = T) %>%
                                                      filter(selected_ | is.na(selected_))))),
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
        
        delta_temp <- plot_ly() %>%
            add_trace(data = strains_sh,
                      type = "histogram",
                      x = ~delta_ecc_0.0.1,
                      frame = ~timepoint2,
                      color = I("#898a8c"), 
                      xbins = list(size = 0.01),
                      showlegend = FALSE) %>%
            layout(xaxis = list(range = c(-1,1),
                                title = "Delta temporal epicluster cohesion index"),
                   yaxis = list(range = c(0, nrow(strains_sh$data(withSelection = T) %>%
                                                      filter(selected_ | is.na(selected_))))),
                   annotations = list(text= "Delta temporal epicluster cohesion index",
                                      xref = "paper",
                                      yref = "paper",
                                      yanchor = "bottom",
                                      xanchor = "center",
                                      align = "center",
                                      x = 0.5,
                                      y = -0.3,
                                      showarrow = FALSE)) %>%
            highlight(color = "#1F78C8")
        
        
        subplot(subplot(geo, temp, nrows=1), subplot(delta_geo, delta_temp, nrows=1), nrows=2, margin = 0.05) %>%
            layout(yaxis = list(title = "Number of strains"))
        
        
    })
    
    output$radar <- renderPlotly({
        
        if (input$region ==1) {
            # change to change
            plot_ly(type="scatterpolar",
                    data = clusters_sh$data(withSelection = TRUE) %>%
                        filter(selected_ | is.na(selected_)) %>%
                        mutate(ecc_comb = ecc_direction_delta) %>%
                        group_by(timepoint) %>%
                        count(ecc_comb, .drop=FALSE) %>%
                        full_join(directions, by = c('ecc_comb')) %>%
                        mutate(cardinal = factor(cardinal, levels =c("N", "NNE", "NE", "ENE",
                                                                     "E", "ESE", "SE", "SSE",
                                                                     "S", "SSW", "SW", "WSW", 
                                                                     "W", "WNW", "NW", "NNW"))),
                    r = ~n,
                    theta = ~degree_mid,
                    color = I("#898a8c"),
                    frame = ~timepoint, 
                    fill = 'toself') %>%
                layout(showlegend = F,
                       margin = list(l = 100, r = 100),
                       polar = list(
                           angularaxis = list(
                               rotation = 90,
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
        } else if (input$region == 2) {
            
            # by country 
            strains_sh_gr$data(withSelection = T) %>%
                filter(selected_ | is.na(selected_)) %>%
                # group_by(country, tp1_cluster, timepoint) %>%
                # summarize(ecc_0.0.1 = mean(ecc_0.0.1),
                #           ecc_0.1.0 = mean(ecc_0.1.0),
                #           ecc_comb = unique(as.character(ecc_direction_delta))) %>%
                group_by(country, timepoint) %>%
                #na.omit() %>%
                count(ecc_comb, .drop=FALSE) %>%
                full_join(directions, by = c('ecc_comb')) %>%
                mutate(cardinal = factor(cardinal, 
                                         levels =c("N", "NNE", "NE", "ENE",
                                                   "E", "ESE", "SE", "SSE",
                                                   "S", "SSW", "SW", "WSW", 
                                                   "W", "WNW", "NW", "NNW"))) %>%
                na.omit() %>%
                arrange(cardinal) %>%
                plot_ly(type="scatterpolar",
                        frame = ~timepoint,
                        r = ~n,
                        split = ~country,
                        theta = ~degree_mid,
                        name = ~country,
                        color  = I("#898a8c"),
                        fill = "tonext") %>%
                layout(margin = list(l = 100, r = 100),
                       polar = list(
                           angularaxis = list(
                               rotation = 90,
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
            
        } else if (input$region == 3){
            # by province 
            strains_sh_gr$data(withSelection = T) %>%
                filter(selected_ | is.na(selected_)) %>%
                #subset(country %in% input$regionProvince) %>% 
                #group_by(province, tp1_cluster, timepoint) %>%
                #summarize(ecc_0.0.1 = mean(ecc_0.0.1),
                #          ecc_0.1.0 = mean(ecc_0.1.0),
                #          ecc_comb = unique(as.character(ecc_direction_delta))) %>%
                group_by(province, timepoint) %>%
                #na.omit() %>%
                count(ecc_comb, .drop=FALSE) %>%
                full_join(directions, by = c('ecc_comb')) %>%
                mutate(cardinal = factor(cardinal, 
                                         levels =c("N", "NNE", "NE", "ENE",
                                                   "E", "ESE", "SE", "SSE",
                                                   "S", "SSW", "SW", "WSW", 
                                                   "W", "WNW", "NW", "NNW"))) %>%
                na.omit() %>%
                arrange(cardinal) %>%
                plot_ly(type="scatterpolar",
                        frame = ~timepoint,
                        r = ~n,
                        split = ~province,
                        theta = ~degree_mid,
                        name = ~province,
                        color  = I("#898a8c"),
                        fill = "tonext") %>%
                layout(margin = list(l = 100, r = 100),
                       polar = list(
                           angularaxis = list(
                               rotation = 90,
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
    
    
    output$changevector <- renderPlotly({
        
        # opacity settings
        op <- clusters_sh$data(withSelection = T)
        op$opacity <- ifelse(op$selected_ | is.na(op$selected_), 0.6, 0.6*0.3)
        op$colour <- ifelse(op$selected_ , "#1F78C8","#898a8c")
        
        plot_ly(type = "scatter", mode="markers") %>% 
            #add segment to connect each point to origin
            add_trace(data = clusters_sh,
                      x = ~delta_ecc_0.1.0, # geographical
                      y = ~delta_ecc_0.0.1, # temporal 
                      split = ~tp1_cluster,
                      legendgroup = ~tp1_cluster,
                      opacity = I(op$opacity),
                      frame = ~timepoint,
                      color = I(op$colour),
                      showlegend = F,
                      opacity = 1,
                      size=I(0)) %>%
            # set range and overlay arrow annotations
            layout(xaxis = list(range = c(-1, 1.5), 
                                title = "Delta geospatial ECC value"),
                   yaxis = list(range = c(-1, 1.5),
                                title = "Delta temporal ECC value"),
                   annotations = list(ax = 0, ay = 0,
                                      axref='x', ayref='y',
                                      x = ~delta_ecc_0.1.0, # geospatial
                                      y = ~delta_ecc_0.0.1, # temporal
                                      opacity = I(op$opacity),
                                      #name = ~paste(tp1_cluster, timepoint, sep="_"),
                                      arrowcolor = I(op$colour),
                                      xref='x', yref='y', text = ""))
    })
    
    
    output$cluster_growth <- renderPlotly({
        
        a <- plot_ly(data = clusters_sh) %>%
            add_trace(type = "scatter",
                      mode = "markers",
                      x = ~timepoint,
                      y = ~overall_cluster_growth_rate,
                      color = I("#898a8c"),
                      showlegend = F,
                      frame=~timepoint) %>%
            layout(yaxis = list(rangemode = "tozero", 
                                title = "Overall growth rate"),
                   xaxis = list(title = "Time point"),
                   annotations = list(list(text= "Overall cluster growth rate",
                                           xref = "paper",
                                           yref = "paper",
                                           yanchor = "center",
                                           xanchor = "center",
                                           align = "center",
                                           textangle = -90,
                                           x = -0.05,
                                           y = 0.5,
                                           showarrow = FALSE),
                                      list(text= "Time point",
                                           xref = "paper",
                                           yref = "paper",
                                           yanchor = "center",
                                           xanchor = "center",
                                           align = "center",
                                           x = 0.5,
                                           y = -0.1,
                                           showarrow = FALSE)))%>%
            highlight(color = "#1F78C8")
        
        b <- plot_ly(data = clusters_sh) %>%
            add_trace(type = "scatter",
                      mode = "markers",
                      x = ~timepoint,
                      y = ~cluster_novel_growth_rate,
                      color = I("#898a8c"),
                      showlegend = F,
                      frame=~timepoint) %>%
            layout(yaxis = list(rangemode = "tozero",
                                title = "Novel growth rate"),
                   xaxis = list(title = "Time point"),
                   annotations = list(list(text= "Novel cluster growth rate",
                                           xref = "paper",
                                           yref = "paper",
                                           yanchor = "center",
                                           xanchor = "center",
                                           align = "center",
                                           textangle = -90,
                                           x = -0.05,
                                           y = 0.5,
                                           showarrow = FALSE),
                                      list(text= "Time point",
                                           xref = "paper",
                                           yref = "paper",
                                           yanchor = "center",
                                           xanchor = "center",
                                           align = "center",
                                           x = 0.5,
                                           y = -0.1,
                                           showarrow = FALSE))) %>%
            highlight(color = "#1F78C8")
        
        
        subplot(a, b, nrows = 1, margin = 0.025)
        #subplot( c, subplot(a, b, nrows = 1, margin = 0.025), nrows=2, margin = 0.025, shareX = F, shareY = F) 
    }) 
    
    output$strainsbycluster <- renderPlotly({
        
        counts <- strains_sh$data(withSelection = T) %>%
            filter(selected_ | is.na(selected_)) %>%
            group_by(tp1_cluster, strain_date) %>%
            tally()
        
        a <- plot_ly(data = counts) %>%
            add_trace(type = "scatter",
                      mode = "line",
                      color = I("#898a8c"),
                      x = ~strain_date,
                      y = ~n, 
                      name = ~tp1_cluster, 
                      showlegend = F) %>%
            layout(yaxis = list(rangemode = "tozero",
                                title = "Number of novel strains identified"),
                   xaxis = list(title = "Time point")) %>%
            highlight(color = "#1F78C8")
        
        
        # need to aggregate data
        cumsum <- strains_sh$data(withSelection = T) %>%
            filter(selected_ | is.na(selected_)) %>%
            group_by(tp1_cluster, strain_date) %>% 
            tally(!is.na(strain)) %>% 
            mutate(cumsum = cumsum(n))
        
        
        b <- plot_ly(data = cumsum) %>%
            add_trace(type = "scatter",
                      mode = "line",
                      color = I("#898a8c"),
                      x = ~strain_date,
                      y = ~cumsum, 
                      name = ~tp1_cluster, 
                      showlegend = F) %>%
            layout(yaxis = list(rangemode = "tozero",
                                title = "Cumulative number of strains identified"),
                   xaxis = list(title = "Time point")) %>%
            highlight(color = "#1F78C8")
        
        subplot(a, b, nrows = 1, margin = 0.025)
        
        
    })
    
    output$newstrainsbydate <- renderPlotly({
        plot_ly(type = "histogram",
                data = strains_sh,
                histfunc = "count",
                x = ~strain_date,
                color = I("#898a8c"),                
                xbins = list(size = 86400000.0),
                hovertemplate = ~paste('<b>', country, '</b><br>',
                                       'Count: %{y}', '<br>',
                                       'Date range: %{x}', '<extra></extra>', sep=" ")) %>%
            layout(barmode = "stack",
                   xaxis = list(title = "Date"),
                   yaxis = list(title = "Number of new strains identified"),
                   updatemenus = list( list(
                       active = -1,
                       x= -0.1,
                       type = 'buttons',
                       buttons = list(
                           list(
                               label = "By day",
                               method = "restyle",
                               args = list(list(xbins = list(size = 86400000.0)))),
                           list(
                               label = "By week",
                               method = "restyle",
                               args = list(list(xbins = list(size = 604800000.0)))),
                           list(
                               label = "By month",
                               method = "restyle",
                               args = list(list(xbins = list(size = "M1"))))
                       )))) %>%
            highlight(color = "#1F78C8")
        
    })
    
    
    output$singlevsmulti <- renderPlotly({
        
        
        a <- plot_ly(type = "histogram") %>%
            add_trace(data = strains_long %>%
                          subset(single_mult == "Multi strain clusters"),
                      histfunc = "count",
                      x = ~strain_date,
                      color = I("#898a8c"),    
                      showlegend = F, 
                      xbins = list(size = 86400000.0)) %>%
            layout(xaxis = list(title = "Date"),
                   annotations = list(list(text= "Count",
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
        
        b <- plot_ly(type = "histogram") %>%
            add_trace(data = strains_long %>%
                          subset(single_mult == "Single strain clusters"),
                      histfunc = "count",
                      x = ~strain_date,
                      color = I("#898a8c"),     
                      showlegend = F,
                      xbins = list(size = 86400000.0)) %>%
            layout(xaxis = list(title = "Date"),
                   yaxis = list(range = c(0, max(strains_long %>%
                                                     subset(single_mult == "Multi strain clusters") %>%
                                                     group_by(strain_date) %>%
                                                     count() %>%
                                                     pull(n)))),
                   annotations = list(list(text= "Count",
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
        
        subplot(b, a, nrows = 2, shareX = T, margin = 0.075)
        
        
    })
    
    # mapbox token 
    mapboxToken <- "pk.eyJ1Ijoic2FtLWEtbGVlIiwiYSI6ImNrb2s0bXVpbzFhMGkybm5zeHI1dHR1aTgifQ.1K8o7OaSOWo_y5UdVH348w"    # You need your own token
    Sys.setenv("MAPBOX_TOKEN" = mapboxToken)
    
    output$cluster_map <- renderPlotly({
        plot_mapbox(mode = 'scattermapbox') %>%
            add_markers(data = clusters_sh,
                        name = "Clusters",
                        x = ~avg_longitude,
                        y = ~avg_latitude,
                        frame = ~timepoint,
                        color= I("#646566"),
                        size = ~log10(cluster_size_2)*10,
                        opacity = 0.6,
                        hovertext = ~tp1_cluster) %>%
            config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN")) %>%
            layout(mapbox = list(zoom = 1,
                                 style = 'light',
                                 center = list(lon = 50, lat = 40)),
                   title = list(text = "Clusters",
                                x=0.1)) %>%
            highlight(color = I("#1F78C8")) %>%            
            animation_opts(1000, redraw = T, transition = 100) %>% 
            animation_button(visible = T) %>%
            animation_slider(active = 0)
    })
    
    
    output$strain_map <- renderPlotly({
        
        # set the color
        col <- ifelse(length(unique(strains_sh$data() %>% pull(tp1_cluster)))>1, "#646566", "#1F78C8") 
        
        plot_mapbox(mode = 'scattermapbox') %>%
            add_markers(data = strains_sh,
                        name = "Clusters",
                        x = ~longitude_jit,
                        y = ~latitude_jit,
                        frame = ~timepoint,
                        color = I(col),
                        size = I(5),
                        opacity = 0.6,
                        hovertext = ~tp1_cluster) %>%
            config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN")) %>%
            layout(mapbox = list(zoom =1,
                                 style = 'light',
                                 center = list(lon = 50, lat = 40)),
                   title = list(text = "Strains",
                                x=0.1)) %>%
            highlight(color = I("#1F78C8")) %>%            
            animation_opts(1000, redraw = T, transition = 100) %>% 
            animation_button(visible = T) %>%
            animation_slider(active = 0)
    })
    
}






# P <- plot_ly(data = clusters_long)
# for(k in 1:nrow(clusters_long)) {
#     dfk <- clusters_long[k,c("delta_ecc_0.1.0", "delta_ecc_0.0.1", "tp1_cluster", "timepoint")]
#     P <- add_trace(P,
#                    y=c(0,~delta_ecc_0.0.1),
#                    x=c(0,~delta_ecc_0.1.0),
#                    frame =~timepoint,
#                    data=dfk,
#                    hovertemplate = ~tp1_cluster,
#                    type="scatter",
#                    mode="lines+markers",
#                    showlegend = F,
#                    color = ~I("#898a8c"))
# }
# 
# P <- P %>%  
#     layout(xaxis = list(range = c(-1,1)),
#            yaxis = list(range = c(-1,1))) %>%
#     highlight(color = "#1F78C8")
# P


# 
# plot_ly() %>%
#     add_trace(data = clusters_long, 
#               type = "scatter",
#               mode = "markers", 
#               x = ~ecc_0.1.0,
#               frame = ~timepoint, 
#               size = ~I(cluster_size_2),
#               color = ~I("#898a8c"), 
#               hovertemplate = ~tp1_cluster,
#               showlegend = FALSE,
#               marker = list(sizemode = "area", opacity = 0.5)) %>%
#     layout(xaxis = list(title = "Geospatial epicluster cohesion index",
#                         range = c(0, 1.05)),
#            yaxis = list(showticklabels = F),
#            annotations = list(text= "Geospatial epicluster cohesion index",
#                               xref = "paper",
#                               yref = "paper",
#                               yanchor = "bottom",
#                               xanchor = "center",
#                               align = "center",
#                               x = 0.5,
#                               y = -0.3,
#                               #font = list(size = 14),
#                               showarrow = FALSE)) %>%
#     highlight(color = "#1F78C8")
# 
# 
# 
# strains_long %>%
#     group_by(country, tp1_cluster, timepoint) %>%
#     summarize(n = n(),
#               index = mean(ecc_0.0.1)) %>%
#     group_by(country) %>%
# do(p=plot_ly(.) %>%
#        add_trace(x = ~index,
#                  type = "scatter",
#                  mode = "markers",
#                  frame = ~timepoint,
#                  size = ~I(n),
#                  color = I("#1F78C8"),
#                  showlegend = FALSE) %>%
#        layout(xaxis = list(range = c(0,1), title = ""),
#               yaxis = list(showticklabels = F, title = ""),
#               annotations = list(text= ~unique(country),
#                                  xref = "paper",
#                                  yref = "paper",
#                                  yanchor = "bottom",
#                                  xanchor = "center",
#                                  align = "center",
#                                  x = 0.5,
#                                  y = 1,
#                                  showarrow = FALSE))) %>%
# subplot(nrows = 3, shareX = TRUE, shareY = TRUE) %>%
# layout( annotations = list(list(text= "Temporal epicluster cohesion index",
#                                 xref = "paper",
#                                 yref = "paper",
#                                 yanchor = "bottom",
#                                 xanchor = "center",
#                                 align = "center",
#                                 x = 0.5,
#                                 y = -0.1,
#                                 showarrow = FALSE)))
