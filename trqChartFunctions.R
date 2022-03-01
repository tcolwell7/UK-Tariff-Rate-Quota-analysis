###

#' TRQ chart functions script
#' Contains functions to generate visualizations for Regional Story Pack work
#' Brings in RData from trq_data script
#'
#'

rm(list = ls())

path<-setwd(stringr::str_extract(rstudioapi::getActiveDocumentContext()$path,".+[/]")) 

# load R.Data ----- 

load("..\\data\\trq_data_all.RData")


# Specify packages
packages <- c("openxlsx", 
              "janitor", 
              "stringr", 
              "readxl", 
              "tidyverse",
              "lubridate"
              )

# Install packages if not already installed
install.packages(setdiff(packages, rownames(installed.packages())))

# Load packages
sapply(packages, require, character.only = TRUE)

options(scipen=999) # remove scientific number formatting

`%notin%` <- Negate(`%in%`) # custom negate function 



trqPlotErrorHandling <- function(region_arg, quota_unit_arg){
  
  #' Error handling function
  #' Identify where main inputs:
  #' Region & Quota Unit
  #' Which do not exist or misspelled
  #' Print list of valid options 
  #' 
  #'
  #' Inputs are consistent across all TRQ functions
  #' Region is the main filter to produce charts for
  #' TRQ quota unit is second main filter for user selection
  #' 
  
  # Logic if only region exists (default input for Quota Unit is NULL)
  if(is.null(quota_unit_arg)){
    
    if(region_arg %notin% unique(sectors_grouping$region)){
      abort(
        c("Please enter valid *.region* name from the available list",
          unique(sectors_grouping$region)
        ))
    }
    
  } else{
    
    
    if(region_arg %notin% unique(sectors_grouping$region)){
      abort(
        c("Please enter valid *.region* name from the available list",
          unique(sectors_grouping$region)
        ))}
    
    if(quota_unit_arg %notin% unique(sectors_region$quota_unit_final)){
      abort(
        c("Please enter valid Quota Unit name from the available list",
          unique(sectors_grouping$quota_unit_final)
          
        ))
    }
    
  } # end else bracket
  
} # end function bracket


sectorCheck <- function(sectorInput){
  
  #' convert character vector to data frame
  #' identify entries which don't exist in sectors list
  #' print warning message for each
  #' 
  
  df <- data.frame(col = sectorInput)
  dfs <- sectors_region %>% distinct(., trq_dsc) 
  n <- df %>% filter(col %notin% dfs$trq_dsc)
  
  for(i in seq_len(nrow(n))){warning("Sector name not valid for: ",n[i,1])}
  
}


countryCheck <- function(countryInput = NULL, groupingInput = NULL){
  
  #' convert character vector to data frame
  #' identify entries which don't exist in country or grouping list
  #' print warning message for each
  #' 
  
  if(!is.null(countryInput) & !is.null(groupingInput)){stop("Please select one input")}
  
  if(!is.null(countryInput)){
    
  df <- data.frame(col = countryInput)
  dfs <- sectors_country %>% distinct(., quota_origin) 
  n <- df %>% filter(col %notin% dfs$quota_origin)
  
  } else{
    
    df <- data.frame(col = groupingInput)
    dfs <- sectors_country %>% distinct(., grouping) 
    n <- df %>% filter(col %notin% dfs$grouping)
  }
  
  for(i in seq_len(nrow(n))){warning("Name not valid for: ",n[i,1])}
  
}



trqPlot <- function(.region,
                    quotaUnit = NULL,
                    country = NULL,
                    countrySelection = NULL,
                    grouping = NULL, 
                    groupSelection = NULL){
  
  
  #' error handling for 
  #' correct region and @region
  #' quota unit inputs  @quotaUnit
  #' 
  
  trqPlotErrorHandling(region_arg = .region, quota_unit_arg = quotaUnit)
  
  
  #' filter quota unit data object. Default is KG. 
  if(is.null(quotaUnit)){
    quotaFilt <- c("Kilograms")
    message("Please note chart is for TRQs measured in Tonnes only")
    } else{ quotaFilt <- quotaUnit}
  
  
  #' filter data 
  #' depending on input for country plot type. 
  #' (I.e. grouping - EEA / Central America vs individual countries)
  #' & quota unit specified by user
  #' 
  
  if(is.null(country)){
    
    # QA check of group input:
    if(!is.null(groupSelection)){countryCheck(groupSelection)}
    
    df <- trqs_grouping %>% filter(., quota_unit_final == quotaFilt) %>%
      {if(is.null(groupSelection)) filter(., region == .region)
        else filter(.,region == .region) %>%
          filter(.,grouping %in% groupSelection)}
    
    xCol <- "grouping"
    
    
  } else if(!is.null(country)){
    
    # QA check of country input:
    
    if(!is.null(countrySelection)){countryCheck(countrySelection)}
    
    df <- trqs_country %>% filter(., quota_unit_final == quotaFilt) %>%
      {if(is.null(countrySelection)) filter(., region == .region)
        else filter(.,quota_origin %in% countrySelection)} 
    
    xCol <- "quota_origin"
    
  } 
  
  # factor levels for plot - plot the largest volume furthest on the x-axis
  myLevels <- df %>% 
    group_by(!!as.symbol(xCol)) %>% 
    summarise(value = sum(total_allocation_fill_rate)) %>%
    arrange(value)
  
  # logic for factor level
  if(is.null(country)){
    df$grouping <- factor(df$grouping , levels=myLevels$grouping)
  } else{
    df$quota_origin <- factor(df$quota_origin , levels=myLevels$quota_origin)
  }

  # convert percentage for chart:
  df <- df %>% mutate(total_allocation_fill_rate = round(total_allocation_fill_rate*100,0))
  
  t <- 
    ggplot()+
    geom_bar(
      data = df, 
      aes(
        x = !!as.symbol(xCol), 
        y = total_allocation_fill_rate, 
        fill = source
      ),
      stat = "identity",
      position = "dodge"
    )+
    geom_point(
      data = df %>% filter(total_allocation_fill_rate == 0),
      aes(
        x = !!as.symbol(xCol), 
        y = total_allocation_fill_rate
      ), 
      color = "#333333", # geom point aesthetic for chart
      shape = 4,
      size = 3)+
    scale_x_discrete(name="")+
    scale_y_continuous(
      "Fill rate (%)", 
      breaks = seq(0,100, by= 10), # manual scale for percentage 0-100%
      limits = c(NA,100)
    )+
    scale_fill_manual(values=c("#3c8dbc","#cf102d"))+
    theme(
      panel.background = element_blank(),
      panel.grid.major.x = element_line(colour="lightgrey", size=0.5),
      legend.title=element_blank(),
      axis.text.x =  element_text(size = 10),
      axis.text.y  = element_text(size = 11)
    )
  
  t            
  
}

 
trqFilledPlot <- function(.region,
                          .trqDsc = NULL,
                          .all_countries = NULL
                          ){



  #' filter data depending if sector input flagged
  if(is.null(.trqDsc)){ df <- uk_trqs_filled} else{ df <- uk_trqs_filled_sector}
  
  
  #' error handling for 
  #' correct region and @region
  #' 
  #' check if .all_countries and region are both selected. 
  #' If Region and .all_countries are not NULL 
  #' all countries will plot. 
  #' 
  
  if(!is.null(.region) & 
     !is.null(.all_countries)){
    stop("Please select .region or .all_countries - not both inputs")}                         
                            
  if(is.null(.all_countries)){
    
  trqPlotErrorHandling(region_arg = .region, quota_unit_arg = NULL)
    
    df <- df %>% filter(region == .region)
  } else if(!is.null(.all_countries)){
    df
  }
  
  #' reorder factor level for country with lowest number of days until quota is filled. 
  #' 
 if(is.null(.trqDsc)){grp <- "quota_origin"} else{grp <- "trq_dsc"}
  
  myLevels <- df %>% 
    group_by(!!as.symbol(grp)) %>% 
    summarise(value = max(quota_length_val[quota_length_cat == "avg_quota_fill"])) %>%
    arrange(-value)
  
  if(is.null(.trqDsc)){lvl = myLevels$quota_origin} else{lvl = myLevels$trq_dsc}
  
  df <- df %>% mutate(!!as.symbol(grp) := factor(!!as.symbol(grp), levels = lvl))
    #df$quota_origin <- factor(df$`quota_origin `, levels=myLevels$quota_origin)
 

  
  if(is.null(.trqDsc)){
    
    t2 <- ggplot()+
      geom_bar(
        data = df %>% filter(quota_length_cat == "avg_quota_length"),
        aes(
          x = quota_origin, 
          y = quota_length_val, 
          fill = "blue"
        ),
        stat = "identity")+
      geom_bar(
        data = df %>% filter(quota_length_cat == "avg_quota_fill"),
        aes(
          x = quota_origin, 
          y = quota_length_val, 
          fill = "red"
        ),
        stat = "identity")
    
  } else{
    
    t2 <- ggplot()+
      geom_bar(
        data = df %>% filter(quota_length_cat == "avg_quota_length"),
        aes(
          x = interaction(trq_dsc,quota_origin), 
          y = quota_length_val, 
          fill = "blue"
        ),
        stat = "identity")+
      geom_bar(
        data = df %>% filter(quota_length_cat == "avg_quota_fill"),
        aes(
          x = interaction(trq_dsc,quota_origin), 
          y = quota_length_val, 
          fill = "red"
        ),
        stat = "identity")
  }
  
  t2 <- t2 +
    scale_y_continuous(
      "Average quota duration (days)",
      labels = scales::comma,
      n.breaks=10
      )+
    scale_x_discrete(
      "", 
      position = "top" # to remove x axis label
      )+ 
    scale_fill_manual(
      values=c("#3c8dbc","#cf102d"), # manual color fill and labels
      labels = c("TRQ length", "TRQ filled")
    )+
    coord_flip()+  # flip chart axis
    theme(
      panel.background = element_blank(),
      panel.grid.major.x = element_line(colour="lightgrey", size=0.5),
      legend.title=element_blank(),
      legend.position = "none",
      axis.text.y  = element_text(size = 11)
    )
  
  
  t2
}



trqSectorRegionPlot <- function(.region, 
                                quotaUnit = NULL,
                                sectorSelect = NULL,
                                sectorRemove = NULL){
  
  
  #' error handling for 
  #' correct region and @region
  #' quota unit inputs  @quotaUnit
  #' 
  
  trqPlotErrorHandling(region_arg = .region, quota_unit_arg = quotaUnit)
  
  
  df <- sectors_region %>% filter(region == .region) %>% filter(source == "UK")
  
  # Quota unit filters ---
  
  if(is.null(quotaUnit)){  # convert data to Tonnes (default option)
    
    quotaFilt <- "Tonnes" # for chart label
    df <- df %>% mutate(across(total_quota_volume:total_quota_usage, ~ ./1000))
    df <- df %>% filter(quota_unit_final == "Kilograms")
  } else {
    
    
    quotaFilt <- quotaUnit
    df <- df %>% filter(quota_unit_final == quotaFilt)
  }
  
  
  
  # sector selection filters --
  
  sect <- c(sectorSelect)
  sectrm <- c(sectorRemove)
  
  # sector QA check:
  
  if(!is.null(sectorSelect)){sectorCheck(sect)}
     else if(!is.null(sectorRemove)){sectorCheck(sectrm)}
     
     
  if(!is.null(sectorSelect) & !is.null(sectorRemove)){
    
    message("Please select single sector input")
    
  } else{
    
    df <- df %>% 
      {if(!is.null(sectorSelect))filter(., trq_dsc %in% sectorSelect)
        else if(!is.null(sectorRemove)) filter(.,trq_dsc %notin% sectorRemove)
        else .}
    
  }
  
  
  # chart factor levels for visualizations - i.e. order chart based on highest values
  myLevels <- df %>% 
    group_by(trq_dsc) %>% 
    summarise(value = sum(total_quota_volume)) %>%
    arrange(value)
  
  df$trq_dsc <- factor(df$trq_dsc , levels=myLevels$trq_dsc)
  
  
  t <- 
    ggplot()+
    geom_bar(
      data = df, 
      aes(x = trq_dsc, 
          y = total_quota_volume, 
          fill = "blue"
      ),
      stat = "identity")+
    geom_bar(
      data = df, 
      aes(x = trq_dsc, 
          y = total_quota_usage, 
          fill = "red"
      ),
      stat = "identity"
    ) +
    geom_text(
      data = df,
      aes(
        x = trq_dsc, 
        y = total_quota_volume
      ),
      label = paste0(round((df$total_allocation_fill_rate*100),1),"%"),
      size = 4
    )+
    scale_y_continuous(
      name =quotaFilt, 
      labels = scales::comma,
      n.breaks=6
    )+
    scale_x_discrete(name="")+
    scale_fill_manual(
      values=c("#3c8dbc","#cf102d"),
      labels = c("Quota volume", "Quota usage")
    )+
    coord_flip()+
    theme(
      panel.background = element_blank(),
      panel.grid.major.x = element_line(colour="lightgrey", size=0.5),
      legend.title=element_blank(),
      legend.position = "bottom",
      axis.text.x  = element_text(size = 11)
    )
  
  
  t
  
  
}



trqSectorCountryPlot <- function(chartType,
                                 .region,
                                 quotaUnit = NULL,
                                 countrySelect = NULL,
                                 countryRemove = NULL,
                                 groupSelect = NULL,
                                 groupRemove = NULL,
                                 sectorSelect = NULL,
                                 sectorRemove = NULL){
  
  
  # Stop function if no sector selected as facet plot is unreadable. 
  
  if(is.null(sectorSelect) & is.null(sectorRemove)){
    abort("Please select specific TRQ sectors for facet plot using sectorSelect or sectorRemove")}
  
  #' error handling for 
  #' correct region and @region
  #' quota unit inputs  @quotaUnit
  #' 
  
  trqPlotErrorHandling(region_arg = .region, quota_unit_arg = quotaUnit)
  
  #' chart type 
  #' and selection filters
  #' filter for country or grouping inputs
  #' 
  
  cs <- c(countrySelect)
  cr <- c(countryRemove) # create arrays
  gs <- c(groupSelect)
  gr <- c(groupRemove)
  
  if(chartType == "country"){  
    
    # abort function if no country input selected:
    
    if(!is.null(countrySelect) & !is.null(countryRemove)){
      
      message("Please select single country input")
      
    } else{
      
  # QA check for country:
  if(is.null(countryRemove) & is.null(countrySelect)){
    message("No country input selected - are you sure?")
  }else if(is.null(countryRemove)){
      countryCheck(countryInput = countrySelect)
    }else{countryCheck(countryInput = countrySelect)}
      
      # filter data based on user input:
      df <- sectors_country %>% filter(region == .region) %>%
        {if(!is.null(countrySelect))  filter(.,quota_origin %in% cs)
          else if(!is.null(countryRemove)) filter(.,quota_origin %notin% cr)
          else .}
      
    }
    
    
  } else if(chartType == "grouping"){
    
    if(!is.null(groupSelect) & !is.null(groupRemove)){
      
      message("Please select single grouping input")
      
    } else{
      
      # QA check for grouping:
    if(is.null(groupRemove) & is.null(groupSelect)){
      message("No grouping input selected - are you sure?")
    } else if(is.null(groupRemove)){
        countryCheck(groupingInput = groupSelect) # check grouping inputs are valid. 
      } else{countryCheck(groupingInput = groupRemove)} # if not output warning message
  
          
      # filter data based on user input
      df <- sectors_grouping %>% filter(region == .region) %>%
        {if(!is.null(groupSelect))  filter(.,grouping %in% gs)
          else if(!is.null(groupRemove)) filter(.,grouping %notin% gr)
          else .}
      
    }
     
  } else{stop("Please select valid chartType input: 'country' or 'grouping' ")}
  
  
  df <- df %>% filter(source == "UK") # filter uk data
  
  # quota unit filter:
  
  if(is.null(quotaUnit)){ 
    
    quotaFilt <- "Tonnes" # for chart label
    
    df <- df %>% 
      mutate(across(total_quota_volume:total_quota_usage, ~ ./1000)) %>%    # convert data to Tonnes
      filter(quota_unit_final == "Kilograms")
    
  } else {
    
    quotaFilt <- quotaUnit
    df <- df %>% filter(quota_unit_final == quotaFilt)
  }
  
  # sector selection filters --
  
  sect <- c(sectorSelect)
  sectrm <- c(sectorRemove)
  
  # sector QA check:
  
  if(!is.null(sectorSelect)){sectorCheck(sect)}
  else if(!is.null(sectorRemove)){sectorCheck(sectrm)}
  
  #  logic for one select sector input:
  if(!is.null(sectorSelect) & !is.null(sectorRemove)){
    
    message("Please select single sector input")
    
  } else{
    
    df <- df %>% 
      {if(!is.null(sectorSelect))filter(., trq_dsc %in% sectorSelect)
        else if(!is.null(sectorRemove)) filter(.,trq_dsc %notin% sectorRemove)
        else .}
    
  }
  
  # create factor levels for plot: i.e. plot largest values at bottom of chart
  
  myLevels <- df %>% 
    group_by(trq_dsc) %>% 
    summarise(value = sum(total_quota_volume)) %>%
    arrange(-value)
  
  df$trq_dsc <- factor(df$trq_dsc , levels=myLevels$trq_dsc)
  
  
  t <- 
    ggplot()+
    geom_bar(
      data = df, 
      aes(
        x = grouping,
        y = total_quota_volume, 
        fill = "blue"
      ),
      stat = "identity")+
    geom_bar(
      data = df, 
      aes(
        x = grouping, 
        y = total_quota_usage, 
        fill = "red"
      ),
      stat = "identity"
    ) +
    geom_text(
      data = df,
      aes(
        x = grouping, 
        y = total_quota_volume
      ),
      label = paste0(round((df$total_allocation_fill_rate*100),1),"%"), # fill rate % as text
      size = 3
    )+
    scale_y_continuous(
      quotaFilt, 
      labels = scales::comma,
      n.breaks=6
    )+
    scale_x_discrete(name="")+
    scale_fill_manual(
      values=c("#3c8dbc","#cf102d"),
      labels = c("Quota volume", "Quota usage")
    )+
    coord_flip()+
    theme(
      panel.background = element_blank(),
      panel.grid.major.x = element_line(colour="lightgrey", size=0.5),
      axis.text.y  = element_text(size = 11),
      legend.title=element_blank(),
      legend.position = "none",
      axis.text.x = element_text(size = 9,angle = 60, vjust = 0.5, hjust=1)
    )+
    facet_grid(. ~ trq_dsc, scales = "free_x")+
    theme(
      strip.text.x = element_text(size=11),
      strip.background = element_rect(colour="cornflowerblue", fill="aliceblue")
    )
  
  
  t
  
}



# trqSectorPlot <- function(.region){
#   
#   # function comparing UK vs EU sector fill rates coming soon ...
#   
# }