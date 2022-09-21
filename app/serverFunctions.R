# - serverFunctions script 


rm(list = ls())


library(tidyverse)
library(janitor)
library(stringr)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(readxl)
library(openxlsx)
library(RColorBrewer)
library(leaflet)
library(rnaturalearth)
library(DT)



options(scipen=999)


path<-setwd(stringr::str_extract(rstudioapi::getActiveDocumentContext()$path,".+[/]")) 
uk_trqs <- read_excel("data\\uk_trq_data_full.xlsx")
# load R.Data ----- 

load("data\\trq_data_all10.RData")


# list arrays for drop downs
countryList <- unique(trqs_country$quota_origin)
groupingList <- unique(trqs_country$grouping)
regionList <- unique(trqs_country$region)
sectorList <- c("All",unique(sectors_region$trq_dsc))
quotaUnitList <- unique(trqs_country$quota_unit_final)




trqPlot <- function(.region,
                    quotaUnit = NULL,
                    sectorSelect = NULL,
                    country = NULL,
                    .grouping = NULL 
                    ){
  
  
  #' error handling for 
  #' correct region and @region
  #' quota unit inputs  @quotaUnit
  #' 
  
  #' filter data based on sector input
  #' region
  #' and quota unit
  #' 
  
  
  # filter dataset base don user input
  if(sectorSelect == "All" & is.null(.grouping)) {
    df <- trqs_grouping
  } else if (sectorSelect == "All" & !is.null(.grouping)){
      df <- trqs_country
  } else if (sectorSelect  != "All" & is.null(.grouping)){
      df <- sectors_grouping
  } else if(sectorSelect != "All" & !is.null(.grouping)){
      df <- sectors_country
  } else{
      df <- trqs_grouping
    } 
    
   df <- df %>% 
     filter(region == .region) %>%
     filter(quota_unit_final == quotaUnit) %>%
     {if(sectorSelect != "All") filter(.,trq_dsc == sectorSelect) else .}
   
  #' filter based on filter by trade agreement:
  #' output is to split chart into countries
  #' of chosen trade agreement
  #' i.e Andean - is split into the 3 countries.
  
  if(!is.null(.grouping)){
    df <- df %>% filter(grouping == .grouping)
    xCol <- "quota_origin"
  } else{
    xCol <- "grouping"
  }
  
  
  #' filter data 
  #' depending on input for country plot type. 
  #' (I.e. grouping - EEA / Central America vs individual countries)
  #' & quota unit specified by user
  #' 
  

  if(nrow(df) > 0){ # error check
    
  # factor levels for plot - plot the largest volume furthest on the x-axis
  myLevels <- df %>% 
    group_by(!!as.symbol(xCol)) %>% 
    summarise(value = sum(total_allocation_fill_rate)) %>%
    arrange(value)
  
  # logic for factor level
  if(is.null(.grouping)){
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
        fill = source,
        text = 
          paste0(
            "Grouping: ", !!as.symbol(xCol), 
            "<br>Quota allocaiton fill rate: ", total_allocation_fill_rate, "%"
            )
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
      axis.text.x =  element_text(size = 9),
      axis.text.y  = element_text(size = 11)
    )
  
  ggplotly(t, tooltip = c("text")) 
  
  } else{
    # error output
    shiny::validate(need((nrow(df)>0), message = "TRQ data not available for this selection."))
  }
  
}



trqAllocationPlot <- function(
                          .region, 
                          quotaUnit = NULL,
                          sectorSelect = NULL
                          ){
  
  
  #' error handling for 
  #' correct region and @region
  #' quota unit inputs  @quotaUnit
  #' 
  

  #' select data based on sector input:
  #' sector input filter:
  
  if(sectorSelect != "All"){
    
    df <- 
      sectors_grouping %>% 
        filter(region == .region) %>%
        filter(trq_dsc == sectorSelect) %>%
        filter(source == "UK")
    
  } else{
    
    df <- 
      trqs_grouping %>% 
      filter(region == .region) %>%
      filter(source == "UK")
  }
  

  # Quota unit filters ---
  
  if(is.null(quotaUnit)){  # convert data to Tonnes (default option)
    
     quotaFilt <- "Tonnes" # for chart label
    
      df <- df %>% 
        mutate(across(total_quota_volume:total_quota_usage, ~ ./1000)) %>%
        filter(quota_unit_final == "Kilograms")
    
  } else {
    
    if(quotaUnit == "Kilograms"){
      quotaFilt <- "Tonnes"
      
      df <- df %>% 
        mutate(across(total_quota_volume:total_quota_usage, ~ ./1000)) %>%
        filter(quota_unit_final == "Kilograms")
      
    } else{
      
      quotaFilt <- quotaUnit
      df <- df %>% filter(quota_unit_final == quotaFilt)
    
    }
    
 }
  
  
 
  
  if(nrow(df) >0){
  
  
  
  # chart factor levels for visualizations - i.e. order chart based on highest values
  myLevels <- df %>% 
    group_by(grouping) %>% 
    summarise(value = sum(total_quota_volume)) %>%
    arrange(value)
  
  df$grouping <- factor(df$grouping , levels=myLevels$grouping)
  
  
  t <- 
    ggplot()+
    geom_bar(
      data = df, 
      aes(x = grouping, 
          y = total_quota_volume, 
          fill = "blue",
          text =
            paste0(
             "Grouping: ", grouping,
             "<br>Total allocation volume: ", 
             formatC(
               total_quota_volume, 
               format = "f", 
               big.mark = ",", 
               digits = 0
               ) # format number of plotly output
            )
        ),
      stat = "identity")+
    geom_bar(
      data = df, 
      aes(x = grouping, 
          y = total_quota_usage, 
          fill = "red",
          text =
             paste0(
               "Grouping: ", grouping,
               "<br>Total allocation usage: ",
               formatC(
                 total_quota_usage, 
                 format = "f", 
                 big.mark = ",", 
                 digits = 0
               ) # format number of plotly output
             )
         ),
      stat = "identity"
    ) +
    geom_text(
      data = df,
      aes(
        x = grouping, 
        y = total_quota_volume,
        text =
          paste0(
            "Grouping: ", grouping,
            "<br>Total allocation fill rate: ", 
            round(total_allocation_fill_rate*100,1), "%"
          )
      ),
      label = paste0(round((df$total_allocation_fill_rate*100),1),"%"),
      size = 4
    )+
    scale_y_continuous(
      name = quotaFilt, 
      labels = scales::comma,
      n.breaks=7
    )+
    scale_x_discrete(name="")+
    scale_fill_manual(
      quotaFilt,
      values=c("#3c8dbc","#cf102d"),
      labels = c("Quota volume", "Quota usage")
    )+
    coord_flip()+
    theme(
      panel.background = element_blank(),
      panel.grid.major.x = element_line(colour="lightgrey", size=0.5),
      legend.title=element_blank(),
      legend.position = "bottom",
      axis.text.y  = element_text(size = 10),
      axis.text.x = element_text(size = 9,angle = 0, vjust = 0.5, hjust=1)
    ) 
  
  
  ggplotly(t, tooltip = c("text")) %>% layout(showlegend = FALSE)
  
  } else{
    # error output
    shiny::validate(need((nrow(df)>0), message = "TRQ data not available for this selection."))
   }
  
}


trqSectorPlot <- function(
                            chartType,
                            .region,
                            quotaUnit = NULL,
                            sectorSelect = NULL,
                            euSwitch = NULL
                          ){
  
  
 
  #' chart type 
  #' and selection filters
  #' filter for country or grouping inputs
  #' 
   
   # select df based on input:
  
  if(chartType == "By Partner Country"){
    df <- sectors_country %>% filter(quota_origin == .region)
  } else if(chartType == "by Trade Agreement"){
    df <- sectors_grouping %>% filter(grouping == .region)
    }else{
      df <- sectors_region %>% filter(region == .region)
    }
 
  # quota unit filter:
  
  if(quotaUnit == "Kilograms"){ 
    
    quotaFilt <- "Tonnes" # for chart label
    
    df <- df %>% 
      mutate(across(total_quota_volume:total_quota_usage, ~ ./1000))
    
  } else {
    quotaFilt <- quotaUnit
    
  } 
  
  
  
  if(is.null(euSwitch)){
    
    df <- df %>% 
        filter(source == "UK") %>%
        filter(quota_unit_final == quotaUnit) 
     
  
  
  if(nrow(df) >0){
  
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
        x = trq_dsc,
        y = total_quota_volume, 
        fill = "blue",
        text = 
          paste0(
            "Sector group: ", trq_dsc,
            "<br>Total allocation volume: ", 
            formatC(
              total_quota_volume, 
              format = "f", 
              big.mark = ",", 
              digits = 0
            ) # format number of plotly output
          )
      ),
      stat = "identity")+
    geom_bar(
      data = df, 
      aes(
        x = trq_dsc, 
        y = total_quota_usage, 
        fill = "red",
        text = 
          paste0(
            "Sector group: ", trq_dsc,
            "<br>Total allocation usage: ", 
            formatC(
              total_quota_usage, 
              format = "f", 
              big.mark = ",", 
              digits = 0
            ) # format number of plotly output
            
          )
      ),
      stat = "identity"
    ) +
    geom_text(
      data = df,
      aes(
        x = trq_dsc, 
        y = total_quota_volume,
        text =
          paste0(
            "Sector group: ", trq_dsc,
            "<br>Total allocation fill rate: ", 
            round(total_allocation_fill_rate*100,1), "%",
            
            "<br>Total allocation volume: ", 
            formatC(
              total_quota_volume, 
              format = "f", 
              big.mark = ",", 
              digits = 0
            ), # format number of plotly output
            
            "<br>Total allocation usage: ", 
            formatC(
              total_quota_usage, 
              format = "f", 
              big.mark = ",", 
              digits = 0
            ) # format number of plotly output
          )
      ),
      label = paste0(round((df$total_allocation_fill_rate*100),1),"%"), # fill rate % as text
      size = 4
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
      axis.text.x = element_text(size = 10,angle = 0, vjust = 0.5, hjust=1)
    )#+
    #theme(
     # strip.text.x = element_text(size=11),
      #strip.background = element_rect(colour="cornflowerblue", fill="aliceblue")
  #  )
  
  
  ggplotly(t, tooltip = c("text"))
  
  } else{
    
    shiny::validate(need((nrow(df)>0), message = "TRQ data not available for this selection."))
    
  }
    
  } else{
    
    df <- df %>%  
      filter(quota_unit_final == quotaUnit) %>%
      mutate(total_allocation_fill_rate = round(total_allocation_fill_rate*100,0))
    
    
    myLevels <- df %>% 
      filter(source == "UK") %>% # added in so the order is the same between charts
      group_by(trq_dsc) %>% 
      summarise(value = sum(total_quota_volume)) %>%
      arrange(-value)
    
    df$trq_dsc <- factor(df$trq_dsc , levels=myLevels$trq_dsc)
    
    p <- ggplot()+
      geom_bar(
        data = df,
        aes(
          x = trq_dsc,
          y = total_allocation_fill_rate,
          fill = source,
          text = 
            paste0(
              "Sector: ", trq_dsc, 
              "<br>",source,
              "<br>Total allocaiton fill rate", total_allocation_fill_rate, " %"
            )
        ),
        stat = "identity",
        position = "dodge"
      )+
      geom_point(
        data = df %>% 
          group_by(trq_dsc) %>% 
          summarise(total_allocation_fill_rate = mean(total_allocation_fill_rate)) %>%
          filter(total_allocation_fill_rate == 0),
        aes(
          x = trq_dsc, 
          y = total_allocation_fill_rate,
          text =
            paste0(
              "Sector: ", trq_dsc, 
              "<br>EU28 fill rate: 0%", 
              "<br>UK fill rate: 0%"
            )
        ), 
        color = "#333333", # geom point aesthetic for chart
        shape = 4,
        size = 2.5
        )+
      scale_y_continuous(
        "Fill rate (%)",
        labels = scales::comma,
        n.breaks=6
      )+
      scale_x_discrete(name="")+
      scale_fill_manual(
        values=c("#3c8dbc","#cf102d"),
        labels = c("2020-EU28", "2021-UK")
      )+
      theme(
        panel.background = element_blank(),
        panel.grid.major.x = element_line(colour="lightgrey", size=0.5),
        axis.text.y  = element_text(size = 11),
        legend.title=element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size = 10,angle = 0, vjust = 0.5, hjust=1)
      )+
      theme(
        strip.text.x = element_text(size=11),
        strip.background = element_rect(colour="cornflowerblue", fill="aliceblue")
      )+
      coord_flip()
    
    
    ggplotly(p, tooltip = c("text"))
    
    
  }
  
}



trqTopCountryPlot <- function(quotaUnit, sectorSelect, quotaMin){
  

     # select dataframe
     if(sectorSelect == "All"){ df <- trqs_country
     } else{
       df <- sectors_country %>% filter(trq_dsc == sectorSelect)
     }
     
     
     df <- df %>% 
       filter(quota_unit_final == quotaUnit) %>%
       filter(source == "UK") %>%
       filter(total_quota_volume >= quotaMin)
     
  
    # factor level to order chart byt largest fill rate:
     
     myLevels <- df %>% 
       group_by(quota_origin) %>% 
       summarise(value = sum(total_allocation_fill_rate)) %>%
       arrange(value)
     
       df$quota_origin <- factor(df$quota_origin , levels=myLevels$quota_origin)
  
  df <- df %>% mutate(total_allocation_fill_rate = round(total_allocation_fill_rate*100,0))
       
     p <- 
       ggplot()+
       geom_bar(
         data = df,
         aes(
          x = quota_origin,
          y = 100,
          text =
             paste0(
               "Country: ", quota_origin,
               "<br>Total allocation volume: ",
               formatC(
                 total_quota_volume,
                 format = "f",
                 big.mark = ",",
                 digits = 0
               ), # format number of plotly output
          
               "<br>Total allocation usage: ",
               formatC(
                 total_quota_usage,
                 format = "f",
                 big.mark = ",",
                 digits = 0
               ), # format number of plotly output

              "<br> Total numer of quotas: ",
              quota_count
            )
        ),
        fill = "#FFFFFF",
        color = "grey",
        stat = "identity"
       )+
       geom_bar(
         data = df,
         aes(
           x = quota_origin,
           y = total_allocation_fill_rate,
           text = 
             paste0(
               "Country: ", quota_origin,
               "<br>Total allocation fill rate: ", 
               total_allocation_fill_rate, "%"
               )
           ),
         fill = "#cf102d",
         color = "black",
         stat = "identity"
       )+
       scale_x_discrete(name="")+
       scale_y_continuous(
         "Fill rate (%)", 
         breaks = seq(0,100, by= 10), # manual scale for percentage 0-100%
         limits = c(NA,100)
       )+
       coord_flip()+
       theme(
         panel.background = element_blank(),
         axis.text.y  = element_text(size = 11),
         legend.title=element_blank()
       )
     
     p
       
     ggplotly(p,  tooltip = c("text"))

}




trqTopChangePlot <- function(quotaUnit, sectorSelect){
  
  
  # select dataframe
  if(sectorSelect == "All"){ df <- trqs_country
  } else{
    df <- sectors_country %>% filter(trq_dsc == sectorSelect)
  }
  
  df <- df %>% filter(quota_unit_final == quotaUnit)
  
  df <- df %>% mutate(total_allocation_fill_rate = round(total_allocation_fill_rate*100,0))
  
  # df levels:
  
  wide <- df %>%
    pivot_wider(
      .,
      names_from = source,
      values_from = c(quota_count:total_allocation_fill_rate)
      ) %>% 
    mutate(pc = total_allocation_fill_rate_UK - total_allocation_fill_rate_EU) %>%
    mutate(
      zeroFilter = 
        ifelse(
          total_allocation_fill_rate_UK == 0 & total_allocation_fill_rate_EU == 0,
          1,
          0
          )
       )
    
  myLevels <- df %>% 
    left_join(wide[,c("quota_origin","pc")], by = "quota_origin") %>%
    arrange(desc(pc)) %>%
    distinct(quota_origin)
  
  # join zeroFilter flag:
  df <- df %>% left_join(wide[,c("quota_origin","zeroFilter")], by = "quota_origin")
  
  df$quota_origin <- factor(df$quota_origin , levels=myLevels$quota_origin)
  
  
  # manual colours for plot:
  cols <- c("UK"="#f04546","EU"="#3591d1")
  
  p <-
    ggplot()+
    geom_line(
      data = df,#%>% filter(zeroFilter != 1),
      aes(
        x = total_allocation_fill_rate,
        y = quota_origin
      ),
      size = 0.25,
      color = "grey"
    )+
    geom_point(
      data = df %>% filter(source == "UK") %>% filter(zeroFilter != 1),
      aes(
        x = total_allocation_fill_rate,
        y = quota_origin,
        color = "UK",
        text =
          paste0(
            "Source: UK",
            "<br>Origin: ",quota_origin,
            "<br>Total allocaiton fill rate:  ", total_allocation_fill_rate, " %"
          )
      ),
      size = 2
    )+
    geom_point(
      data = df %>% filter(source == "EU") %>% filter(zeroFilter != 1),
      aes(
        x = total_allocation_fill_rate,
        y = quota_origin,
        color = "EU",
        text =
          paste0(
            "Source: EU",
            "<br>Origin: ",quota_origin,
            "<br>Total allocaiton fill rate:  ", total_allocation_fill_rate, " %"
          )
      ),
      size = 2
    )+
    geom_point(
      data = df %>% filter(source == "EU") %>% filter(zeroFilter == 1),
      aes(
        x = total_allocation_fill_rate,
        y = quota_origin,
        #color = "EU",
        text =
          paste0(
            "Source: UK and EU",
            "<br>Origin: ",quota_origin,
            "<br>Total allocaiton fill rate:  ", total_allocation_fill_rate, " %"
          )
      ),
      size = 2,
      shape = 4,
      color = "darkgrey"
    )+
    scale_y_discrete(name="")+
    scale_x_continuous(
      "Fill rate (%)", 
      breaks = seq(0,100, by= 10), # manual scale for percentage 0-100%
      limits = c(NA,100)
    )+
    scale_colour_manual(name="",values=cols) +
    theme(
      panel.background = element_blank(),
      legend.key=element_blank()
    )

  ggplotly(p, tooltip = c("text"))
    
  
}


trqDataTable <- function(.region, .sector){
  

  df <- uk_trqs %>%
    clean_names() %>%
    filter(region == .region) %>%
    dplyr::select(
      quota_number,
      trq_dsc,
      quota_product_description,
      quota_origin,
      validity_start_date,
      validity_end_date,
      quota_volume,
      remaining_balance,
      quota_fill_rate,
      quota_unit_final,
      commodity_codes,
      quota_status,
      last_allocation_date,
      trq_type,
      data_type
    ) 
  
  # filter for sector
  df <- df %>%
    {if(.sector == "All") .
     else filter(., trq_dsc == .sector)}
  
  
  
  DT::datatable(df, 
                colnames = c("Quota Number", 
                             "Sector", 
                             "Quota Description", 
                             "Quota origin",
                             "Quota start date",
                             "Quota end date", 
                             "Quota Volume", 
                             "Quota remaining balance",
                             "Quota Fill Rate", 
                             "Quota unit", 
                             "Commodity Codes",
                             "Status",
                             "Last allocation date",
                             "Quota type",
                             "Data type"),
                filter = "top",
                rownames = FALSE,
                extensions = c('Buttons'),
                options = list(
                  dom = 'Bfti',
                  scrollY = 700,
                  scrollX = 100,
                  scrollCollapse = TRUE,
                  buttons = c('copy', 'excel', 'print'),
                  searchHighlight = TRUE,
                  searchDelay = 0,
                  selection = "single",
                  pageLength = -1,
                  lengthMenu = list(c(20, -1), c("20", "All"))
                )) %>% 
          formatCurrency(7:8, currency = "", interval = 3, mark = ",", digits = 0) %>%
          formatPercentage(9, digits = 2)
  
  
  
}
  
  
#trqDataTable()

trqUKxl <- function(){
  
  wb <- createWorkbook()
  
  addWorksheet(wb, "Notes")
  addWorksheet(wb, "UK-TRQs")
  

  # UK first come first serve data for excel output:
  uk_trq_excel <-
    uk_trqs %>%
    arrange(., quota_number) %>%
    dplyr::select(`Quota Number` = quota_number,
           `Sector` = trq_dsc,
           `Quota product description` = quota_product_description,
           Year = year,
           `Quota origin` = quota_origin,
           `Region` = region,
           `Validity start date` = validity_start_date,
           `Validity end date` = validity_end_date,
           `Quota volume` = quota_volume,
           `Remaining balance` = remaining_balance,
           `Quota fill rate` = quota_fill_rate,
           `Quota unit` = quota_unit,
           `Commodity codes` = commodity_codes,
           `Quota status` = quota_status,
           `Last allocation date` = last_allocation_date,
           `TRQ type` = trq_type,
           `Data type` = data_type
    )
  
  # format numbers for xl output:
  
  
  
  
  writeData(wb, "UK-TRQs", uk_trq_excel)
  
  freezePane(wb, "UK-TRQs", firstActiveRow = 1, firstActiveCol = 1)
  
  df_end  <- length(uk_trq_excel)
  
  headerStyle <- createStyle(fontSize = 11, halign = "center", textDecoration = "bold")
  addStyle(wb, "UK-TRQs",headerStyle, rows = 1, cols = 1:df_end, gridExpand = TRUE)
  
  
  setColWidths(wb, "UK-TRQs", cols = c(3:13), widths =  30)
  setColWidths(wb, "UK-TRQs", cols = c(1,2:12,14:17), widths = 15)
  
  
  # add number styles:
  
  numberStyle  <- createStyle(numFmt = "#,##0")
  numberStyle2 <- createStyle(numFmt = "0%")
  
  addStyle(wb,"UK-TRQs",numberStyle, cols = 9:10, rows = 2:nrow(uk_trq_excel), gridExpand = TRUE)
  addStyle(wb,"UK-TRQs",numberStyle2, cols = 11, rows = 2:nrow(uk_trq_excel), gridExpand = TRUE)
  
  # notes: --
  
  # write data --
  
  # title
  writeData(wb,"Notes","Title",startCol = 1, startRow = 1)
  writeData(wb,"Notes","UK Tariff Rate Quotas", startCol = 1, startRow = 2)
  
  # dsc
  writeData(wb,"Notes","Description",startCol = 1, startRow = 4)
  writeData(wb,"Notes","Quota level dataset of UK tariff rate quotas with historic and live utilisaiton rates for 2021 onwards", startCol = 1, startRow = 5)
  
  # date:
  writeData(wb,"Notes","Data extracted",startCol = 1, startRow = 7)
  writeData(wb,"Notes",uk_trq_date, startCol = 1, startRow = 8)
  

  
  #notes
  writeData(wb, "Notes", "UK quota data notes:", startCol = 1, startRow = 10)
  writeData(wb, "Notes", "1.	UK Tariff Rate Quota data shows historical and live quota data for 2021 onwards following EU-exit. Equivalent historical UK quota data prior to 2021 is not available.", startCol = 1, startRow = 11)
  writeData(wb, "Notes", "2.	Data is presented at the quota level. Individual country usage of Erga Omnes quotas is not available.", startCol = 1, startRow = 12)
  writeData(wb, "Notes", "3.	Quota volumes and balances are providing in the quota unit of measurement", startCol = 1, startRow = 13)
  writeData(wb, "Notes", "4.	Quota historic and live utilisation rates can be found in the Quota fill rate column.", startCol = 1, startRow = 14)
  writeData(wb, "Notes", "5.	Quota status indicates the status of whether quotas are closed or live. If a quota is “exhausted” this shows that the quota was filled before the quota period ended.", startCol = 1, startRow = 15)
  writeData(wb, "Notes", "6.	Quotas which have not yet opened have their quota status denoted as Future.", startCol = 1, startRow = 16)
  writeData(wb, "Notes", "7.	The last allocation date is the last recorded date a first come first serve quota was used. This data is not available for licenced quotas.", startCol = 1, startRow = 17)
  writeData(wb, "Notes", "8.	TRQ type indicates whether the quota is part of an FTA, under WTO terms, an Autonomous Tariff Quota (ATQ) or safeguards.", startCol = 1, startRow = 18)
  writeData(wb, "Notes", "9.	Data type distinguishes licensed agricultural quotas vs non-license first come first serve quotas.", startCol = 1, startRow = 19)
  
  
  # data sources:
  
  writeData(wb, "Notes", "Data sources:", startCol = 1, startRow = 21)
  
  writeData(wb, "Notes", "Non-licensed tariff-quotas:", startCol = 1, startRow = 22)
  
  x <- c("https://www.trade-tariff.service.gov.uk/quota_search")
  class(x) <- "hyperlink"
  names(x) <- "uk-trade-tariff-service"
  
  writeData(wb, "Notes", x = x, startCol = 1, startRow = 23)
  
  x <- c("https://data.gov.uk/dataset/4a478c7e-16c7-4c28-ab9b-967bb79342e9/uk-trade-quotas")
  class(x) <- "hyperlink"
  names(x) <- "uk-trade-quotas public dataset .gov"
  
  writeData(wb, "Notes", x = x, startCol = 1, startRow = 24)
  
  
  writeData(wb, "Notes", "Licensed tariff-quotas:", startCol = 1, startRow = 25)
  
  x <- c("https://www.gov.uk/government/collections/uk-tariff-rate-quotas-allocation-of-co-efficients")
  class(x) <- "hyperlink"
  names(x) <- "Rural Payments Agency uk-tariff-quotas public datasets"
  
  writeData(wb, "Notes", x = x, startCol = 1, startRow = 26)
  
  
  writeData(wb, "Notes", "- Non-license quota data is up to date as of date of extraction.", startCol = 1, startRow = 27)
  writeData(wb, "Notes", "- License quota data is up to date as of 30/3/2022.", startCol = 1, startRow = 28)
  
  
  # reference documents:
  
  writeData(wb, "Notes", "Reference documents for UK tariff quotas:", startCol = 1, startRow = 30)
  
  x <- c("https://www.gov.uk/government/publications/reference-documents-for-the-customs-tariff-quotas-eu-exit-regulations-2020")
  class(x) <- "hyperlink"
  names(x) <- "Reference documents for tariff-quotas."
  
  writeData(wb, "Notes", x = x, startCol = 1, startRow = 31)
  

  x <- c("https://www.gov.uk/guidance/duty-suspensions-and-tariff-quotas#atq-review")
  class(x) <- "hyperlink"
  names(x) <- "Autonomous Tariff Quotas (ATQs)"
  
  writeData(wb, "Notes", x = x, startCol = 1, startRow = 32)
  
  

  x <- c("https://www.gov.uk/government/publications/trade-remedies-notices-tariff-rate-quotas-on-steel-goods/trade-remedies-notice-2021-no-1-safeguard-measure-tariff-rate-quota-on-steel-goods-web-version")
  class(x) <- "hyperlink"
  names(x) <- "Safeguarding quotas for steel goods"
  
  writeData(wb, "Notes", x = x, startCol = 1, startRow = 33)
  
  

  x <- c("https://www.gov.uk/guidance/duty-suspensions-and-tariff-quotas#atq-review")
  class(x) <- "hyperlink"
  names(x) <- "Free Trade Agreement with non-EU countries"
  
  writeData(wb, "Notes", x = x, startCol = 1, startRow = 34)
  writeData(wb, "Notes", "Individual quota data can be found within each countries FTA text", startCol = 1, startRow = 35)
  
  boldStyle <- createStyle(textDecoration = "bold", fontSize = 12)
  italStyle <- createStyle(textDecoration = "italic", fontSize = 10)
  
  addStyle(wb,"Notes",boldStyle, rows = 1, cols = 1)
  addStyle(wb,"Notes",boldStyle, rows = 4, cols = 1)
  addStyle(wb,"Notes",boldStyle, rows = 7, cols = 1)
  addStyle(wb,"Notes",boldStyle, rows = 10, cols = 1)
  addStyle(wb,"Notes",boldStyle, rows = 21, cols = 1)
  addStyle(wb,"Notes",boldStyle, rows = 29, cols = 1)
  
  addStyle(wb,"Notes",italStyle, rows = 27, cols = 1)
  addStyle(wb,"Notes",italStyle, rows = 28, cols = 1)
  addStyle(wb,"Notes",italStyle, rows = 35, cols = 1)
  
  return(wb)
  
}

summaryMap <- function(switch = NULL){

 map <- ne_countries()

 # country isos to filter data:

 isos <- trqs_country %>% 
   mutate(iso = strsplit(iso, ",")) %>% 
   unnest(iso) %>% 
   distinct(iso) %>%
   pull()


# filter data
 data2 <- trqs_country %>% filter(iso %in% isos) %>%
   filter(source == "UK") %>%
   filter(quota_unit_final == "Kilograms") %>%
   mutate(
     total_quota_usage = total_quota_usage / 1000,
     total_quota_volume = total_quota_volume / 1000
   )

# table is a tibble - convert to df to match into map data
data3 <- as.data.frame(data2)

# create variable you want to plot using polygons using created df mapping on iso codes
map$volume <- data3[match(map$iso_a2, data3$iso), "total_quota_volume"]
map$usage  <- data3[match(map$iso_a2, data3$iso), "total_quota_usage"]
map$fill_rate  <- data3[match(map$iso_a2, data3$iso), "total_allocation_fill_rate"]



if(is.null(switch)){

  # define custom text for hover labels
    map$labels <- paste0(
      "<strong> Country: </strong> ",
      map$name, "<br/> ",
      "<strong> Total Quota Allocation (Tonnes): </strong> ",
      formatC(
        map$volume,
        format = "f",
        big.mark = ",",
        digits = 0
       ), "<br/> "
      ) %>%
      lapply(htmltools::HTML)
    
  
  bins <- c(0, 1000,10000, 50000, 100000, 200000, 300000, 500000, Inf)
  
  pal <- colorBin(
    #palette = "viridis", domain = map$trade,
    palette = "YlOrRd", 
    domain = map$volume,
    bins = bins
  )
  
  
mp <-  leaflet(map) %>%
        addTiles() %>%
        setView(lng = 0, lat = 30, zoom = 2) %>%
        addPolygons(
          fillColor = ~ pal(volume),
          weight = 1,
          opacity = 1,
          color = "grey",
          dashArray = "1",
          fillOpacity = 0.7,
          label = ~labels,
          highlight = 
          highlightOptions(
          weight = 2,
          color = "white",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        )
     )

} else{
  

  map$labels <- paste0(
    "<strong> Country: </strong> ",
    map$name, "<br/> ",
    "<strong> Total Quota Utilisation (%): </strong> ",
    formatC(
      map$fill_rate*100,
      format = "f",
      big.mark = ",",
      digits = 2
    ), "<br/> "
  ) %>%
    lapply(htmltools::HTML)
  
  
  bins <- c(0, 0.05,0.1, 0.2, 0.4, 0.6, 0.8, 0.9, 1)
  
  pal <- colorBin(
    #palette = "viridis", domain = map$trade,
    palette = "YlOrRd", 
    domain = map$fill_rate,
    bins = bins
  )
  
  mp <-  leaflet(map) %>%
    addTiles() %>%
    setView(lng = 0, lat = 30, zoom = 2) %>%
    addPolygons(
      fillColor = ~ pal(fill_rate),
      weight = 1,
      opacity = 1,
      color = "grey",
      dashArray = "1",
      fillOpacity = 0.7,
      label = ~labels,
      highlight = 
        highlightOptions(
          weight = 2,
          color = "white",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        )
    ) 
}

return(mp)


}


countryDataTable <- function(.sectorSwitch = NULL){


  df <- trqs_country %>%
    filter(source == "UK") %>%
    select(-iso, -source)

  DT::datatable(df, 
                colnames = c("Region", 
                             "Agreement", 
                             "Quota origin",
                             "Quota unit", 
                             "Number of quotas",
                             "Total Quota Allocation", 
                             "Total Quota Allocation Utilisation",
                             "Total Allocation Fill Rate"
                    ),
                filter = "top",
                rownames = FALSE,
                extensions = c('Buttons'),
                options = list(
                  dom = 'Bfti',
                  scrollY = 700,
                  scrollX = 100,
                  scrollCollapse = TRUE,
                  buttons = c('copy', 'excel', 'print'),
                  searchHighlight = TRUE,
                  searchDelay = 0,
                  selection = "single",
                  pageLength = -1,
                  lengthMenu = list(c(20, -1), c("20", "All"))
                )) %>% 
    formatCurrency(6:7, currency = "", interval = 3, mark = ",", digits = 0) %>%
    formatPercentage(8, digits = 2)


}