## TRQ test script. 
## initial draft complete 10/2/22
#' - v2 - 14/02/22 - update to include license quota data. 
#'      - 18/02/22 - average individual quota fill rates added into aggregated tables to allow for comparison
#' - v3 - 18/02/22 - Section 6 added to create a sector field for data outptus and visuals for RSP/dashboard. 
#'      - 20/02/22 - Update to sector analysis, creation of Sector function to create TRQ classifications for visualisations. 
#' - v4 - 20/02/22 - Aggregation Function created to reduce replication throughout script. 
#'      - 20/02/22 - Removal of function - unnecessary complication to script. Full audit would require data cleaning at start of script before applying function across TRQ/sector level figures. 
#'      - 21/02/22 - Addition of EU agg. function to reduce replication when aggregating EU TRQs by sector/quota unit type too. 
#'      - 22/02/22 - Sector filled quota data added. 
#' - v5 - 23/03/22 - Removal of non KG quotas to outputs. 
#' - v6 - 23/02/22 - Outputs to be dis-aggregated to output grouping, quota unit and TRQ dsc. This is to then be used separately for visuals (i.e. functions to have additional input for quota unit filter)
#'      - 23/02/22 - Input file created to align quota units between UK and EU data. 
#'      - 23/03/22 - Quota unit final added and HL converted to L. Quota unit added to group by aggregation within functions. 
#'      - 24/02/22 - Manual inputs for TRQ dsc updated. Initial outputs created. 
#'      - 25/02/22 - Outputs re-run to reflect updated RSP HS descriptions. 
#' - v7 - 01/03/22 - outputs re-run with Thailand removed. This was incorrectly in region list. UK country input file updated. QA sections removed as they are now dedundant. 
#'      
#'      
#'      

# Set up ------

rm(list = ls())

# set file path where the R script is saved
path<-setwd(stringr::str_extract(rstudioapi::getActiveDocumentContext()$path,".+[/]")) 

# load packages
library(readxl)
library(janitor) # for the clean_names function
library(tidyverse)
library(dplyr)
library(countrycode)
library(jsonlite)
library(httr)
library(zoo)
library(downloader)
library(gdata)
library(filesstrings)
library(stringr)
library(reshape2)
library(lubridate)
library(openxlsx) 
library(plotly)
library(RColorBrewer)
library(extrafont)

options(scipen=999)

`%notin%` <- Negate(`%in%`)

# Functions ----------------------------------------------------

uk_aggFunction <- function(df, .grouping = NULL){
  
  dfnm <- deparse(substitute(df))
  
  if(!exists(dfnm)){message("Please input valid dataframe")}
  
  for(g in c("grouping","region","quota_origin")){
    for(c in unique(uk_trq$region)){
      
      clean_grouping <- make_clean_names(.grouping)
      nm <- paste0(g,"_",c) # name for df
      
      df2 <- df %>% filter(region == c) # filter by region
      
      # logic to capture user inputting grouping
      if(!is.null(.grouping)){
        df3 <- df2 %>% group_by(!!as.symbol(g), !!as.symbol(clean_grouping), quota_unit_final)
      } else{
        df3 <- df2 %>% group_by(!!as.symbol(g), quota_unit_final)
      }
      
      df3 <- df3 %>%
        summarise(
          quota_count = length(quota_number),
          total_quota_volume = sum(quota_volume),
          total_balance_remaining = sum(remaining_balance), 
          .groups = "drop"
        ) %>%
        mutate(total_quota_usage = total_quota_volume - total_balance_remaining) %>%
        mutate(total_allocation_fill_rate = total_quota_usage / total_quota_volume) 
      
      
      uk_list[[nm]] <- df3
      
      print(paste0("df created for ",c," by ", g))
      
    }  # end second loop bracket
  }  # end first loop bracket
  
  return(uk_list) # return function object 
  
} # end function bracket

# EU aggregation Function -

#'
#' Aggregation function of EU data. 
#' Separate function required to reduce complication given EU data
#' Has a different structure i.e. different unit of measurement (no balance)
#' Simplifies process if splitting uk and eu functions
#' Rather than combined into one longer function with an additional input. 
#'  
#'  
eu_aggFunction <- function(df, .grouping = NULL){
  
  # grouping QA check
if(!is.null(.grouping)){ 
  .qa.grouping <- make_clean_names(.grouping)
  col_check <- colnames(df)
  if(.qa.grouping %notin% col_check){
    
    print(message(".grouping does not exist - please check group name"))
  }
} # end QA check
  for(g in c("grouping","region","origin")){
    for(c in unique(eu_trq$region)){
      
      clean_grouping <- make_clean_names(.grouping)
      nm <- paste0(g,"_",c) 
      
      df2 <- df %>% filter(region == c) %>%
        group_by(quota_number) %>%
        mutate(quota_disinct =  n()) %>%
        ungroup() 
      
      if(!is.null(.grouping)){
        df3 <- df2 %>% group_by(!!as.symbol(g), !!as.symbol(clean_grouping), quota_unit_final)
      } else{
        df3 <- df2 %>% group_by(!!as.symbol(g), quota_unit_final)
      }
      
      df3 <- df3 %>%
        summarise(
          quota_count = length(quota_number),
          total_quota_volume = sum(volume),
          total_quota_usage = sum(eu_usage),
          .groups = "drop"
        ) %>%
        mutate(total_allocation_fill_rate = total_quota_usage / total_quota_volume)
      
      eu_list[[nm]] <- df3
      
      print(paste0("df created for ",c," by ", g))
      
    } # end second loop bracket
  } # end 1st loop bracket
  
  return(eu_list)
} # end function bracket

#'
#' Sector function is a step by step process of ungrouping the concatenated commodity code cell
#' Then identifying the more common HS chapter 
#' Then creating a simple sector classification to produce outputs for. 
#'
#'

sectorFunction <- function(.df, .colname, .delim){
  
  
  nm <- paste0(deparse(substitute(.df)),"_dups")
  print(nm)
  
  #.df <- uk_trq
  df2 <- .df %>%
    mutate(
      !!as.symbol(.colname) := 
        strsplit(as.character(!!as.symbol(.colname)),.delim)) %>% 
    unnest(!!as.symbol(.colname)) %>%  # unconcatenate the commodity codes and return a new repeaed row for each individual code
    select(quota_number,!!as.symbol(.colname)) %>%
    mutate(
      !!as.symbol(.colname) := 
        str_trim(!!as.symbol(.colname), side = "both")) %>% # remove white space
    mutate(
      hs2 = str_sub(!!as.symbol(.colname),1,2)) %>% 
    group_by(quota_number, hs2) %>%
    summarise(quota_count = length(quota_number))
  
  # clean strings and calculate if a quota is repated (i.e. has multiple HS chapters within the goods codes)
  df3 <- df2 %>%
    left_join(hs_dsc, by = "hs2") %>%
    mutate(across(hs_section, ~ str_replace_all(., "[:digit:]", ""))) %>%
    mutate(across(hs_section, ~ str_replace_all(., " - ", ""))) %>%
    mutate(across(hs_section, ~ str_trim(., side = "both"))) %>%
    group_by(quota_number) %>%
    mutate(quota_dup = n()) # identify where quota has multiple instances (i.e. multple HS chapters)
  
  
  #
  #' some quotas have a combined 50.50 split between HS chapters. 
  #' Where this exists the more broad HS section description is used.
  #' Where a HS chapter dominates (i.e. > 50%) the more specific RSP HS section is used. 
  #
  
  df4 <- df3 %>% 
    mutate(hs_prop = quota_count / sum(quota_count)) %>% # proportion of commodity code HS chapters. 
    filter(quota_count == max(quota_count)) %>% # Filter based on the max occurring HS2 chapter. 
    mutate(trq_dsc = ifelse(hs_prop > 0.5, rsp_hs_section, hs_section)) # if one hs chapter is 50%  or mroe of all codes - the RSP (more specific) sector classificaiton is used. 
  
  
  dups <- df4 %>% group_by(quota_number) %>% summarise(dup_count = length(quota_number)) %>% filter(dup_count > 1)
  
  df_dup <- df4 %>% filter(quota_number %in% dups$quota_number)
  list[[nm]] <- df_dup
  print(head(df_dup,5))
  
  # extract duplicate codes where even split between HS chapter codes. 
  # Identify any required manual adjustments as method won't capture 100% of codes. 
  
  # remove duplicates so there is on quota per row. 
  
  df5 <- df4 %>% distinct(., quota_number, .keep_all = TRUE) %>% select(quota_number, trq_dsc)
  df_final <- .df %>% left_join(df5, by = "quota_number") %>% relocate(trq_dsc, .after = quota_number)
  
  nm2 <- paste0("df_final_", deparse(substitute(.df)))
  list[[nm2]] <- df_final
  
  return(list)
}

# TRQ LEVEL FILL RATES---------------------------------------------------------
# 0. Clean data -----------

#' Combine  both quota data sources together for UK TRQs. 
#' Clean strings and change quota name for EU TRQ data. 
#' 
#' 
#' Once data is more streamlined 
#' Can create single UK aggregation function to increase efficiency and reduce replication. 
#' 
#'
#'

# hs descriptions
hs_dsc <- 
  read_excel("..\\data\\hs_descriptions.xlsx") %>%
  clean_names() %>%
  mutate(hs2 = str_sub(hs2_description,1,2))

# UK TRQs:
# country input file
country_input <- read_excel("..\\data\\uk_trq_country_input.xlsx") %>% clean_names()

# upload and clean data
# first come first serve quotas
uk_trq <- read_excel("..\\data\\uk_trq_data.xlsx") %>% 
  clean_names() %>% 
  filter(year == 2021) %>%
  left_join(country_input, by = c("quota_origin" = "country_name")) %>% ## compile reigon names
  filter(!region == "NA") %>%
  filter(!is.na(region)) %>%
  mutate(
    remaining_balance = 
      ifelse(remaining_balance > quota_volume, 0, remaining_balance)
  ) %>% # need to convert tonnes to KG (& chnage unit type)
  mutate(
    tonne_flag = str_detect(quota_unit, "Tonne")
  ) %>%
  mutate(
    quota_unit = 
      ifelse(tonne_flag == TRUE, "Kilogram (kg)",quota_unit)
  ) %>%
  mutate(
    across(c(quota_volume:remaining_balance), ~ ifelse(tonne_flag == TRUE, .x*1000, .x))) 

# license quotas
license_quotas <- 
  read_excel("..\\data\\uk_license_trq_input.xlsx") %>%
  clean_names() %>%
  filter(rsp_region != "NA") %>%
  left_join(country_input, by = c("quota_origin" = "country_name"))

colnames(uk_trq)
colnames(license_quotas)

license_quotas <- license_quotas %>%
  rename(
    quota_product_description = quota_description,
    remaining_balance = x2021_quantity_remaining,
  ) %>%
  select(
    quota_number,
    quota_origin,
    quota_product_description,
    year,
    quota_volume = x2021_quantity_available,
    remaining_balance = remaining_balance,
    commodity_codes,
    trq_type,
    quota_unit,
    grouping,
    region
  )

uk_trq <- bind_rows(uk_trq, license_quotas)

# convert hectolitres to litre (for EU comparison)

uk_trq <- uk_trq %>%
  mutate(hl_flag = ifelse(quota_unit == "Hectolitre (hl)", 1,0)) %>%
  mutate(
    quota_volume = 
      ifelse(
        hl_flag == 1, quota_volume / 100, quota_volume),
    remaining_balance =
      ifelse(
        hl_flag == 1, remaining_balance / 100, remaining_balance),
    quota_unit = 
      ifelse(
        hl_flag == 1, "Litre (l)", quota_unit) 
  ) 

# EU TRQ data:

country_input_eu <- read_excel("..\\data\\eu_trq_data_input.xlsx") %>% clean_names()

eu_trq <- 
  read_excel("..\\data\\eu_trq_data.xlsx") %>%
  clean_names() %>%
  filter(year == "2020") %>%
  left_join(country_input_eu, by = "origin") %>%
  filter(region != "NA")

eu_trq <- rename(eu_trq, quota_number = "order_number")
# clean cn8 codes:
eu_trq <- eu_trq %>% 
  mutate(cn8_codes = str_remove_all(cn8_codes,"'")) %>%
  mutate(cn8_codes = str_squish(cn8_codes))

#' Quota unit input -
#' input contains UK and EU quota units
#' and the quota unit to be changed to for final outputs.

quota_input <- read_excel("..\\data\\trq_quota_unit.xlsx") %>% clean_names()


#'
#'  match both UK and EU data and change quota unit
#'  so there is a consistent quota unit to compare 
#'  UK and EU data against each other
#'  


uk_trq <- 
  uk_trq %>% 
  left_join(
    quota_input[,c("uk_quota_unit","quota_unit_final")], 
    by = c("quota_unit" = "uk_quota_unit")
  )


eu_trq <-
  eu_trq %>%
  left_join(
    quota_input[,c("eu_quota_unit","quota_unit_final")], 
    by = c("unit" = "eu_quota_unit")
  )
  

## 1. UK TRQ data ----

#'
#'  Compile different level data for RSPs: 
#'  country, grouping (i.e. Andean) and total region.
#'  loop through and filter UK TRQs for each grouping type for each region 
#'  (i.e. 3 x 5 iterations)
#'  Output each groupings total volume/usage/allocation fill rate. 
#'  
#'  

uk_list <- list() # create list for function

# filter for KG only:
uk_trq_data <- uk_aggFunction(uk_trq) # apply aggregation function to cleaned/combined UK quota data. 

### 1i. Bind UK data --------

## bind together group datasets. 
trqs_region_uk <- bind_rows(uk_trq_data[grepl("region", names(uk_trq_data))])
country_uk    <- bind_rows(uk_trq_data[grepl("quota_origin", names(uk_trq_data))])
grouping_uk    <- bind_rows(uk_trq_data[grepl("grouping", names(uk_trq_data))])


# compile with datasets full region/grouping list. 
trqs_country_uk <- 
  country_uk %>% 
  left_join(country_input, by = c("quota_origin" = "country_name")) %>% 
  relocate(c("grouping","region"), .after = quota_origin) %>%
  arrange(region)

trqs_grouping_uk <- 
  grouping_uk %>%
  left_join(country_input %>% distinct(grouping, .keep_all = TRUE), by = "grouping") %>%
  relocate("region", .after = grouping) %>%
  arrange(region)


#save(trqs_country, trqs_grouping, trqs_region, file = "..\\data\\uk_trq_data.RData")


## 2. EU TRQ data -----


eu_list <- list() # create list for function
eu_trq_data <- eu_aggFunction(eu_trq) # apply aggregation function to EU trq data



### 2i. Bind EU data ------
trqs_region_eu <- bind_rows(eu_trq_data[grepl("region", names(eu_trq_data))])
country_eu     <- bind_rows(eu_trq_data[grepl("origin", names(eu_trq_data))])
grouping_eu    <- bind_rows(eu_trq_data[grepl("grouping", names(eu_trq_data))])


# compile with datasets full region/grouping list. 
trqs_country_eu <- 
  country_eu %>% 
  left_join(country_input_eu, by = "origin") %>% 
  relocate(c("grouping","region"), .after = origin) %>%
  arrange(region) %>% 
  rename("quota_origin" = origin,
         "iso" = origin_code)

trqs_grouping_eu <- 
  grouping_eu %>%
  left_join(country_input_eu %>% distinct(grouping, .keep_all = TRUE), by = "grouping") %>%
  relocate("region", .after = grouping) %>%
  arrange(region) %>% 
  rename("quota_origin" = origin,
         "iso" = origin_code)


#save(trqs_country_eu, trqs_grouping_eu, trqs_region_eu, file = "..\\data\\eu_trq_data.RData")


## 3. Combine UK and EU data ----

## need to remove two columns from uk data to bind together for matching df.

trqs_country_uk  <- trqs_country_uk   %>% select(-total_balance_remaining)
trqs_grouping_uk <- trqs_grouping_uk  %>% select(-total_balance_remaining,-country_name)
trqs_region_uk   <- trqs_region_uk   %>% select(-total_balance_remaining)

# create uk/eu flag

trqs_country_uk     <- trqs_country_uk  %>% mutate(source = "UK")
trqs_grouping_uk    <- trqs_grouping_uk %>% mutate(source = "UK")
trqs_region_uk      <- trqs_region_uk   %>% mutate(source = "UK")
trqs_country_eu  <- trqs_country_eu %>% mutate(source = "EU")
trqs_grouping_eu <- trqs_grouping_eu %>% mutate(source = "EU") %>% select(-quota_origin)
trqs_region_eu   <- trqs_region_eu   %>% mutate(source = "EU")


trqs_country  <- rbind(trqs_country_uk,trqs_country_eu) %>% 
  relocate(c("region","grouping"), .before = quota_origin) %>%
  relocate(source, .after = quota_origin) %>% 
  arrange(region,grouping,quota_origin, quota_unit_final)

trqs_grouping <- rbind(trqs_grouping_uk, trqs_grouping_eu) %>% 
  relocate(region, .before = grouping) %>%
  relocate(source, .after = grouping) %>% 
  arrange(region,grouping,quota_unit_final)

trqs_region   <- rbind(trqs_region_uk,trqs_region_eu) %>% 
  relocate(source, .after = region) %>% 
  arrange(region, quota_unit_final)

#save(trqs_country, trqs_grouping, trqs_region, file = "..\\data\\trq_data6.RData")


## 4. Excel output -----

wb3 <- createWorkbook()

addWorksheet(wb3, sheetName = "country_level")
addWorksheet(wb3, sheetName = "grouping_level")
addWorksheet(wb3, sheetName = "region_level")

writeData(wb3, sheet = "country_level", trqs_country)
writeData(wb3, sheet = "grouping_level", trqs_grouping)
writeData(wb3, sheet = "region_level", trqs_region)

headerStyle <- createStyle(textDecoration = "bold")

addStyle(wb3, sheet = "country_level", headerStyle, rows = 1,cols = 1:ncol(trqs_country))
addStyle(wb3, sheet = "grouping_level", headerStyle, rows = 1,cols = 1:ncol(trqs_grouping))
addStyle(wb3, sheet = "region_level", headerStyle, rows = 1,cols = 1:ncol(trqs_region))

setColWidths(wb3, "country_level", cols = c(1), widths =  25)
setColWidths(wb3, "country_level", cols = c(2:ncol(trqs_country)), widths =  15)
setColWidths(wb3, "grouping_level", cols = c(1), widths =  25)
setColWidths(wb3, "grouping_level", cols = c(2:ncol(trqs_grouping)), widths =  15)
setColWidths(wb3, "region_level", cols = c(1), widths =  25)
setColWidths(wb3, "region_level", cols = c(2:ncol(trqs_region)), widths =  15)

saveWorkbook(wb3, file = "..\\outputs\\trq_data_output7.xlsx", overwrite = T)

## 5. TRQ filled quotas --------

uk_trq4 <- uk_trq %>% 
  mutate(
    no_days = 
      as.numeric(difftime(validity_end_date, validity_start_date, units = c("days"))),
    last_day = 
      as.numeric(difftime(last_allocation_date,validity_start_date, units = c("days"))))


uk_trqs_filled <- uk_trq4 %>%
  filter(quota_fill_rate >= 0.999) %>%
  filter(!is.na(last_allocation_date)) %>%
  group_by(quota_origin) %>%
    summarise(
      quota_count = length(quota_origin),
      total_quota_volume = sum(quota_volume),
      avg_quota_length = mean(no_days),
      avg_quota_fill   = mean(last_day, na.rm = T)
    ) %>%
  pivot_longer(cols = c(avg_quota_length:avg_quota_fill), 
               names_to = "quota_length_cat",
               values_to ="quota_length_val") %>%
  left_join(country_input, by = c("quota_origin" = "country_name")) %>%
  relocate(c("grouping","region"), .after = quota_origin) %>% 
  select(-iso) %>%
  arrange(region)


# save data 

write.xlsx(uk_trqs_filled, "..\\outputs\\uk_trqs_filled7.xlsx", overwrite = T)

## 6. Save Rdata (all dfs) ----

#save(trqs_country, trqs_grouping, trqs_region, uk_trqs_filled, file = "..\\data\\trq_data6.RData")

# TRQ SECTOR FILL RATES------------------------------------------------------------
# Categorize commodity code descriptions into simpler categories, i.e. Fish, Vegetables, poultry etc. 
#'
#' Issue is quotas have multiple codes, which have multiple HS chapters i.e. 
#' Good categorization (Meat, preserved meat, dairy etc.)
#' This makes it difficult to summarize a quota description simply. 
#' Need to identify quotas which have single HS chapters (02, 04 - i.e. first 20 numbers of good code)
#' Then to identify those with multiple and identify where a HS chapter dominates. 
#' If a goods chapter dominates (i.e. say > 50% of all goods code under the quota)
#' This makes categorizing the quota simply more straight forward. 
#'
#'
#'
###


list <- list() # list for duplicate quotas. 
## 1. Extract sector data -------

# Apply sector function to UK and EU data:

sector_list_uk <- sectorFunction(.df = uk_trq,
                          .colname = "commodity_codes", 
                          .delim = ";")

sector_list_eu <- sectorFunction(.df = eu_trq,
                          .colname = "cn8_codes", 
                          .delim = ",")

# Extract dataframes:

uk_trq_sector <- sector_list_uk[["df_final_uk_trq"]]
eu_trq_sector <- sector_list_eu[["df_final_eu_trq"]]


# match in manual TRQ dsc. input file to fill in blanks function couldn't populate due to data gaps. 
eu_trq_dsc <- read_excel("..\\data\\eu_trq_sector_manual_input.xlsx") %>% clean_names %>% distinct(., quota_number, .keep_all = TRUE)

eu_trq_sector <- eu_trq_sector %>% 
  left_join(
    eu_trq_dsc[,c("quota_number", "manual_trq_dsc")], 
    by = c("quota_number")
  ) %>%
  mutate(trq_dsc = ifelse(is.na(trq_dsc), manual_trq_dsc, trq_dsc))


## 2. Aggregate UK/EU sector data ----------------------

#'
#' Aggregate TRQ data by country/region grouping breaking down sector level fill rates. 
#' Apply aggregation functions
#'

### 2.1 UK data -----------

sectors_uk <- uk_aggFunction(uk_trq_sector, .grouping = "trq_dsc")

sectors_region_uk   <- bind_rows(sectors_uk[grepl("region", names(sectors_uk))]) %>% mutate(source = "UK")
sectors_country_uk  <- bind_rows(sectors_uk[grepl("origin", names(sectors_uk))]) %>% mutate(source = "UK")
sectors_grouping_uk <- bind_rows(sectors_uk[grepl("grouping", names(sectors_uk))]) %>% mutate(source = "UK")


# compile with datasets full region/grouping list. 
sectors_country_uk <- 
  sectors_country_uk %>% 
  left_join(country_input, by = c("quota_origin" = "country_name")) %>% 
  relocate(c("grouping","region"), .after = quota_origin) %>%
  arrange(region)
         

sectors_grouping_uk <- 
  sectors_grouping_uk %>%
  left_join(country_input %>% distinct(grouping, .keep_all = TRUE), by = "grouping") %>%
  relocate("region", .after = grouping) %>%
  arrange(region)


### 2.2 EU data -----------


sectors_eu <- eu_aggFunction(eu_trq_sector, .grouping = "trq_dsc")

sectors_region_eu   <- bind_rows(sectors_eu[grepl("region", names(sectors_eu))]) %>% mutate(source = "EU")
sectors_country_eu  <- bind_rows(sectors_eu[grepl("origin", names(sectors_eu))]) %>% mutate(source = "EU")
sectors_grouping_eu <- bind_rows(sectors_eu[grepl("grouping", names(sectors_eu))]) %>% mutate(source = "EU")


# compile with datasets full region/grouping list. 
sectors_country_eu <- 
  sectors_country_eu %>% 
  left_join(country_input_eu, by = "origin") %>% 
  relocate(c("grouping","region"), .after = origin) %>%
  arrange(region) %>% 
  rename("quota_origin" = origin,
         "iso" = origin_code)

sectors_grouping_eu <- 
  sectors_grouping_eu %>%
  left_join(country_input_eu %>% distinct(grouping, .keep_all = TRUE), by = "grouping") %>%
  relocate("region", .after = grouping) %>%
  arrange(region) %>% 
  rename("iso" = origin_code) %>%
  select(-origin)
  


## 3. Combine UK/EU data ---------

# remove remaining balance columns from UK data to bind together with EU data:

sectors_region_uk   <- select(sectors_region_uk, - total_balance_remaining)
sectors_grouping_uk <- select(sectors_grouping_uk, - total_balance_remaining, -country_name)
sectors_country_uk  <- select(sectors_country_uk, - total_balance_remaining)

sectors_region <- 
  rbind(sectors_region_uk, sectors_region_eu) %>%  
  relocate(source, .after = region) %>% 
  arrange(region, trq_dsc, quota_unit_final)


sectors_grouping <- 
  rbind(sectors_grouping_uk, sectors_grouping_eu) %>%
  relocate(region, .before = grouping) %>%
  relocate(source, .after = grouping) %>% 
  arrange(region, grouping, trq_dsc, quota_unit_final)

sectors_country <-
  rbind(sectors_country_uk, sectors_country_eu) %>%
  relocate(c("region","grouping"), .before = quota_origin) %>%
  relocate(source, .after = quota_origin) %>% 
  arrange(region,grouping,quota_origin, trq_dsc, quota_unit_final)
  

## 4. Excel output -------


wb2 <- createWorkbook()

addWorksheet(wb2, sheetName = "country_level")
addWorksheet(wb2, sheetName = "grouping_level")
addWorksheet(wb2, sheetName = "region_level")

writeData(wb2, sheet = "country_level", sectors_country)
writeData(wb2, sheet = "grouping_level", sectors_grouping)
writeData(wb2, sheet = "region_level", sectors_region)

headerStyle <- createStyle(textDecoration = "bold")

addStyle(wb2, sheet = "country_level", headerStyle, rows = 1,cols = 1:ncol(sectors_country))
addStyle(wb2, sheet = "grouping_level", headerStyle, rows = 1,cols = 1:ncol(sectors_grouping))
addStyle(wb2, sheet = "region_level", headerStyle, rows = 1,cols = 1:ncol(sectors_region))

setColWidths(wb2, "country_level", cols = c(1), widths =  25)
setColWidths(wb2, "country_level", cols = c(2:ncol(sectors_country)), widths =  15)
setColWidths(wb2, "grouping_level", cols = c(1), widths =  25)
setColWidths(wb2, "grouping_level", cols = c(2:ncol(sectors_grouping)), widths =  15)
setColWidths(wb2, "region_level", cols = c(1), widths =  25)
setColWidths(wb2, "region_level", cols = c(2:ncol(sectors_region)), widths =  15)

saveWorkbook(wb2, file = "..\\outputs\\trq_sector_data_output7.xlsx", overwrite = T)

## 5. Sector filled quotas --------------

uk_trq_sector2 <- uk_trq_sector %>% 
  mutate(
    no_days = 
      as.numeric(difftime(validity_end_date, validity_start_date, units = c("days"))),
    last_day = 
      as.numeric(difftime(last_allocation_date,validity_start_date, units = c("days"))))


uk_trqs_filled_sector <- uk_trq_sector2 %>%
  filter(quota_fill_rate >= 0.999) %>%
  filter(!is.na(last_allocation_date)) %>%
  group_by(quota_origin, trq_dsc) %>%
  summarise(
    quota_count = length(quota_origin),
    total_quota_volume = sum(quota_volume),
    avg_quota_length = mean(no_days),
    avg_quota_fill   = mean(last_day, na.rm = T)
  ) %>%
  pivot_longer(cols = c(avg_quota_length:avg_quota_fill), 
               names_to = "quota_length_cat",
               values_to ="quota_length_val") %>%
  left_join(country_input, by = c("quota_origin" = "country_name")) %>%
  relocate(c("grouping","region"), .after = quota_origin) %>% 
  select(-iso) %>%
  arrange(region)

write.xlsx(uk_trqs_filled_sector, "..\\outputs\\uk_trqs_filled_sector7.xlsx", overwrite = T)

## 6. Save Rdata (all dfs) -------------

save(trqs_country, trqs_grouping, trqs_region, uk_trqs_filled, 
     sectors_country, sectors_grouping, sectors_region, uk_trqs_filled_sector,
     file = "..\\data\\trq_data_all7.RData")

### 6.1. Full UK TRQ data excel -----

uk_trq_sector3 <- uk_trq_sector2 %>% 
  select(-tonne_flag, - hl_flag) %>%
  relocate(c("grouping","region"), .after = quota_origin)

write.xlsx(uk_trq_sector3,"..\\outputs\\uk_trq_data_full.xlsx", overwrite = TRUE)


### 6.2 Full EU TRQ data excel ----


eu_trq_sector2 <- eu_trq_sector %>% 
  select(-manual_trq_dsc) %>%
  relocate(c("grouping","region"), .after = origin)


write.xlsx(eu_trq_sector2,"..\\outputs\\eu_trq_data_full.xlsx", overwrite = TRUE)



