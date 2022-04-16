# web scrape UK licnese data

# notes ---------------------------------------------------------------

# version control:
#' - 07/04/2022 - updated script name to uk_trqs_web_scrape to to finish first iteration of completed code. 


#' Description --------------------------------------
#' 
#' 
#' 
#' 
#' 
#' 
#' 

#' Methodology ---------------------------------------
#' 
#' 
#' 
#' 
#' 
#' 
#' 


# set up --------------------------------------------


rm(list = ls()) # remove items from global environment

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
options(scipen=999)


#library(RSelenium)
library(tidyverse)
library(rvest)
library(janitor)
library(tidyverse)
library(readxl)
library(readr)
library(janitor)
library(openxlsx)
library(stringr)
library(shiny)
library(lubridate)
library(rlang)

`%notin%` <- Negate(`%in%`)

metadata     <- read_excel("..\\data\\licensed_quotas.xlsx") %>% clean_names()
metadata_fta <- read_excel("..\\data\\licensed_fta_quotas.xlsx") %>% clean_names()


# quota year/month variables to determine quota year:
cross_year1 <- c("july", "aug", "sep", "oct", "nov", "dec")
cross_year2 <- c("oct","nov","dec")
cross_year3 <- c("june","july", "aug", "sep", "oct", "nov", "dec")


# urls <- c("https://www.gov.uk/guidance/uk-tariff-rate-quotas",
#           "https://www.gov.uk/guidance/uk-tariff-rate-quotas-2022")



uk_trqsWebScrape <- function(url,yr1,yr2){

  print(url)
  print(yr1)
  print(yr2)
  
# scrape data tables ------------

scrape_data <- 
  read_html(url) %>%
  html_nodes("table") %>%
  html_table() 


## alternative way to scrape only quota numbers from table heading:
# scrape_data2 <-
#   read_html(url) %>%
#   html_nodes('thead > tr > th') %>%
#   html_text()


#' each data table scrapped from the web-link is structured the same
#' They contain the quota information/sub-periods in the first section
#' In the second section the quota usage figures are contained
#' It is simple to iterate through each table and 
#' use basic data frame transformations to extract the data required
#' The following is a step by step process or algorithm to scrape
#' all required data
#'        

# empty lists to insert scrapped data
quota_list <- list()
usage_list <- list()

## loop through scraped data tables -------------------------------------
for(i in 1:length(scrape_data)){

  
#' Some data tables contian no rows which breaks the loops
#' Error check to skip any iterations where no data
#' insert algorithm within tryCatch function
  
tryCatch({ 
    
     # loop through every data table from scrape:
     df <- scrape_data[[i]]

     # create column with row numbers 1:n
     df <- df %>% mutate(n = 1:n())

     # row number to filter usage data for:
     filterRow <- df %>% filter(X1 == "Period") %>% select(n) %>% pull()

      # filter quota information and bind together into single dataframe:

     df_quota <- as.data.frame(df[1,2])
     df_year <- as.data.frame(df[2,2])
     df_quant <- as.data.frame(df[3,2])

     df2 <- cbind(df_quota,df_year,df_quant)
        
     colnames(df2) <- c("quota_number", "quota_period", "quota_volume")

     name <- paste0("df",i) 
     
     quota_list[[name]] <- df2

    # filter data frame for usage data:

      df3 <- 
       df %>% 
        filter(n >= filterRow) %>%
        select(-n) %>%
        row_to_names(row_number = 1) # converts first row to column headings

      df4 <- cbind(df2, df3) 

      # create name and insert df into list before moving on to next iteraiton within loop
      name <- paste0("df_usage",i) 
      usage_list[[name]] <- df4
      
      
  } , error = function(e) { skip_to_next <<- TRUE})

}


quota_data <- bind_rows(quota_list) # quota number df
usage_data <- bind_rows(usage_list) # usage df 



# remove commas and convert to numeric for usage figures to aggregate:

quota_data <- quota_data %>% 
  clean_names() %>%
  mutate_at(c("quota_number"), ~ str_remove_all(., ","))

# identify duplicate rows:
dups <- usage_data %>% get_dupes()

### clean & separate data -------------------------

#' separate out quota period
#' cross-year quotas need identifying 
#' so usage data can correctly be apportioned 
#' to the correct quota year
#' quota period needs separating 
#' then logic is applied for quota periods 
#' to determine correct quota year 
#' depending on the period given 
#' in the web-link data tables
#' 

usage_data2 <- 
  usage_data %>% 
  distinct() %>%
  clean_names() %>%
  separate(
    period, 
    c("start_month", "end_month"), 
    "-", 
    remove = "FALSE"
    ) %>%
    mutate(
      end_month = 
        ifelse(
          is.na(end_month), 
          start_month, 
          end_month
          )
      ) %>%
    mutate(start_month = str_trim(start_month)) %>% # remove white space
    mutate(quota_number = str_remove(quota_number,"[.]")) %>% # needs square brackets - otherwise dot isn't removed
    mutate(across(c("start_month","end_month"), ~ str_squish(.x))) %>% # remove white space
    mutate(across(c("start_month","end_month"), ~  tolower(.x))) %>% # lower case text
    mutate(across(c(contains("quantity"),quota_volume), ~ str_remove_all(.x, ","))) %>% # remove non numerical strings:
    mutate(across(c(contains("quantity"),quota_volume), ~ str_remove(.x,"kgs")))
  


### quota year logic -------------------------------------------------------

usage_data3 <- 
  usage_data2 %>%
   mutate(
     quota_year = 
      ifelse(
        start_month == "",
        yr2,
       ifelse(
         quota_period == "1 Jan - 31 Dec", 
         yr2, 
          ifelse(
            quota_period == "1 Oct - 30 Sep" & start_month %in% cross_year2, 
            yr2,
            ifelse(
              quota_period == "1 July - 30 June" & start_month %in% cross_year1, 
              yr2,
              ifelse(
                quota_period == "1 June - 31 May" & start_month %in% cross_year3, 
                yr2, 
                yr1
                )
              )
            )
         )
       )
      ) %>%
  relocate(quota_year, .after = "end_month")
               


# convert to values and replace NAs - to aggregated 
usage_data4 <- 
  usage_data3 %>%
    mutate(
      across(
        c(contains("quantity"),quota_volume), ~ str_remove_all(.x, ","))
      ) %>%
    mutate(quota_volume = str_remove(quota_volume,"kgs")) %>%
    mutate(across(c(quota_volume, contains("quantity")), ~ as.numeric(.x))) %>%
    replace(is.na(.),0)
                  
  

### summarise quantity issued -------------------------------
usage_data5 <- 
  usage_data4 %>%
  group_by(quota_number, quota_year, quota_volume, quota_period) %>%
  summarise(
    quantity_issued  = sum(quantity_issued_kgs),
    .groups = 'drop'
    )


usage_data5 <-
  usage_data5 %>%
  mutate(
    quantity_remaining = quota_volume - quantity_issued,
    fill_rate = quantity_issued / quota_volume
  )
  


# combine metadata ------------------------------------------------------------
# upload license quotas metadata to join usage data on:


# match in WTO licnese quota metadata:
license_quota_data <- 
  usage_data5 %>% 
  left_join(metadata
                    [,c("order_number",
                        "commodity_code",
                        "annex_2_description",
                        "origin",
                        "units",
                        "quota_rate",
                        "iso",
                        "country_exclude",
                        "trq_type")], by = c("quota_number" = "order_number")) %>%
  arrange(quota_number)

# match in FTA license quota data:

license_quota_data <-
  license_quota_data %>%
  left_join(metadata_fta, by = c("quota_number"="fta_quota_number"))

# where fields are blank - use FTA quota data:

license_quota_data2 <-
  license_quota_data %>%
  mutate(
    commodity_code = ifelse(is.na(commodity_code),fta_commodity_code,commodity_code),
    description    = ifelse(is.na(annex_2_description), quota_description, annex_2_description),
    origin         = ifelse(is.na(origin), fta_quota_origin, origin),
    units          = ifelse(is.na(units), fta_quota_units, units),
    quota_rate     = ifelse(is.na(quota_rate), fta_quota_duty_rate, quota_rate),
    iso            = ifelse(is.na(iso), fta_iso, iso),
    trq_type       = ifelse(is.na(trq_type), fta_trq_type, trq_type)
    )
         
# select final data --------------------------------------------

license_quota_data3 <-
  license_quota_data2 %>%
  select(
    quota_number,  
    description,
    quota_year,
    origin,
    quota_period,
    quota_volume,
    quantity_issued,
    quantity_remaining,
    fill_rate,
    units,
    commodity_code,
    quota_rate,
    trq_type,
    iso,
    country_exclude,
         )


# format in quota tariff:
format_tariff_rates <- function(data, rate_column) {
  
  # Detects where precision error has occurred 
  # Transforms to numeric for correct rounding/signif level
  data <- data %>%
    mutate(
      !!as.symbol(rate_column) := case_when(
        
        grepl("E-3",!!as.symbol(rate_column)) ~
          suppressWarnings(formatC(
            as.numeric(gsub("E-.", "", !!as.symbol(rate_column))) / 1000,
            digits = 4, format = "f"
          )),
        
        grepl("E-",!!as.symbol(rate_column)) ~
          suppressWarnings(formatC(
            as.numeric(gsub("E-.", "", !!as.symbol(rate_column))) / 100,
            digits = 4, format = "f"
          )),
        grepl("00000000|99999999",!!as.symbol(rate_column)) &
          !grepl("E-",!!as.symbol(rate_column)) ~
          suppressWarnings(formatC(
            as.numeric(gsub("E-", "", !!as.symbol(rate_column))),
            digits = 4, format = "f"
          )),
        TRUE ~ !!as.symbol(rate_column)
      )
    )
  return(data)
}


license_quota_data3 <- format_tariff_rates(license_quota_data3, "quota_rate")

df_list[["usage_data"]] <- license_quota_data3
df_list[["quota_data"]] <- quota_data

return(df_list)

} # end function


### combine years ---------------

df_list <- list() 
uk1 <- uk_trqsWebScrape("https://www.gov.uk/guidance/uk-tariff-rate-quotas",2020,2021)
uk2 <- uk_trqsWebScrape("https://www.gov.uk/guidance/uk-tariff-rate-quotas-2022",2021,2022)

usage_2021 <- uk1[["usage_data"]]
usage_2022 <- uk2[["usage_data"]]
quota_list_2021 <- uk1[["quota_data"]]
quota_list_2022 <- uk2[["quota_data"]]


### manual adjustments ------------------------------------------------

#' There are several issues in the HTML tables 
#' missing columns, commas instead of bullet points etc. 
#' All HTML tables have been audited and manual corrections identified
#' which have been summarised in an input file:
#' 

usage_2021 <-
  usage_2021 %>%
  mutate(
    quantity_issued =
      ifelse(
        quota_number == "054129",
        700000,
      ifelse(
        quota_number == "054181",
        11600.2,
      ifelse(
        quota_number == "054212",
        8948987,
        quantity_issued
        )
      )
    )
  ) %>%
  mutate(
    fill_rate =
      ifelse(
        quota_number == "054129",
        0.876095119,
      ifelse(
        quota_number == "054181",
        0.025272767,
      ifelse(
        quota_number == "054212",
        0.840912141,
        fill_rate
          )
        )
      )
    )
  
# 2022:

usage_2022 <-
  usage_2022 %>%
  mutate(
    quantity_issued =
      ifelse(
        quota_number == "054181",
        2796.37,
        quantity_issued
        )
    ) %>%
  mutate(
    fill_rate =
      ifelse(
        quota_number == "054181",
        0.006092309,
        fill_rate
          )
      )
    
# removal of redundant quota which causes duplication with 2021 data:
usage_2022 <- usage_2022 %>% filter(quota_number != "054263")

# manual adjustment for 054002 quota volume (which is incorrect on 2021 website)

usage_2021 <- 
  usage_2021%>%
  mutate(quota_volume = ifelse(quota_number == "054002", 1000000,quota_volume))
         
        
uk <- rbind(usage_2021,usage_2022)

# aggregate cross-year quotas into single year:

uk <- uk %>%
  group_by(
    quota_number, 
    description,
    quota_year,
    origin,
    quota_period,
    quota_volume,
    units,
    commodity_code,
    quota_rate,
    iso,
    country_exclude,
    trq_type
    ) %>%
  summarise(quantity_issued = sum(quantity_issued)) %>%
  mutate(quantity_remaining = quota_volume - quantity_issued) %>%
  mutate(fill_rate = quantity_issued / quota_volume) %>%
  relocate(c(quantity_issued:fill_rate), .after = quota_volume)




### final output -------------------------------------------------------

uk2 <-
  uk %>%
  select(
    `Quota number` = quota_number, 
    `Quota description` = description,
    `Quota year` = quota_year,
    `Quota origin` = origin,
    `Quota period` = quota_period,
    `Quota volume` = quota_volume,
    `Quantity issued` = quantity_issued,
    `Quantity remaining` = quantity_remaining,
    `Quantity available fill rate` = fill_rate,
    `Quota unit` = units,
    `Commodity codes` = commodity_code,
    `In quota rate` = quota_rate,
    `Country ISO` = iso,
    `Country exclude` = country_exclude,
    `Quota type` = trq_type
    )  


# save excel -----------------------------------------------------------------------------

wb <- createWorkbook()

addWorksheet(wb, sheetName = "2021-quota-list")
addWorksheet(wb, sheetName = "2021-web-scrape")
addWorksheet(wb, sheetName = "2022-quota-list")
addWorksheet(wb, sheetName = "2022-web-scrape")
addWorksheet(wb, sheetName = "combined-data")


writeData(wb, sheet = "2021-quota-list", quota_list_2021)
writeData(wb, sheet = "2022-quota-list", quota_list_2022)
writeData(wb, sheet = "2021-web-scrape", usage_2021)
writeData(wb, sheet = "2022-web-scrape", usage_2022)
writeData(wb, sheet = "combined-data", uk2)

saveWorkbook(wb, file = "..\\output\\uk_trqs_web_scrape_output1.xlsx", overwrite = TRUE)


# end