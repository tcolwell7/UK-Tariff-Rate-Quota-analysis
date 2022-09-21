# Sources summary table

sources_summary_kable <- function() {
  # define path names (just copied from global.r sources, could make more uniform?)
  
  
  # p1 <- "app/data/processed/covid_daily.csv" # covid daily
  # p2 <- "app/data/processed/goods_monthly.csv" # goods monthly data
  # p3 <- "app/data/processed/total_trade_ONS.csv" # for total trade summaries
  # p4 <- "app/data/processed/2020-05-05_blavatnik_data.csv" # oxford data
  # p5 <- "app/data/processed/trade_policy_template.csv" #csv from trade restrictions
  #
  
  Data <- c("UK quota balances", 
            "UK Non-license-quotas",
            "UK license-quotas",
            "Quota reference documents",
            "Safeguard quotas")
  
  Source <-  c("UK-GOV: HMRC trade-tariff-service",
               "UK-GOV: Department for International Trade UK-quota-data",
               "UK-GOV: Rural Payments Agency License-quota-data",
               "UK gov.",
               "UK gov." )
  
  source_url <- c('https://www.trade-tariff.service.gov.uk/quota_search',
                  'https://data.gov.uk/dataset/4a478c7e-16c7-4c28-ab9b-967bb79342e9/uk-trade-quotas',
                  'https://www.gov.uk/government/collections/uk-tariff-rate-quotas-allocation-of-co-efficients',
                  'https://www.gov.uk/government/publications/reference-documents-for-the-customs-tariff-quotas-eu-exit-regulations-2020',
                  'https://www.gov.uk/government/publications/trade-remedies-notices-tariff-rate-quotas-on-steel-goods/trade-remedies-notice-2021-no-1-safeguard-measure-tariff-rate-quota-on-steel-goods-web-version#tariff-rate-quota-on-steel-goods')
  
  Link <- c(
    
    paste0("<a href='",source_url[1], "' target = '_blank' >Trade-tariff-service</a>"),
    paste0("<a href='",source_url[2], "' target = '_blank' >DIT data source page</a>"),
    paste0("<a href='",source_url[3], "' target = '_blank' >UK-quota webpage</a>"),
    paste0("<a href='",source_url[4], "' target = '_blank' >UK-gov publication</a>"),
    paste0("<a href='",source_url[4], "' target = '_blank' >UK-gov publication</a>"))
  
 
  Notes = c(
    #UK quota balances
    "Quota balances are updated daily on the trade-tariff-service. DIT scrpae this data using the public API and produce the public dataset in source 2. 
          ",
    #UK .gov quota data
    "Department for International trade public dataset for tariff quotas. <strong>Note:</strong> some notes refer to combined data contained within dashboard. 
            <ul>
            <li> UK Tariff Rate Quota data shows historical and live quota data for 2021 onwards following EU-exit. Equivalent historical UK quota data prior to 2021 is not available.  </li>
            <li> Data is presented at the quota level. Individual country usage of Erga Omnes quotas is not available. </li>
            <li> Quota volumes and balances are providing in the quota unit of measurement </li>
            <li> Quota status indicates the status of whether quotas are closed or live. If a quota is `exhausted` this shows that the quota was filled before the quota period ended. Quotas which have not yet opened have their quota status denoted as Future. </li> 
            <li> The last allocation date is the last recorded date a first come first serve quota was used. This data is not available for licenced quotas </li>
            <li> TRQ type indicates whether the quota is part of an FTA, under WTO terms, an Autonomous Tariff Quota (ATQ) or safeguards. </li>
            <li> Data type distinguishes licensed agricultural quotas vs non-license first come first serve quotas. </li></ul>"
    ,
    # Licnese quota data
    "<ul> Note license quota data is adminstired by the rural Payments Agency and is updated periodacally. 
            <li> License quota data is based no licenses granted to traders to trade under a specific quota.  </li>
            <li> Licenses which are granted or issued may not be utilised in practice. However, the amount of licenses granted is used to approximate each quotas utilisation.  </li>
            <li> Note that 2021 and 2022 data are provided in separate web-links. Cross-year quotas combine each year's data to create the full quota year. </li>
            <li> Data has been web-scrapped and compiled together for use within this dashboard. Please see the web-scrape Github repository for this code. </li>
            </ul> ",
    #BSG
    "Reference document containing tariff-quota metadata. See web-link for more details",
    #new Row
    "Publication of safeguard tariff-quotas. See web-link for more details."
  )
  
  sources_summary <-  tibble(
    Data = Data,
    Source = Source,
    Further_Details = Link, # no hyperlink for this team, so this column not uniform
    Notes = Notes
  )
  
  
  
  kable(sources_summary,
        escape = FALSE,
        col.names = c("Data",
                      "Source",
                      "Further info",
                      "Notes")) %>% 
    kable_styling(bootstrap_options = c("striped", "hover"))
  
}

#sources_summary_kable()
