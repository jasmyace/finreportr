
source("/Users/Jason/Documents/GitHub/finreportr/R/GetCashFlow.R")
source("/Users/Jason/Documents/GitHub/finreportr/R/GetFinancial.R")
source("/Users/Jason/Documents/GitHub/finreportr/R/AnnualReports.R")
source("/Users/Jason/Documents/GitHub/finreportr/R/ReportPeriod.R")
source("/Users/Jason/Documents/GitHub/finreportr/R/GetAccessionNo.R")
source("/Users/Jason/Documents/GitHub/finreportr/R/CompanyInfo.R")
source("/Users/Jason/Documents/GitHub/finreportr/R/GetBalanceSheet.R")
source("/Users/Jason/Documents/GitHub/finreportr/R/GetIncome.R")
source("/Users/Jason/Documents/GitHub/finreportr/R/getURL.R")
source("/Users/Jason/Documents/GitHub/finreportr/R/utils.R")


library(dplyr)
library(edgarWebR)
library(xml2)
library(httr)
library(rvest)
library(XBRL)
library(curl)

tib <- tibble::tibble(year = seq(2010, 2019, 1)) %>% 
     dplyr::mutate(income = purrr::map(.data$year, ~ GetIncome("AAPL", .x))) 

tib2 <- tib %>% 
     tidyr::unnest(c(income)) %>% 
     dplyr::mutate(startDateD = as.Date(.data$startDate), 
                   endDateD = as.Date(.data$endDate), 
                   diffMonths = lubridate::interval(.data$startDateD, .data$endDateD) %/% months(1),
                   fiscalYear = lubridate::year(.data$endDateD)) %>% 
     dplyr::filter(.data$diffMonths %in% c(11, 12), 
                   .data$year == .data$fiscalYear) %>% 
     dplyr::arrange(.data$Metric, .data$year) %>% 
     dplyr::distinct() %>%
     dplyr::left_join(lu, by=c("Metric")) %>% 
     dplyr::select(-.data$Metric, -.data$startDate, -.data$endDate, .data$NewMetric) %>% 
     # dplyr::filter(.data$NewMetric == "Earnings Per Share, Basic") %>% 
     dplyr::mutate(symbol = "AAPL")

lu <- tibble::tibble(Metric = c("Earnings Per Share Basic", "Earnings Per Share, Basic", "Basic (in US dollars per share)", "Basic earnings per common share"),
                     NewMetric = rep("Earnings Per Share, Basic", 4))




x <- quantmod::getSymbols("AAPL", from='2017-01-01', to="2020-05-30", warnings=FALSE, auto.assign=TRUE)

                   
                   


x <- bind_rows(tib$income) %>% 
     dplyr::arrange(.data$Metric, dplyr::desc(.data$endDate))
                    
ugh14 <- GetIncome("AAPL", 2014)
ugh19 <- GetIncome("AAPL", 2019)

x[x$Metric == "Earnings Per Share, Basic",]