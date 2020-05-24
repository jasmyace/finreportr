
source("/Users/Jason/Documents/GitHub/finreportr/R/GetCashFlow.R")
source("/Users/Jason/Documents/GitHub/finreportr/R/GetFinancial.R")
source("/Users/Jason/Documents/GitHub/finreportr/R/AnnualReports.R")
source("/Users/Jason/Documents/GitHub/finreportr/R/ReportPeriod.R")
source("/Users/Jason/Documents/GitHub/finreportr/R/GetAccessionNo.R")
source("/Users/Jason/Documents/GitHub/finreportr/R/CompanyInfo.R")
source("/Users/Jason/Documents/GitHub/finreportr/R/GetBalanceSheet.R")
source("/Users/Jason/Documents/GitHub/finreportr/R/get_income.R")
source("/Users/Jason/Documents/GitHub/finreportr/R/getURL.R")
source("/Users/Jason/Documents/GitHub/finreportr/R/utils.R")


library(dplyr)
library(xml2)
library(httr)
library(rvest)
library(XBRL)
library(curl)

a <- GetCashFlow("AAPL", 2018)