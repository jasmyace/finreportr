# Function to acquire Instance Document URL
GetURL <- function(symbol, year) {
     
 # symbol <- "AAPL"
 # year <- 2016
 
  lower.symbol <- tolower(symbol)
  
  accession.no.raw <- GetAccessionNo(symbol, year, foreign = FALSE)
  accession.no <- gsub("-", "" , accession.no.raw)
  
  CIK <- CompanyInfo(symbol)
  CIK <- as.numeric(CIK$CIK)
  
  report.period <- ReportPeriod(symbol, CIK, accession.no, accession.no.raw)
  report.period <- gsub("-", "" , report.period)
  
  inst.url <- paste0("https://www.sec.gov/Archives/edgar/data/", CIK, "/", 
                     accession.no, "/", lower.symbol, "-", report.period, ".xml")
  return(inst.url)
}


# current 2016 - "https://www.sec.gov/Archives/edgar/data/320193/000162828016020309/aapl-20160924.xml"
# current 2018 - "https://www.sec.gov/Archives/edgar/data/320193/000032019318000145/aapl-20180929.xml"
# forreal - "https://www.sec.gov/Archives/edgar/data/320193/000032019319000119/0000320193-19-000119-index.htm"