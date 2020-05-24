# Function to acquire Instance Document URL
GetURL <- function(symbol, year) {
     
 # symbol <- "AAPL"
 # year <- 2014
 
  lower.symbol <- tolower(symbol)
  
  accession.no.raw <- GetAccessionNo(symbol, year, foreign = FALSE)
  accession.no <- gsub("-", "" , accession.no.raw)
  
  CIK <- CompanyInfo(symbol)
  CIK <- as.numeric(CIK$CIK)
  
  report.period <- ReportPeriod(symbol, CIK, accession.no, accession.no.raw)
  report.period <- gsub("-", "" , report.period)
  
  if(year >= 2019){
    FilingsonEdgar <- edgarWebR::company_filings(x = symbol, type = "10-K")
    findCorrectYear <- which(lubridate::year(as.Date(FilingsonEdgar$accepted_date)) == as.character(year))  # assumes i'll only ever find one.
    DocumentsonEdgar <-  edgarWebR::filing_documents(x = FilingsonEdgar$href[findCorrectYear])  
    inst.url <- DocumentsonEdgar[DocumentsonEdgar[5] == 'XML', 4]
  } else {
    inst.url <- paste0("https://www.sec.gov/Archives/edgar/data/", CIK, "/", accession.no, "/", lower.symbol, "-", report.period, ".xml")
  }
  return(inst.url)
}