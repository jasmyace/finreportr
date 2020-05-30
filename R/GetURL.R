# Function to acquire Instance Document URL
GetURL <- function(symbol, year, annual, quarter) {
     
  # symbol <- "AAPL"
  # year <- 2014
  # annual <- TRUE
  # quarter <- TRUE
 
  lower.symbol <- tolower(symbol)
  
  accession.no.raw <- GetAccessionNo(symbol, year, annual, quarter)
  accession.no <- gsub("-", "" , accession.no.raw)
  
  CIK <- CompanyInfo(symbol)
  CIK <- as.numeric(CIK$CIK)
  
  report.period <- ReportPeriod(symbol, CIK, accession.no, accession.no.raw)
  report.period2 <- gsub("-", "" , report.period)
  names(report.period2) <- names(report.period)
  report.period <- report.period2
  
  if(year >= 2019){
    if(annual == TRUE){
      FilingsonEdgark <- edgarWebR::company_filings(x = symbol, type = c("10-K"))
      findCorrectYeark <- which(lubridate::year(as.Date(FilingsonEdgark$accepted_date)) == as.character(year))  # assumes i'll only ever find one.
      DocumentsonEdgark <- dplyr::bind_rows(lapply(FilingsonEdgark$href[findCorrectYeark], edgarWebR::filing_documents))
      inst.urlk <- DocumentsonEdgark[DocumentsonEdgark[5] == 'XML', 4]
      inst.url <- inst.urlk
    } 
    if(quarter == TRUE){
      FilingsonEdgarq <- edgarWebR::company_filings(x = symbol, type = c("10-Q"))
      findCorrectYearq <- which(lubridate::year(as.Date(FilingsonEdgarq$accepted_date)) == as.character(year) | 
                                  lubridate::year(as.Date(FilingsonEdgarq$accepted_date) + 1) == as.character(year + 1) & months(as.Date(FilingsonEdgarq$accepted_date)) %in% c("January", "February"))  # filings could occur in 1-2020, 2-2020, or more?
      DocumentsonEdgarq <- dplyr::bind_rows(lapply(FilingsonEdgarq$href[findCorrectYearq], edgarWebR::filing_documents))
      inst.urlq <- DocumentsonEdgarq[DocumentsonEdgarq[5] == 'XML', 4]
      inst.url <- inst.urlq
    }
    if(annual == TRUE & quarter == TRUE){
      inst.url <- c(inst.urlk, inst.urlq)
    }
    names(inst.url) <- names(report.period)
  } else {
    inst.url <- paste0("https://www.sec.gov/Archives/edgar/data/", CIK, "/", accession.no, "/", lower.symbol, "-", report.period, ".xml")
    names(inst.url) <- names(report.period)
  }
  return(inst.url)
}