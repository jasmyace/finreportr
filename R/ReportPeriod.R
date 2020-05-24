ReportPeriod <- function(symbol, CIK, accession.no, accession.no.raw) {
     
  # symbol <- "AAPL" 
  # CIK <- 
  # accession.no <- 
  # accession.no.raw
     
  url <- paste0("https://www.sec.gov/Archives/edgar/data/", CIK, "/", 
                accession.no, "/", accession.no.raw, "-index.htm")
  search.result <- xml2::read_html(url)
  report.period <- ExtractInfo(search.result, ".formGrouping+ .formGrouping .info:nth-child(2)")
  return(report.period)
}