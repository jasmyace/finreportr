ReportPeriod <- function(symbol, CIK, accession.no, accession.no.raw) {
     
  # symbol <- "AAPL" 
  # CIK <- 
  # accession.no <- 
  # accession.no.raw
     
  url <- paste0("https://www.sec.gov/Archives/edgar/data/", CIK, "/", accession.no, "/", accession.no.raw, "-index.htm")
  search.result <- lapply(url, xml2::read_html)
  report.period <- lapply(search.result, ExtractInfo, ".formGrouping+ .formGrouping .info:nth-child(2)")
  names(report.period) <- names(accession.no.raw)
  return(report.period)
}