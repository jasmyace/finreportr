GetAccessionNo <- function(symbol, year, annual=TRUE, quarter=TRUE) {
  
  # symbol <- "AAPL" 
  # year <- 2016
  # foreign <- FALSE
     
  year.char <- as.character(year)
  
  reports.df <- Reports(symbol, annual, quarter)
  reports.df <- reports.df %>% 
       dplyr::mutate(filing.year = substr(.data$filing.date, 1, 4)) %>%
       dplyr::filter(.data$filing.year == year.char) %>%
       dplyr::filter(.data$filing.name == "10-K" | .data$filing.name == "20-F" | .data$filing.name == "10-Q")
  
  accession.no.raw <- reports.df %>% 
       dplyr::select(.data$accession.no) %>%
       as.character()
  
  # Error message for function
  if(accession.no.raw == "character(0)") {
       stop("no filings available for given year")
  }
  
  return(accession.no.raw)
}