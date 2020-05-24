#' @export
#'
#' @title Acquire listing of company annual reports.
#'
#' @description Extracts and displays listing of annual reports filed by a
#'   company in a data frame.
#'
#' @param symbol A character vector specifying the stock symbol of the company
#'   of interest.
#'
#' @param foreign A logical vector indicating whether the company is domestic or
#'   foreign. \code{foreign = FALSE} by default.
#'
#' @examples
#' AnnualReports("TSLA")
#' AnnualReports("HTHIY", foreign = TRUE)

AnnualReports <- function(symbol, foreign = FALSE) {

  # symbol <- "AAPL" 
  # foreign <- FALSE
     
  if(foreign == FALSE) {
    url <- paste0("http://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=", 
                symbol, "&type=10-k&dateb=&owner=exclude&count=100")
  } else {
    url <- paste0("http://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=", 
                  symbol, "&type=20-f&dateb=&owner=exclude&count=100")
  }

  filings <- xml2::read_html(url)

  # Acquire filing name
  filing.name <- ExtractInfo(filings, "#seriesDiv td:nth-child(1)")

  # Error message for function
  if(length(filing.name) == 0) {
    stop("invalid company symbol or foreign logical")
  }

  # Acquire filing date
  filing.date <- ExtractInfo(filings, ".small+ td")

  # Acquire accession number
  accession.no.raw <- ExtractInfo(filings, ".small")

  accession.no <- gsub("^.*Acc-no: ", "", accession.no.raw) %>%
    substr(1, 20)

  # Create dataframe
  info.df <- tibble::tibble(filing.name = filing.name, 
                            filing.date = filing.date, 
                            accession.no = accession.no)
  return(info.df)
}