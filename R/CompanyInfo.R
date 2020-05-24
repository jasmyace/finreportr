#' Acquire basic company information.
#' 
#' Extracts and displays basic information relating to a given company in a data frame.
#' 
#' @export
#' @import dplyr
#' @param symbol A character vector specifying the stock symbol of the company of interest.
#' @examples
#' CompanyInfo("GOOG")
#' CompanyInfo("TSLA")

CompanyInfo <- function(symbol) {
        
  # symbol <- "AAPL"

  url <- paste0("https://www.sec.gov/cgi-bin/browse-edgar?CIK=", symbol, "&owner=exclude&action=getcompany&Find=Search")
  search.result <- xml2::read_html(url)
     
  # Acquire company name string
  company.name.raw <- ExtractInfo(search.result, ".companyName") %>% 
    strsplit(" CIK")
     
  # Error message for function
  if(length(company.name.raw) == 0) {
    stop("invalid company symbol")
   }
     
  # Parse company name string
  company.name <- company.name.raw[[1]][1]
     
  # Acquire CIK number
  CIK.raw <- ExtractInfo(search.result, ".companyName a") %>% 
    strsplit(" ")
     
  CIK <- CIK.raw[[1]][1]
     
  # Acquire SIC code
  SIC <- ExtractInfo(search.result, ".identInfo acronym+ a")
     
  # Acquire street address
  street.address <- ExtractInfo(search.result, ".mailer:nth-child(1) .mailerAddress:nth-child(1)")
     
  # Acquire city, state, ZIP
  city.state.raw <- ExtractInfo(search.result, ".mailer:nth-child(1) .mailerAddress+ .mailerAddress")
  city.state <- sub("\\s+$", "", city.state.raw)
  city.state <- gsub("\n", "", city.state)
     
  # Fix problems associated with multiple street address lines
  if(length(city.state) == 2){
    street.address <- paste(street.address, city.state[1])
    city.state <- city.state[2]
  }
     
  # Acquire fiscal year end
  company.details <- ExtractInfo(search.result, ".identInfo")
     
  fiscal.year.end <- gsub("^.*Fiscal Year End: ", "", company.details) %>%
    substr(1,4)
     
  if(fiscal.year.end == "SIC:"){fiscal.year.end <- NA}      ## Fix in case no fiscal year displayed
     
  # Acquire state location
  state <- gsub("^.*State location: ", "", company.details) %>%
    substr(1,2)
     
  # Acquire state of incorporation
  state.inc <- gsub("^.*State of Inc.: ", "", company.details) %>%
    substr(1,2)
     
  if(state.inc == "SI"){state.inc <- NA}       ## Fix in case no incorporation year displayed
     
  # Create dataframe
  info.df <- tibble::tibble(company = company.name, CIK = CIK, SIC = SIC, 
                            state = state, state.inc = state.inc, 
                            FY.end = fiscal.year.end, 
                            street.address = street.address, city.state = city.state)
  return(info.df)
}