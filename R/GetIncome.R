#' @export
#'
#' @title Acquire income statement.
#'
#' @description Extracts and displays income statement from the annual report of
#'   a given company. This functionality is only available for queries of income
#'   statements that belong to domestic companies. Note that all data returned
#'   by this function comes from the company's 10-K, not 10-K/A.
#'
#' @param symbol A character vector specifying the stock symbol of the company
#'   of interest.
#'
#' @param year A numeric vector specifying the year during which the annual
#'   report was filed.
#'   
#' @param annual A logical vector indicating if annual reports should be pulled.
#'   \code{annual = TRUE} by default.
#'
#' @param quarter A logical vector indicating if quarterly reports should be
#'   pulled. \code{quarter = TRUE} by default.
#'   
#' @examples 
#' \dontrun{
#' GetIncome("FB", 2016)
#' }


GetIncome <- function(symbol, year, annual, quarter) {
     
  # symbol <- "AAPL"
  # year <- 2016
  # annual <- TRUE
  # quarter <- TRUE
     
  stopifnot(!(annual == FALSE & quarter == FALSE))
  
  income.descriptions.a <- c("CONSOLIDATED STATEMENTS OF INCOME", 
                           "CONSOLIDATED STATEMENT OF INCOME", 
                           "CONSOLIDATED STATEMENTS OF OPERATIONS", 
                           "CONSOLIDATED STATEMENT OF OPERATIONS", 
                           "CONSOLIDATED STATEMENT OF EARNINGS", 
                           "CONSOLIDATED STATEMENTS OF EARNINGS",
                           "INCOME STATEMENTS", 
                           "CONSOLIDATED RESULTS OF OPERATIONS",
                           "CONSOLIDATED STATEMENTS OF OPERATIONS")
  
  income.descriptions.q <- c("CONDENSED CONSOLIDATED STATEMENTS OF OPERATIONS")   # just add condensed to all these?
  
  income.descriptions <- c(income.descriptions.a, income.descriptions.q)
     
  GetFinancial(income.descriptions, symbol, year, annual, quarter)
}