
GetFinancial <- function(statement.type, symbol, year) {
        
  # statement.type <- cash.flow.descriptions
  # symbol <- "AAPL"
  # year <- 2016

  inst.url <- GetURL(symbol, year)
     
  # Check if url exits
  check <- tryCatch(is.list(httr::GET(inst.url)), error = function(e) {return(FALSE)})
  if(check == FALSE) {
    stop("no XBRL-format filings detected")
  }
     
  # Download Instance Document
  instFile <- GetInstFile(inst.url)
     
  # Clear Cache Dir
  file.remove("out_calculations.csv", "out_contexts.csv", "out_definitions.csv", 
              "out_elements.csv", "out_facts.csv", "out_footnotes.csv", 
              "out_labels.csv", "out_presentations.csv", "out_roles.csv", "out_units.csv")
     
  unlink("XBRLcache", recursive = TRUE)
     
  # Get Role ID from Instance Document
  role.df <- instFile$role %>%
    dplyr::filter(toupper(description) %in% statement.type)

  role.id <- as.character(role.df$roleId)

  # Create statement template from Presentation Linkbase
  statement.skeleton <- instFile$presentation %>%
    dplyr::filter(roleId == role.id)

  rowid <- c(1:nrow(statement.skeleton))
  statement.skeleton <- dplyr::mutate(statement.skeleton, rowid = rowid)

  # Merge with Label Linkbase
  statement <- statement.skeleton %>% 
    dplyr::left_join(instFile$label, by=c("toElementId"="elementId")) %>%
    dplyr::filter(labelRole == "http://www.xbrl.org/2003/role/label") %>% 
    dplyr::left_join(instFile$fact, by=c("toElementId"="elementId")) %>%   # Fact Linkbase
    dplyr::left_join(instFile$context, by=c("contextId")) %>%              # Context Linkbase
    dplyr::arrange(rowid)

  # Clean combined table
  statement <- subset(statement, is.na(statement$dimension1))

  clean.statement <- dplyr::select(statement, .data$labelString, .data$unitId, .data$fact, .data$contextId, .data$startDate, .data$endDate, .data$rowid)
  clean.statement <- dplyr::select(clean.statement, -.data$contextId)

  colnames(clean.statement)[1] <- "Metric"
  colnames(clean.statement)[2] <- "Units"
  colnames(clean.statement)[3] <- "Amount"

  clean.statement <- dplyr::arrange(clean.statement, .data$rowid)
  clean.statement <- dplyr::select(clean.statement, -.data$rowid)
  return(clean.statement)
}