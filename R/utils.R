##   Function to download Instance Document
GetInstFile <- function(url) {
  XBRL::xbrlDoAll(url, cache.dir="XBRLcache", prefix.out ="out", verbose=FALSE)
}

##   Generic function to extract info
ExtractInfo <- function(thing, html.node) {
  info <- thing %>%
    rvest::html_nodes(html.node) %>%
    rvest::html_text()
  return(info)
}


