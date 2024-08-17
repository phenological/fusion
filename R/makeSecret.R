#' function to make string secret to protect data privacy
#'
#' @param string string
#' @return md5sum of the string
#'
#'
#' @export
makeSecret <- function(string) {
  hash = system(paste("printf", shQuote(string), "| md5sum"), intern = TRUE)
  res <- strsplit(hash, " ")[[1]][1]
  return(res)
}
