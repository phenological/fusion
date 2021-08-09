#' extract xml quantification information from a bruker xml
#'
#' @param path - the path to the expName folder
#' @return the title
#'
#' @export
#' @importFrom xml2 read_xml
getEretic <- function(path){
  path <- file.path(path, "QuantFactorSample.xml")
  if (file.exists(path)) {
    xml <- read_xml(path, options = "NOBLANKS")
    # assign("content", list(), envir = .GlobalEnv)
    content <- list()
    readXML(xml)
    quant <- data.frame(do.call(rbind, content))
    # rm("content", envir = .GlobalEnv)
    return(quant)
  } else {
    cat(crayon::yellow("fusion::getEretic >>", path, "not found\n"))
  }
}

