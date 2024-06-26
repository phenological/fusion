#' Bind together multiple dataElement objects by rows.
#'
#' This function takes a list of dataElement objects and binds them together
#' by rows into a single dataElement object. It checks that the dataElements
#' are compatible in terms of type and method before binding. The observation
#' descriptions are concatenated and made unique where necessary.
#' rbind dataElements
#'
#' @param ... a list of dataElements  
#' @return the information about dataElement
#'
#' @export rbind.dataElement
#' @export
#' @importFrom methods hasArg
#' @importFrom crayon %+%
#' @importFrom data.table rbindlist

rbind.dataElement <- function(...){
  
  newData <- list()
  newObs <- list()
  type <- method <- "init"
  counter <- 1

  # Loop through each dataElement to extract data and metadata
  for (el in list(...)) {
    
    if (is(el)[1] != "dataElement"){
      stop(crayon::red("fusion::rbind >> Some are not dataElements"))
      
    } else {
      
      if (el@type != type & type != "init") {
        stop(crayon::red("fusion::rbind >> All elements must be of same type"))
        
      } else {
        
        if (el@type != "ANN") {
          
          if (el@method != method & method != "init") {
            cat(crayon::red("fusion::rbind >> All elements must be of same method"))
            warning(crayon::red("fusion::rbind >> CHECK THAT THOSE DATA ARE COMPATIBLE"))
            
            newData[[counter]] <- el@.Data
            
          } else {
            newData[[counter]] <- el@.Data
          }
          
        }
        
        newObs[[counter]] <- el@obsDescr
        method = el@method
        
      }
      
      type = el@type
      
    }
    
    counter <- counter + 1
    
  }

  obsDescr <- list()
  
  # Concatenate observation metadata
  for (obs in 1:length(el@obsDescr)) {
    
    newDescr <- lapply(newObs, function(x) x[[obs]])
    newDescr <- do.call("rbindlist", list(newDescr, use.names = TRUE, fill = TRUE))
    
    if ("sampleID" %in% colnames(newDescr)) {
      newDescr$sampleID <- makeUnique(newDescr$sampleID, sep = "#")
    }
    
    obsDescr[[obs]] <- data.frame(newDescr, check.names = FALSE)
    
  }

  # Create new dataElement object
  if (el@type != "ANN") {
    
    .Data <- do.call("rbind", newData)
    newElement <- new("dataElement",
                      .Data = .Data,
                      obsDescr = obsDescr,
                      varName = el@varName,
                      type = type,
                      method = method)
                      
  } else {
    
    newElement <- new("dataElement",
                      obsDescr = obsDescr,
                      type = type,)
                      
  }

  return(newElement)
  
}

