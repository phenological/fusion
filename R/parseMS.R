#' parseMS
#'
#' @param folder Either character (file path) to folder/file or a data frame from a mass spec rolodex.
#' @param assay character for mass spec assay. Must be one of "ddhN", "AA", "TRY", "BA", "SCFA".
#' @param fileType character for file type if folder argument is a path to a folder/file.
#' Must be "txt", "tsv" or "xml".
#' @param optns List for extra arguments
#' @import ms.parser
#' @import methods
#' @import reshape2
#' @return data element
#' @export

parseMS <- function(folder, assay, fileType = NULL, optns = list()) {

  ######CONFIGURATION######

  if (!exists("optns")) {
    optns <- list()
  }

  #must provide assay
  if (is.null(assay)) {
    stop("The 'assay' argument must be provided.")
  }

  if (is.null(folder)) {
    stop("The 'folder' argument must be provided.")
  }

  if(is(folder)[1] == "character" && is.null(fileType)){
    stop("If 'folder' is a file or folder, fileType' must also be specified.")
  }

  #####fileType#####

  if(is(folder)[1] == "character"){
    if(tolower(fileType) == "xml"){
      pattern = "\\.xml$"
    }else if(tolower(fileType) == "txt"){
      pattern = "\\.TXT$"
    }else if(tolower(fileType) == "tsv"){
      pattern = "\\.TSV$"
    }else{stop("must provide the fileType argument correctly")}
  }

  ####read#####
  if(is(folder)[1] != "data.frame" && is(folder)[1] == "character"){

    files <- file.path(folder)

    plates <- dir(files, pattern = pattern)

    read_function <- getExportedValue("ms.parser", paste0("read", assay))

    #if there is only one file
    if(identical(plates, character(0))){
      plates <- folder

      d <- list()
      for (i in plates) {
        d[[i]] <- read_function(file = plates,
                                optns = list())
      }
    } else{ #if there are multiple files
      d <- list()
      for (i in plates) {
        d[[i]] <- read_function(file = file.path(files, i),
                                optns = list())
      }
    }
    result <- do.call(rbind, d)
  } else{result <- folder}

  #########make obsDescr###########

  #columns other than sampleMatrixType, sampleID, sampleType, AnalysisName and AnalyteName
  obCols <- c(
    "m/z",
    "expected m/z",
    "Area",
    "Height",
    "mSigma",
    "Retention Time[min]",
    "Expected Retention Time[min]",
    "Quantity",
    "Internal Standard(ISTD)",
    "Expected Quantity",
    "Unit of Quantity",
    "R2",
    "Accuracy/Recovery[%]",
    "Residuals[%]",
    "Visited",
    "plateID",
    "Internal standard used for calibration function calculation",
    "ISTD Area",
    "relative Area",
    "Manually Integrated",
    "Acquisition Date"
  )

  #are the obsCols available
  obCols %in% unique(result$paramName)

  #should be obsDescr for each analyte in a list.
  analytes <- unique(result$AnalyteName)

  obsDescr <- list()
  for(analyte in analytes){
    idx <- which(result$AnalyteName == analyte)
    newish <- result[idx, ]
    newish <- newish[which(newish$paramName %in% obCols),]

    # Use reshape to convert long to wide
    reshaped_df <- reshape(newish,
                           idvar = c("sampleID", "AnalysisName", "AnalyteName", "projectName",
                                     "cohortName", "sampleType", "sampleMatrixType"),
                           timevar = "paramName",
                           direction = "wide")

    # Clean up column names by removing the "paramValue." prefix added during reshaping
    colnames(reshaped_df) <- gsub("paramValue\\.", "", colnames(reshaped_df))

    ####Acquisition Date####
    #change Acquisition dates to poscixt
    if("Acquisition Date" %in% names(reshaped_df)){

      #from a tsv originally
      acDate <- gsub(" [A-Z]{3,4} ", " ", reshaped_df[, "Acquisition Date"])
      acDate <- as.POSIXct(acDate,
                           format = "%a %b %d %H:%M:%S %Y",
                           tz = "Australia/Perth")

      if(all(is.na(acDate))){
        #from xml orginally
        reshaped_df$`Acquisition Date` <- as.POSIXct(reshaped_df$`Acquisition Date`,
                                                     format = "%d-%b-%y %H:%M:%S",
                                                     tz = "Australia/Perth"
        )
      } else{reshaped_df$`Acquisition Date` <- acDate}
    }

    obsDescr[[analyte]] <- reshaped_df
  }

  #original sampleIDs
  original_sampleIDs <- lapply(obsDescr, function(df) df$sampleID)

  #####fix duplicated sampleIDs for all non samples (ltrs, qc, blanks etc)######
  for(i in seq_along(obsDescr)){

    df <- obsDescr[[i]]

    #if -rerun instead of _rerun
    if (any(grepl("-rerun", df$AnalysisName, ignore.case = TRUE))) {
      result$AnalysisName <- gsub(pattern = "-rerun", replacement = "_rerun", x = result$AnalysisName,
                                  ignore.case = FALSE)
    }

    #counter at end of analysis name
    df$plateExperimentID <- sapply(strsplit(df$AnalysisName, "_"), function(parts) {
      idx <- length(parts)

      # Loop backwards through the parts until we find a numeric value
      while (idx > 0) {
        # Suppress warnings during the integer conversion attempt
        num <- suppressWarnings(as.integer(parts[idx]))

        if (!is.na(num)) {
          return(num)
        }

        # Move to the previous part if current part is not numeric
        idx <- idx - 1
      }

      # Return NA if no numeric value is found
      return(NA)
    })


    # Identify duplicated sampleIDs where sampleType is not "sample"
    idx <- which(duplicated(df$sampleID) & df$sampleType != "sample")
    if(length(idx) > 0 ){
      # Loop over the indices of duplicated sampleIDs
      for (id in idx) {
        # Extract the plate ID number from the plateID column (e.g., COVp021 -> 21)
        plate_id_number <- suppressWarnings(as.numeric(sub(".*?(\\d+)$", "\\1", df[id, "plateID"])))
        if(is.na(plate_id_number)){
          plate_id_number <- df[id, "plateID"]
        }
        # Append the plate ID number to the sampleID (e.g., "sampleID#21")
        df[id, "sampleID"] <- paste0(df[id, "sampleID"], "#", plate_id_number)
      }

    }

    #are there duplicates left
    idx <- which(duplicated(df[["sampleID"]]) |
                   duplicated(df[["sampleID"]], fromLast = TRUE))

    if(length(idx) > 0){

      for (id in idx) {
        # Append the numeric part to sampleID and break out of the loop
        df$sampleID[id] <- paste0(df$sampleID[id], "#", df$plateExperimentID[id])
      }

    }

    #any duplicates left
    idx <- which(duplicated(df[["sampleID"]]) |
                   duplicated(df[["sampleID"]], fromLast = TRUE))
    for (id in idx) {
    # Loop through each duplicated sampleID and modify it
    for (id in idx) {
      # Split AnalysisName by underscore
      parts <- strsplit(df$AnalysisName[id], "_")[[1]]
      end <- length(parts)
      num_part <- suppressWarnings(as.integer(parts[end]))
      if (is.na(num_part)) {
        # Append the rerun part to sampleID and break out of the loop
        df$sampleID[id] <- paste0(df$sampleID[id], ".", parts[end])
      }

    }
    }
    #any duplicated left add plateID
    idx <- which(duplicated(df[["sampleID"]]) |
                   duplicated(df[["sampleID"]], fromLast = TRUE))
    if(length(idx) > 0){

      for (id in idx) {
        # Append the numeric part to sampleID and break out of the loop
        df$sampleID[id] <- paste0(df$sampleID[id], "#", df$plateID[id])
      }

    }



    #are there duplicates left, print them
    idx <- which(duplicated(df[["sampleID"]]) |
                   duplicated(df[["sampleID"]], fromLast = TRUE))
    if(length(idx) > 0){
      stop(paste0(unique(df[idx, "sampleID"]), "sampleIDs is duplicated, please fix"))
    } else{
      obsDescr[[i]]$sampleID <- df$sampleID
    }
  }

  reference_sampleID <- obsDescr[[1]]$sampleID

  # Create an empty list to store the data
  data_list <- lapply(obsDescr, function(obs) {
    # Extract AnalyteName, sampleID, and Quantity
    data.frame(
      sampleID = obs$sampleID,
      Quantity = obs$Quantity,
      AnalyteName = obs$AnalyteName
    )
  })

  # Combine all data frames into a single data frame
  combined_data <- do.call(rbind, data_list)

  # Reshape the data to make AnalyteName columns and sampleID rows
  newData <- suppressWarnings( reshape(
    combined_data,
    timevar = "AnalyteName",
    idvar = "sampleID",
    direction = "wide"
  ))

  # Clean column names to remove "Quantity." prefix
  colnames(newData) <- gsub("Quantity\\.", "", colnames(newData))

  #ensure in same order as obsDescr
  newData_ordered <- newData[match(reference_sampleID, newData$sampleID), ]
  rownames(newData_ordered) <- seq_len(nrow(newData_ordered))

  if (all(newData_ordered$sampleID == reference_sampleID)) {
    cat("The sampleID in newData_ordered is in the same order as reference_sampleID.\n")
  } else {
    cat("The sampleID in newData_ordered does NOT match the order of reference_sampleID.\n")

    # Optionally, show mismatches
    mismatch_indices <- which(newData_ordered$sampleID != reference_sampleID)
    cat("Mismatches at rows:", mismatch_indices, "\n")
  }
  #drop sampleID and AnalysisName
  newData_ordered <- newData_ordered[ , -which(names(newData_ordered) %in% c("AnalysisName","sampleID"))]

  ######make dae########
  da <- new("dataElement",
            .Data = newData_ordered,
            obsDescr = obsDescr,
            varName = unlist(colnames(newData_ordered)),
            type = "T-MS",
            method = assay)


  for (i in seq_along(da@obsDescr)) {
    da@obsDescr[[i]]$sampleID <- original_sampleIDs[[i]]
  }

  return(da)
}
