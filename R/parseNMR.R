#' reads all information from bruker folder structures
#' @param folder - the root folder to read from
#' @param opts - opts (what(spec, brxlipo, prxpacs, brxsm, spcglyc), specOpts(procno, uncalibrate, fromTo, length.out))
#' @export
#' @importFrom nmr.parser scanFolder readExperiment
#' @importFrom utils menu
# req <- rldx_get("link", "?runName=EXTr01")
# folder <- req
# parseNMR(folder)

parseNMR <- function(folder, opts = NULL) {
  .SD <- NULL
  noWrite <- FALSE

  ########################################################################
  # CONFIGURATION
  ########################################################################


  # Define default options
  default_opts <- list(
    what = c("spec"),
    projectName = "",
    cohortName = "",
    runName = "",
    method = "",
    sampleMatrixType = "",
    specOpts = list(
      procno = 1,
      uncalibrate = FALSE,
      fromTo = c(-0.1, 10),
      length.out = 44079
    ),
    EXP = "",
    outputDir = "."
  )


  # Merge provided options with defaults
  if (is.null(opts)) {
    opts <- default_opts
  } else {
    # Handle nested specOpts separately
    if ("specOpts" %in% names(opts) && !is.null(opts$specOpts)) {
      opts$specOpts <- modifyList(default_opts$specOpts, opts$specOpts)
    }
    opts <- modifyList(default_opts, opts)
  }

  # if spcglyc spectra must be read first
  if (opts$what == "spcglyc") {
    opts$what <- "spec"
    spcglyc <- TRUE
    opts$specOpts$uncalibrate <- TRUE
  } else {
    spcglyc <- FALSE
  }


  if (all(c("content", "totalCount") %in% names(folder))) {

    # CASE WHERE WE USE A REQUEST TO ROLODEX #############################

    lop <- folder$content$list

    choices <- names(lop)
    choice <- menu(choices, title = "Select")

    opts$method <- names(lop)[choice]
    lop <- lop[[choice]]


    if (opts$projectName == "") {
      opts$projectName <- unique(lop$projectName[!is.na(lop$projectName)])[1]
    }

    if (opts$cohortName == "") {
      opts$cohortName <- unique(lop$cohortName[!is.na(lop$cohortName)])[1]
    }

    if (opts$runName == "" ) {
      opts$runName <- unique(lop$runName[!is.na(lop$runName)])[1]
    }

    if (opts$sampleMatrixType == "") {
      opts$sampleMatrixType <- unique(lop$sampleMatrixType[!is.na(lop$sampleMatrixType)])[1]
    }


    loe <- data.frame(dataPath = lop$dataPath,
                      sampleID = makeUnique(lop$sampleName),
                      sampleType = "sample",
                      experiment = lop$experiment,
                      nmrFolderId = lop$pathId)


    # this should be take care of on rolodex in the future
    idx <- grep("sltr", tolower(loe$sampleID))
    loe$sampleType[idx] <- "sltr"

    idx <- grep("^ltr", tolower(loe$sampleID))
    loe$sampleType[idx] <- "ltr"

    idx <- grep("^pqc", tolower(loe$sampleID))
    loe$sampleType[idx] <- "pqc"

    idx <- grep("^qc", tolower(loe$sampleID))
    loe$sampleType[idx] <- "qc"



  } else if ("dataPath" %in% names(folder)) {

    # CASE WHERE WE USE DIRECT PATH ######################################

    loe <- data.frame(dataPath = folder$dataPath,
                      sampleID = "sampleID_",
                      sampleType = "sampleType_",
                      experiment = "experiment_")
    noWrite = TRUE

  } else {

    # CASE WHERE WE USE A LOCAL FOLDER ###################################

    if (!("method" %in% names(opts))) {
      opts$method <- ""
    }


    if (!("projectName" %in% names(opts))) {
      opts$projectName <- ""
    }

    if (opts$projectName == "") {
      req <- rldx_get(route = "projects/", request = "?un=1")
      choices <- paste0(req$content$name,
                        ": ", req$content$prefix)
      choice <- menu(choices, title = "Choose a project.")
      projectName <- req$content$name[choice]
      projectId <- req$content$id[choice]
      prefix <- req$content$prefix[choice]
    } else {
      projectName <- opts$projectName
    }


    if (!("cohortName" %in% names(opts))) {
      opts$cohortName <- ""
    }

    if (opts$cohortName == "") {
      req <- rldx_get(route = "projects/", request = paste0(projectId, "/cohorts?un=1"))
      choices <- paste0(req$content$cohortName,
                        ": ", req$content$description)
      choice <- menu(choices, title = "Choose a cohort.")
      cohortName <- req$content$cohortName[choice]
    } else {
      cohortName <- opts$cohortName
    }


    if (!("runName" %in% names(opts))) {
      opts$runName <- ""

      choice <- readline(prompt = "runName: ")
      choice <- nmr.parser::cleanNames(choice)
      runName <- paste0(prefix, "r", "XX", choice)
    } else {
      opts$runName <- paste0(opts$runName, "@local")
    }

    if (!("sampleMatrixType" %in% names(opts))) {
      opts$sampleMatrixType <- ""
    }

    if (opts$sampleMatrixType == "") {
      req <- rldx_get(route = "matrices/", request = "?un=1")
      choices <- paste0(req$content$matrixName,
                        ": ", req$content$matrixDescription)
      choice <- menu(choices, title = "Choose matrix.")
      sampleMatrixType <- req$content$matrixName[choice]
    } else {
      sampleMatrixType <- opts$sampleMatrixType
    }


    lof <- scanFolder(folder, opts)


    EXP <- nmr.parser::cleanNames(lof$EXP[1])
    if (!is.na(lof$USERA2[1]) & !(lof$USERA2[1] == "")) {

      cat(crayon::blue("parseNMR >> ANPC sampleID (USERA2) found\n"))
      sampleID <- lof$USERA2
      sampleID <- sapply(sampleID, function(x) gsub("SLTR", "sltr", x))
      sampleID <- sapply(sampleID, function(x) gsub("LTR", "ltr", x))
      sampleID <- sapply(sampleID, function(x) gsub("PQC", "pqc", x))
      sampleID <- sapply(sampleID, function(x) gsub("QC", "qc", x))
      sampleID <- unname(sampleID)
      sampleType <- sampleID
      sampleType[!grepl("ltr|qc", sampleID)] <- "sample"
    } else {
      choices <- strsplit(lof$file[1], "/")[[1]]
      choices <- c("a timeStamp", choices)
      choice <- menu(choices, title = "Select what part of the path to use as sampleID.")
      if (choice == 1) {
        sampleID <- stamp(nrow(lof))
        sampleType <- rep("sample", nrow(lof))
        print(sampleID)
        print(sampleType)
        print(lof)
      } else {
        sampleID <- sapply(lof$file, function(x) strsplit(x, "/")[[1]][choice - 1])
        sampleType <- rep("sample", nrow(lof))
      }
    }

    # making sampleID unique (in case of repetitions in non anpc folders)
    sampleID <- makeUnique(sampleID)

    loe <- data.frame(dataPath = lof$file,
                      sampleID,
                      sampleType,
                      experiment = EXP)


  }
  ########################################################################
  # READING DATA
  ########################################################################


  if ("spec" %in% opts$what) {

    if (opts$method == "") {
      availableMethods <- meltdown()$NMR$method
      choice <- menu(suppressMessages(availableMethods))
      method <- suppressMessages(availableMethods[choice])
    } else {
      method <- opts$method
    }

    # we need to append the EXP to method to make it unique name
    opts$method <- paste0(method, "@", loe$experiment[1])

    spec <- readExperiment(loe$dataPath,
                           list(what = "spec", specOpts = opts$specOpts))


    if (length(spec) > 0) {
      ppm <- seq(from=opts$specOpts$fromTo[1],
                 to=opts$specOpts$fromTo[2],
                 length.out = opts$specOpts$length.out)
      varName <- as.character(ppm)


      if ("im" %in% names(opts$specOpts)) {

        dat <- as.matrix(do.call("rbind",
                                 lapply(spec$spec$spec,
                                        function(x) complex(real = x$spec$y,
                                                            imaginary = x$spec$yi))))
      } else {

        dat <- as.matrix(do.call("rbind",
                                 lapply(spec$spec$spec,
                                        function(x) x$spec$y)))

      }


      type <- "NMR"
    } else {
      cat(crayon::red("parseNMR >> No spectra found. Aborting\n"))
      stop("Aborted")
    }
  }

  if (spcglyc) {

    idx = c(which(ppm >= 4.6 & ppm <= 4.85),
            which(ppm >= min(ppm) & ppm <= 0.2),
            which(ppm >= 10.0))

    trimmedSpectra <- dat[,-idx, drop = FALSE]
    trimmedPpm <- ppm[-idx]
    dw <- trimmedPpm[2]-trimmedPpm[1]

    # trimmedSpectra <- baseline.corr(trimmedSpectra)
    trimmedSpectra <- data.frame(trimmedSpectra)

    # check for 180 flip
    idx <- which(apply(trimmedSpectra[, which(trimmedPpm >= 3.2 & trimmedPpm <= 3.3)],
                       1, sum) < 0)

    if(length(idx) > 0){
      trimmedSpectra[idx,] <- -trimmedSpectra[idx,]
    }

    tsp <- do.call("rbind",
                 lapply(spec$spec$spec,function(s){
                   ppm <-s$spec$x
                   y <- s$spec$y

                   y[(ppm >= min(ppm) & ppm <= 0.5)]

      }))
    colnames(tsp) <- ppm[(ppm >= min(ppm) & ppm <= 0.5)]


    spc <- trimmedSpectra[,which((trimmedPpm > 3.18) & ( trimmedPpm < 3.32))]
    colnames(spc) <- trimmedPpm[which((trimmedPpm > 3.18) & ( trimmedPpm < 3.32))]

    glyc <- trimmedSpectra[which((trimmedPpm > 2.050) & ( trimmedPpm < 2.118))]
    colnames(glyc) <- trimmedPpm[which((trimmedPpm > 2.050) & ( trimmedPpm < 2.118))]

    # SPC
    dat <- data.frame(
      SPC_All = apply(
        trimmedSpectra[, which((trimmedPpm > 3.18) & ( trimmedPpm < 3.32))], 1, sum
      ) * dw,
      SPC3 = apply(
        trimmedSpectra[, which((trimmedPpm > 3.262) & ( trimmedPpm < 3.3))], 1, sum
      ) * dw,
      SPC2 = apply(
        trimmedSpectra[, which((trimmedPpm >= 3.236) & ( trimmedPpm < 3.262))], 1, sum
      ) * dw,
      SPC1 = apply(
        trimmedSpectra[, which((trimmedPpm >= 3.2) & ( trimmedPpm < 3.236))], 1, sum
      ) * dw,
      Glyc_All = apply(
        trimmedSpectra[, which((trimmedPpm > 2.050) & ( trimmedPpm < 2.118))], 1, sum
      ) * dw,
      GlycA = apply(
        trimmedSpectra[, which((trimmedPpm > 2.050) & ( trimmedPpm < 2.089))], 1, sum
      ) * dw,
      GlycB = apply(
        trimmedSpectra[, which((trimmedPpm >= 2.089) & ( trimmedPpm < 2.118))], 1, sum
      ) * dw,
      Alb1 = apply(
        trimmedSpectra[, which((trimmedPpm > 0.2) & ( trimmedPpm < 0.7))], 1, sum
      ) * dw,
      Alb2 = apply(
        trimmedSpectra[, which((trimmedPpm > 6.0) & ( trimmedPpm < 10.0))], 1, sum
      ) * dw
    )

    dat$SPC3_2 <- dat$SPC3 / dat$SPC2
    dat$SPC_Glyc <- dat$SPC_All / dat$Glyc_All
    varName <- colnames(dat)
    opts$method <- paste0("spcglyc_", loe$experiment[1])
    type = "QUANT"

    # we correct for 3mm tubes
    idx <- which(grepl("3mm", tolower(loe$dataPath)))
    dat[idx, ] <- dat[idx, ] / 2

  }

  if ("brxlipo" %in% opts$what) {

    lipo <- readExperiment(loe$dataPath, list(what = c("lipo")))

    if (length(lipo) > 0) {
      dat <- as.matrix(lipo$lipo[,.SD, .SDcols = grep("value", names(lipo$lipo))])
      varName <- gsub("value.", "", colnames(dat))
      opts$method <- "brxlipo"
      type <- "QUANT"
    } else {
      cat(crayon::red("parseNMR >> No brxlipo found. Aborting\n"))
      stop("Aborted")
    }
  }

  if ("brxpacs" %in% opts$what) {

    brxpacs <- readExperiment(loe$dataPath, list(what = c("pacs")))

    if (length(brxpacs) > 0) {
      dat <- as.matrix(brxpacs$pacs[,.SD, .SDcols = grep("value", names(brxpacs$pacs))])
      varName <- gsub("value.", "", colnames(dat))
      opts$method <- "brxpacs"
      type <- "QUANT"
    } else {
      cat(crayon::red("parseNMR >> No brxpacs found. Aborting\n"))
      stop("Aborted")
    }
  }

  if ("brxsm" %in% opts$what) {

    brxsm <- readExperiment(loe$dataPath, list(what = c("quant")))

    if (length(brxsm) > 0) {
      dat <- as.matrix(brxsm$quant[,.SD, .SDcols = grep("value", names(brxsm$quant))])
      varName <- gsub("value.", "", colnames(dat))
      opts$method <- "brxsm"
      type <- "QUANT"
    } else {
      cat(crayon::red("parseNMR >> No brxsm found. Aborting\n"))
      stop("Aborted")
    }
  }


  # READING PARAMETERS AND QUALITY CHECKS ################################

  acqus <- readExperiment(loe$dataPath, list(what = c("acqus")))

  # qc are only found in IVDr data
  qc <- readExperiment(loe$dataPath, list(what = c("qc")))


  if (any(sapply(qc$qc, function(x) !is.null(x)))) {
    ivdr <- TRUE

  } else {

    cat(crayon::red("parseNMR >> Non IVDr data, no QC found\n"))
    ivdr <- FALSE
  }


  ########################################################################
  # MERGING
  ########################################################################


  if ("brxpacs" %in% opts$what) {

    arrayList <- lapply(list(brxpacs$pacs$path,
                             acqus$acqus$path,
                             loe$dataPath), function(x) unlist(x))

    intersection <- Reduce(intersect, arrayList)

    idx <- match(acqus$acqus$path, intersection)
    acqus$acqus <- acqus$acqus[!is.na(idx),]

    idx <- match(brxpacs$pacs$path, intersection)
    brxpacs$pacs <- brxpacs$pacs[!is.na(idx),]
    dat <- dat[!is.na(idx),]

    idx <- match(loe$dataPath, intersection)
    loe <- loe[!is.na(idx),]


    excluded <- setdiff(arrayList[[1]], intersection)

    if (length(excluded) > 0) {
      cat(crayon::yellow("excluded:", setdiff(arrayList[[1]], intersection), "\n"))
    }
  }



  if ("brxsm" %in% opts$what) {

    arrayList <- lapply(list(brxsm$quant$path,
                             acqus$acqus$path,
                             loe$dataPath), function(x) unlist(x))

    intersection <- Reduce(intersect, arrayList)

    idx <- match(acqus$acqus$path, intersection)
    acqus$acqus <- acqus$acqus[!is.na(idx),]

    idx <- match(brxsm$quant$path, intersection)
    brxsm$quant <- brxsm$quant[!is.na(idx),]
    dat <- dat[!is.na(idx),]

    idx <- match(loe$dataPath, intersection)
    loe <- loe[!is.na(idx),]


    excluded <- setdiff(arrayList[[1]], intersection)

    if (length(excluded) > 0) {
      cat(crayon::yellow("excluded:", setdiff(arrayList[[1]], intersection), "\n"))
    }
  }


  if ("brxlipo" %in% opts$what) {

    arrayList <- lapply(list(lipo$lipo$path,
                             acqus$acqus$path,
                             loe$dataPath), function(x) unlist(x))

    intersection <- Reduce(intersect, arrayList)

    idx <- match(acqus$acqus$path, intersection)
    acqus$acqus <- acqus$acqus[!is.na(idx),]

    idx <- match(lipo$lipo$path, intersection)
    lipo$lipo <- lipo$lipo[!is.na(idx),]
    dat <- dat[!is.na(idx),]

    idx <- match(loe$dataPath, intersection)
    loe <- loe[!is.na(idx),]

    excluded <- setdiff(arrayList[[1]], intersection)

    if (length(excluded) > 0) {
      cat(crayon::yellow("excluded:", setdiff(arrayList[[1]], intersection), "\n"))
    }

  }


  if ("spec" %in% opts$what) {

    arrayList <- lapply(list(spec$spec$path,
                             acqus$acqus$path,
                             loe$dataPath), function(x) unlist(x))

    intersection <- Reduce(intersect, arrayList)



    idx <- match(acqus$acqus$path, intersection)
    acqus$acqus <- acqus$acqus[!is.na(idx),]

    idx <- match(spec$spec$path, intersection)
    spec$spec <- spec$spec[!is.na(idx),] # just for the order
    dat <- dat[!is.na(idx),] # actual data

    idx <- match(loe$dataPath, intersection)
    loe <- loe[!is.na(idx),]

    procs <- data.frame(do.call("rbind",
                                lapply(spec$spec$spec,
                                       function(x) x$info)))


    excluded <- setdiff(arrayList[[1]], intersection)

    if (length(excluded) > 0) {
      cat(crayon::yellow("excluded:", setdiff(arrayList[[1]], intersection), "\n"))
    }

  } else {
    procs <- data.frame()
  }

  # PREPING TESTS & INFO #################################################
  # tests


  if (ivdr) {
    # else we make a left join
    rowName <- unname(unlist(qc[[1]]$path))

    # we should not have duplicated column names here.
    # this has to be fixed in nmr.parser
    test_tests_name <- makeUnique(qc[[1]]$testNames[[1]])

    test_tests_comment <- data.frame(do.call("rbind",
                                             lapply(qc[[1]]$tests,
                                                    function(x) x$comment)))
    colnames(test_tests_comment) <- test_tests_name
    test_tests_comment$path <- rowName

    test_tests_comment <- leftJoinWithAcqus(acqus$acqus, test_tests_comment, by = "path")

    test_tests_value <- data.frame(do.call("rbind",
                                           lapply(qc[[1]]$tests,
                                                  function(x) x$value)))
    colnames(test_tests_value) <- makeUnique(test_tests_name)
    test_tests_value$path <- rowName

    test_tests_value <- leftJoinWithAcqus(acqus$acqus, test_tests_value, by = "path")

    # infos
    test_infos_name <- makeUnique(qc[[1]]$infoNames[[1]])

    test_infos_value <- data.frame(do.call("rbind",
                                           lapply(qc[[1]]$infos,
                                                  function(x) x$value)))
    colnames(test_infos_value) <- test_infos_name
    test_infos_value$path <- rowName

    test_infos_value <- leftJoinWithAcqus(acqus$acqus, test_infos_value, by = "path")
  }

  # CREATING DATAELEMENT #################################################
  if (ivdr) {
    info <- list("info" = loe,
                 "procs" = procs,
                 "params" = acqus$acqus,
                 "test_tests_comment" = test_tests_comment,
                 "test_tests_value" = test_tests_value,
                 "test_infos_value" = test_infos_value)
  }

  if(opts$method=="spcglyc"){

    info <- list("info" = loe,
                 "procs" = procs,
                 "params" = acqus$acqus,
                 "tsp" = tsp,
                 "spc" = spc,
                 "glyc" = glyc)

  }else {
    info <- list("info" = loe,
                 "procs" = procs,
                 "params" = acqus$acqus)
  }


  # store versions
  version <- paste0(c(paste("daE: 1.0; rldx:", utils::packageVersion("rldx")),
                      paste("nmr.parser:", utils::packageVersion("nmr.parser")),
                      paste("fusion:", utils::packageVersion("fusion"))),
                    collapse = "; ")


  # create dataElement
  da <- new("dataElement",
            .Data = dat,
            obsDescr = info,
            varName = varName,
            type = type,
            method = opts$method,
            version = version)

  if (!noWrite) {
  fileName <- paste(c(opts$projectName,
                      opts$cohortName,
                      opts$sampleMatrixType,
                      opts$runName,
                      opts$method), collapse = "_")

  assign(fileName, da)


  save(list=(fileName),
       file = file.path(opts$outputDir, paste0(fileName, ".daE")))
  txt <- paste0("daE can be loaded as var <- local(get(load(\"",
                file.path(".", paste0(fileName, ".daE\"")),
                "))) to rename them on the fly")
  message(cat(crayon::blue(txt)))
  } else {

    # res <- cbind(dataPath = loe$dataPath, dat)

    return(da)

  }
}
