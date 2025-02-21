#' checkRun 
#' collect information if the 1D, CPMG, PGPE has been run, Bruker Report (QC, LIPO, Quant, PACS) has been generated for each samples/ltr as planned or not.
#'
#'#'
#' @param runID to link between the server and the expected
#' @param by the order of the EXPNO default is by row and alternative is "column"
#' @param ltr to consider ltr and/or sltr when calculating the EXPNO. 
#' default is ltr = "both" assuming 4 ltr and 4 sltr per plate. 
#' if ltr = "sltr" meaning the plate has 0 ltr but 4 sltr per plate
#' if ltr = "ltr" meaning the plate has 4 ltr and 0 sltr per plate
#' if ltr = "none" meaning 0 ltr and 0 sltr per plate
#' @importFrom utils strcapture
#' @return tdf1 

#' 
#' first the expected samples per plate is gather from the run Layout. 
#'    This must be matched with sample layout found in the configurationfile. 
#'    from "expected", combination of the "plateName", "expno", and the "sampleID" named "expname" should be unique.
#' then the dataPath information about the 1D noesy run in the server is extracted from loe.
#'    from "dataPath", combination of the "plateName", "expno", and the "sampleID" named "check" should be unique.
#' by mergin those two by "expname" and "check" you have the new data frame called "tdf1" that capture which samples/ltrs has been run and which are the re-runs.
#' 
#' the next will be a qc report check.
#' match the previous tdf1 using the dataPath this time to see how many of the run has QC report and which are missing.
#' %%% Note: only the sampleMatrixType of SER, PLA, and URI should have qc report %%%
#' only if the qc report exists, lipo, quant, pacs report will also get checked.
#' 
#' once the 1D run has been checked then cpmg and the pgpe (zggppezf) run will be checked if exist.
#' for cpmg the EXPNO - 1 to match the 1D dataPath
#' for zggppezf, the EXPNO - 2 to match the 1D dataPath
#'   
#'

checkRun<-function(runName = "SPTr04", by = "row",ltr = "both"){
  request = paste0("?runName=",runName)
  ## ltr =sltr, ltr = both
  expect = paste0(request, "&as=",by,"&sltr=",ltr)
  loe = rldx_get(route = "link", request = request)
  loe<-loe$content$list
  expected = rldx_get(service = "",route = "layoutByRunName", expect)
  expected = expected$content[,c("sampleID", "sourceName","plateName","expno")]  
  expected$expname<-paste0(expected$plateName,"/",expected$expno,"-",expected$sampleID)
  loe_names<-names(loe)
  loe_names<-loe_names[grepl("noesy|cpmg|zggppezf", loe_names)]
  i<-loe_names[grep("noesy",loe_names)]
  tdf = loe[[i]]
  tdf <-tdf[, which(colnames(tdf) %in% c("dataPath","experiment","sampleName"))]
  tdf$plateID<-sapply(strsplit(tdf$dataPath,"/"),"[",6)
  tdf$plateID<-sapply(strsplit(tdf$plateID,"_"),"[",6)
  tdf$EXPNO<-sapply(strsplit(tdf$dataPath,"/"),"[",7)
  tdf$check = paste0(tdf$plateID,"/",tdf$EXPNO,"-",tdf$sampleName)    
  # tdf <-tdf[-grep("ltr",tdf$sampleId,ignore.case = T), which(colnames(tdf) %in% c("dataPath","experiment","sampleId"))]
  names(tdf)[2]<-i
  tdf1<-merge(expected,tdf,
              by.x = "expname",
              by.y = "check",
              all = T)
  rm(tdf)
  tdf1<-tdf1[!is.na(tdf1$sampleName),]
  
  qc <-readExperiment(loe$noesygppr1d$dataPath, list(what = c("acqus")))
  
  if (length(qc$acqus) > 0) {
    qc <- qc[["acqus"]][, c("path", "acqus.USERA2")]
    # qc<-qc[-grep("ltr",qc$acqus.USERA2,ignore.case = T),]
    
    tdf1 = merge(tdf1,
                 qc,
                 by.x = "dataPath",
                 by.y = "path",
                 all.x = TRUE)
    # tdf1$reruns[which(duplicated(tdf1$sampleID) == TRUE)] = "Yes"
    rm(qc)
    lipo <-readExperiment(loe$noesygppr1d$dataPath, list(what = c("lipo")))
    if (length(lipo$lipo) > 0) {
      lipo <- lipo[["lipo"]][, "path"]
      lipo$lipo <- "Yes"
      tdf1 <- merge(tdf1,
                    lipo,
                    by.x = "dataPath",
                    by.y = "path",
                    all = TRUE)
      
    }
    rm(lipo)
    
    sm <-readExperiment(loe$noesygppr1d$dataPath, list(what = c("quant")))
    if (length(sm$quant) > 0) {
      sm <- sm[["quant"]][, "path"]
      sm$sm <- "Yes"
      tdf1 <- merge(tdf1,
                    sm,
                    by.x = "dataPath",
                    by.y = "path",
                    all = TRUE)
      
    } 
    rm(sm)
    pacs <-readExperiment(loe$noesygppr1d$dataPath, list(what = c("pacs")))
    if (length(pacs$pacs) > 0) {
      pacs <- pacs[["pacs"]][, "path"]
      pacs$pacs <- "Yes"
      tdf1 <- merge(tdf1,
                    pacs,
                    by.x = "dataPath",
                    by.y = "path",
                    all = TRUE)
    }
    rm(pacs)
  }
  rm(i)
  if(length(grep("cpmg",loe_names))>0){
    i<-loe_names[grep("cpmg",loe_names)]
    tdf = loe[[i]]
    tdf <-tdf[-grep("ltr",tdf$sampleName,ignore.case = T), which(colnames(tdf) %in% c("dataPath","experiment","sampleName"))]
    names(tdf)[2]<-i
    
    parts <- strcapture("(.*/)(\\d+)$", tdf$dataPath, proto = list(prefix="", number=""))
    # Decrease the last digit by 1
    last_number_decreased <- as.numeric(parts$number) - 1
    # Concatenate the parts back together
    tdf$dataPath <- paste0(parts$prefix, last_number_decreased)
    
    tdf1 <- merge(tdf1,
                  tdf[, c("dataPath", i)],
                  by.x = "dataPath",
                  by.y = "dataPath",
                  all = T)
    rm(i,tdf,parts, last_number_decreased)
  }
  if(length(grep("zggppezf",loe_names))>0){
    i<-loe_names[grep("zggppezf",loe_names)]
    tdf = loe[[i]]
    tdf <-tdf[-grep("ltr",tdf$sampleName,ignore.case = T), which(colnames(tdf) %in% c("dataPath","experiment","sampleName"))]
    names(tdf)[2]<-i        
    parts <- strcapture("(.*/)(\\d+)$", tdf$dataPath, proto = list(prefix="", number=""))
    # Decrease the last digit by 2
    last_number_decreased <- as.numeric(parts$number) - 2
    # Concatenate the parts back together
    tdf$dataPath <- paste0(parts$prefix, last_number_decreased)
    tdf1 <- merge(tdf1,
                  tdf[, c("dataPath", i)],
                  by.x = "dataPath",
                  by.y = "dataPath",
                  all = T)
    rm(tdf,parts, last_number_decreased)
  }

  # remove the ltr/sltrs 
  tdf1<-tdf1[which(!is.na(tdf1$sampleName)),]
  tdf1$reruns<-"No"
  tdf1$reruns[which(duplicated(tdf1$sampleName)==TRUE)]<-"Yes"
  
  return(tdf1)
}
