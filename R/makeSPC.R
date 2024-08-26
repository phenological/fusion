#' function to make dataElements of SPC 1,2,3 and Glyc A and B integrals
#' @param 
#' @return 
#' @importFrom nmr.parser baselineCorrection
#' @importFrom fusion uncalibrate
#' 
#' @examples
#' # da<-local(get(load(~.daE)))
#' # test<-makeSPC(da,calibrate = TRUE)
#' 
#' @export
#' 
#' 
makeSPC<-function(da,calibrate =TRUE){
  
  if(class(da)[1]!="dataElement"){
    stop("input data must be a dataElement")
  }
  
  if(da@type!="NMR"){
    stop("input data must be a NMR dataElement")
  }

  if(!grepl("ivdr",da@method)){
    stop("input data must be a NMR dataElement with method .ivdr ")
  }  
  
  if(calibrate==TRUE){
    ## If the pgpe daE is made WITHOUT SR = 0
    spec = fusion::uncalibrate(da)
  }else{
    ## If the pgpe daE is made WITH SR = 0
    spec = da@.Data
  }
  
  info = da@obsDescr
  
  # if(eretic==TRUE){
  #   ## If the pgpe daE is made WITHOUT SR = 0
  #   spec = spec/info$params$
  # }
  ppm = as.numeric(da@varName)
  colnames(spec)<-ppm
  
  
  idx = c(which(ppm>=4.6 & ppm<=4.85),which(ppm>=min(ppm) & ppm<=0.4),which(ppm>=9.5 & ppm<=max(ppm)))
  spec = spec[,-idx]
  ppm = ppm[-idx]
  rm(idx)
  spec<-baselineCorrection(spec)
  # check the 180 fllip
  idx<-which(apply(spec[,which(ppm>=3.2 & ppm<=3.3)],1,sum)<0)
  if(length(idx)>0){
    spec[idx,]<--spec[idx,]
  }
  rm(idx)
  # SPC
  dat<-data.frame(SPC_All = apply(spec[,which((ppm>3.18) & ( ppm<3.32))],1,sum),
                   SPC3 = apply(spec[,which((ppm>3.262) & ( ppm<3.3))],1,sum),
                   SPC2 = apply(spec[,which((ppm>=3.236) & ( ppm<3.262))],1,sum),
                   SPC1 = apply(spec[,which((ppm>=3.2) & ( ppm<3.236))],1,sum),
                   Glyc_All = apply(spec[,which((ppm>2.050) & ( ppm<2.118))],1,sum),
                   GlycA = apply(spec[,which((ppm>2.050) & ( ppm<2.089))],1,sum),
                   GlycB = apply(spec[,which((ppm>=2.089) & ( ppm<2.118))],1,sum))
  dat$SPC3_2 = dat$SPC3/dat$SPC2
  dat$SPC_Glyc = dat$SPC_All/dat$Glyc_All
  varName<-colnames(dat)
  # create dataElement
  da <- new("dataElement",
            .Data = dat,
            obsDescr = info,
            varName = varName,
            type = "NMR",
            method = "SPC_integral",
            version = paste0(c(paste("daE: 1.0; rldx:", utils::packageVersion("rldx")),
                               paste("nmr.parser:", utils::packageVersion("nmr.parser")),
                               paste("fusion:", utils::packageVersion("fusion"))),
                             collapse = "; "))
  
  # fileName <- paste(c(opts$projectName,
  #                     opts$cohortName,
  #                     opts$sampleMatrixType,
  #                     opts$runName,
  #                     "SPC_integral", collapse = "_")
  # 
  # assign(fileName, da)
  
  return(da)
  
  
}


