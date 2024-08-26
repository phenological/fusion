#' function to reset spectral reference to zero
#' @param data - dataElements
#' @return dataElements 
#' @examples
#' # dae<-local(get(load(~.daE)))
#' #test<-uncalibrate(data = dae)
#' 
#' @export
#' 

uncalibrate<-function(data){
  if(class(data)[1]!="dataElement"){
    stop("input data must be in dataElement")
  }
  
  if(data@type!="NMR"){
    stop("input dataElement type must be NMR")
  }
  
  tdf<-list()
  df<-data@.Data
  ppm<-as.numeric(data@varName)
  SR<-data@obsDescr$procs$SR
  SF<-data@obsDescr$procs$SF
  shift_idx<-round(c(SR/SF)/(ppm[2]-ppm[1]))
  for(i in 1:nrow(df)){
    s<-shift_idx[i]
    if(s>0){
      ppm_idx<-seq(1,length(ppm),1)-s
      ppm_idx<-ppm_idx[which(ppm_idx>0)]
      SR_corrected<-c(rep(0,s),df[i,ppm_idx])
      tdf[[i]]<-SR_corrected
      rm(ppm_idx)
    }
    if(s<0){
      ppm_idx<-seq(-s,length(ppm),1)
      SR_corrected<-c(df[i,ppm_idx],rep(0,abs(s)-1))
      tdf[[i]]<-SR_corrected
      rm(ppm_idx)
    }
    if(s==0){
      tdf[[i]]<-as.numeric(df[i,])
    }
    rm(s)
  }
  
  tdf<-as.matrix(do.call("rbind",tdf))
  data@.Data<-tdf
  return(data)
  
}

