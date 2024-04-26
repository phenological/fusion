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
    }
    if(s<0){
      ppm_idx<-seq(-s,length(ppm),1)
      SR_corrected<-c(df[i,ppm_idx],rep(0,abs(s)-1))
      tdf[[i]]<-SR_corrected
    }
    if(s==0){
      tdf[[i]]<-as.numeric(df[i,])
    }
    rm(s,ppm_idx)
  }
  
  tdf<-as.matrix(do.call("rbind",tdf))
  data@.Data<-tdf
  return(data)
  
}

