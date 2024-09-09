#' function to calculate total lipids, subfraction and the percentage
#' @param data - lipoprotein daE or other format such as data.frame
#' @return 112 new column of either calc. (calculated values) or perc. (percentage)
#' @importFrom nmr.parser extend_lipo
#' @importFrom utils stack
#' @examples
#' #data<-local(get(load("~lipo.daE")))
#' #extended_lipo<-extend_lipo(data)
#' @export
#'


extendLipo<-function(data){

  if(class(data)[1]=="dataElement"){
    data<-data.frame(apply(data@.Data,2,as.numeric))
  }
  lipo_name<-nmr.parser::getLipoTable()$ID
  if(any(grepl("value.",colnames(data)))){
    colnames(data)<-gsub("value.","",colnames(data))
  }
  if(length(which(lipo_name %in% colnames(data)))!=112){
    stop("some lipoprotein parameters does not match")
  }
  if(class(data)[1]!="data.frame"){
    data<-data.frame(data)
  }
  # data<-data[,which(colnames(data) %in% lipo_name)]
  # tdf<-apply(data,1,as.list)
  #
  # tdf1<-lapply(tdf, as.data.frame)
  # melt_and_rename <- function(df) {
  #     df_long  = stack(df)
  #     colnames(df_long) = c("value","id")
  #   return(df_long)
  # }
  # tdf2 <- lapply(tdf1, melt_and_rename)
 tdf1 <- list()
  for(i in 1:nrow(data)){
    lipo <- get0("lipo", envir = asNamespace("nmr.parser"))
    lipo$data$value <-t(data[i,])
    tdf1[[i]] <- lipo
    rm(lipo)
  }

 tdf2<-lapply(tdf1, nmr.parser::extend_lipo)
 result <- do.call(rbind, lapply(1:length(tdf2), function(i) tdf2[[i]][["data"]][["value"]]))
 colnames(result) <- tdf2[[1]][["data"]][["id"]]
  # data<-lapply(tdf2, nmr.parser::extend_lipo)
  # data<-data.frame(do.call("rbind",data))
  return(result)

}
