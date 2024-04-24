#' function to calculate total lipids, subfraction and the percentage
#' @param data - lipoprotein daE or other format such as data.frame
#' @return 112 new column of either calc. (calculated values) or perc. (percentage)
#' @examples
#' data<-local(get(load("~lipo.daE")))
#' extended_lipo<-extend_lipo(data)
#' 
#' 


extend_lipo<-function(data){
  
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
  if(class(data)!="data.frame"){
    data<-data.frame(data)
  }
  data<-data[,which(colnames(data) %in% lipo_name)]
  data$calc.HDTL = data$HDTG+data$HDCH+data$HDPL
  data$calc.HDCE = data$HDCH-data$HDFC
  data$perc.HDTG = round(data$HDTG/data$calc.HDTL,4)*100
  data$perc.H1TG = round(data$H1TG/data$HDTG,4)*100
  data$perc.H2TG = round(data$H2TG/data$HDTG,4)*100
  data$perc.H3TG = round(data$H3TG/data$HDTG,4)*100
  data$perc.H4TG = round(data$H4TG/data$HDTG,4)*100
  data$perc.HDCH = round(data$HDCH/data$calc.HDTL,4)*100
  data$perc.H1CH = round(data$H1CH/data$HDCH,4)*100
  data$perc.H2CH = round(data$H2CH/data$HDCH,4)*100
  data$perc.H3CH = round(data$H3CH/data$HDCH,4)*100
  data$perc.H4CH = round(data$H4CH/data$HDCH,4)*100
  data$perc.HDFC = round(data$HDFC/data$calc.HDTL,4)*100
  data$perc.H1FC = round(data$H1FC/data$HDFC,4)*100
  data$perc.H2FC = round(data$H2FC/data$HDFC,4)*100
  data$perc.H3FC = round(data$H3FC/data$HDFC,4)*100
  data$perc.H4FC = round(data$H4FC/data$HDFC,4)*100
  data$perc.HDCE = round(data$calc.HDCE/data$calc.HDTL,4)*100
  data$perc.H1CE = round(c(data$H1CH-data$H1FC)/data$calc.HDCE,4)*100
  data$perc.H2CE = round(c(data$H2CH-data$H2FC)/data$calc.HDCE,4)*100
  data$perc.H3CE = round(c(data$H3CH-data$H3FC)/data$calc.HDCE,4)*100
  data$perc.H4CE = round(c(data$H4CH-data$H4FC)/data$calc.HDCE,4)*100
  data$perc.HDPL = round(data$HDPL/data$calc.HDTL,4)*100
  data$perc.H1PL = round(data$H1PL/data$HDPL,4)*100
  data$perc.H2PL = round(data$H2PL/data$HDPL,4)*100
  data$perc.H3PL = round(data$H3PL/data$HDPL,4)*100
  data$perc.H4PL = round(data$H4PL/data$HDPL,4)*100
  data$calc.VLTL = data$VLTG+data$VLCH+data$VLPL
  data$calc.VLCE = data$VLCH-data$VLFC
  data$perc.VLTG = round(data$VLTG/data$calc.VLTL,4)*100
  data$perc.V1TG = round(data$V1TG/data$VLTG,4)*100
  data$perc.V2TG = round(data$V2TG/data$VLTG,4)*100
  data$perc.V3TG = round(data$V3TG/data$VLTG,4)*100
  data$perc.V4TG = round(data$V4TG/data$VLTG,4)*100
  data$perc.V5TG = round(data$V5TG/data$VLTG,4)*100
  data$perc.VLCH = round(data$VLCH/data$calc.VLTL,4)*100
  data$perc.V1CH = round(data$V1CH/data$VLCH,4)*100
  data$perc.V2CH = round(data$V2CH/data$VLCH,4)*100
  data$perc.V3CH = round(data$V3CH/data$VLCH,4)*100
  data$perc.V4CH = round(data$V4CH/data$VLCH,4)*100
  data$perc.V5CH = round(data$V5CH/data$VLCH,4)*100
  data$perc.VLFC = round(data$VLFC/data$calc.VLTL,4)*100
  data$perc.V1FC = round(data$V1FC/data$VLFC,4)*100
  data$perc.V2FC = round(data$V2FC/data$VLFC,4)*100
  data$perc.V3FC = round(data$V3FC/data$VLFC,4)*100
  data$perc.V4FC = round(data$V4FC/data$VLFC,4)*100
  data$perc.V5FC = round(data$V5FC/data$VLFC,4)*100
  data$perc.VLCE = round(data$calc.VLCE/data$calc.VLTL,4)*100
  data$perc.V1CE = round(c(data$V1CH-data$V1FC)/data$calc.VLCE,4)*100
  data$perc.V2CE = round(c(data$V2CH-data$V2FC)/data$calc.VLCE,4)*100
  data$perc.V3CE = round(c(data$V3CH-data$V3FC)/data$calc.VLCE,4)*100
  data$perc.V4CE = round(c(data$V4CH-data$V4FC)/data$calc.VLCE,4)*100
  data$perc.V5CE = round(c(data$V5CH-data$V5FC)/data$calc.VLCE,4)*100
  data$perc.VLPL = round(data$VLPL/data$calc.VLTL,4)*100
  data$perc.V1PL = round(data$V1PL/data$VLPL,4)*100
  data$perc.V2PL = round(data$V2PL/data$VLPL,4)*100
  data$perc.V3PL = round(data$V3PL/data$VLPL,4)*100
  data$perc.V4PL = round(data$V4PL/data$VLPL,4)*100
  data$perc.V5PL = round(data$V5PL/data$VLPL,4)*100
  data$calc.IDTL = data$IDTG+data$IDCH+data$IDPL
  data$calc.IDCE = data$IDCH-data$IDFC
  data$perc.IDTG = round(data$IDTG/data$calc.IDTL,4)*100
  data$perc.IDCH = round(data$IDCH/data$calc.IDTL,4)*100
  data$perc.IDFC = round(data$IDFC/data$calc.IDTL,4)*100
  data$perc.IDCE = round(data$calc.IDCE/data$calc.IDTL,4)*100
  data$perc.IDPL = round(data$IDPL/data$calc.IDTL,4)*100
  data$calc.LDTL = data$LDTG+data$LDCH+data$LDPL
  data$calc.LDCE = data$LDCH-data$LDFC
  data$perc.LDTG = round(data$LDTG/data$calc.LDTL,4)*100
  data$perc.L1TG = round(data$L1TG/data$LDTG,4)*100
  data$perc.L2TG = round(data$L2TG/data$LDTG,4)*100
  data$perc.L3TG = round(data$L3TG/data$LDTG,4)*100
  data$perc.L4TG = round(data$L4TG/data$LDTG,4)*100
  data$perc.L5TG = round(data$L5TG/data$LDTG,4)*100
  data$perc.L6TG = round(data$L6TG/data$LDTG,4)*100
  data$perc.LDCH = round(data$LDCH/data$calc.LDTL,4)*100
  data$perc.L1CH = round(data$L1CH/data$LDCH,4)*100
  data$perc.L2CH = round(data$L2CH/data$LDCH,4)*100
  data$perc.L3CH = round(data$L3CH/data$LDCH,4)*100
  data$perc.L4CH = round(data$L4CH/data$LDCH,4)*100
  data$perc.L5CH = round(data$L5CH/data$LDCH,4)*100
  data$perc.L6CH = round(data$L6CH/data$LDCH,4)*100
  data$perc.LDFC = round(data$LDFC/data$calc.LDTL,4)*100
  data$perc.L1FC = round(data$L1FC/data$LDFC,4)*100
  data$perc.L2FC = round(data$L2FC/data$LDFC,4)*100
  data$perc.L3FC = round(data$L3FC/data$LDFC,4)*100
  data$perc.L4FC = round(data$L4FC/data$LDFC,4)*100
  data$perc.L5FC = round(data$L5FC/data$LDFC,4)*100
  data$perc.L6FC = round(data$L6FC/data$calc.LDTL,4)*100
  data$perc.LDCE = round(data$calc.LDCE/data$calc.LDTL,4)*100
  data$perc.L1CE = round(c(data$L1CH-data$L1FC)/data$calc.LDCE,4)*100
  data$perc.L2CE = round(c(data$L2CH-data$L2FC)/data$calc.LDCE,4)*100
  data$perc.L3CE = round(c(data$L3CH-data$L3FC)/data$calc.LDCE,4)*100
  data$perc.L4CE = round(c(data$L4CH-data$L4FC)/data$calc.LDCE,4)*100
  data$perc.L5CE = round(c(data$L5CH-data$L5FC)/data$calc.LDCE,4)*100
  data$perc.L6CE = round(c(data$L6CH-data$L6FC)/data$calc.LDCE,4)*100
  data$perc.LDPL = round(data$LDPL/data$calc.LDTL,4)*100
  data$perc.L1PL = round(data$L1PL/data$LDPL,4)*100
  data$perc.L2PL = round(data$L2PL/data$LDPL,4)*100
  data$perc.L3PL = round(data$L3PL/data$LDPL,4)*100
  data$perc.L4PL = round(data$L4PL/data$LDPL,4)*100
  data$perc.L5PL = round(data$L5PL/data$LDPL,4)*100
  data$perc.L6PL = round(data$L6PL/data$LDPL,4)*100
  data$calc.TBPN = data$VLPN+data$IDPN+data$L1PN+data$L2PN+data$L3PN+data$L4PN+data$L5PN+data$L6PN
  data$perc.VLPN = round(data$VLPN/data$calc.TBPN,4)*100
  data$perc.IDPN = round(data$IDPN/data$calc.TBPN,4)*100
  data$perc.L1PN = round(data$L1PN/data$calc.TBPN,4)*100
  data$perc.L2PN = round(data$L2PN/data$calc.TBPN,4)*100
  data$perc.L3PN = round(data$L3PN/data$calc.TBPN,4)*100
  data$perc.L4PN = round(data$L4PN/data$calc.TBPN,4)*100
  data$perc.L5PN = round(data$L5PN/data$calc.TBPN,4)*100
  data$perc.L6PN = round(data$L6PN/data$calc.TBPN,4)*100
  
  data<-data[,-which(colnames(data) %in% lipo_name)]
  
  return(data)
  
}
