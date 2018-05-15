#' Calculates CPRAd from S1-S5 tables.
#'
#' @param  List containing the S1-S5 tables
#' @param  Vector of the ethnic weights.
#' @param  Number of decimal points to use.
#'
#' @return CPRAd
#'
#' @examples
#' \dontrun{
#' pak_calc(in,weights,decimals)
#' }
#' 
#' @export
pak_calc<-function(In,weights,decimals) {
  
#Now make DF containing S1 to S5 and calculate P of negative XM for each ethnicity
res<-as.data.frame(NULL)
res<-rbind(res,colSums(In[[1]])[2:5])
res<-rbind(res,colSums(In[[2]])[3:6])
res<-rbind(res,colSums(In[[3]])[4:7])
res<-rbind(res,colSums(In[[4]])[5:8])
res<-rbind(res,colSums(In[[5]])[6:9])
PnegXM.CAU<-1-((1-res[1,1]+res[2,1]-res[3,1]+res[4,1]-res[5,1])^2)
PnegXM.AFA<-1-((1-res[1,2]+res[2,2]-res[3,2]+res[4,2]-res[5,2])^2)
PnegXM.HIS<-1-((1-res[1,3]+res[2,3]-res[3,3]+res[4,3]-res[5,3])^2)
PnegXM.ASI<-1-((1-res[1,4]+res[2,4]-res[3,4]+res[4,4]-res[5,4])^2)
PnegXM<-c(PnegXM.CAU,PnegXM.AFA,PnegXM.HIS,PnegXM.ASI)

val<-sum(PnegXM * weights)
val<-round(val,decimals)

return(val)
} #function