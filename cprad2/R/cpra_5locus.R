#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param excludes PARAM_DESCRIPTION
#' @param DT PARAM_DESCRIPTION
#' @param ethnic.weights PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname cpra_5locus
#' @export 
cpra_5locus<-function(excludes,DT,ethnic.weights) { 
  
  need<-c("A","B","C","Q","R")
  z<-rep(0,length(need))
  S1<-data.table("CAU"=z,"AFA"=z,"HIS"=z,"API"=z)
  for (i in 1:length(need)) { #would make this an apply
    cols<-unlist(str_extract_all(need[i],"")) 
    DT<-setkeyv(DT,cols) #set key
    ex<-unlist(excludes[[cols[1]]]) #make ex = excludes[Ag]
    if ( !is.null(ex) ) {
      temp<-DT[list(ex),]
      S1[i,names(S1) := (temp[,lapply(.SD,sum,na.rm=TRUE),.SDcols=c("CAU","AFA","HIS","API")]) ]
    } #if
  } #for
  
  need<-c("AB","AC","AR","AQ","BC","BR","BQ","CR","CQ","RQ")
  z<-rep(0,length(need))
  S2<-data.table("CAU"=z,"AFA"=z,"HIS"=z,"API"=z)
  for (i in 1:length(need)) { #would make this an apply
    cols<-unlist(str_extract_all(need[i],"")) 
    DT<-setkeyv(DT,cols) #set key
    ex1<-unlist(excludes[[cols[1]]]) #make ex = excludes[Ag]
    ex2<-unlist(excludes[[cols[2]]])
    if ( !(is.null(ex1) | is.null(ex2)) ) {
      temp<-DT[list(expand.grid(ex1,ex2)),]
      S2[i,names(S2) := (temp[,lapply(.SD,sum,na.rm=TRUE),.SDcols=c("CAU","AFA","HIS","API")]) ]
    } #if
  } #for
  
  need<-c("ABC","ABQ","ABR","AQC","ARC","ARQ","BQC","BRC","BRQ","RQC")
  z<-rep(0,length(need))
  S3<-data.table("CAU"=z,"AFA"=z,"HIS"=z,"API"=z)
  for (i in 1:length(need)) { #would make this an apply
    cols<-unlist(str_extract_all(need[i],"")) 
    DT<-setkeyv(DT,cols) #set key
    ex1<-unlist(excludes[[cols[1]]])
    ex2<-unlist(excludes[[cols[2]]])
    ex3<-unlist(excludes[[cols[3]]])
    if ( !(is.null(ex1) | is.null(ex2) | is.null(ex3)) ) {
      temp<-DT[list(expand.grid(ex1,ex2,ex3)),]
      S3[i,names(S3) := (temp[,lapply(.SD,sum,na.rm=TRUE),.SDcols=c("CAU","AFA","HIS","API")]) ]
    } #if
  } #for
  
  need<-c("ABQC","ABRC","ABRQ","ARQC","BRQC")
  z<-rep(0,length(need))
  S4<-data.table("CAU"=z,"AFA"=z,"HIS"=z,"API"=z)
  for (i in 1:length(need)) { 
    cols<-unlist(str_extract_all(need[i],""))
    DT<-setkeyv(DT,cols)
    ex1<-unlist(excludes[[cols[1]]])
    ex2<-unlist(excludes[[cols[2]]])
    ex3<-unlist(excludes[[cols[3]]])
    ex4<-unlist(excludes[[cols[4]]])
    if ( !(is.null(ex1) | is.null(ex2) | is.null(ex3) | is.null(ex4)) ) {
      temp<-DT[list(expand.grid(ex1,ex2,ex3,ex4)),]
      S4[i,names(S4) := (temp[,lapply(.SD,sum,na.rm=TRUE),.SDcols=c("CAU","AFA","HIS","API")]) ]
    } #if
  } #for

  need<-c("ABCQR")
  z<-rep(0,length(need))
  S5<-data.table("CAU"=z,"AFA"=z,"HIS"=z,"API"=z)
  cols<-unlist(str_extract_all(need,""))
  DT<-setkeyv(DT,cols)
  ex1<-unlist(excludes[[cols[1]]])
  ex2<-unlist(excludes[[cols[2]]])
  ex3<-unlist(excludes[[cols[3]]])
  ex4<-unlist(excludes[[cols[4]]])
  ex5<-unlist(excludes[[cols[5]]])
  if ( !(is.null(ex1) | is.null(ex2) | is.null(ex3) | is.null(ex4) | is.null(ex5)) ) {
    temp<-DT[list(expand.grid(ex1,ex2,ex3,ex4,ex5)),]
    S5[1,names(S5) := (temp[,lapply(.SD,sum,na.rm=TRUE),.SDcols=c("CAU","AFA","HIS","API")]) ]
  } #if

  ##Equation
  #CPRA-eth= 1 – (1 – S1 + S2 – S3 + S4 – S5)2
  vector<-1 - (1 - (S1[,lapply(.SD,sum,na.rm=TRUE),.SDcols=c("CAU","AFA","HIS","API")]) + (S2[,lapply(.SD,sum,na.rm=TRUE),.SDcols=c("CAU","AFA","HIS","API")]) - (S3[,lapply(.SD,sum,na.rm=TRUE),.SDcols=c("CAU","AFA","HIS","API")]) + (S4[,lapply(.SD,sum,na.rm=TRUE),.SDcols=c("CAU","AFA","HIS","API")]) - (S5[,lapply(.SD,sum,na.rm=TRUE),.SDcols=c("CAU","AFA","HIS","API")]) )^2
  
  return(sum(vector*ethnic.weights))

} #function
