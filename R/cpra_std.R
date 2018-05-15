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
#' @rdname cpra_std
#' @export 
cpra_std<-function(excludes,DT,ethnic.weights) { 
  
  need<-c("A","B","C","Q","R")
  need<-c(need,"AB","AC","AQ","AR","BC","BQ","BR","QC","RC","RQ")
  need<-c(need,"ABC","ABQ","ABR","AQC","ARC","ARQ","BQC","BRC","BRQ","RQC")
  need<-c(need,"ABQC","ABRC","ABRQ","ARQC","BRQC")
  need<-c(need,"ABRQC")
  
  z<-rep(0,5)
  S1<-data.table("Caucasian"=z,"African.American"=z,"Hispanic"=z,"Asian"=z)
  j<-1
  k<-5
  for (i in j:k) {
    cols<-unlist(str_extract_all(need[i],"")) 
    DT[[i]]<-setkeyv(DT[[i]],cols) #set key
    ex<-unlist(excludes[[cols[1]]]) #make ex = excludes[Ag]
    if ( !is.null(ex) ) {
      temp<-DT[[i]][list(ex),]
      S1[i,names(S1) := (temp[,lapply(.SD,sum,na.rm=TRUE),.SDcols=c("Caucasian","African.American","Hispanic","Asian")]) ]
    } #if
  } #for
  
  z<-rep(0,10)
  S2<-data.table("Caucasian"=z,"African.American"=z,"Hispanic"=z,"Asian"=z)
  j<-6
  k<-15
  for (i in j:k) {
    cols<-unlist(str_extract_all(need[i],"")) 
    DT[[i]]<-setkeyv(DT[[i]],cols) #set key
    ex1<-unlist(excludes[[cols[1]]]) #make ex = excludes[Ag]
    ex2<-unlist(excludes[[cols[2]]])
    if ( !(is.null(ex1) | is.null(ex2)) ) {
      temp<-DT[[i]][list(expand.grid(ex1,ex2)),]
      S2[(i-5),names(S2) := (temp[,lapply(.SD,sum,na.rm=TRUE),.SDcols=c("Caucasian","African.American","Hispanic","Asian")]) ]
    } #if
  } #for
  
  z<-rep(0,10)
  S3<-data.table("Caucasian"=z,"African.American"=z,"Hispanic"=z,"Asian"=z)
  j<-16
  k<-25
  for (i in j:k) {
    cols<-unlist(str_extract_all(need[i],"")) 
    DT[[i]]<-setkeyv(DT[[i]],cols) #set key
    ex1<-unlist(excludes[[cols[1]]])
    ex2<-unlist(excludes[[cols[2]]])
    ex3<-unlist(excludes[[cols[3]]])
    if ( !(is.null(ex1) | is.null(ex2) | is.null(ex3)) ) {
      temp<-DT[[i]][list(expand.grid(ex1,ex2,ex3)),]
      S3[(i-15),names(S3) := (temp[,lapply(.SD,sum,na.rm=TRUE),.SDcols=c("Caucasian","African.American","Hispanic","Asian")]) ]
    } #if
  } #for
  
  z<-rep(0,5)
  S4<-data.table("Caucasian"=z,"African.American"=z,"Hispanic"=z,"Asian"=z)
  j<-26
  k<-30
  for (i in j:k) {
    cols<-unlist(str_extract_all(need[i],""))
    DT[[i]]<-setkeyv(DT[[i]],cols) #set key
    ex1<-unlist(excludes[[cols[1]]])
    ex2<-unlist(excludes[[cols[2]]])
    ex3<-unlist(excludes[[cols[3]]])
    ex4<-unlist(excludes[[cols[4]]])
    if ( !(is.null(ex1) | is.null(ex2) | is.null(ex3) | is.null(ex4)) ) {
      temp<-DT[[i]][list(expand.grid(ex1,ex2,ex3,ex4)),]
      S4[(i-25),names(S4) := (temp[,lapply(.SD,sum,na.rm=TRUE),.SDcols=c("Caucasian","African.American","Hispanic","Asian")]) ]
    } #if
  } #for

  z<-rep(0,1)
  S5<-data.table("Caucasian"=z,"African.American"=z,"Hispanic"=z,"Asian"=z)
  i<-31
  cols<-unlist(str_extract_all(need[i],""))
  DT[[i]]<-setkeyv(DT[[i]],cols) #set key
  ex1<-unlist(excludes[[cols[1]]])
  ex2<-unlist(excludes[[cols[2]]])
  ex3<-unlist(excludes[[cols[3]]])
  ex4<-unlist(excludes[[cols[4]]])
  ex5<-unlist(excludes[[cols[5]]])
  if ( !(is.null(ex1) | is.null(ex2) | is.null(ex3) | is.null(ex4) | is.null(ex5)) ) {
    temp<-DT[[i]][list(expand.grid(ex1,ex2,ex3,ex4,ex5)),]
    S5[1,names(S5) := (temp[,lapply(.SD,sum,na.rm=TRUE),.SDcols=c("Caucasian","African.American","Hispanic","Asian")]) ]
  } #if

  ##Equation
  #CPRA-eth= 1 – (1 – S1 + S2 – S3 + S4 – S5)2
  vector<-1 - (1 - (S1[,lapply(.SD,sum,na.rm=TRUE),.SDcols=c("Caucasian","African.American","Hispanic","Asian")]) + (S2[,lapply(.SD,sum,na.rm=TRUE),.SDcols=c("Caucasian","African.American","Hispanic","Asian")]) - (S3[,lapply(.SD,sum,na.rm=TRUE),.SDcols=c("Caucasian","African.American","Hispanic","Asian")]) + (S4[,lapply(.SD,sum,na.rm=TRUE),.SDcols=c("Caucasian","African.American","Hispanic","Asian")]) - (S5[,lapply(.SD,sum,na.rm=TRUE),.SDcols=c("Caucasian","African.American","Hispanic","Asian")]) )^2
  
  return(sum(vector*ethnic.weights))

} #function
