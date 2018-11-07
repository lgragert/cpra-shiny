# Gebel_UAs.R
#
# Compute NMDP and UNOS CPRA values on Emory cases from Howie Gebel

if(!require("data.table"))
  install.packages("data.table")
if(!require("stringr"))
  install.packages("stringr")
if(!require("rstudioapi"))
  install.packages("rstudioapi")

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load cprad2 functions
source("./cprad2/R/cpra_5locus.R")
source("./cprad2/R/equiv.R")
source("./cprad2/R/cpra_std.R")

# load Rdata files for haplotype frequencies
RDA <-list.files(path = "./cprad2/data/")
for (file in RDA) {
  path = "./cprad2/data/"
  filename = paste0(path,file)
  load (filename)
}

# Create input string of UAs for case 563578727 from spreadsheet
# TODO - load all cases from flat file

input = {}
# input$string="A1,A2,A23,A25,A26,A29,A3,A30,A31,A32,A33,A34,A36,A43,A66,A68,A69,A74,A80,B13,B18,B27,B37,B38,B39,B41,B42,B44,B45,B47,B48,B49,B50,B52,B54,B55,B56,B57,B58,B59,B60,B61,B62,B63,B64,B65,B67,B7,B71,B72,B73,B75,B76,B77,B8,B81,B82,C15,C17,DQ6,DR1,DR10,DR103,DR13,DR14,DR16,DR17,DR18"
input$string="A1 A2 A23 A25 A26 A29 A3 A30 A31 A32 A33 A34 A36 A43 A66 A68 A69 A74 A80 B13 B18 B27 B37 B38 B39 B41 B42 B44 B45 B47 B48 B49 B50 B52 B54 B55 B56 B57 B58 B59 B60 B61 B62 B63 B64 B65 B67 B7 B71 B72 B73 B75 B76 B77 B8 B81 B82 C15 C17 DQ6 DR1 DR10 DR103 DR13 DR14 DR16 DR17 DR18"

# input processing from server.R
# TODO make into R function that processes one list of UAs at a time

# initialize table.allele.sero data.table and data data.frame

table.allele.sero<-data.table(table.allele.sero)
table.allele.sero<-table.allele.sero[,Allele:=as.character(Allele)] #PROB: strings turning into factors?
table.allele.sero<-table.allele.sero[,Locus.Antigen:=as.character(Locus.Antigen)]
table.allele.sero<-setkey(table.allele.sero,Locus.Antigen) #set key

data = {}

data$s <- NULL
data$ag <- NULL
data$all <- NULL
data$df.all <- data.frame(A=NA,B=NA,C=NA,DQ=NA,DR=NA)
data$df.ag <- data.frame(A=NA,B=NA,C=NA,DQ=NA,DR=NA)

#Process input string; separate into antigens and alleles
if (!(is.null(input$string))) {
  data$s <- unlist(str_split(input$string,"[ ,;//]"))
} #if

for (i in 1:length(data$s)) {
  if (grepl("\\*",data$s[i])) {
    data$all<-c(data$all,data$s[i])
  } else { data$ag<-c(data$ag,data$s[i]) }
} #for

#If antigens are input
if (!(is.null(data$ag))) {

  #Determine equivalent antigens for input antigens
  strip<-function(ag,locus) { #if empty will return numeric(0)
    x<-ag[grep(locus,ag)]
    x<-as.numeric(str_extract(x,"\\d+"))
    return(x)
  } #fun
  A<-unlist(sapply(data$ag,strip,"A"))
  if (length(A)==0) { A<-NA }
  B<-unlist(sapply(data$ag,strip,"B"))
  if (length(B)==0) { B<-NA }
  C<-unlist(sapply(data$ag,strip,"C"))
  if (length(C)==0) { C<-NA }
  Q<-unlist(sapply(data$ag,strip,"DQ"))
  if (length(Q)==0) { Q<-NA }
  R<-unlist(sapply(data$ag,strip,"DR"))
  if (length(R)==0) { R<-NA }
  excludes<-list(A=A,B=B,C=C,Q=Q,R=R)
  excludes2<-equiv(table.equiv,excludes)

  #Display antigens as data.frame via DT package
  for (i in 1:length(excludes2)) {
    for (j in 1:length(excludes2[[i]])) {
      if (!is.null(excludes2[[i]][j])) {
        data$df.ag[j,i]<-excludes2[[i]][j]
      } else { data$df.ag[j,i]<-NA }
    } #for j
  } #for i
  colnames(data$df.ag)<-c("A","B","C","DQ","DR")

  #Convert antigens to alleles
  convert<-function(ag,locus) { #if empty will return numeric(0)
    locuskey<-paste0(locus,ag)
    temp<-table.allele.sero[list(locuskey),"Allele"]
  } #fun
  A<-unlist(sapply(excludes2["A"],convert,"A"))
  B<-unlist(sapply(excludes2["B"],convert,"B"))
  C<-unlist(sapply(excludes2["C"],convert,"C"))
  Q<-unlist(sapply(excludes2["Q"],convert,"DQ"))
  R<-unlist(sapply(excludes2["R"],convert,"DR"))
  excludes3<-list(A=A,B=B,C=C,Q=Q,R=R)
} else {
  excludes2<-list(A=NULL,B=NULL,C=NULL,Q=NULL,R=NULL)
  excludes3<-list(A=NA,B=NA,C=NA,Q=NA,R=NA)
} #else

#Add any input alleles to excludes3 and remove dups
excludes4<-list(A=NULL,B=NULL,C=NULL,Q=NULL,R=NULL)

strip2<-function(ag,locus) { #if empty will return numeric(0)
  x<-ag[grep(locus,ag)]
  x<-str_extract(x,"\\d+:\\d+")
  return(x)
} #fun
A<-unlist(sapply(data$all,strip2,"A"))
excludes4[[1]]<-unique(na.omit(c(A,excludes3[[1]])))
B<-unlist(sapply(data$all,strip2,"B"))
excludes4[[2]]<-unique(na.omit(c(B,excludes3[[2]])))
C<-unlist(sapply(data$all,strip2,"C"))
excludes4[[3]]<-unique(na.omit(c(C,excludes3[[3]])))
Q<-unlist(sapply(data$all,strip2,"DQB1"))
excludes4[[4]]<-unique(na.omit(c(Q,excludes3[[4]])))
R<-unlist(sapply(data$all,strip2,"DRB1"))
excludes4[[5]]<-unique(na.omit(c(R,excludes3[[5]])))

#Display alleles as data.frame via DT package
data$df.all<-data.frame(NULL)
for (i in 1:length(excludes4)) {
  for (j in 1:length(excludes4[[i]])) {
    if (!is.null(excludes4[[i]][j])) {
      data$df.all[j,i]<-excludes4[[i]][j]
    } else { data$df.all[j,i]<-NA }
  } #for j
} #for i
colnames(data$df.all)<-c("A","B","C","DQ","DR")

# excludes2 are antigens
# excludes3 are alleles with strange labels
# excludes4 are alleles - SLOW!

# data$nmdp<-round(cpra_5locus(excludes2,table.5l.nmdp.allele,ethnic.weights),6)*100

# table.haplo are UNOS antigen frequencies
# table.5l.nmdp.ag are NMDP antigen frequencies
# data$nmdp<-cpra_5locus(excludes4,table.5l.nmdp.allele,ethnic.weights) # SLOW and takes too much memory
data$nmdp<-cpra_5locus(excludes2,table.5l.nmdp.ag,ethnic.weights) # FAST
data$unos<-cpra_std(excludes2,table.haplo,ethnic.weights)

cpra_nmdp = data$nmdp*100
cpra_unos = data$unos*100

# LCD - likelihood of compatible donor
lcd_nmdp = 1-data$nmdp
lcd_unos = 1-data$unos

# show data
cpra_nmdp
cpra_unos
lcd_nmdp
lcd_unos
