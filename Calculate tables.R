packages<-c("")
lapply(packages, require, character.only=T)

#Load clinical and filter what you want
setwd("/Users/evankransdorf/Documents/Academic/Programming/R/NMDP CPRA/Make 8 locus 092417")
  

##How many combinations?
result<-data.frame(NULL)
 
loci<-9
for (i in 1:loci) {
  result[i,1]<-i
  result[i,2]<-( factorial(loci) / (factorial(loci-i) * factorial(i)) )
}

sum(result[[2]]) 
#511 tables


##5 locus case
result<-NULL
loci<-c("A","B","C","DQA1","DQB1")
for (i in 1:5) {
  temp<-combn(loci,i)
  for (j in 1:ncol(temp)) {
    temp2<-paste0(temp[,j],collapse="~")
    result<-c(result,temp2)
  } #for j
} #for i


##9 locus case
result<-NULL
loci<-c("A","B","C","DQA1","DQB1","DRB3/4/5","DRB1","DPA1","DPB1")
for (i in 1:9) {
  temp<-combn(loci,i)
  for (j in 1:ncol(temp)) {
    temp2<-paste0(temp[,j],collapse="~")
    result<-c(result,temp2)
  } #for j
} #for i
