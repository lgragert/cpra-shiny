#' Generates S1-S5 tables by pulling HLA-antigen frequency data for all antigens from tables.
#'
#' @param Vector containing HLA-A antigens
#' @param Vector containing HLA-B antigens
#' @param Vector containing HLA-C antigens
#' @param Vector containing HLA-Q antigens
#' @param Vector containing HLA-R antigens 
#' 
#' @return List containing the S1-S5 tables.
#'
#' @examples
#' \dontrun{
#' pak_apply(A.excludes,B.excludes,C.excludes,Q.excludes,R.excludes)
#' }
#'  
#' @export
pak_apply<-function(vector.A,vector.B,vector.C,vector.Q,vector.R) {
  
#S1
S1<-list()
S1[[1]]<-lapply(vector.A, function(x){ Tables[[1]][Tables[[1]][,1]==x,] })
S1[[2]]<-lapply(vector.B, function(x){ Tables[[2]][Tables[[2]][,1]==x,] })
S1[[3]]<-lapply(vector.C, function(x){ Tables[[3]][Tables[[3]][,1]==x,] })
S1[[4]]<-lapply(vector.Q, function(x){ Tables[[4]][Tables[[4]][,1]==x,] })
S1[[5]]<-lapply(vector.R, function(x){ Tables[[5]][Tables[[5]][,1]==x,] })
resS1<-matrix(,0,5)
for (i in 1:length(S1)) {
  if (!(is.null(unlist(S1[[i]])))) {
    resS1<-rbind(resS1,matrix(unlist(S1[[i]]),ncol=5,byrow=TRUE))
  } else {
    resS1<-rbind(resS1,matrix(0,1,5))
  } #ifelse
} #for

#S2
S2<-list()
S2[[1]]<-lapply(vector.A, function(x) lapply(vector.B, function(y) { Tables[[6]][((Tables[[6]][,1]==x)&(Tables[[6]][,2]==y)),] }))
S2[[2]]<-lapply(vector.A, function(x) lapply(vector.C, function(y) { Tables[[7]][((Tables[[7]][,1]==x)&(Tables[[7]][,2]==y)),] }))
S2[[3]]<-lapply(vector.A, function(x) lapply(vector.Q, function(y) { Tables[[8]][((Tables[[8]][,1]==x)&(Tables[[8]][,2]==y)),] }))
S2[[4]]<-lapply(vector.A, function(x) lapply(vector.R, function(y) { Tables[[9]][((Tables[[9]][,1]==x)&(Tables[[9]][,2]==y)),] }))
S2[[5]]<-lapply(vector.B, function(x) lapply(vector.C, function(y) { Tables[[10]][((Tables[[10]][,1]==x)&(Tables[[10]][,2]==y)),] }))
S2[[6]]<-lapply(vector.B, function(x) lapply(vector.Q, function(y) { Tables[[11]][((Tables[[11]][,1]==x)&(Tables[[11]][,2]==y)),] }))
S2[[7]]<-lapply(vector.B, function(x) lapply(vector.R, function(y) { Tables[[12]][((Tables[[12]][,1]==x)&(Tables[[12]][,2]==y)),] }))
S2[[8]]<-lapply(vector.Q, function(x) lapply(vector.C, function(y) { Tables[[13]][((Tables[[13]][,1]==x)&(Tables[[13]][,2]==y)),] }))
S2[[9]]<-lapply(vector.R, function(x) lapply(vector.C, function(y) { Tables[[14]][((Tables[[14]][,1]==x)&(Tables[[14]][,2]==y)),] }))
S2[[10]]<-lapply(vector.R, function(x) lapply(vector.Q, function(y) { Tables[[15]][((Tables[[15]][,1]==x)&(Tables[[15]][,2]==y)),] }))
resS2<-matrix(,0,6)
for (i in 1:length(S2)) {
  if (!(is.null(unlist(S2[[i]])))) {
    resS2<-rbind(resS2,matrix(unlist(S2[[i]]),ncol=6,byrow=TRUE))
  } else {
    resS2<-rbind(resS2,matrix(0,1,6))
  } #ifelse
} #for

#S3
S3<-list()
S3[[1]]<-lapply(vector.A, function(x) lapply(vector.B, function(y) lapply(vector.C, function(z) { Tables[[16]][((Tables[[16]][,1]==x)&(Tables[[16]][,2]==y)&(Tables[[16]][,3]==z)),] })))
S3[[2]]<-lapply(vector.A, function(x) lapply(vector.B, function(y) lapply(vector.Q, function(z) { Tables[[17]][((Tables[[17]][,1]==x)&(Tables[[17]][,2]==y)&(Tables[[17]][,3]==z)),] })))
S3[[3]]<-lapply(vector.A, function(x) lapply(vector.B, function(y) lapply(vector.R, function(z) { Tables[[18]][((Tables[[18]][,1]==x)&(Tables[[18]][,2]==y)&(Tables[[18]][,3]==z)),] })))
S3[[4]]<-lapply(vector.A, function(x) lapply(vector.Q, function(y) lapply(vector.C, function(z) { Tables[[19]][((Tables[[19]][,1]==x)&(Tables[[19]][,2]==y)&(Tables[[19]][,3]==z)),] })))
S3[[5]]<-lapply(vector.A, function(x) lapply(vector.R, function(y) lapply(vector.C, function(z) { Tables[[20]][((Tables[[20]][,1]==x)&(Tables[[20]][,2]==y)&(Tables[[20]][,3]==z)),] })))
S3[[6]]<-lapply(vector.A, function(x) lapply(vector.R, function(y) lapply(vector.Q, function(z) { Tables[[21]][((Tables[[21]][,1]==x)&(Tables[[21]][,2]==y)&(Tables[[21]][,3]==z)),] })))
S3[[7]]<-lapply(vector.B, function(x) lapply(vector.Q, function(y) lapply(vector.C, function(z) { Tables[[22]][((Tables[[22]][,1]==x)&(Tables[[22]][,2]==y)&(Tables[[22]][,3]==z)),] })))
S3[[8]]<-lapply(vector.B, function(x) lapply(vector.R, function(y) lapply(vector.C, function(z) { Tables[[23]][((Tables[[23]][,1]==x)&(Tables[[23]][,2]==y)&(Tables[[23]][,3]==z)),] })))
S3[[9]]<-lapply(vector.B, function(x) lapply(vector.R, function(y) lapply(vector.Q, function(z) { Tables[[24]][((Tables[[24]][,1]==x)&(Tables[[24]][,2]==y)&(Tables[[24]][,3]==z)),] })))
S3[[10]]<-lapply(vector.R, function(x) lapply(vector.Q, function(y) lapply(vector.C, function(z) { Tables[[25]][((Tables[[25]][,1]==x)&(Tables[[25]][,2]==y)&(Tables[[25]][,3]==z)),] })))
resS3<-matrix(,0,7)
for (i in 1:length(S3)) {
  if (!(is.null(unlist(S3[[i]])))) {
    resS3<-rbind(resS3,matrix(unlist(S3[[i]]),ncol=7,byrow=TRUE))
  } else {
    resS3<-rbind(resS3,matrix(0,1,7))
  } #ifelse
} #for

#S4
S4<-list()
S4[[1]]<-lapply(vector.A, function(x) lapply(vector.B, function(y) lapply(vector.Q, function(z) lapply(vector.C, function(a) { Tables[[26]][((Tables[[26]][,1]==x)&(Tables[[26]][,2]==y)&(Tables[[26]][,3]==z)&(Tables[[26]][,4]==a)),] }))))
S4[[2]]<-lapply(vector.A, function(x) lapply(vector.B, function(y) lapply(vector.R, function(z) lapply(vector.C, function(a) { Tables[[27]][((Tables[[27]][,1]==x)&(Tables[[27]][,2]==y)&(Tables[[27]][,3]==z)&(Tables[[27]][,4]==a)),] }))))
S4[[3]]<-lapply(vector.A, function(x) lapply(vector.B, function(y) lapply(vector.R, function(z) lapply(vector.Q, function(a) { Tables[[28]][((Tables[[28]][,1]==x)&(Tables[[28]][,2]==y)&(Tables[[28]][,3]==z)&(Tables[[28]][,4]==a)),] }))))
S4[[4]]<-lapply(vector.A, function(x) lapply(vector.R, function(y) lapply(vector.Q, function(z) lapply(vector.C, function(a) { Tables[[29]][((Tables[[29]][,1]==x)&(Tables[[29]][,2]==y)&(Tables[[29]][,3]==z)&(Tables[[29]][,4]==a)),] }))))
S4[[5]]<-lapply(vector.B, function(x) lapply(vector.R, function(y) lapply(vector.Q, function(z) lapply(vector.C, function(a) { Tables[[30]][((Tables[[30]][,1]==x)&(Tables[[30]][,2]==y)&(Tables[[30]][,3]==z)&(Tables[[30]][,4]==a)),] }))))
resS4<-matrix(,0,8)
for (i in 1:length(S4)) {
  if (!(is.null(unlist(S4[[i]])))) {
    resS4<-rbind(resS4,matrix(unlist(S4[[i]]),ncol=8,byrow=TRUE))
  } else {
    resS4<-rbind(resS4,matrix(0,1,8))
  } #ifelse
} #for

#S5
S5<-lapply(vector.A, function(x) lapply(vector.B, function(y) lapply(vector.R, function(z) lapply(vector.Q, function(a) lapply(vector.C, function(b) { Tables[[31]][((Tables[[31]][,1]==x)&(Tables[[31]][,2]==y)&(Tables[[31]][,3]==z)&(Tables[[31]][,4]==a)&(Tables[[31]][,5]==b)),] })))))
if (!(is.null(unlist(S5)))) {
  resS5<-matrix(unlist(S5),ncol=9,byrow=TRUE)
} else {
  resS5<-matrix(0,1,9)
} #ifelse

out<-list(resS1,resS2,resS3,resS4,resS5)
return(out)

} #fun