#' Calculates HLA antigen equivalencies.
#'
#' @param Vector containing HLA-A antigens
#' @param Vector containing HLA-B antigens
#' @param Vector containing HLA-Bw antigens
#' @param Vector containing HLA-C antigens
#' @param Vector containing HLA-Q antigens
#' @param Vector containing HLA-R antigens 
#'
#' @return List of vectors, each containing the HLA-A, -B, -C, -Q, and -R antigens now with their equivalent antigens.
#'
#' @examples
#' \dontrun{
#' pak_equiv(A.excludes,B.excludes,Bw.exludes,C.excludes,Q.excludes,R.excludes)
#' }
#' 
#' @export
pak_equiv<-function(in.A,in.B,in.Bw,in.C,in.Q,in.R) {
    
out.A<-unique(unlist(lapply(in.A, function(x){ Equiv[[1]][(Equiv[[1]][,1]==x),2] }))) 

out.B<-unlist(lapply(in.B, function(x){ Equiv[[2]][(Equiv[[2]][,1]==x),2] }))
  
out.B<-c(out.B,unlist(lapply(in.Bw, function(x){ Equiv[[3]][(Equiv[[3]][,1]==x),2] }))) 
out.B<-unique(out.B)

out.C<-unique(unlist(lapply(in.C, function(x){ Equiv[[4]][(Equiv[[4]][,1]==x),2] })))

out.Q<-unique(unlist(lapply(in.Q, function(x){ Equiv[[5]][(Equiv[[5]][,1]==x),2] })))

out.R<-unique(unlist(lapply(in.R, function(x){ Equiv[[6]][(Equiv[[6]][,1]==x),2] })))

out<-list(out.A,out.B,out.C,out.Q,out.R)
return(out)

} #function