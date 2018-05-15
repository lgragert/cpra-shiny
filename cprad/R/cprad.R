#' Generates CPRAd from vectors of unacceptable antigens
#'
#' @param Vector containing HLA-A antigens
#' @param Vector containing HLA-B antigens
#' @param Vector containing HLA-Bw antigens
#' @param Vector containing HLA-C antigens
#' @param Vector containing HLA-Q antigens
#' @param Vector containing HLA-R antigens 
#' @param Number of decimal points 
#' 
#' @return CPRAd calculated from UNOS tables.
#'
#' @examples
#' \dontrun{
#' cprad<-function(vector.A,vector.B,vector.Bw,vector.C,vector.Q,vector.R,decimals)
#' }
#'  
#' @export
cprad<-function(vector.A,vector.B,vector.Bw,vector.C,vector.Q,vector.R,decimals) {

E<-pak_equiv(vector.A,vector.B,vector.Bw,vector.C,vector.Q,vector.R)
working.tables<-pak_apply(E[[1]],E[[2]],E[[3]],E[[4]],E[[5]])
final.result<-pak_calc(working.tables,ethnic.weights,decimals)
return(final.result)

} #fun