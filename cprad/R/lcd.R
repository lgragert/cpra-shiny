#' Generates CPRAd from vectors of unacceptable antigens
#'
#' @param Vector containing HLA-A antigens
#' @param Vector containing HLA-B antigens
#' @param Vector containing HLA-Bw antigens
#' @param Vector containing HLA-C antigens
#' @param Vector containing HLA-Q antigens
#' @param Vector containing HLA-R antigens 
#' 
#' @return Likelihood of a compatible donor.
#'
#' @examples
#' \dontrun{
#' lcd<-function(vector.A,vector.B,vector.Bw,vector.C,vector.Q,vector.R)
#' }
#'  
#' @export
lcd<-function(vector.A,vector.B,vector.Bw,vector.C,vector.Q,vector.R) {

E<-pak_equiv(vector.A,vector.B,vector.Bw,vector.C,vector.Q,vector.R)
working.tables<-pak_apply(E[[1]],E[[2]],E[[3]],E[[4]],E[[5]])
final.result<-pak_calc(working.tables,ethnic.weights,5)

if (final.result >= 0.5) {
  print(paste0("1 in ",round((1/(1-final.result)),0)))
} else { print("") }

} #fun