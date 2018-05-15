#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param table.equiv PARAM_DESCRIPTION
#' @param input PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname equiv
#' @export 
equiv<-function(table.equiv,input) { 
  
  v<-NULL
  out<-list(A=v,B=v,C=v,Q=v,R=v)
  
  for (i in 1:5) {
    x<-NULL
    if ( !anyNA(input[[i]]) ) { #if there are any not NA values
      temp<-table.equiv[[i]][list(input[[i]]),-c(1)]
      temp<-data.frame(temp)
      for (j in 1:nrow(temp)) {
        x<-c(x,temp[j,!is.na(temp[j,])])
      } #for
      x<-unlist(x)
      if (!is.null(x)) { #this is needed because if x is NULL this list element is automatically removed!
        out[[i]]<-unique(x)
      } #if
    } #if
  } #for
  
  return(out)
  
} #function
