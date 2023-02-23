#' @export
detect_sign=function(x){
  if(abs(min(x))>max(x)){
    x=(-x)
  }
  return(x)
}

#' @export
correct_sign=function(x){
  y<-apply(x,2,detect_sign)
  
  return(y)
}