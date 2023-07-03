#' @export
detect_sign=function(x){
  if(abs(min(x))>max(x)){
    x=(-x)
  }
  return(x)
}

#' @export
correct_sign=function(data){
  A = data@reductions$ica@feature.loadings
  S = data@reductions$ica@cell.embeddings
  i=1
  while(i<=dim(A)[2]){
    if(abs(min(A[,i]))>max(A[,i])){
      A[,i]=(-A[,i])
      S[,i]=(-S[,i])          
    }
    i=i+1
  }
  
  data@reductions$ica@feature.loadings = A
  data@reductions$ica@cell.embeddings = S
  
  return(data)
}
