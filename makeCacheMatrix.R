##a pair of function that is the inverse of the matrix
##this function creates a special matrix and that can cache its inverse
makeCacheMatrix<-function(x=matrix()){
  j<-NULL
  set<-function(y){
  x<<-y
  j<<-NULL
  }
  get<-function()x
  setInverse<-function(inverse) j<<-inverse
  getInverse<-function()j
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}
##this is used to find the inverse of the special matrix returned by the 
##makeCacheMatrix.

cacheSolve<-function(x,...){
##inverse of the x  
  j<-x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
    
  }
  mat<-x$get()
  j<-solve(mat,...)
  x$getInverse(j)
  j
}