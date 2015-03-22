## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  #sets value of the matrix
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  #gets value of the matrix
  get<-function() x
  
  #sets value of inverse 
  setInverse<-function(solve) m<<- solve
  
  #gets value of inverse
  getInverse<-function() m
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}

cacheSolve <- function(x, ...) {
  #checks if inverse already computed
  inv<-x$getInverse()
  #returns inverse if already computed
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  #computes inverse if not computed
  matrix<-x$get()
  inverse<-solve(matrix, ...)
  x$setInverse(inv)
  #returns computed inverse
  inv
}
