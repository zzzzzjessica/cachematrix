## Put comments here that give an overall description of what your
## functions do
## This function creates a matrix, the function consists set, get,
## setinverse, and getinverse.

## Write a short comment describing this function
## I use "inv" to represent inverse, and set "inv" as a null.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinv<-function(inverse)inv<<-inverse
  getinv<-function()inv
  list(set=set,get=get,
       setinv=setinv,
       getinv=getinv)
}


## Write a short comment describing this function
## This function calculate the inverse of the matrix created by the function
## above.

cacheSolve <- function(x, ...) {
  inv<-x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat, ...)
  x$setinv(inv)
  inv
}
