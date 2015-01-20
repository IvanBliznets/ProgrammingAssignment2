## makeCacheMatrix, cacheSolve --- functions which creates
## cached version of matrix in which inverse value of matrix is
## stored in "cache" in order to speed up computations
 

## makeCacheMatrix creates cached vesrion of 
## input matrix. Cached version of a matrix stores matrix and
## also inverse of the matrix if it was computed previously 
makeCacheMatrix <- function(x = matrix()) {
  inv=NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinv<-function(inverse){
    inv<<-inverse
  }
  getinv<-function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve outputs inverse of cached version of
## matrix and stores this value if it was computed first time
cacheSolve <- function(x, ...) {
  inverse<-x$getinv()
  if(!is.null(inverse))
  {
    message("getting cached inverse")
    return(inverse)
  }
  data<-x$get()
  ans<-solve(data,...)
  x$setinv(ans)
  ans
}