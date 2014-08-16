## Matrix inversion is usually a costly computation 
## and their may be some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly.
## Functions in this file solve this problem.

## This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(matr = matrix()) {
  cachedSolve <- NULL
  set <- function(newMatr) {
    matr <<- newMatr
    cachedSolve <<- NULL
  }
  get <- function() matr
  setSolve <- function(solve) cachedSolve <<- solve
  getSolve <- function() cachedSolve
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cachedSolve <- x$getSolve()
  if(!is.null(cachedSolve)) {
    message("getting cached data")
    return(cachedSolve)
  }
  data <- x$get()
  cachedSolve <- solve(data, ...)
  x$setSolve(cachedSolve)
  cachedSolve
}

matrixCacheTest <- function(){
  matr1 <- makeCacheMatrix(matrix(c(2,0,0,4),nrow=2,ncol=2))
  print(cacheSolve(matr1))
  print(cacheSolve(matr1))
  matr1$set(matrix(c(2,0,0,3,-2,0,0,0,0.5),nrow=3,ncol=3))
  print(cacheSolve(matr1))
  print(cacheSolve(matr1))
}