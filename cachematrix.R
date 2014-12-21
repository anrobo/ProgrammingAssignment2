## These functions create a matrix object, caches the inverse of that object
## solve that inverse of the object, and if the matrix has not changed,  
## pulls the inverse from the cache.

## The makeCacheMatrix function takes an input matrix, solves the inverse
## of the matrix and sets that solution in the cache, and returns
## the inverse to the following function.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setsolve <- function(solve)m <<- solve
  getsolve <- function()m
  list(set = set, get = get, 
       setsolve = setsolve, 
       getsolve = getsolve)

}

## The cacheSolve function takes the input 'x' which was cached
## by makeCacheMatrix.  If 'x' is not null, it then pulls the already 
## calculated inverse from the cache and returns it as 'm',
## otherwise it calculates the inverse of 'x' and returns it as 'm'.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  ## Returns a matrix 'm' that is the inverse of 'x'
}