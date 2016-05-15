## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a special "matrix" object 
  ## that can cache its inverse
  ## 1. set the matrix
  ## 2. get the matrix
  ## 3. set the inverse
  ## 4. get the inverse
  
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinverse = function(inverse) inv <<- inverse 
  getinverse = function() inv
  list(set=set, get=get, setinv=setinverse, getinv=getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## This function computes the inverse of the special 
  ## "matrix" returned by makeCacheMatrix above
  
  inv = x$getinverse()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinverse(inv)
  
  return(inv)
}
