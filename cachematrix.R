## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a matrix that can cache its inverse
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvmatrix <- function(inverse) m <<- inverse
  getinvmatrix <- function() m
  list(set = set,
       get = get,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)
}




cacheSolve <- function(x, ...) {
  ## This fuction computes the inverse matrix, if the inverse has been calculated, then the function should retrieve the inverse from the cache
  m <- x$getinvmatrix()
  
  if (!is.null(m)){
    message("getting cached data")
    return(m)
    
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinvmatrix(m)
  m
}
