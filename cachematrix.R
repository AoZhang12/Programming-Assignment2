## Caching the inverse of a matrix

## Creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setinv <- function() m <<- solve(x)
  getinv <- function() m
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Inverse the matrix that input to the above function

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if (!is.null(m)){
    message("Getting cached data")
    return (m)
  }
  matrix <- x$get()
  m <- solve(matrix,...)
  x$setinv(m)
  return (m)
  ## Return a matrix that is the inverse of 'x'
}

