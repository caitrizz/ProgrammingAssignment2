## Put comments here that give an overall description of what your
## functions do:
# The makeCacheMatrix function will create the inverse of a matrix x, 
# and then the cacheSolve function will create the inverse of that new matrix,
# provided it doesn't already exist.

## Write a short comment describing this function: 
# makeCacheMatrix creates the inverse of a matrix x.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set  <- function(y) {
    x <<- y
    i <<- NULL
}
get <- function() x
setinverse <- function(solve) i
getinverse <- function() i
list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
cacheinverse <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting inverse matrix")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}


## Write a short comment describing this function:
# the cacheSolve function will create the inverse of that new matrix,
# provided it doesn't already exist

cacheSolve <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
  }

## Return a matrix that is the inverse of 'x'

#makeVector <- function(x = numeric()) {
#  m <- NULL
#  set <- function(y) {
#    x <<- y
#    m <<- NULL
#  }
#  get <- function() x
#  setmean <- function(mean) m <<- mean
#  getmean <- function() m
#  list(set = set, get = get,
#       setmean = setmean,
#       getmean = getmean)
# }

# cachemean <- function(x, ...) {
#  m <- x$getmean()
#  if(!is.null(m)) {
#    message("getting cached data")
#    return(m)
#  }
#  data <- x$get()
#  m <- mean(data, ...)
#  x$setmean(m)
#  m
# }


