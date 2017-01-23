## Put comments here that give an overall description of what your
## functions do

#These two functions permit to take advantage of R scoping rules by storing in memory
# the result of a matrix inverse calculation in order to avoid re-calculing the inverse but
# directly accessing the stored result.

## Write a short comment describing this function

#makeCacheMatrix is a function that stores a matrix and posibly the result of its inverse
# if it has already been calculated.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
}


## Write a short comment describing this function

# This function check if the inverse of the matrix was previously 
#calculated by checking if m  in makeCacheMatrix is NULL.  
#If m is not NULL it means the inverse has already been calculated 
# then the function returns the stored value of m
#If m is NULL, it means the inverse has never been calculated so the function calculates
# the matrix inverse and store the result in makeCacheMatrix
cacheSolve <- function(x,...) {
  m <- x$getsolve()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  else {
  data <- x$get()
  m <- solve(data,...)
  x$setsolve(m)
  m
  }
}