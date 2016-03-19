#################################################################################
##
## Name: Syed Jawwad Hussain
## Project Description:
##      The purpose of this Project is to ease the computation of the matrix by
##      create two functions in R language to use the caching feature to
##      increase the performance of applying inverse functionality to a matrix
##
################################################################################

#################################################################################
##
## Function Name: makeCacheMatrix
## Function Input: matrix
## Function Output: matrix
## Function Purpose:  Create a Matrix which is cached so that 
##                    it is created only on demand using the set and get function
##                    the solve function is used to inverse the matrix
##
#################################################################################

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setm <- function(solve) m <<- solve
  getm <- function() m
  list(set = set, get = get,
       setm = setm,
       getm = getm)
}


#################################################################################
##
## Function Name:     cacheSolve
## Function Input:    matrix
## Function Output:   matrix
## Function Purpose:  The cache function will create the matrix and then call
##                    the solve function to inverse the matrix
##
#################################################################################

cacheSolve <- function(x, ...) {
    
  m <- x$getm()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setm(m)
  m
}
