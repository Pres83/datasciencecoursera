## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly . 
## Below functions cache the inverse of a matrix.


## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## x is a square invertible matrix
  ## function returns a list containing functionS to:
  ##   - set the value of the matrix
  ##   - get the value of the matrix
  ##   - set the value of the inverse matrix
  ##   - get the value of the inverse matrix
  m <-NULL
  set<-function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## x is output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  m <- x$getmatrix()
  # if the inverse has already been calculated
  if(!is.null(m)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(m)
  }
  # otherwise, calculates the inverse 
  matrix <- x$get()
  m <- solve(matrix, ...)
  # sets the value of the inverse in the cache with setmatrix function
  x$setmatrix(m)
  m
}


## Example 
## Sample run
## x <- matrix(c(2, 0, 0, 2), c(2, 2))
## m = makeCacheMatrix(x)
## m$get()
 ##      [,1] [,2]
 ## [1,]    2    0
 ## [2,]    0    2
 
 ## No cache in the first run
 ## cacheSolve(m)
 ##      [,1] [,2]
 ## [1,]  0.5  0.0
 ## [2,]  0.0  0.5
 
 ## Retrieving from the cache in the second run
 ## cacheSolve(m)
 ## getting cached data
 ##      [,1] [,2]
 ## [1,]  0.5  0.0
 ## [2,]  0.0  0.5
