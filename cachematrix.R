
## Keeping the inverse of a matrix in cache environment

makeCacheMatrix <- function(x = matrix()) {
      cMat <- NULL     ## store it in cache and initialize the value to null 
      ## create matrix in working environment
      set <- function(y) {
         x <<- y
         cMat <<- NULL
      }
      
      ## get the value of matrix
      get <- function() x
      
      ## matrix inversion and storing in cache
      setInverse <- function(inverse) cMat <<- inverse
      ## get the inverted matrix from cache
      getInverse <- function() cMat
      ## creating the list of inverted matrix
      list(set = set, get = get, 
           setInverse = setInverse, 
           getInverse = getInverse)
}




cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  cMat <- x$getInverse()
  if(!is.null(cMat)){
    message("getting cached data")
    return(cMat)  ## get the matrix
  }
  data <- x$get()
  cMat <- solve(data)
  x$setInverse(cMat)  ## set the matrix
  cMat  
}

# Example usage
# data <- matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)
# cMat <- makeCacheMatrix(data)
# cacheSolve(cMat)
#       [,1] [,2]
# [1,]  0.0    1
# [2,]  0.5    0
