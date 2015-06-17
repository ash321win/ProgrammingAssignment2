## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  matInv <- NULL
  
  setMatrix <- function(y){
    x <<- y
    matInv <<- NULL
  }
  getMatrix <- function() x
  
  setInverseMatrix <- function(inverse) matInv <<- inverse
  getInverseMatrix <- function() matInv
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  im <- x$getInverseMatrix() 
  if(!is.null(im)){
    message("getting cached inverse matrix")
    return(im)
  }
  
  data <- x$getMatrix()
  im <- solve(data, ...)
  x$setInverseMatrix(im)
  
  im
}
