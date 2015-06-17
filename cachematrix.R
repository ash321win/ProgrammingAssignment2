## This file contains two functions that are used to create a special object 
## that stores matrix and cache's its inverse

## makeCacheMatrix creates a special matrix, which is actually a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
  matrixInv <- NULL
  
  setMatrix <- function(y){
    x <<- y
    matrixInv <<- NULL
  }
  getMatrix <- function() x
  
  setInverseMatrix <- function(inverse) matrixInv <<- inverse
  getInverseMatrix <- function() matrixInv
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)

}


## cacheSolve returns the inverse of the matrix created using the above function.
## it first checks to see if the inverse of the matrix has already been calculated, 
## if yes, it returns the stored value without actually computing the inverse again
## if no, then it computes the inverse of the matrix using solve() function
      ## and stores it in cache using setInverseMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInverseMatrix() 
  if(!is.null(invMatrix)){
    message("getting cached inverse matrix")
    return(invMatrix)
  }
  
  myMatrix <- x$getMatrix()
  invMatrix <- solve(myMatrix, ...)
  x$setInverseMatrix(invMatrix)
  
  invMatrix
}
