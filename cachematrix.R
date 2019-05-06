## These functions help to retrieve the inverse of matrix by 
## Caching the inverse of matrix


# This function creates setters and getters for inverse of matrix inorder
# to cache them. It returns a "special" matrix containing these setters and getters

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #Matrix Inverse set to NULL
  setMatrix <- function(y){
    x <<- y
    m <<- NULL # Intial Invocation should set Matrix inverse to NULL
    
  }
  getMatrix <- function() x
  setInverseMatrix <- function(inv) m <<- inv
  getInverseMatrix <- function() m
  
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix, 
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)

}



## This function takes "special" Matrix and returns the inverse of the Matrix

cacheSolve <- function(x, ...) {
  m <- x$getInverseMatrix()
  if(!is.null(m)) {  #if Matrix Inverse already exists return it
    message("getting cached data")
    return(m)
  }
  data <- x$getMatrix()
  m <- solve(data, ...)  ## else solve for matrix inverse and set it to special vector
  x$setInverseMatrix(m)
  m  
}





