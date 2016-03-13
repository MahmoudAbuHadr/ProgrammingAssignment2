## this function creates a special matrix that can store(cache) it's own inverse .


makeCacheMatrix <- function(x = matrix()) { 
  inverse <- NULL                             ##if it is a new matrix we don't know the inverse yet 
  setMatrix <- function(newMatrix) {                    
    x <<- newMatrix                             ##cache new matrix in x
    inv <<- NULL                        ## cache the inverse to be null , we didn't compute it yet 
  }
  getMatrix <- function() x               ## returns the cached matrix 
  
  setMatrixInverse <- function(inv) inverse <<- inv      ##cache the inverse 
  getMatrixInverse <- function() inverse                          ##returns the cached inverse 
  list(setMatrix = setMatrix, getMatrix = getMatrix, setMatrixInverse = setMatrixInverse, getMatrixInverse = getMatrixInverse)  ##returns our special matrix
}




## this function checks if the inverse already computed and returns it if so , otherwise it computes caches  an returns it

cacheSolve <- function(x, ...) {
  
  if( !is.null(    x$getMatrixInverse()   ) ) {
    return(x$getMatrixInverse() )               ##if the inverse is already cached return the cached value 
  }
  inverse <- solve(x$getMatrix(), ...)                 ##if it hasn't been solved , compute the inverse of the matrix 
  x$setMatrixInverse(inverse)                          ## cache it 
  inverse                                              ## and return it 
}
