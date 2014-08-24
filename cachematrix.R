## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##Assignment: Caching the Inverse of a Matrix
## Two functions makeCacheMatrix and cacheSolve where solve to reverse the matrix(assuming that 
## provided matrix is reversible and makeCache to cache the inverse matrix - faster operation)


## Creates a function accepting the matrix as param
makeCacheMatrix <- function( myMatrix = matrix() ) {
  
  ## Init the variable for inverse matrix
  InverseCache <- NULL
  
  ##create/assign the matrix in the variable
  set <- function( matrix ) {
    myMatrix <<- matrix
    InverseCache <<- NULL
  }
  
  ##this function will simply return the matrix
  get <- function() {
    myMatrix
  }
  
  ##same as set function above but for the inverted matrix
  setInverse <- function(inverse) {
    InverseCache <<- inverse
  }
  
  ## same as the get function above but for the inverted matrix
  getInverse <- function() {
    InverseCache
  }
  
  ## to get the list of the functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##Seconf function - to reverse the input matrix
cacheSolve <- function(x, ...) {
  
  ## the below line returns the inverted matrix for x
  myMatrix <- x$getInverse()
  
  
  if( !is.null(myMatrix) ) {
    message("We got the cached data")
    return(myMatrix)
  }
  
  ## to get the matrix
  data <- x$get()
  
  ##to get the inverse of the matrix
  myMatrix <- solve(data) %*% data
  
  
  x$setInverse(myMatrix)
  
  myMatrix
}