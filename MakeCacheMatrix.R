## Write the following functions:
## makeCacheMatrix: This function creates a special "matrix" object 
##                  that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" 
##            returned by makeCacheMatrix above. If the inverse has already 
##            been calculated (and the matrix has not changed), then the 
##            cachesolve should retrieve the inverse from the cache.

## assume that the matrix supplied is always invertible.

## The first function, makeCacheMatrixr creates a special "matrix" object that 
## can cache its inverse, 
## and contains a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of matrix
## 4. get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  ## x: a square invertible matrix
  inv <- NULL
  
  set <- function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## computes the inverse of the "matrix" returned by makeCacheMatrix(). 
## If the inverse has already been calculated and the matrix has not changed, 
## it'll retrieves the inverse from the cache directly.

cacheSolve <- function(x, ...) {
  ## x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
