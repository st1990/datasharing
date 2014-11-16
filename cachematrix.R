## Use cacheSolve(makeCacheMatrix(x = matrix(X))) to get the inverse of the matrix X
## 

## The makeCacheMatrix function creates a special "vector", which is really a list containing a function to

##set the value of the vector
##get the value of the vector
##set the value of the inverse of the matrix
##get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()){
  inv = NULL ## inverse of matrix is set to null
  set <- function(y){ ## set x as the matrix and inverse of the matrix as NULL
    x <<- y
    inv <<- NULL
  }
  get <- function()x ## gets the value of the matrix x
  setinverse <- function(solve) inv <<- solve(x) ## set inv as the inverse of the matrix x
  getinverse <- function() inv ## gets the inverse of the matrix which is saved as inv
  ## creates a list which as the options to set the matrix, get the matrix, set the inverse of the matrix and get the inverse of the matrix
  return(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))
}


## The following function calculates the inverse of the special "vector" created in makeCacheMatrix
cacheSolve <- function(x, ...){
  inv <- x$getinverse() ## gets inverse from the list of set get setinv and get inv
  if(!is.null(inv)){ ## if the matrix is NOT NULL then get the cached value
    message("getting cached data")
    return(inv)   ## return the inverse matrix
  }
  data <- x$get()  ## if inv is NULL, then we get the matrix from the list.
  inv <- solve(data, ...) ## inverse of the matrix is found
  x$setinverse(inv) ## the value of the inverse of the matrix is set
  return(inv)
}
