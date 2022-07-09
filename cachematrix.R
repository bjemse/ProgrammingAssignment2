## The following two functions allows to save an invertible matrix in cache in order to avoid   
## having to calculate over and over the inverse of this matrix, knowing that this can
## be rather time consuming.


## The first function takes a matrix and creates a special matrix in order to store 
## the calculation of its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
get <- function() x
setsolve <- function(solve) m <<- solve
getsolve <- function() m
list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}


## The second function calculates the inverse of the special matrix 
## created with the above function. However it first checks to see if the 
## inverse has already been calculated. If so it gets the inverse matrix 
## from the cache and skips the calculation. Otherwise it calculates the 
## inverse matrix and sets the result in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
      m <- x$getsolve() 
      if(!is.null(m)){
              message("getting cahed data")
              return(m)  ## returns the inverse already calculated
      }        
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m ## Return a matrix that is the inverse of 'x'
}
## For example if we define
M <- makeCacheMatrix(matrix(c(1,0,3,2, 1,1,-1,2,-6, 7,7,9,-5, -1, -8,-6),  nrow=4, ncol=4))
## We can then calculate the inverse of M by
casheSolve(M)
## If the inverse has already been calculated the result will be fetched in the
## cache.