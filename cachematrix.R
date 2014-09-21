## The functions below work to compute the inverse of a matrix.  
## It first checks whether or not the inverse has already been computed 
## and cached, and if so then it recalls the cached inverse matrix.  
## Otherwise it computes and stores the inverse matrix in the cache
## in case it's needed again.

## The function makeCacheMatrix will create a special "matrix" which is
## a list containing a function to:
## 1. Set the value of the vector
## 2. Get the value of the vector
## 3. Set the inverse matrix
## 4. Get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get, 
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function calculates the inverse of the special "matrix" created with the above function.
## Before it does that though, it checks to see if the inverse has already been computed, and if 
## it has then it gets the inverse from the cache and skips the computation.  If it isn't though,
## then it computes the inverse matrix and sets the value of the inverse in the cache using the above
## function.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
          message("Getting cached data")
          return(m)
        }
        data <- x$get()
        ## solve() is the function in R to find the inverse matrix
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
