## makeCacheMatrix uses a superassignment operator ( <<- ) to create a "cached" matrix 
## and its inverse (m) which can be 

## The first function, makeCacheMatrix creates a special "matrix", which is 
## really a list containing a function to
## 
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL         # holds the cached matrix inverse, initialize to NULL
    set <- function(y) {
        x <<- y
        m <<- NULL    # reinitialize the the inverse if a new matrix is set
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve
## 
## The following function calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks to see if the inverse 
## has already been calculated. If so, it gets the inverse from the cache and 
## skips the computation. Otherwise, it calculates the inverse of the data and 
## sets the value of the inverse in the cache via the setInverse function.


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()           # get the cached matrix
    m <- solve(data, ...)     # returns the inverse of data (matrix)
    x$setInverse(m)           # stores the inverse in the cache variable
    m                         # returns the inverse matrix
                              # can also be retrieved by x$getInverse()
}

# Usage might be:
#   
# test <- makeCacheMatrix(test)
# test$set(matrix(rnorm(100),10,10))
# test$get()
# 
# itest <- cacheSolve(test)   
# class(itest)              # returned as matrix()
# class(test)               # class is a list "CacheMatrix"
# 
# itest %*% test$get()      # test$get() returns a matrix()
