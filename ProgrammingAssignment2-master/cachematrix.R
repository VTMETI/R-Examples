## Below functions helps in computing and cacheing the matrix inverse 
## as computing inverse of a matrix is costly affair

## This function take matrix as input and create a object of matrix
## also help in getting and setting the matrix 
makeCacheMatrix <- function(x = matrix()) {
    k <- NULL
    set <- function(b) {
        x <<- b
        k <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) k <<- inverse
    getinverse <- function() k
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function checks whether the computed inverse is available? 
## if it is then return the same, if not then compute the inverse, 
## store and then return the inverse matrix
cacheSolve <- function(x, ...) {
    j <- x$getinverse()
    if (!is.null(j)) {
        message("retrieving cached data")
        return(j)
    }
    data <- x$get()
    j <- solve(data, ...)
    x$setinverse(j)
    j
}