## Down below, there are two functions, makeCacheMatrix and cacheSolve, makeCacheMatrix is a function that gets and sets the
## matrix and its inverse, and cachesolve is a function which gives us as result the inverse of the matrix from the cache or by ## computing it again if the matrix got modified or the inverse never been calculated.

## makeCacheMatrix to get/set matrix & get/set inverse:

makeCacheMatrix <- function(x = matrix()) {
    I <- NULL
    set <- function(y){
      x <<- y
      I <- NULL
    }
    get <- function() x
    getinverse <- function() I
    setinverse <- function(solve) I <- solve(x)
    list(get=get,set=set,getinverse=getinverse,setinverse=setinverse)
}


## cacheSolve to get the inverse of the matrix:

cacheSolve <- function(x, ...) {
    I <- x$getinverse()
    if (!is.null(I)) {message("getting inverse of the cached data")}
    mat1 <- x$get()
    I <- solve(mat1, ...)
    x$setinverse(I)
    I
}