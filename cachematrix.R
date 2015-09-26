## This pair of functions will make a function list for a matrix
## and its inverse, and then either get the cached inverse
## matrix or create/cache the inverse matrix.
## 
## 
## 
## The first function takes as its input "x" an invertible
## matrix and outputs a function list containing:  set, get,
## setInv and getInv.  (Note:  if x is not an invertible matrix,
## the function will throw an error.)
##
## x$get() and x$getInv() can be called to return values for the
## matrix ("x") and inverse of the matrix ("i"), respectively.
## x$set() can be used to set a new matrix for the function list;
## likewise, x$setInv() can be used to set a new inverse matrix.
## 
## "i" is an empty value representing the inverse of x, and will
## remain "NULL" until it is set by cacheSolve() or x$setInv.
##
## To use the output in the second function, assign it to a
## new variable.  (e.g. yourMatrixCache <- makeCacheMatrix(x))

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInv <- function(solve) i <<- solve
    getInv <- function() i
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve() takes as its variable "x" a matrix cache list
## (e.g. yourMatrixCache <- makeCacheMatrix(x)).  "i" is the
## inverse of the original matrix.  If the value of "i" is
## assigned in the cache, cacheSolve() will return the value of "i";
## otherwise, it will compute the inverse of the original matrix 
## and store it in "i".  "i" can then be retrieved from the cache
## in subsequent actions.

cacheSolve <- function(x, ...) {
    i <- x$getInv()
    if(!is.null(i)) {
        message("getting cached matrix")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInv(i)
    i
}

