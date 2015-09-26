## Cache the inverse of a matrix to save computation time when the
## content of the matrix is not changing. This is done by providing
## two functions, 
## makeCacheMatrix() - creates a speical matrix object that stores
## a matrix and its inverse
## cacheSolve() - calculates the inverse of the matrix, by returing
## a valid cached inverse if it is available, or by computing the
## inverse and storing the result in the cache.


## This function creates a special matrix, which is a list containing
## a function to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the inverse of the matrix
## 4.  get the inverse of the matrix
##
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function calculates the inverse of the special matrix created
## with the function makeCacheMatrix(). However, it first checks to see
## if the inverse of the matrix has already been calculated. If so,
## it gets the inverse from the cache and skips the computation.
## Otherwise, it calulcates the inverse of the matrix and store the result
## in the cache via the setinverse() function.
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
