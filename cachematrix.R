# Caching the inverse of a matrix

# Two functions are defined to cache the inverse of matrix, assuming that the function will be used only to invertible matrices.

# Part 1: "makeCacheMatrix" function

# The "makeCacheMatrix" function creates a "special matrix object", which is actually a list,
# that can cache its inverse. The following steps are used to define this function:

# 1.1. Set the values of the matrix, with the "set" object
# 1.2. Get the values of the matrix, with the "get" object
# 1.3. Set the value of the inverse of the matrix, with the "setinverse" object
# 1.4. Get the value of the inverse of the matrix, with the "getinverse" object
# 1.5. Return the list with the objects previously defined

makeCacheMatrix <- function (x = matrix()) {
        mat <- NULL
        set <- function(y){
                x <<- y
                mat <<- NULL      
        }
        get <- function() x
        setinverse <- function(solve) mat <<- solve
        getinverse <- function() mat
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


# Part 2: "cacheSolve" function

# The "cacheSolve" function computes the inverse of the of the special matrix returned by the 
# "makeCacheMatrix" function. If the inverse was previously calculated from the 
# "makeCacheMatrix" function (and the matrix is has not changed), the cacheSolve function skips
# calculation and simply retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        mat <- x$getinverse()
        if(!is.null(mat)) {
                message("getting cached matrix")
                return(mat)
        }
        data <- x$get()
        mat <- solve(data, ...)
        x$setinverse(mat)
        mat
}
