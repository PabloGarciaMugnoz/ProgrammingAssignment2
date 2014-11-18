## These two funcctions are used to store a matrix and its inverse.
## The goal is to store it in a way that inverse can be accessed not
## having to compute its value each time it is needed. 

# makeCacheMatrix creates a CacheMatrix.
# CacheMatrix is an object used to store both a matrix and 
# and its inverse. The object consists of four functions used to 
# stored and return the matrix values.
#    set(a) stores matrix a
#    get() returns matrix a
#    setinverse(inverse_a) stores inverse of matrix a
#    getinverse() returns inverse of matrix a
makeCacheMatrix <- function(x = matrix()) {
    
    # as cacheMatrix is built inverse-matrix has not been computed.
    inverse <- NULL
    
    # set function stores the matrix in a variable named x, inverse
    # is has not been computed.
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    # get function returns matrix x
    get <- function() x
    
    # setinverse computed inverse of matrix x and stores it in
    # variable inverse
    setinverse <- function(solve) inverse <<- solve
    
    # getinverse function returns the inverse of matrix x
    getinverse <- function() inverse
    
    # list of functions included in the object makeCacheMatrix
    list( set = set, get = get, 
          setinverse = setinverse,
          getinverse = getinverse)
}

#####################################################################
# cacheSolve is a function used to get the inverse of a cacheMatrix
# it checks if the inverse has already been computed if it has stored
# matrix is return. In case inverse does not exist it is computed 
# and stored in cache memory.
# It access the matrix through functions defined in makeCacheMatrix
#####################################################################
cacheSolve <- function(x, ...) {
    
    # get the inverse matrix stored in x cacheMatrix object
    inverse <- x$getinverse()
    
    # if this matrix exists (not NULL element) return it
    if(!is.null(inverse)) {
        message("Getting cached inverse.")
        return(inverse)
    }
    
    # if this matrix does not exist compute it
    # get the matrix from the cacheMatrix object x
    data_matrix <- x$get()    
    #compute the inverse of the matrix
    inverse <- solve(data_matrix)
    # set the value of the inverse in the cacheMatrix object
    x$setinverse(inverse)
    # return inverse matrix
    inverse
}
