## take a matrix, invert it and cache the result with the matrix
## if new computing with the same matrix, return the previoulsy cached result
## and do not computing in ordre to spare time and cpu.

## take a matrix, invert it and cache the result

makeCacheMatrix <- function(x = matrix()) {
        inverse_x <- NULL
        ## setting new matrix M by calling x$set(matrix) 
        set  <- function(y) {
                x <<- y
                inverse_x <<- NULL
        }
        get <- function() x
        ## caching the invert matrix in inverse_x
        setInverse <- function(inverse) inverse_x <<- inverse
        ## retrieving the invert matrix
        getInverse <- function() inverse_x
        list(set = set, 
             get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


## invert a matrix after verifying :
## - that the result is not in cache
## - that the matrix is the same as the cached one

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_x <- x$getInverse()
        ## veryfying cached matrix
        if (!is.null(inverse_x)) {
                message("getting caching matrix")
                return(inverse_x)
        }
        matrice <- x$get()
        inverse <- solve(matrice)
        ## caching inverse in inverse_x by using setInverse
        x$setInverse(inverse)
        inverse
}
