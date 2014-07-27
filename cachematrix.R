## special matrix which can cache inverse matrix
makeCacheMatrix <- function( m = matrix() ) {
        i <- NULL
        
        ## set the matrix
        set <- function( matrix ) {
                m <<- matrix
                i <<- NULL
        }
        
        ## get the matrix
        get <- function() {
                m
        }
        
        ## set the inverse of the matrix
        setInverse <- function(inverse) {
                i <<- inverse
        }
        
        ## get the inverse of the matrix
        getInverse <- function() {
                i
        }
        
        ## 
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        if( !is.null(m) ) {
                message("getting cached data")
                return(m)
        }
        
        ## Get the matrix from our object
        data <- x$get()
        
        ## Calculate the inverse using matrix multiplication
        m <- solve(data) %*% data
        
        ## Set the inverse to the object
        x$setInverse(m)
        
        m
}
