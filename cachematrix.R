# The function "makeCacheMatrix" creates a list containing a function to

# 1 set the value of the matrix
# 2 get the value of the matrix
# 3 set the value of the inverse matrix
# 4 get the value of the inverse matrix

# The purpose of the function is to cache the inverse matrix of the imput matrix.  


makeCacheMatrix <- function(x = matrix()) {
        
        Inverse <- NULL
        set <- function(y) {
                x <<- y
                Inverse <<- NULL
        }
        
        get <- function() x
        setinvers <- function(invert) Inverse <<- invert
        getinvers <- function() Inverse
        list(set = set, get = get,
             setinvers = setinvers,
             getinvers = getinvers)
        
}


## The function "cacheSolve" determines the inverse of the imput matrix: If this inverse matrix is available, 
## it will be returned, otherwise it will be calculated.          

cacheSolve <- function(x, ...) {
        
        Inverse <- x$getinvers()
        if(!is.null(Inverse)) {
                message("getting cached data")
                return(Inverse)
        }
        
        data <- x$get()
        Inverse <- solve(data, ...)
        x$setinvers(Inverse)
        Inverse
}
