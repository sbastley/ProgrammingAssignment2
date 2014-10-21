## The first function takes in a matrix and puts functions that are inside in a list.
## The second takes in the list from the first and will calculate the inverse matrix
## if there is a cached matrix

## This function takes in a matrix and returns a list of other functions which can
## then be called using a subset. The inverse matrix is calculated if called.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        getinverse <- function() {
                m <<- solve(x)
                return(m)
        }
        setMatrix <- function(matrix) m <<- matrix
        getMatrix <- function() m
        list(set = set, get = get, getinverse = getinverse,
             setMatrix = setMatrix,
             getMatrix = getMatrix)
        

}


## This function takes the list from the makeCacheMatrix function and will calculated
## the inverse matrix if a matrix is not already cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setMatrix(m)
        m
}
