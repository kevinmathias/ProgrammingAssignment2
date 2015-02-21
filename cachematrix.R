## makeCacheMatrix is used to create a matrix object that can cache its inverse.
## cacheSolve is used to return the inverse if it is already calculated. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setmatrix function.

## The function creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setmatrix<-function(solve) m<<- solve
    getmatrix<-function() m
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix defined above. 
## If the inverse has already been calculated (and the matrix has not changed), then the function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m<-x$getmatrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}
