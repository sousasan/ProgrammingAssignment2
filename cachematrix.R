## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse. It returns a list of functions to # 1. set the value of 
## the matrix, 2. get the value of the matrix, 3. set the value of inverse of 
## the matrix and 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
	      set <- function(y){
	              x <<- y
	              inv <<- NULL
	      }
        get <- function() x
        setinv <- function(solved) inv <<- solved
        getinv <- function() inv
        ## <- returns the "matrix" list object 
        list(set=set, 
             get=get, 
             setinv=setinv, 
             getinv=getinv)

}


## cacheSolve computes the inverse of the "special" matrix returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                ## exit function and return the inverse from cache
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
