## Put comments here that give an overall description of what your
## functions do

#This function creates a special "matrix", which is really a list 
#containing a function to: 1.) set the value of the vector; 2.) get 
#the value of the vector; 3.) set the value of the mean; 4.) get the 
#value of the mean
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


#This function calculates the inverse of the special "matrix" created 
#with the makeCacheMatrix.  It checks to see if the inverse has 
#already been calculated and gets the inverse from the cache if it 
#has. Otherwise, it calculates the inverse of the data and sets the 
#value of the inverse in the cache via the setmean function.
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
