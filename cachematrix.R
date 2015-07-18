## cachematrix.R
##  functions for calculating (and caching) inverse matrices
##  Mikael Puittinen / 2015

## makeCacheMatrix(matrix) creates and returns a cached matrix object, e.g.
##  m<-matrix(c(1,0,5,2,1,6,3,4,0),nrow=3,ncol=3)
##  cm<-makeCacheMatrix(m)
## The matrix can be retrieved with the get function, i.e.
##  cm<-get()
##        [,1] [,2] [,3]
##	[1,]    1    0    5
##  [2,]    2    1    6
##  [3,]    3    4    0

makeCacheMatrix <- function(x = matrix()) {
# Initialize the cached inverse "i"
	i <- NULL
# Function for setting the matrix (store the matrix in x, initialize the inverse i)
    set <- function(y) {
         x <<- y
         i <<- NULL
    }
 # Function for getting the matrix
    get <- function() x

 # Function for storing the inverse matrix into the cache
    setinverse <- function(inversematrix) i <<- inversematrix

 # Function for retrieving the inverse matrix from cache
    getinverse <- function() i

 # Return the object, which consists of the function methods defined above
    return(list(set = set, 
    	 		get = get,
         		setinverse = setinverse,
         		getinverse = getinverse))
}

## This function solves the the inverse of the matrix
## It looks up in the cache whether it has already been calculated.
## If a cached value exists, it is returned (without recalculation). The text "getting cached data" is displayed
## If a cached value does not exist, the inverse is solved, stored into cache and returned.
##
## e.g.
##   cacheSolve(cm)
##   getting cached data
##        [,1] [,2] [,3]
##   [1,]  -24   20   -5
##   [2,]   18  -15    4
##   [3,]    5   -4    1

cacheSolve <- function(x) {
# Lookup whether the inverse is in cache
        i <- x$getinverse()
        if(!is.null(i)) {
# Yep, it is in cache, return it from there
                message("getting cached data")
                return(i)
        }
# Nope, it is not. Retrieve the original matrix and calculate its inverse
        matrix <- x$get()
        i <- solve(matrix)
# Store the inverse into the cache and return it
        x$setinverse(i)
        return(i)
}
