## The following pair of functions allow fast repeated computation of inverse 
## matrices via caching. The first creates a new list object suitable for 
## storing both matrix and cached inverse data if peviously calculated. The second
## calculates inverse or returns cahce as necessary.

####

## makeCacheMatrix reads in a matrix x and constructs a list of 4 functions: 
## set (sets data to x), get (gets data x), setinverse (sets inverse of x), 
## getinverse (gets inverse of x)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

####

## cacheSolve inverts a matrix, first checking if the inverse has been previously 
## calculated. it does this by taking the input matrix in the form of a makeCacheMatrix
## list, which contains both the data and any previously calculated inverse

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
