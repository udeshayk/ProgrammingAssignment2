
## The following pair of functions allow us to cache the inverse of a matrix. 
## We take advantage of lexical scoping to achieve the required result. 
## The first function creates an object that stores the matrix and its inverse. 
## The second function allows us to retrieve the cached inverse.

## "makeCacheMatrix" function creates four functions that set the value of the matrix,
## get its value, set the value of its inverse and get the value of the inverse respectively.
## It returns a list of these functions.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
  	set <- function(y) {
		x <<- y
          	inv <<- NULL
  	}
  	get <- function() x
  	setinverse <- function(inverse) inv <<- inverse
  	getinverse <- function() inv
  	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## "cacheSolve" function accepts the result of the first function. If the inverse exists already,
## it retrieves the cached solution.

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
  	if (!is.null(i)) {
          	message("getting cached data")
          	return(i)
	}
  	data <- x$get()
  	i <- solve(data, ...)
  	x$setinverse(i)
  	i
}