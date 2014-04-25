##Author: Nitish Vashishtha
#email: nitish.v87@gmail.com
#
# makeCacheMatrix - Creates a cacheable matrix object along with it's inverse
# cacheSolve - Provides the inverse for the cached matrix object created using 'makeCacheMatrix'

# Creates a cacheable matrix 'x' and its inverse, the function provides additional getters and setters for these objects  
makeCacheMatrix <- function(x = matrix()) {
  #Warn for empty matrix, empty matrix is considered invertible, but it not useful 
	if(is.na(x)[1,1]) {
		warning("Input matrix is empty!")	
	}
  #Check If the matrix has been incorrectly defined wrt to columns and rows
	#A square matrix is a necessary condition for invertability, exit if not
  if(nrow(x) != ncol(x) ){
		stop("Expected a square matrix for inversion!")
	}
  
	i <- NULL
	
  #Define custom functions. getters and setters for matrix and its inverse
  set <- function(mat) {
		x <<- mat
		i <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) i <<- inverse
	getInverse <- function() i

	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# Calculates the inverse of matrix 'x' and returns it (after caching it), if the inverse has been cached already it is returned from cache instead. 
cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
  
  #Check cache
	if(!is.null(inv)) {
		message("Found matrix inverse in cache. Loading result from cache.")
		return(inv)
	}
	data <- x$get()
  
	#Solve for inverse
  inv <- solve(data)
	x$setInverse(inv)
	
  inv
}
