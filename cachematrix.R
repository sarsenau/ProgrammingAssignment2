## These two functions illustrate one method of creating an object in R
## through lexical scoping and a closure. This code uses the programming
## assignment 2 example for caching the mean of a vector as a model.
## (https://class.coursera.org/rprog-003)

## The function makeCacheMatrix() creates a CacheMatrix object with methods 
## and private data, the function cacheSolve() shows the use of that object.


## Create a CacheMatrix object that contains a matrix 
## and a cache of that matrix's inverse

## This object has methods set(), get(), setinverse() and getinverse()

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
 
  set <- function(y) {
          x <<- y
          inv <<- NULL
  }
 
  get <- function() x
 
  setinverse <- function(inverse) inv <<- inverse	
  getinverse <- function() inv

  list(
       set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse
  )
}


## Return a matrix that is the inverse of the the input CacheMatrix object

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
