## makeCacheMatrix contains funcions to set and get the inverse of the matrix.
## It used the containing environment to store inverse of the matrix and 
## and return the value through get functions.

makeCacheMatrix <- function(x = matrix()) {
## returns a list of four functions. Uses a mariable m that is stored in th containing environment
## set and get functions are used to set and get the variable m in the containing environment.
## setinverse and getinverse a re used to set and return the inverse of the matrix.

         m <- NULL
         set <- function(y) {
                 x <<- y
                 m <<- NULL
         }
         get <- function() x
         setinverse <- function(inverse) m <<- inverse
         getinverse <- function() m
         list(set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
 }
 
cacheSolve <- function(x, ...) {
## This function computes the inverse of the matrix is available in the containing environment,
## else it computes the inverse of the matrix using solve function and stores it in the containing environment
## The matrix is considered to be invertible.

         m <- x$getinverse()
         if(!is.null(m)) {
                 message("getting cached data")
                 return(m)
         }
         data <- x$get()
         m <- solve(data, ...)
         x$setinverse(m)
         m
 }
