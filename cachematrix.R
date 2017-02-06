## This pair of functions are wrote to provide a matrix inversion and 
#as matrix inversion is a computation costly, one of the fuctions 
#makes a cache of the inverse matrix to be used instead of 
#calculate it again

#The function makeCacheMatrix will cache the inverse of a matrix


makeCacheMatrix <- function(x = matrix()) 
  {
  inv <- NULL
  set <- function(y) 
    {
      x <<- y #Used to assign parent variables
      inv <<- NULL
    }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
  }



# This function will create the cache to be used

cacheSolve <- function(x, ...) 
  {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) 
    {
    message("This inverse matrix come from cache")
    return(inv)
    }
  matrx <- x$get()
  inv <- solve(matrx, ...)
  x$setInverse(inv)
  inv
  }
