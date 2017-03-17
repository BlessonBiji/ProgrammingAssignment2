## The function creates a inverse of square matirx given as input
## If inverse already exists, then caches from the getinv() func

## This function returns a list as output to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
#x is a square invertible matrix
  
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      
      get = function() x
      setinv <- function(inverse) inv <<- inverse 
      getinv <- function() inv
      
      #returning a list containing functions to this list is used as 
      #the input to cacheSolve()
      list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve() checks if the inverse already exists, caches the result
## else returns the matrix inverse.

cacheSolve <- function(x) {
  #x is output of makeCacheMatrix()
  #return: inverse of the original matrix input to makeCacheMatrix()
  
  inv<-x$getinv()
  
  # check if inverse is already calculated
  if (!is.null(inv)){
    # if not null, caching the inverse matrix 
    message("Caching inverse matrix")
    return(inv)
  }
  
  # else, calculates the inverse 
  i <- x$get()
  inv <- solve(i)
  
  # setting the value of the inverse in the cache with the setinv func
  x$setinv(inv)
  
  return(inv)
}
