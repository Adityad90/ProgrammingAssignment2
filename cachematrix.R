##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix())
{
   inverse <- NULL
   set <- function(y)
	{
	  x <<- y
	  inverse <- NULL
	}
   #primitive function
   #returns x
   get <- function() x 
   #Does Inverse of Matrix provided as argument 
   setinverse <- function(resolve) inverse <<- solve
   #get Inverse of matrix 
   getinverse <- function() inverse
   # Assign all these functions to named list 
   list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
 }

##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x,..)
{
   # Find in Vector if inverse exists
   inverse <-x$getinverse()
   # if not found compute and store it.
   if(!is.null(inverse))
	{
	  message("Inverse exists in Cache")
	  return(inverse)
	}
   data <- x$get()
   inverse <- solve(data)
   x$setinverse(inverse)
   inverse
}
