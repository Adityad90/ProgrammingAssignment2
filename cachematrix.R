## This function creates a special "matrix" object that can cache its inverse.

## Object is a list which contains the Matrix itself with the functions to store and retrieve the Matrix and its Mean.
## Parameter to pass is a Matrix (Considering that the Matrix can be inversed.)

makeCacheMatrix <- function(x = matrix())
{
   inverse <- NULL
   # set variable which can be called from another function to store the Matrix.
   set <- function(y)
	{
	  x <<- y
	  inverse <- NULL
	}
   # primitive function
   # returns x
   get <- function() x 
   # Set Inverse of Matrix provided as argument,iif not already stored in the list.
   setinverse <- function(resolve) inverse <<- solve
   # Fetches Inverse of matrix from the list
   getinverse <- function() inverse
   # Assign all these functions to named list 
   list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
 }

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
## This function assumes that the matrix is always invertible.
# paramter to the function is the Matrix inverse of which to be calculated.

cacheSolve <- function(x,..)
{
   # Find in Vector in the list if inverse exists
   inverse <-x$getinverse()
   if(!is.null(inverse))
	{
	  message("Inverse exists in Cache")
	  return(inverse)
	}
  # if not found compute and store it.
   data <- x$get()
   inverse <- solve(data)
   x$setinverse(inverse)
   inverse
}
