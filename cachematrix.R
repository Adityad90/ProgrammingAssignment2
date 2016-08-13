## Creates a special vector which stores object and functions to fetch and store it


## Creates a list which contains function and object to get and fetch the matrix and it's inverse if exists.
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


## It checks whether matrix in a particular list has computed inverse, if not exist, calculate inverse and store it in list.

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
}