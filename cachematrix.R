# taking advantage of caching the inverse of a matrix rather than computing repeatedly.

# set the value of the matrix
# get the value of the matrix
# set the value of inverse of the martix
# get the value of inverse of the martix

makeCacheMatrix <- function(x = matrix())
{
	inv<-NULL
	
	set <- function(y)
	{
		x<<-y
		inv<<-NULL
	}
	
	get<- function() x
	setInverse<- function(inverse) inv<<- inverse
	getInverse<- function() inv
	list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
	
}

# the following function returns the inverse of the matrix. it first checks
#if the inverse has already been computed. if so, it gets the result and
#skips the computation. if not, it compyes the inverse, sets the value in
#the cache via setinverse function

cacheSolve <- function(x, ...)
{
        ## Return a matrix that is the inverse of 'x'
        
      inv <- x$getInverse()
      
      if(!is.null(inv))
      {
      	message("getting cashed data")
      	return(inv)
      } 
      
      data<- x$get()
      inv<-solve(data,...)
      x$setInverse(inv)
      inv        
}
