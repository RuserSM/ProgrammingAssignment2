## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
        ## x represents an invertible matrix
        ## This function creates a special "matrix" object that can cache its inverse
        ##              1. set the matrix
        ##              2. get the matrix
        ##              3. set the inverse
        ##              4. get the inverse
        ##         this list is used as the input to cacheSolve()
        
        i = NULL
        set = function(y) 
	{
                # `<<-` is used to assign a value to an object 
                # in an environment different from the current environment. 
                x <<- y
                i <<- NULL
        }
        get = function() 
	{x}
        setinv = function(inverse) i <<- inverse 
        getinv = function() 
	{i}
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) 
{
        ## Use the output of makeCacheMatrix()function
        ## This function returns inverse of the original matrix input to makeCacheMatrix()
        
        i = x$getinv()
        
        ## This is to check if the inverse is already estimated
        if (!is.null(i))
	{
                ## If yes, get it from the cache and skip the calculation. 
                message("Get the cached data")
                return(i)
        }
        
        ## If not, calculate the inverse 
        matrix.data = x$get()

	## solve(x) returns the inverse of the matrix
        i = solve(matrix.data, ...)
        
        # Set the value of the inverse in the cache via the setinv function.
        x$setinv(i)
        
        return(i)
}
