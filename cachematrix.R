### define the argument with default mode of "matrix"
CacheMatrix <- function(x = matrix()) {
       ## initialize inv as NULL; will hold value of matrix inverse
inv <- NULL
        ## define the set function to assign new 
set <- function(y) {
    ## value of matrix in parent environment     
x <<- y
          ## if there is a new matrix, reset inv to NULL
inv <<- NULL
      }
## define the get fucntion - returns value of the matrix argument
        get <- function() x
## assigns value of inv in parent environment
        setinverse <- function(inverse) inv <<- inverse
        ## gets the value of inv where called
getinverse <- function() inv
## need this in order to refer 
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## cacheSolve will retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
inv <- x$getinverse()
if(!is.null(inv)) {
message("getting cached data.")
return(inv)
}
data <- x$get()
inv <- solve(data)
x$setinverse(inv)
inv
}
##Testing my function
my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$get()
my_matrix$getinverse()
cacheSolve(my_matrix)
