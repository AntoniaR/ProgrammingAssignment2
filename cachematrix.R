# makeCacheMatrix -> sets up the structure of the object x and can assign inverse for caching
# cacheSolve -> for a given matrix (already setup using makeCacheMatrix) either return cached 
# inverse or calculate inverse and store in cache

makeCacheMatrix <- function(x = matrix()) {
  # sets up the structure of the cache and can assign new cache values
  # set up your matrix by running e.g. x2 <- makeCacheMatrix(matrix(data=c(1,1,1,3),nrow=2,ncol=2))
  # then run through cache solve by cacheSolve(x2)
  # only set up your matrix once as otherwise you lose cached value (i is set to NULL during setup)
  # assign new values and obtain stored values using set, setinverse, get and getinverse
  # e.g. x$get(), x$set(x)
  
  i <- NULL
  set <- function(y) {  # setup the chache for storing the results of inverting x with NULL as the inverse
    x <<- y
    i <<- NULL
  }
  get <- function() x # returns the value of x
  setinverse <- function(inverse) i <<- inverse # sets the value of the inverse
  getinverse <- function() i # returns the inverse of x
  list(set=set, # the list of functions that can operate on x
       get=get,
       setinverse=setinverse,
       getinverse=getinverse
       )
}


## Write a short comment describing this function

cacheSolve <- function(x2) {
  # this function outputs the inverse of the Matrix x2, either using a cached value or 
  # calculates using the R function chol2inv. If calculated, the inverse is then cached 
  # for next time.
  # note the input x2 is a Matrix that has already been passed through makeCacheMatrix
  # Example output:
  # > x2 <- makeCacheMatrix(matrix(data=c(1,1,1,3),nrow=2,ncol=2))
  # > cacheSolve(x2)
  # [,1]       [,2]
  # [1,]  1.1111111 -0.1111111
  # [2,] -0.1111111  0.1111111
  # > cacheSolve(x2)
  # getting cached data
  # [,1]       [,2]
  # [1,]  1.1111111 -0.1111111
  # [2,] -0.1111111  0.1111111
  
  i <- x2$getinverse() # get the cached inverse of 'x' if it is available
  if(!is.null(i)) { # if there is a chached inverse of 'x' return that value
    message("getting cached data")
    return(i)
  }

  i <- chol2inv(x2$get()) # the inverse of a matrix
  x2$setinverse(i) # cache the new inverse of the matrix
  i <- x2$getinverse() # get the cached inverse of 'x' if it is available

  return(i) # return the inverse of the matrix
}
