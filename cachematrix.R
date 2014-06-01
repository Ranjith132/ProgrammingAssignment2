## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#makeCacheMatrix shall set or get the matrix or the inverse of the matrix
#set fucntion sets the matrix
#get function shall get the matrix
#setinv shall set the inverse of the matrix
#getinv shall get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
  
  set <- function(y) 
  {
    x <<- y
    m <<- NULL
  }
  
  get <- function() 
    x
  
  setinv <- function(inv) 
    m <<- inv
  
  getinv <- function() 
    m
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
#cacheSolve Function calculates the inverse of the matrix. If inverse is already calculated it shall return the inverse
#from the cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
  
  if(!is.null(m)) 
  {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data, ...)
  
  x$setinv(m)
  
  m
}
