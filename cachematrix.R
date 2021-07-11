## Put comments here that give an overall description of what your
## functions do

##substituted inverse to solve function
##The following are two functions makeCacheMatrix,makeCacheMatrix
##makeCacheMatrix will include set,get,setinv,getinv
##library(MASS) this is  to calculate the inverse for non squared as well as square matrices
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL      #initializes the inverse as NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x        #this is to get the function of x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function(){
    inver<-ginv(x)
    inver%*%x               #this is a function to obtain the inverse of the matrix 
  }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
##The following is used to get the cache data

cacheSolve <- function(x, ...) ##caches data
  {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {     #checks if inverse is null 
    message("getting cached data!")
    return(inv)       #returns the inverse value
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv ##Return a matrix that is the inverse of 'x'  
}
