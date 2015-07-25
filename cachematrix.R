## cachematrix.R
## The function is used to cache a matrix and then cache its inverse
## @Eric P.
## 2015-07-25 - First version for Coursera JHU R Programming course


## function makeCacheMatrix
## used to save/get a matrix into a variable
## as well as it inverse using various nested functions
## args :
##        x : matrix
##        inverse : inverse matrix for x
## returns :
##        matrix "x" or its inverse matrix "m"
## call : var <- makeCacheMatrix(<Matrix>)
##        var$get() to get <Matrix>
##        var$set() to set <Matrix>
##        var$getinverse() to get <Matrix> inverse
##        var$setinverse() to set <Matrix> inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #initialize the inverse matrix

  #set matrix function
  set <- function(y) {
    x <<- y
    m <<- NULL #reset inverse matrix => needs to be recalculated
  }

  #get matrix function
  get <- function() x

  #set inverse matrix function
  setinverse <- function(inverse) m <<- inverse

  #get inverse matrix function
  getinverse <- function() m

  #returns a list containing all objects
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## function cacheSolve
## used to retrieve inverse matrix from cache
## or solve inverse matrix and cache it if it does not exists
## args :
##        x : matrix
##        ... : arguments for solve function
## returns :
##        matrix "x" inverse called "m"
## call : var2 <- cacheSolve(<Matrix>)

cacheSolve <- function(x, ...) {
  m <- x$getinverse() #get the current inverse matrix cached

  #if the current cache is not null,
  #returns the inverse matrix previously cached
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }

  #if inverse matrix needs to be calculated
  data <- x$get()  #get current matrix data
  m <- solve(data, ...)  #solve inverse matrix
  x$setinverse(m)  #cache the inverse matrix

  ## Return a matrix that is the inverse of 'x'
  m
}
