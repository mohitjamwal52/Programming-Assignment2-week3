#This function creates a matrix object that cache it's inverse. 
#it has a list containing function of set and getthe value of the matrix,
# set and get inverse to get the value of the inverse of the matrix
make.cache.matrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#The below function computes the inverse of the matrix returned by make.cache.matrix() above. 
#If the inverse has already been calculated (and the matrix has not changed),
#then the cache.solve() retrieve's the inverse from the cache.

cache.solve <- function(x) {
  m <- x$getinverse()
  if (!is.null(m))  {
    message("getting cached data of inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, fraction = TRUE)
  x$setinverse(m)
  m
}

#for test we always assume that the matrix supplied is always invertible.
