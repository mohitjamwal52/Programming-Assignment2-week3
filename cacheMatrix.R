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
