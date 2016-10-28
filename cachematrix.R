##stores a matrix and caches its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  # Making sure the input is correct
  if(!is.matrix(x))
  {
    stop("you need to input a matrix")
  }
  
  invertedM <- NULL
  set <- function(y) {
    x <<- y
    invvertedM <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invertedM <<- inverse
  getInverse <- function() invertedM
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##After the inverse has been calculated, then the below code will retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invertedM <- x$getInverse()
  if (!is.null(invertedM)) {
    message("getting cached data")
    return(invertedM)
  }
  mat <- x$get()
  invertedM <- solve(mat, ...)
  x$setInverse(invertedM)
  invertedM
}

