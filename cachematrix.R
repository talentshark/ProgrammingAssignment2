## Creat a list to store a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inversem <- NULL
  set <- function(y) {
    x <<- y
    inversem <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inversem <<- inverse
  getinverse <- function() inversem
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Calculate the inverse of the matrix, if claculated before retrieve
## the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversem <- x$getinverse()
  if(!is.null(inversem)) {
    message("getting cached inverse matrix")
    return(inversem)
  }
  data <- x$get()
  inversem <- solve(data, ...)
  x$setinverse(inversem)
  inversem
}
