## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  if(nrow(x) == ncol(x) && det(x) != 0){
    i <- NULL
    set <- function(y){
      x <- y
      i <- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  }else{
    return(message("Sorry, the matrix cannot be inverted!"))
  }
}


## Write a short comment describing this function
## Compute the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
