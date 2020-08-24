cachemat <- function(x = matrix()) {
 
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getinv <- function() x
  set_inverse <- function(inverse) inv <<- inverse
  get_inverse <- function() inv
  list(set = set, getinv = getinv,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

solvecache <- function(x, ...) {

  inverse <- x$getinverse()
  if(!is.null(inv)) {
    return(inv)
  }
  data <- x$getinv()
  inverse <- solve(data, ...)
  x$set_inverse(inv)
  inv
}