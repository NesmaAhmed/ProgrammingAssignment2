## repeating calculation make your programm so slow we can solve this by caching the result
## when i need it we can call it again instead of calculate it again

## it is the setter and getter for the matirx and the inverse matrix function  
makeCacheMatrix <- function(x = matrix()) {
  matrix_inverse <- NULL
  set <- function(y) {
    x <<- y
    matrix_inverse <<- NULL
  }
  get <- function() x
  setmatrix_inverse <- function(solve) matrix_inverse <<- solve
  getmatrix_inverse <- function() matrix_inverse
  list(set = set, get = get,
       setmatrix_inverse = setmatrix_inverse,
       getmatrix_inverse = getmatrix_inverse)

}
## The following function calculates the inverse  of the special "square matrix" created
## with the above function. However, it first checks to see if the inverse
## has already been calculated. If so, it gets the inverse from the cache and 
## skips the computation. Otherwise, it calculates the inverse of the matrix
## and sets the value of the inverse in the cache via the setmatrix_inverse function.
cacheSolve <- function(x, ...) {
  Inverse <- x$getmatrix_inverse()
  if(!is.null(Inverse)) {
    message("getting cached data")
    return(Inverse)
  }
  # manual calculation for inverse matrix using solve
  # function
  data <- x$get()
  Inverse <- solve(data, ...)
  x$setmatrix_inverse(Inverse)
  Inverse
}
## testing the code
#x=matrix(1:4,2,2)
#y=makeCacheMatrix(x)
#z=cacheSolve(y)
