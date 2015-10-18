## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. The
## following two functions are used to cache the inverse of a matrix.

## makeCacheMatrix creates a list of fucntions containing:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix
## NOTE: similar to getters & setters design in Java business objects


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  # function to set matrix, clear cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # function to get matrix
  get <- function() x
  # function to set matrix inverse
  setinverse <- function(inverse) inv <<- inverse
  # function to get get matrix inverse
  getinverse <- function() inv
  ## return list of functions
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The following function returns the inverse of the matrix. It first checks if
## the inverse has already been computed. If so, it gets the result and skips the
## computation. If not, it computes the inverse, sets the value in the cache via
## setinverse function.


cacheSolve <- function(x, ...) {
  # get the inverse matrix if cache was not empty
  cached_inv <- x$getinverse()
  if(!is.null(cached_inv)) {
    message("getting cached matrix...")
    return(cached_inv)
  }
  
  # inverse matrix was not cached, take the inverse of the matrix and save it in cache
  matrix_data <- x$get()
  inv <- solve(matrix_data)
  x$setinverse(inv)
  return(inv)
}

## test if matrix is square; if not, return user friendly message
## solve returns an error anyway; but using this to experiment with try/catch
## out <- tryCatch(solve(matrix_data) %*% matrix_data, error = function(e) e)
## misnotsquare <- any(class(out) == "error")
## if (misnotsquare == TRUE) {
##   return(message("Error: matrix is not square and therefore not invertible."))
## }


## Sample runs:
## > x = matrix(rnorm(16), 4, 4)
## > m = makeCacheMatrix(x)
## > m$get()
##            [,1]       [,2]      [,3]       [,4]
## [1,] -0.5733012 -0.1818438 1.1330254 -0.5539460
## [2,]  0.1820053  0.5031568 0.8096392 -0.8199572
## [3,] -2.4330059  1.6280777 1.3693719  0.2692612
## [4,] -0.6913306 -0.6983429 0.5454571  0.2401392


## No cache in the first run
## > cacheSolve(m)
##            [,1]       [,2]        [,3]       [,4]
## [1,] -0.2648684 -0.2798912  1.23174407 -1.4950244
## [2,]  0.6416617 -0.1498452 -0.06006046  0.7198118
## [3,] -0.2984405  1.3339760 -0.56776032 -0.1626833
## [4,]  0.4862743  0.8917791 -0.88781358 -0.0727462

## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached matrix...
##            [,1]       [,2]        [,3]       [,4]
## [1,] -0.2648684 -0.2798912  1.23174407 -1.4950244
## [2,]  0.6416617 -0.1498452 -0.06006046  0.7198118
## [3,] -0.2984405  1.3339760 -0.56776032 -0.1626833
## [4,]  0.4862743  0.8917791 -0.88781358 -0.0727462
## > 

## sample run with system.time() - showing benefit of caching with larger matrix
## > x = matrix(rnorm(1000000), 1000, 1000)
## > m = makeCacheMatrix(x)
## > system.time(cacheSolve(m))
##  user  system elapsed 
## 5.450   0.039   5.497 

## cached
## > system.time(cacheSolve(m))
## getting cached matrix...
##  user  system elapsed 
## 0.000   0.000   0.001


## try with non-square matrix - final version of cachematrix has tryCatch commented out
## > x = matrix(rnorm(12), 3, 4)
## > m = makeCacheMatrix(x)
## > m$get()
## [,1]      [,2]       [,3]       [,4]
## [1,] -0.2689975 1.6566862 -0.1029788 -0.8466161
## [2,]  2.1082166 0.4624608  2.5428625  0.6416370
## [3,]  0.9591343 0.6978466  1.4249083  0.1951135
## > 
## > cacheSolve(m)
## Error: matrix is not square and therefore not invertible.

