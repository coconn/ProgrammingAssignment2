# cachematrix.r
# 
# second coursera assignment for r programming, rprog-016
#
# assignment consists of creating two functions, makeCacheMatrix and cacheSolve
# together these employ caching (cacheing?) when computing the inverse of an invertible matrix
# 
# CS O'Connell, UMN EEB/IonE

## makeCacheMatrix
# makeCacheMatrix creates a list of four functions, similar to object-oriented programming, called get(), set(), getinverse() and setinverse().  These four functions will be employed in cacheSolve.  Running makeCacheMatrix resets the inverse of matrix x back to null, so erases the cache.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL # inv is the answer we want, the inverse of matrix x; set to null in makeCacheMatrix
      set <- function(y) { # set function
            x <<- y
            inv <<- NULL
      }
      get <- function() x # lookup data set function (matrix x)
      setinverse <- function(inverse) inv <<- inverse # sets the inverse so it can be cached
      getinverse <- function() inv # lookup inverse get function
      list(set = set, get = get, # sets the list of these four functions
           setinverse = setinverse,
           getinverse = getinverse)
}


## cacheSolve

cacheSolve <- function(makeCacheMatrixObj, ...) {
      ## Return a matrix that is the inverse of 'x'
      
      inv <- makeCacheMatrixObj$getinverse() # gets the inverse as it's currently stored
      if(!is.null(inv)) { # is it not a NULL?  then just return the value - it's already in your cache!
            message("getting cached data")
            return(inv)
      }
      data <- makeCacheMatrixObj$get() # otherwise, get matrix x
      inv <- solve(data, ...) # solve for the inverse
      makeCacheMatrixObj$setinverse(inv) # set the inverse (cache it)
      inv # return the inverse matrix value
}


## testing makeCacheMatrix, cacheSolve
set.seed(75) # pick a random matrix to invert
x<-matrix(rnorm(4),2,2)
x
makeCacheMatrixObj <- makeCacheMatrix(x) # set the list of functions
cacheSolve(makeCacheMatrixObj) # caches the inverse
cacheSolve(makeCacheMatrixObj) # retrieves the cached answer
solve(x) # confirm this got the right answer

## this works!  Rad!

# printed results:
# set.seed(75)
# > x<-matrix(rnorm(4),2,2)
# > x
# [,1]      [,2]
# [1,] -0.7930947 -1.433823
# [2,]  0.4788257  0.125575
# > makeCacheMatrixObj <- makeCacheMatrix(x)
# > cacheSolve(makeCacheMatrixObj)
# [,1]      [,2]
# [1,]  0.2139418  2.442802
# [2,] -0.8157743 -1.351194
# > cacheSolve(makeCacheMatrixObj)
# getting cached data
# [,1]      [,2]
# [1,]  0.2139418  2.442802
# [2,] -0.8157743 -1.351194
# > solve(x)
# [,1]      [,2]
# [1,]  0.2139418  2.442802
# [2,] -0.8157743 -1.351194




## for christine's reference

## the vector-mean example goes here
# see https://class.coursera.org/rprog-016/human_grading/view/courses/973757/assessments/3/submissions

# makeVector <- function(x = numeric()) {
#       m <- NULL
#       set <- function(y) {
#             x <<- y
#             m <<- NULL
#       }
#       get <- function() x
#       setmean <- function(mean) m <<- mean
#       getmean <- function() m
#       list(set = set, get = get,
#            setmean = setmean,
#            getmean = getmean)
# }
# 
# 
# cachemean <- function(x, ...) {
#       m <- x$getmean()
#       if(!is.null(m)) {
#             message("getting cached data")
#             return(m)
#       }
#       data <- x$get()
#       m <- mean(data, ...)
#       x$setmean(m)
#       m
# }
# 
# # how to use
# x <- rnorm(1:10)
# xcache<-makeVector(x) # xcache is a list of 4 functions, set, get, getmean, setmean
# cachemean(xcache)
# #do it again
# cachemean(xcache) # now gets cached result
# 
# # is this allowed?
# cachemean(makeVector(x)) # yes, but never gets cached data; just keeps calling it from the first time
# 
