## Put comments here that give an overall description of what your
## functions do
#
# Arun Simha, Sat May 24, 2014
#
##This program contains two functions, makeCacheMatrix and cacheSolve.
## 	FUNCTION1: makeCacheMatrix() creates a special "matrix" object that can cache its inverse
## 	FUNCTION2: cacheSolve() computes the matrix inverse returned by makeCacheMatrix()
#
## Write a short comment describing this function
## FUNCTION1: makeCacheMatrix creates a special "matrix" object.
## This object is a list that contains a function to:
## 	STEP 1) set the value of the matrix
## 	STEP 2) get the value of the matrix
## 	STEP 3) set the value of the inverse of the matrix
## 	STEP 4) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## FUNCTION2 cacheSolve() first determines whether the inverse has been already calculated
## If YES, then it obtains the value from the cache, and DOES NOT compute inverse, inv
## If NO, then it calculates the inverse of the matrix and stores the value in cache
## The caching is done using the call setInverse()
# Additional error conditions noted, but not run are:
# ERROR 1: check for det(A) != 0; then stop ("Determinant is zero")
# ERROR 2: check for nrow(A) = ncol(A) - square matrix; then stop ("Not a square matrix")
#

cacheSolve <- function(x, ...) {
inv <- x$getInverse()
    if(!is.null(inv)) {
        message("OUTPUT2: now getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    message("OUTPUT1: calculating inverse")
    inv    ## Return a matrix that is the inverse of 'x'       
} 
############################################################
# How to Test above functions?
###
# Within R or RStudio, type the following lines:
#
# TEST 1 - use 3 x 3 matrix shown below
# 1    2    3
# 0    4    5
# 1    0    6
#
# > c <- matrix(c(1,0,1,2,4,0,3,5,6),3,3)
# > b <- makeCacheMatrix(c)
# > cacheSolve(b)  # run it first time, inv is calculated
#
# OUTPUT1: calculating inverse
# [,1]        [,2]        [,3]
# [1,]  1.0909091 -0.54545455 -0.09090909
# [2,]  0.2272727  0.13636364 -0.22727273
# [3,] -0.1818182  0.09090909  0.18181818
#
# > cacheSolve(b)  # run it again, it should get cached data. Yes!
#
# OUTPUT2: now getting cached data
# [,1]        [,2]        [,3]
# [1,]  1.0909091 -0.54545455 -0.09090909
# [2,]  0.2272727  0.13636364 -0.22727273
# [3,] -0.1818182  0.09090909  0.18181818
#  
# # > cacheSolve(b)  # once again, just to be sure of behavior. Yes!
# 
# OUTPUT2: now getting cached data
# [,1]        [,2]        [,3]
# [1,]  1.0909091 -0.54545455 -0.09090909
# [2,]  0.2272727  0.13636364 -0.22727273
# [3,] -0.1818182  0.09090909  0.18181818
#
# TEST 2 use 2 x 2 matrix shown below
# 1    -1/4
# -1/4   1
#
# > d <- matrix(c(1, -1/4, -1/4, 1),2,2)
# > e <- makeCacheMatrix(d)
# > cacheSolve(e)  # First run: inv is computed
# > OUTPUT1: calculating inverse
# [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667
#
# > cacheSolve(e)  # Second run: inv from cache
#
# OUTPUT2: now getting cached data
# [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667
#
# > cacheSolve(e) # Third run: inv from cache
#
# OUTPUT2: now getting cached data
# [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667
#
# Let's matrix multiple %*% orig matrix d and inv. Yes, Identity.
#
# > d %*% cacheSolve(e)
# OUTPUT2: now getting cached data
# [,1] [,2]
# [1,]    1    0
# [2,]    0    1
# > 
#############################################################