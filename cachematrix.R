##############################################
## This script contains 2 functions.
## They provide a way to solve a matrix (calculate its inverse),
## and to cache the result, in order to spare processing time.
## User has to pass his original matrix to the function makeCacheMatrix,
## the returned object (a list) must then be passed to the function cacheSolve
## that returns the solved matrix, but it performs the calculation only if not
## cached yet.
##############################################


## makeCacheMatrix receives in input a matrix and returns a list that allows
## the caller to manage the matrix to get/set it and to get/set its inverse
## the cached solved matrix is reset at each call, because it has to be solved
makeCacheMatrix <- function(x = matrix()) {
  #solvedMatrix will contain the cached solved matrix
  solvedMatrix <- NULL
  
  #define the function "set" that stores the passed new matrix and clear solvedMatrix (it has to be solved again)
  set <- function(newMatrix) {
    x <<- newMatrix
    solvedMatrix <<- NULL
  }
  
  #define the function "get" that returns the current matrix
  get <- function() x
  
  #define the function "setSolved" that stores in solvedMatrix the solved matrix received in input
  setSolved <- function(solved) solvedMatrix <<- solved
  
  #define the function "getSolved" that returns the current solved matrix 
  #(can be NULL if matrix changed and/or it has not been solved yet)
  getSolved <- function() solvedMatrix
  
  #last statement: it creates a list of the 4 functions and returns it to the caller
  list(set = set,
       get = get,
       setSolved = setSolved,
       getSolved = getSolved)
}



## cacheSolve receive a "special" matrix returned by the function makeCacheMatrix,
## and returns the solved matrix. If the original matrix has bean already solved,
## this function returns the cached solved matrix, otherwise it performs the calculations.
cacheSolve <- function(x, ...) {
  
  #get solved matrix (if x was solved)
  solvedMatrix <- x$getSolved()
  
  #if returned solvedMatrix is not NULL, x was already solved, so return it to the caller
  if(!is.null(solvedMatrix)) {
    message("Getting cached data")
    return(solvedMatrix)
  }
  
  #get original matrix, to be solved now for the first time
  myMatrix <- x$get()
  
  #solve the matrix to get its inverse
  solvedMatrix <- solve(myMatrix, ...)
  
  #set the solved matrix in cache
  x$setSolved(solvedMatrix)
  
  #finally return the just solved matrix
  solvedMatrix
}
