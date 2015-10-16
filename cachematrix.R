# cachematric.R -- Matrix inversion routines for speedy and efficient
#		   second & subsequent inversions of same matrix.
#
# Lee Tibbert
# October 16, 2015
#
# Based on template & sample code provided to the Fall 2015 Corsera "R
# programming" course (rprog-033). Submitted as Programming Assignment 2 for
# that course.

# Matrix inversion is a computationally expensive task for moderate to
# large arrays. In some workloads the same matrix is inverted more
# than once. While the cost of the first inversion can not be avoided,
# it is possible to minimize the cost of subsequent invocations
# with out adding substantially to the implementation and/or execution
# complexity of the first call.
#
# This file provides a matrix inversion function, called cacheSolve() and
# the makeCacheMatrix() function in order to use it.
#
# The first time cacheSolve() is called for a given matrix, the it stores
# an internal copy (memoizes) of the inverted matrix and also returns
# that inverted matrix.
#
# Subsequent calls to invert the unchanged matrix are quick and efficient
# because cacheSolve() can return a copy of the cached inverted matrix.
#
# Maintaining an internal copy spends memory in order to gain execution
# speed. For some workloads, this can be a major win.
#
# Example:
#
#   elements <- c(1, rep(0, 3), 2, rep(0,3), 1)
#   m <- matrix(elements, nrow=3, ncol = 3)
#   print(m)
#   [,1] [,2] [,3]
#   [1,]    1	 0    0
#   [2,]    0	 2    0
#   [3,]    0	 0    1
#
#
#   cm <- makeCacheMatrix(m) # required before first cacheSolve()
#
#   mInv <- cacheSolve(cm) # requires a CacheMatrix from makeCacheMatrix()
#   print(mInv)
#   [,1] [,2] [,3]
#   [1,]    1  0.0    0
#   [2,]    0  0.5    0
#   [3,]    0  0.0    1
#
#   Show that mInv is a proper inverse
#
#   print(m %*% mInv)
#   [,1] [,2] [,3]
#   [1,]    1	 0    0
#   [2,]    0	 1    0
#   [3,]    0	 0    1

# Honor Code declaration
#  While the work in this file is my own, it draws heavily on
#  the example provided in the assignment.
#    https://class.coursera.org/rprog-033/human_grading\
#	 /view/courses/975107/assessments/3/submissions


# Return a matrix analog function (pseudo-class CacheMatrix).
# which later gets passed to cacheSolve() & invoked to save
# a copy of the first matrix inversion so that copy can be
# returned on subsequent calls with an unchanged matrix.

makeCacheMatrix <- function(mat = matrix()) {

  matInv <- NULL

  set <- function(y) {
    mat <<- y
    matInv <<- NULL
  }

  get <- function() {
    mat
    }

  setSolve <- function(matToSolve) {
    matInv <<- matToSolve
  }

  getSolve <- function() {
    matInv
  }

  list(
    set = set,
    get = get,
    setSolve = setSolve,
    getSolve = getSolve
  )
}

## Given x, a CacheMatrix as returned by makeCacheMatrix,
## return the inverse of 'x' as a simple, 'proper'  Matrix.

cacheSolve <- function(x, ...) {

  inv <- x$getSolve()

  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }

  mat <- x$get()

  inv <- solve(mat)

  x$setSolve(inv)

  inv
}

# -30- #
