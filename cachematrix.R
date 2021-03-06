# cachematric.R -- Matrix inversion routines for speedy and efficient
#		   second & subsequent inversions of same matrix.
#
# Lee Tibbert
# October 16, 2015
#
# Based on template & sample code provided to the Fall 2015 Corsera "R
# programming" course (rprog-033). Submitted as Programming Assignment 2 for
# that course.
#
# As always, I am indebted to & freely make use of many public domain
# StackOverflow & general WWW articles; for concepts, not actual work.

# I have commented this code more heavily than I normally would, so that
# I have a sporting chance of remembering/re-discovering some of the
# finer details of memory mangement and of the super-assignment (<<-)
# operator when I re-visit the code in six months or a year.
#
# One can skip the "too long" Description section by searching for the
# text "Code:" below.

# Description:
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

# A note on memory management:
#   The memory in the cached inverse matrix can be large when the
#   original memory is large (where large is defined by the amount
#   of memory available on the hardware.)
#
#   That cached memory follows the usual R rules: It is released
#   for possibly later gargage collection (gc()) when it goes out
#   of scope, either at the end of a function or when the last reference
#   has been removed.
#
#   dummy <- function () {
#     elements <- c(1, rep(0, 3), 2, rep(0,3), 1)
#      m <- matrix(elements, nrow=3, ncol = 3)
#      cm <- cachematrix(m)
#      mInv <- cacheSolve(cm)
#      # work with mInv
#      # do work
# } # mInv goes out of scope here
#   # cm & cache inside of become available for garbage collection.
#
#   Now, if the "do work" part is more like
#   " do lots of work with other large arrays which does not involve cm"
#   one may want to manually release cm and recover memory at that point:
#	rm(cm)

# Code:
#
# Return a matrix analog function (pseudo-class CacheMatrix).
# which later gets passed to cacheSolve() & invoked.
# This function saves a copy of the first matrix inversion
# so that saved copy can be quickly & efficiently returned
# on subsequent calls with an unchanged matrix.
#
# Note on namespace management.
# Because both variable mat and matInv have previosul been declared in the
# parent (immediatly enclosing) environment of the makeCAsheMatrix::set()
# funtion, the super-assignment (<<-) operator used in set() will find &
# set them at that parent levels. The instances of the two variable will be
# visible only to the unique function instance returned at each call to
# makeCacheMatrix. This establishes separate namespaces for each function
# and keeps the base/global environment uncluttered.
#
# E.g.:
#   cm <- cachematrix(m)
#   cm2 <- cachematrix(m2)
#
# Variables mat & matInv inside cm & cm2 are distinct. They can and most
# likely do have separate values.
#

makeCacheMatrix <- function(mat = matrix()) {

  # Two declarations required for proper/intended namespace management magic.
  # variable mat, declared above, is required at this level
  matInv <- NULL  # required for namespace management

  set <- function(y) {
  # Powerful Magic Happens Here!"
  # See "Note on namespace management" in function description above.
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
##
## If feasible, use previously cached copy, rather than computing
## the inverse anew. This should execute more quickly and use fewer compute
## resources.

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
