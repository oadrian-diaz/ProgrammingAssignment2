# Author: Oscar Adrian Diaz del Rosario
#
# Assignment:
# @makeCacheMatrix: Creates a "matrix" object that can cache its inverse
# @cacheSolve: Calculates/caches the inverse of the "matrix" object returned
#   by makeCacheMatrix
#
# Extra:
# @getInverseMatrix1: Calculates the inverse of the "matrix" object returned
#   by makeCacheMatrix. It uses QR decomposition, and does not cache.
# @getInverseMatrix2: Calculates the inverse of the "matrix" object returned
#   by makeCacheMatrix. It uses Choleski (QR) decomposition, and does not cache.
# @computeTimes: Evaluates the performace (time-wise) of ways of calculating the
#   inverse of a square matrix. This tool might be useful for large matrices.
# @generateRandomMatrix: Generates a NxN symmetric random matrix.
#

# /**
#  * Creates a special "matrix" object that can cache its inverse
#  * @param  {[type]} x  NxN matrix
#  * @return {[type]}   [description]
#  */
makeCacheMatrix <- function(x = matrix()) {
    # Check if matrix is square
    if(! ncol(x) == nrow(x)){
      message("Matrix is not square. Try again with a NxN matrix.")
      return(list(set=NULL, get=NULL, setinverse=NULL, getinverse=NULL))
    }

    # Init
    inv_matrix <- NULL

    # Set matrix
    set <- function(y) {x <<- y; inv_matrix <<- NULL}

    # Retrieve the matrix
    get <- function() {x}

    # Cache calculated inverse
    setinverse <- function(inverse) {inv_matrix <<- inverse}

    # Retrieve cached matrix
    getinverse <- function() {inv_matrix}

    # Matrix object
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# /**
#  * Computes the inverse of the special "matrix" returned by makeCacheMatrix.
#  * If the inverse has already been calculated (and the matrix has not
#  * changed), then cacheSolve should retrieve the inverse from the cache.
#  * @param  {matrix} x   NxN matrix
#  * @param  {opt}    ... optional arguments
#  * @return {matrix}     inverted matrix of x
#  */
cacheSolve <- function(x, ...) {
    # Verify if the inverse has been cached
    inv_matrix <- x$getinverse()
    if(!is.null(inv_matrix)) {
        message("Retrieving cached matrix")
        return(inv_matrix)
    }

    # Retrieve the matrix
    matrix <- x$get()

    # Calculate inverse
    inv_matrix <- solve(matrix)

    # Cache inverse matrix
    x$setinverse(inv_matrix)
    inv_matrix
}

# /**
#  * Uses the QR decomposition of a Matrix to calculate its inverse. It does not
#  * retrieve the inverse of the matrix if already calculated.
#  * @param  {matrix} x   NxN matrix
#  * @param  {opt}    ... optional arguments
#  * @return {matrix}     inverted matrix of x
#  */
getInverseMatrix1 <- function(x, ...){
  matrix <- x$get()
  # QR method
  inv_matrix <- qr.solve(matrix)
  x$setinverse(inv_matrix)
  inv_matrix
}

# /**
#  * Inverse from Choleski decomposition of a Matrix to calculate its inverse.
#  * It does not retrieve the inverse of the matrix if already calculated.
#  * @param  {matrix} x   NxN matrix
#  * @param  {opt}    ... optional arguments
#  * @return {matrix}     inverted matrix of x
#  */
getInverseMatrix2 <- function(x, ...){
  matrix <- x$get()
  # Choleski
  inv_matrix <- chol2inv(chol(matrix))
  x$setinverse(inv_matrix)
  inv_matrix
}

# /**
#  * Calculates the time performance of the different methods to obtain the
#  * inverse of a given matrix.
#  * @param  {matrix} x   NxN matrix
#  * @param  {opt}    ... optional arguments
#  * @return {bool}       whether or not if was successful
#  */
computeTimes <- function(x){
  # Compute Times
  message("Using cacheSolve method")
  tcs1 <- system.time(CS1 <- solve(x))
  print(tcs1)
  message("Using getInverseMatrix1 method")
  tcs2 <- system.time(CS2 <- qr.solve(x))
  print(tcs2)
  message("Using getInverseMatrix2 method")
  tcs3 <- system.time(CS3 <- chol2inv(chol(x)))
  print(tcs3)

  # Compare results
  if(all.equal(CS1, CS2) & all.equal(CS2, CS3)){
    message("All methods returned the same inverse matrix")
    TRUE
  }
  else{
    message("All methods did not returned the same inverse matrix. Something went
    wrong")
    FALSE
  }
}

# /**
#  * Generates a random but symetric matrix
#  * @param  {numeric} nrow=2000
#  * @param  {numeric} rho=.5
#  * @return {matrix}  R          NxN symmetric random matrix
#  */
generateRandomMatrix <- function(nrow = 2000, rho=.5){
  # Make sure the package is loaded
  if(!"package:MASS" %in% search())
  {
    if(!library(MASS, logical.return = TRUE)){
      return(FALSE)
    }
  }

  cat("Generating matrix, this might take a while")
  # Generate the matrix
  k <- nrow
  rho <- .3
  S <- matrix(rep(rho, k*k), nrow=k)
  diag(S) <- 1
  dat <- mvrnorm(10000, mu=rep(0,k), Sigma=S)
  R <- cor(dat)
  cat("..OK")

  return(R)
}
