## Function create a list with 4 elements in it
## each element is a function
## The object returned by the function also hold 2 varibles which as scoped 
## to each instance of the object only these are x and InvMat. Both are matrix
## this means that multiple list objects which are created by makeCacheMatrix have
## their own copies of x and InvMat and the values of these persist so long as 
## the parent list does
## 
## functions
## getMatrix returns the current value of the main matrix held in var x
## setMatrix overides the value of the main matrix x with a new one (passed as a parameter) 
## and resets MatInv to NULL
## getInverse returns the inverse matrix MatInv
## setInverse sets the inverse matrix MatInv 

makeCacheMatrix <- function(x = matrix(), ... ) {

      InvMat <- NULL
      setMatrix <- function(y) {
            x <<- y
            InvMat <<- NULL
            }
      getMatrix <- function() x
      getInverse <- function() InvMat
      setInverse <- function(mm) InvMat <<- mm
      
      list(getMatrix=getMatrix,setMatrix=setMatrix,getInverse=getInverse,setInverse=setInverse)
}


## Write a short comment describing this function
## This function accepts a list object as created by function makeCacheMatrix above
## It returns the inverse of the matrix held in that object
## 
## It initially checks if the inverse matrix is already stored in x by use of function getInverse
## If that isnt null this is returned as inverse matrix
## If it hasnt already been calculated then it calculates the inverse using "solve" 
## and then stores it back into object x using the function setInverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      MatTemp <- x$getInverse()
      print(MatTemp)
      if (!is.null(MatTemp)) {
            message ("getting cached data")
                        
      } else {
            message ("calculating Inverse Matrix and storing")
            MatTemp <- solve(x$getMatrix())
            x$setInverse(MatTemp)
      }
      MatTemp
}

##  To be used to validate function makeCacheMatrix and cacheSolve
##  function to test solution works by returning the product of mainmatrix and stored inverse
##  This should be an identity matrix (so that AB=BA=I where A is a matrix and B its inverse)
##  round to integer for ease of checking

TestInverse <- function(x) {

      MatI <- round(x$getMatrix()%*%x$getInverse(),digit=0)
      MatI

      
}
