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

      if (TestMatrix(x)) {
            message("Matrix set successfully")                     
      } else {
            x <- NULL
            message("Error - object must be a 2 Dim Matrix with number rows = number columns")
      }
      
      InvMat <- NULL
      setMatrix <- function(y) {
            
            ## Error handle here to check if x is matrix and square?
            if (TestMatrix(y)) {
                  x <<- y
                  InvMat <<- NULL ##reset inverse matrix
                  message("Matrix set successfully")
            } 
            else {
                  message("Error - object must be a 2 Dim Matrix with number rows = number columns")
                  message("Original Matrix NOT reset")
            }
      }      
      getMatrix <- function() x
      getInverse <- function() {
            if (is.null(InvMat)){
                  message("Inverse not stored yet, call cacheSolve function to get Inverse matrix")
      
            
            }
            InvMat
            
      }
      setInverse <- function(mm) {
            ##check matrix passed is inverse of x
            if (TestInverse(mm,x))  {
                  InvMat <<- mm 
                  message ("Inverse stored on object")
            } else {
                  message("Matrix passed is not valid to be set as Inverse")
            }
            
            
      }
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

##should function be passed object with functions or just matrix??

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      MatTemp <- x$getInverse()
      print(MatTemp)
      if (!is.null(MatTemp)) {
            message ("getting cached data")
                        
      } else {
            message ("calculating Inverse Matrix and storing ...")
            MatTemp <- solve(x$getMatrix())
            x$setInverse(MatTemp)
            
      }
      MatTemp
}

##  To be used to validate function makeCacheMatrix and cacheSolve
##  function to test solution works by use of test AB=BA=I 
##  where  A is a matrix and B its inverse and I is an identity matrix of same size 
##  round to integer for ease of checking

#  creates logical matrix I = (AB == BA),
## check sum(I)=length(I), (in R TRUE =1 and FALSE=0)
 
TestInverse <- function(MatA,MatB) {

      retval=FALSE
      flag = TRUE
      
      ## need initial test that matrix MatA and MatB are both 2 dimensions and square
      ## this is deliberate limitation of function for simplification as all that is required by project
      
      if (length(MatA)!=length(MatB) ) {
            message("ERROR - Matrix's are different sizes")
            flag <- FALSE
      } 
      if ( length(dim(MatA)) != 2  ) {
            message("ERROR - Matrix A is not 2 dimensional")
            flag <- FALSE
      } 
      if ( length(dim(MatB)) != 2  ) {
            message("ERROR - Matrix B is not 2 dimensional")
            flag <- FALSE
      } 
      if (dim(MatA)[1]!=dim(MatA)[2]   ) {
            message("ERROR - Matrix A is not square")
            flag <- FALSE
      }
      if (dim(MatB)[1]!=dim(MatB)[2]   ) {
            message("ERROR - Matrix B is not square")
            flag <- FALSE
      }
      
      if (flag) {
      
            MatAB <- round(MatA %*% MatB,digit=5)   ##remove decimal rounding issues
            MatBA <- round(MatB %*% MatA,digit=5)   
            MatI <- MatAB == MatBA
            ## check 1, AB==BA
            if (sum(MatI)==length(MatI)) {
                  ## check 2 that either MAtAB or MatBA is identity matrix
                  ## we already know that matrix's are both same dim and square
                  if (sum(MatAB)==length(MatAB[1,])) {
                        retval=TRUE
                  }
            }
      } 
            
      retval
}

##  Function to test that object passed is a square matrix and so valid for inversion
TestMatrix <- function(x) {
      ##set default return value

      ##Initial test that correctly formated matrix, if not reset x to NULL
      if (  ( (is.matrix(x)) && (length(dim(x)) == 2) && ( dim(x)[1]==dim(x)[2]) )  ) {
            retVal <- TRUE
      }     
            else (retVal=FALSE)
      
      retVal
}
