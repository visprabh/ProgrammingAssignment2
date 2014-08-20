## Version:: v1.0
## Author:: visprabh
## Description: 
##              Intent of the code is compute Inverse of a matrix and cache it. 
##              Make use of the cache if the same matrix is passed again for inverse computation
##              Assumption: Only square invertible matrix is passed as input parameter


## makeCacheMatrix(x):: 
##      This function creates a special "matrix" object that can cache it's inverse.
##      It takes square invertible matrix as input parameter

makeCacheMatrix <- function(x=matrix()) {
        inverseFlag <- FALSE              ## fLag used to check if the inverse matrix is already cached
        inverseMatrix <- matrix()         ## initiallizing empty inverse matrix, it will be cached to store inverse of the original matrix
        originalMatrix <- x               ## original matrix which will be cached to check if the matrix changed or not
        
        ## set function to set initial data and cache original matrix
        set <- function(y) {
                x <<-y
                inverseFlag<<-FALSE
                originalMatrix <<-y
        }
        
        ## function to get matrix which needs to be inversed
        getMatrix <- function() x
        
        ##function to set and cache inverse matrix, also set the inverse flag to TRUE
        setInverse <- function(invOfx) {
                inverseMatrix<<-invOfx
                inverseFlag<<-TRUE
        }
        
        ## Function to return inverse of the matrix
        getInverse <- function() inverseMatrix
        
        ## Function to return inverse flag, used to check if cache can be used or not
        getInverseFlag <- function() inverseFlag
        
        ## Function to pass original matrix, used to check if cache can be used or not
        getOriginalMatrix <- function() originalMatrix
        
        list (set=set, getMatrix=getMatrix, getInverseFlag = getInverseFlag,setInverse=setInverse, getInverse=getInverse, getOriginalMatrix=getOriginalMatrix)
        
}


## cacheSolve(x):: 
##      This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
##      If the inverse has already been calculated and the matrix has not changed, then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x) {
        inverseMatrix <- x$getInverse()
        inverseFlag <- x$getInverseFlag()
        originalMatrix <- x$getOriginalMatrix()
        data <- x$getMatrix()
        
        ## Using cache if the matrix is unchanged and a cache already exists
        
        if(inverseFlag==TRUE && identical(originalMatrix, data) ==TRUE ) {
                message("Providing inverse matrix from cache")
                return(inverseMatrix)     ## Return the inverse matrix from Cache
        }
        
        ## Creating Inverse matrix in case cache is not present
        inverseMatrix <- solve(data)
        
        ## function call to set and the inverse matrix for future use
        x$setInverse(inverseMatrix)
        
        ##Return inverse matrix 
        inverseMatrix
}
