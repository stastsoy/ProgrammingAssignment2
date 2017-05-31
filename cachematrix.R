##These two functions is a result of Programming Assignment 2 of R Programming course. The goal is to understand lexical scoping in R.

##We will need the following for checking if we can inverse the matrix
install.packages("matrixcalc")
library(matrixcalc)


## this function creates special object that can cache its inverse, it also checks if the input matrix square and not singular

makeCacheMatrix <- function(x = matrix()) {
        
##Check if the matrix is inversible
        
        if(!is.square.matrix(x)){
                message("The matrix is not square and cannot be inversed, please define square matrix, see https://en.wikipedia.org/wiki/Invertible_matrix")
                return(x)
        } 
        
        if(is.singular.matrix(x)){
                message("The matrix is singular and cannot be inversed, please define non singular matrix, see https://en.wikipedia.org/wiki/Invertible_matrix")
                return(x)
        } 
        
        
        
        i <- NULL
        
## I'm not goint to set the matrix without calling makeCacheMatrix, so I exclude "set" function
        
        get <- function() x
        
        ## to be able to use i in parent environment we use <<-
        setInverse <- function(inv) i <<- inv
        getInverse <- function() i

        list(get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function checks if the inverse is already calculated, if yes then return it, if no then calculates it. We use "solve" function to calculate Inverse

cacheSolve <- function(x,...) {
        
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        i <- solve(x$get(), ...)
        x$setInverse(i)
        return(i)
}