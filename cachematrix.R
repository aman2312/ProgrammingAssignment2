## The following is a set of two functions that cache inverse of a matrix for
## future use. The two functions include:
# 1. makeCacheMatrix
# 2. cacheSolve

## Instructions:
# Pass the matrix whose inverse is needed using makeCacheMatrix function
# Example -     source("cachematrix.R")
#               > z<-makeCacheMatrix()
#               > z$set(matrix(1:4,2,2))
#               > cacheSolve(z)


## The FIRST function makeCacheMatrix creates a list consisting of functions to:
# 1. Set the value of a matrix
# 2. Get the value of the matrix
# 3. Set the value of the matrix' inverse 
# 4. Get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}

## The following function calculates the inverse of the matrix set using above 
## function. However, it first checks if the inverse has already been cached.
## If it is, then it retrives the inverse from Cache. Otherwise, it calculates
## the inverse and sets the inverse in the cache for future purpose. 
## This is done using setmatrix function.

cacheSolve <- function(x=matrix(), ...) {
        m<-x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
}
