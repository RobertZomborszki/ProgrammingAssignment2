## Matrix inversion is usually a costly computation 
## and their may be some benefit to caching the inverse
## of a matrix rather than compute it repeatedly

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## at the beginning there is no inverze calculated
    inverz<-NULL
    ## set: to change the matrix
    set<-function(y){
        x<<- y
        ### inverz is dropped
        inverz<<-NULL
    }
    ## simply just return the matrix
    get<- function() x
    
    ## function to set the inverz
    setinverz <- function(inv) inverz<<- inv
    
    ## function to get the inverz
    getinverz <- function() inverz
    
    ###Provide a list of function of the CasheMatrix
    list(set=set,get=get, setinverz=setinverz, 
         getinverz=getinverz)
}


## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed),
## then the cachesolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ### get the inverz from the parameter if possible
    inv<- x$getinverz()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ### otherwise calculate it
    else {
        datamatrix<-x$get()
        inv<- solve(datamatrix)
        ### don't forget to save it
        x$setinverz(inv)
        ###and return it in the end
        inv
    }
    
}
