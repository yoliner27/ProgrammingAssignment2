## Create a matrix and calculate its inverse, but it also saves the cache of said calculation, 
##so you do not have to recalculate and time is optimized.

## This function creates an array object that can store the cache of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        get<-function() {x}
        setInverse<-function(inverse) (inv<<-inverse)
        getInverse<-function() {inv}
        list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)

}


## This function calculate the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat<-x$get()
        inv<-solve(mat,...)
        x$setInverse(inv)
        inv
}
