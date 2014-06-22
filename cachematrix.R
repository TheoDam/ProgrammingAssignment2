## The two functions below cache the inverse of a matrix.


## The function makeCacheMatrix creates a matrix object that can cache its inverse

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

}


## The function cacheSolve computes the inverse of the matrix object returned by
## the makeCacheMatrix function above. If the inverse has already been calculated
## and the matrix hasn't changed, the function retrieves the inverse from the
## cache (displaying the message "Getting cached data")

cacheSolve <- function(x, ...) {
       
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

