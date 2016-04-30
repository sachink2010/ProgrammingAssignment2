## Below functions provides inverse of a square invertible matrix


## makeCacheMatrix sets the matrix (user provided) and returns cached inverse matrix (inv) 
##if it is already calculated earlier

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        get=function() x
        set=function(a){
                x<<-a
                inv<<-NULL
        }
        getinv=function()inv
        setinv <- function(invq) inv <<- invq
        list(get=get,set=set,getinv=getinv, setinv=setinv)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinv()
        
        ##check if inverse already calculated in past
        if(!is.null(inv)){
                print("Getting cached inv")
                return (inv)
        }
        y <- x$get()
        invq<-solve(y)
        x$setinv(invq)
}
