## implements a matrix which implements caching
## of its own inverse. Should run faster than repeatedly
## calling solve(), if the inverse has to be accessed repeatedly.

## makeCacheMatrix() takes as argument a matrix x and
## returns a 'matrix object', i.e. a list of 4 functions
## for setting/accessing the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y) {
                x <<- y         # binds y to the symbol x defined in the
                I <<- NULL      # PARENT environment. Similar for I.
        }
        get <- function() x
        setInverse <- function(inv) {
                I <<- inv       # again, binds to the symbol I defined in
                                # makeCacheMatrix's environment
        }
        getInverse <- function() I
        list(set=set, get=get, 
             setInverse=setInverse, 
             getInverse=getInverse)
}


## cacheSolve() checks whether the inverse of the
## 'matrix' x was already computed. If so, it returns
## this cached inverse. Otherwise, it uses solve() to
## compute the inverse, then stores it in x for future use.

cacheSolve <- function(x, ...) {
        I <- x$getInverse()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        # else: inverse was not previously computed
        data <- x$get()
        I <- solve(data)
        x$setInverse(I)
        I
        # next call to cacheSolve with same x
        # will use precomputed inverse
}
