#Jason de Villiers, Assignment 2
#Coursera R Programming

## This function provides stores a local copy of the inverse matrix together with some access wrapper functions
makeCacheMatrix <- function(x = matrix()) {
    #set cache variable to NULL wen initialised
    cachedInv <- NULL
   
    #Set the array and reset the cached matrix to NULL since it is no longer valid
    set <- function(y) {
       x <<- y
       cachedInv <<- NULL
    }
    
    ## return the requested cahced Matrix
    get <- function() x
   
    ## Set the inverse matrix blindly, hoping the caller of this function is both
    ## competent and benevolent
    setInv <- function(theInv) cachedInv <<- theInv
   
    ## blindly return whatever is stored in the cached inverse matrix
    getInv <- function() cachedInv
   
    #Return the list of these local functions
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

## This function uses a vector made with the aove makeCache Matrix function
## To return the cached inverse if it exist or otherwise calculate and store the
## the inverse to the function
cacheSolve <- function(x, ...) {
    ## Check if the inverse is already cached and return it if so
    retMat <- x$getInv()
    if(!is.null(retMat)) {
        message("w00t inverse is cached")
        return(retMat)
    }
    
    ##otherwise get the matrix and find the inverse and return that
    arr <- x$get()
    retMat <- solve(arr, ...)
    x$setInv(retMat)
    retMat
}
