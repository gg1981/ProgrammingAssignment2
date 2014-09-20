## The first function creates an object matrix with four methods (get, set, getInverse, setInverse), 
## The second function return the inverse of the matrix 
## In the comments after the two function there is an example to how test the code.


## The following function create an object matrix with 4 methods
## we assume that the matrix is n*n and it is always possible to calculate the inverse
##(e.g. if a not squared matrix is inserted we get an error if we use the function "cacheSolve")

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
            x <<- y
            m <<- NULL
    }
    
    get <- function() 
            x
    
    setInverse <- function(inverse) 
            m <<- inverse
    
    getInverse <- function() 
            m
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

##Next function returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    
    ## first check if we already calculated the inverse, in that case return the cached value    
    m <- x$getInverse()                     #retrieve the value of the inverse
    if(!is.null(m)) {                       #check if the value is available or NA
        message("getting cached data")      #if is available return the inverse and exit the function, no calculation done
        return(m)
    }
    
    ##if the matrix is new, or changed and the inverse is not in the cache we calculate it
    
    data <- x$get()                         #data contains the the matrix's entries
    m <- solve(data, ...)                   #the function solve calculate the inverse
    x$setInverse(m)                         #set memorize the inverse (so next time we don't calculate it)
    m                                       #return the inverse of the x matrix
}

################################################################################ 
## Example to test the code
# A<-makeCacheMatrix( matrix (c(1,0,0,1),nrow=2, ncol=2))   # matrix created
# A$get()     #returns the values in matrix form
#  >        [,1] [,2]
#  > [1,]    1    0
#  > [2,]    0    1
#
# A$getInverse()  #return NULL because we did not calculate the inverse yet
# > NULL
#
# inv<-cacheSolve(A)     #calculate the inverse
# A$getInverse()         #inverse is returned (easy to check with the identity matrix the inverse is the same)
# >       [,1] [,2]
# > [1,]    1    0
# > [2,]    0    1
# inv2<-cacheSolve(A)    #if we call again the function cacheSolve it takes the cached data
# > getting cached data
