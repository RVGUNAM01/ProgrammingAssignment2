########### R Programming Assignment 2 ###########
################### April 2015 ###################

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x=matrix()) { 
        m <- NULL 
        set <- function(y) { 
                x <<- y
                m <<- NULL
        }

# We proceed to create a list that will contain information of the matrix

        get <- function() x 
        setinv <- function(invr) m <<- invr 
        getinv <- function() m 
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv) 
}

## cacheSolve: This function computes the inverse of the special "matrix" 

cacheSolve <- function(x, ...) { 
        m <- x$getinv()
        if(!is.null(m)) { 
                message("...getting cached data...")
                return(m) 
        }
        data <- x$get() 
        m <- solve(data, ...) 
        x$setinv(m) 
        return(m)
}

######################### EXAMPLE #########################

w <- c(1,2,3,4) 

a <- makeCacheMatrix(matrix(w,2)) 

cacheSolve(a)

# > cacheSolve(a)
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5