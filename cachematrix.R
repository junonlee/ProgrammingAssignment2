## The first function shall take a matrix and cache it in itself
## Second function shall provide the inverse of the matrix
## and cache it in the first function

## makeCacheMatrix function creates a "matrix" object 
## that can cache the matrix object and its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    s <- NULL
    getmatrix <- function() x  #get matrix argument x from cache
    setmatrix <- function(y){  #set matrix argument x
        x <<- y
    }
    getstorematrix <- function() s  ##get stored matrix to compare with the matrix object
    setstorematrix <- function(z) { #store matrix to compare with the matrix object
        s <<- z
    }
    
    getinverse <- function() m                      #get matrix inverse from cache
    setinverse <- function(inverse) m <<- inverse   #set matrix inverse
    
    list(setmatrix = setmatrix, getmatrix = getmatrix, #create a list to store the functions
         setinverse = setinverse,
         getinverse = getinverse,
         getstorematrix = getstorematrix,
         setstorematrix = setstorematrix)
}


## cacheSolve function computes the inverse of the matrix, x
## returned by makeCacheMatrix.
## It checks if the inverse has already been calculated

cacheSolve <- function(x, ...) {
    m <- x$getinverse()               #get inverse of matrix
    data <- x$getmatrix()             #get matrix
    stored_data <- x$getstorematrix() #get stored matrix
    
    if(!is.null(m) & identical(stored_data,data)) {
        message("The input matrix is the same as previous one.")
        message("Getting cached inverse matrix data:")
        print(m)
    }
    m <- solve(data)       #solve for inverse of a matrix
    x$setinverse(m)        #cache inverse in makeCacheMatrix.R
    x$setstorematrix(data)
    ## Return a matrix that is the inverse of 'x'
    m
}

