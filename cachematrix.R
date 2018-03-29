## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix(): creates a special "matrix" object that can cache its inverse
# set(): set the value of the matrix
# get(): get the value of the matrix
# setinverse(): set the value of the inverse
# getinverse(): get the value of the inverse

makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

# Construct another function to get inverse of the special "Matrix".
# cacheinverse(): compute the inverse of the special "Matrix" and let the special "Matrix" store this inverse.
# solve(): compute the inverse of a square matrix
# Assume that the matrix supplied is always invertible.

cacheinverse <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

#Test Area
# Let's try to create a special "Matrix"
aMatrix <- makeCacheMatrix(matrix(c(-8, -3, 24, 2), nrow = 2))
aMatrix$get()

aMatrix$set(matrix(c(-5, 0, 10, 1, -2, 3, 6, -2, 1), nrow = 3))
aMatrix$get()

# retrieve the value of m, which should be NULL(default).
aMatrix$getinverse()



# Let's try to use the function cacheinverse()
cacheinverse(aMatrix)
aMatrix$getinverse()

# If you reset the value of the matrix, the inverse will be reset.
aMatrix$set(matrix(c(-8, -3, 24, 2), nrow = 2))
aMatrix$getinverse()

cacheinverse(aMatrix)
aMatrix$getinverse()


