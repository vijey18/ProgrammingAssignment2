## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # inv will store the cached inverse matrix
    inv <- NULL

    # Setter for the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # Getter for the matrix
    get <- function() x

    # Setter for the inverse
    setinv <- function(inverse) inv <<- inverse
    # Getter for the inverse
    getinv <- function() inv

    # Return the matrix with our newly defined functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		   inv <- x$getinv()

    # If the inverse is already calculated, return it
    if (!is.null(inv)) {
        message("get cached data")
        return(inv)
    }

    # The inverse is not yet calculated, so we calculate it
    data <- x$get()
    inv <- solve(data, ...)

    # Cache the inverse
    x$setinv(inv)

    # Return it
    inv
}

# Example:
# > m <- matrix(rnorm(16), nrow = 4)          // Create a matrix x
# > mx <- makeCacheMatrix(x)                  // Create our special matrix
# > mx$get()                                  // Return the matrix
# > cacheSolve(mx)                            // Return the inverse
# > cacheSolve(mx)                            // Call the 2nd time, hence return
#                                             // the cached inverse
