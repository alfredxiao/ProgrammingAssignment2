## Overall Comments: cache result of solving a matrix and returns the result when cache found
## Example usage:
## > m1 <- rbind(c(1, -1/4), c(-1/4, 1))
## > class(m1)
## [1] "matrix"
## > m2 <- makeCacheMatrix(m1)
## > class(m2)
## [1] "list"
## > cacheSolve(m2)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > cacheSolve(m2)
## getting cached data
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## makeCacheMatrix converts an matrix object into an object containing the cached value from potentially costly computation

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        set_solved <- function(solved) s <<- solved
        get_solved <- function() s
        list(set = set, get = get,
             set_solved = set_solved,
             get_solved = get_solved)

}


## cacheSolve accepts an object made via makeCacheMatrix() and returns calculated or cached result of solve

cacheSolve <- function(x, ...) {
        s <- x$get_solved()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$set_solved(s)
        s
}
