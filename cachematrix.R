## 

## makeCacheMatrix take a matrix as an input, stores the matrix with 
## functions in a list

makeCacheMatrix <- function(x = matrix()){ 
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

# cacheSolve takes the makeCacheMatrix list as inputs,
# calculates the inverse if there is nothing in cache
# references cache if the inverse is present

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        #if cached m is NOT null, return cache...
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        #else m is null, calc the inverse...
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

# test against Al's results

# m <- matrix(c(-1, -2, 1, 1), 2,2)
# x <- makeCacheMatrix(m)
# x$get()
# [,1] [,2]
# [1,]   -1    1
# [2,]   -2    1

# first run:

# inv<-cacheSolve(x)
# inv
# [,1] [,2]
# [1,]    1   -1
# [2,]    2   -1

# no cached data message, calculates and saves to cache, matches Al's

# run again, see if cache is returned

# inv<-cacheSolve(x)
# inv
#getting cached data
# [,1] [,2]
# [1,]    1   -1
# [2,]    2   -1

# cached data message, returns cache, matches Al's









