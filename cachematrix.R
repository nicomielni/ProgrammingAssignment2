## -----I will be following Google's R Style Guide.-----
## -[[https://google.github.io/styleguide/Rguide.html]]-


## This function creates a makeCacheMatrix object that stores a matrix and it's inverse.

makeCacheMatrix <- function(my_matrix = matrix()) {
        
        ## Initializing mat_inverse as NULL
        mat_inverse <- NULL
        
        ## set is a function that stores the input into the parent's my_matrix and sets the parent's mat_inverse to NULL
        set <- function(input) {
                my_matrix <<- input
                mat_inverse <<- NULL
        }
        
        ## get calls the stored matrix
        get <- function() my_matrix
        
        ## setInverse stores the inverse matrix
        setInverse <- function(Inversed) mat_inverse <<- Inversed
        
        ## getInverse calls the stored inverse in makeCacheMatrix
        getInverse <- function() mat_inverse
        
        ## assigns the previous functions as an element within a list(), and returns it to the parent environment.
        return(list(set = set, get = get, setInverse = setInverse, getInverse = getInverse))
        
}


## cacheSolve requires an argument that is returned by makeCacheMatrix() in order to retrieve 
## the inverse from the cached value that is stored in the makeCacheMatrix() object's environment.

cacheSolve <- function(my_matrix, ...) {
        
        ## Return a matrix that is the inverse of my_matrix and store it in cache.
        
        ## Retrieve the inverse from the makeCacheMatrix object.
        mat_inverse <- my_matrix$getInverse()
        
        ## Check to see if it's NULL or not.
        if(!is.null(mat_inverse)) {
                message("Getting cached data ...")
                
                ## If a cached inverse exists, then it returns the stored value.
                return(mat_inverse)
        }
        
        ## If no inverse is stored, then it solves for the inverse.
        
        ## Calls the matrix frome the makeCacheMatrix object.
        dummyMatrix <- my_matrix$get()
        
        ## Solves the invers and stores it in mat_inverse
        mat_inverse <- solve(dummyMatrix, ...)
        
        ## Stores the now solved inverse in my_matrix using setInverse
        my_matrix$setInverse(mat_inverse)
        
        ## Returns the inverse of the matrix.
        return(mat_inverse)
        
}
