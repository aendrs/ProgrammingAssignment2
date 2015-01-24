

## makeCacheMatrix starts savinf null default values for some variables.
# Then a few functions are defined inside for saving the input matrix and inverse, 
# and for  returning the input matrix and getting the cached values.


makeCacheMatrix <- function(in_mat = matrix()) {

	#Save initial default Null values
		saved_inv<-NULL
		saved_mat<-NULL
		
		#Function for saving Matrix and Inverse in cache
		setinv<-function(y){
					saved_mat <<- y
					saved_inv <<- solve(y)	
		}
		
		#self explanatory functions
		get_input_matrix <- function() in_mat
		
		get_saved_matrix <-function() saved_mat

		getinv <-function() saved_inv
		
		list(setinv = setinv, get_input_matrix = get_input_matrix,
             get_saved_matrix=get_saved_matrix, getinv=getinv)

}


## cacheSolve starts retrieving the input and cached matrix. 
# It compares both of them and if they are the same it retrieves the cached
# inverse. If they are dissimilar it means the matrix has changed so it calculates
# the inverse and saves it in cache.

cacheSolve <- function(x, ...) {
        
		#retrieve input matrix and cached matrix
		inputmat <- x$get_input_matrix()
		savedmat <- x$get_saved_matrix()
		
		#retrieve cached inverse
		inv<- x$getinv()
		
		
		#compare cached and input matrix, copy inverse when appropriate
		if(!is.null(inv) && identical(inputmat, savedmat)  ) {
				message("The inverse of this matrix has been calculated. 
				Let's get the damn thing:")
		        return(inv)
		}else{
				message("The inverse of this matrix is not in cache. 
				I'll calculate it explicitly and save it.")
				
				newinv<-x$setinv(inputmat)
				newinv
		}
				
}
