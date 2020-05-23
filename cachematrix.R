
#  *****   Function 1:  makeCacheMatrix   ******* #

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix())   # Here we have defined x as a function argument which implies that further initialization within function isn't required. The value of x is initialised as a empty matrix
{   
  k <- NULL					        # Initialising the object m as NULL
  set <- function(y) {		  # We have defined a setter function called set() for setting the input argument to the objects. It takes an argurmnt "y" which in our case is a matrix		
    x <<- y			          # Assigns the input argument to the  object x in the parent environment
    k <<- NULL			      # Here we are setting vale of K to NULL since the input atgument x has been changed and we no longer want the old value that was in k
  }
  get <- function() x		  # Here x is not defined within the get() function and hence value of x is extracted from the parent environment of makeCacheMatrix() function
  
  setinvmat <- function(invmat) k <<- invmat  # Here we assign the input argument to the value of 'k' in the parent environment
  getinvmat <- function() k	                  # This getter getinvmat() function is similar to the getter function for x mentioned above. Both work on the concept of lexical scoping	
  
  # This is a named list which will be returned once this function is executed.Each of the functions here is assigned as an element in the list. This list which is formed is returned to the parent environment
  list( set = set, 			          # set() function defined above is given the name set
        get = get,			          # get() function defined above is given the name get
        setinvmat = setinvmat,		# setinvmat() function defined above is given the name setinvmat
        getinvmat = getinvmat     # getinvmat() function defined above is given the name getinvmat
  )		
  # Since list returned above is named it will be easier to access the functions using the names with the $ operator which is convenient and easy than using [[ ]]
}



#  *****   Function 2:  cacheSolve   ******* #

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
# It is expected to get the populate and/or to retrieve the Inverse of a matrix from the object of type makeCacheMatrix ()

cacheSolve <- function(x, ...)    # Here we have x as one of the argument followed by ellipsis using which allows additional arguments to be passed into the function
{	
  k <- x$getinvmat()	        # We are calling the getinvmat() function from object that has beeen passed as the argument and stores it in 'k'	
  if(!is.null(k))                     # Here we are checking whether k is not null, if it is not NULL it would mean that the input has not changed 
  {	
    message("getting cached data")	# Display message flashed to the end user Indicating that the value displayed is being got from the cache and displayed
    return(k)		                    # This line terminates the execution of the function and returns the value present in object 'k'
  }
  
  # if value of !is.null(k) returns as FALSE the control comes here and the below code is executed
  matdata <- x$get()	      # since k is null ther control comes here and we get the matrix from the input object using the get() function aand store the matrix in 'matdata'
  k <- solve(matdata, ...)	# Here the inverse of the matdata is being calculated using the solve() function
  x$setinvmat(k)	          # This line of code uses the setinvmat() function on input object to set the Inverse of the matrix in the input object
  k			
}