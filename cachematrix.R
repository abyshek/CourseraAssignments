## -------------------------------------------------------------------------------------
## Following set of two functions takes a matrix as input, calculates its inverse,      |
## stores it in a cache variable and returns the result. The function returns an        |
## error message if determinant of the input matrix is zero.                  |         |
## However, if the inverse of the matrix provided has already been calculated,          |
## it simply returns the value stored in the cache.This process is performed by two     |
## functions namely makecachematrix() and cachesolve().Example of usage                 |
##                                                                                      |
## > a<-matrix((2,5,7,9),ncol=2,nrow=2)                                                 |
## > b<-makecachematrix(a)                                                              |
## > cachesolve(b)                                                                      |
##--------------------------------------------------------------------------------------



##------------------------------------------------------------------------------------------
## makecachematrix(): This function takes a matrix as an input and creates a special        | 
## vector(actualy a list containing four functions).This function also stores the mean      |
## of the input matrix in the global environment and uses the scoping rules of R.           |
## Comments have been provided wherever a new object/variable has been defined/introduced.  |
## As per the instructions of assignment, comments for sub-functions have not been provided.|                                                                                                                       |
##------------------------------------------------------------------------------------------

makecachematrix <- function(x = matrix())  # create a function makecachematrix with a matrix input
{
        
        inv <- NULL                        # inverse of the matrix is stored in object "inv" and is initialized as NULL
        setmatrix<- function(y)            # set the matrix using "<<-" assignment in the objects                 
        {
                x<<-y
                inv <<-NULL
        }
        
        getmatrix<-function() x             # get the input matrix     
        setinverse <- function(inverse0) inv<<-inverse0
        getinverse<-function() inv
        
        # create a list of four sub-functions
        list(setmatrix=setmatrix, getmatrix=getmatrix,setinverse=setinverse,getinverse=getinverse) 
        
}




## ----------------------------------------------------------------------------------------
## cachesolve(): This function takes the special matrix from makecachematrix() function    |
## and returns its inverse. It verifies if the inverse already exists in the object "inv"  |
## and returns the exisitng value if it is in the memory with a message. If not been       |
## calculated earlier,it gets the input matrix from get() sub-function of makecachematrix()|
## and calculates its determinant. If determinant is zero, then it displays appropriate    |
## message. Else, it calculates and returns the inverse of the inputted matrix.            |                            |
##-----------------------------------------------------------------------------------------

cachesolve <- function(z, ...)         
{
        
        inv<-z$getinverse()
        if(!is.null(inv))
        {
                message("Inverse of this matrix has already been calculated.....
                        Getting the cached data")
                return(inv)
        }
        
        data<-z$getmatrix()                  # store the special vector from makecachematrix() in an object "data"
        if(det(data)==0)                     # Check if the determinant of input matrix is zero                              
        {
                message("ERROR :This is a singular matrix and its inverse cannot be determined")
                return()
        }
        
        inv<-solve(data,...)           # assign the inverse of input matrix to object "inv"
        z$setinverse(inv)
        inv
        
        
}
