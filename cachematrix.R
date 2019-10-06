makeCacheMatrix <- function(x = matrix()) #This function creates a special 
                                          #"matrix" object that can cache
                                          #its inverse.
                                          #define a default arg mode with "matrix".
{
        invrtd <- NULL #NULL will hold the value for the inverse marix right now.
        
        set <- function(y) #Define the set function to assign a new value(s) of
                           #the matrix from the parent environment.
        { 
                x <<- y #Value of the matrix from the parent environment is assigned.
                invrtd <- NULL #If there is a new matrix, invrtd is back to NULL.
        }
        
        get <- function() x #Define the "get" function and create an envrironment
                            #for x and values to live in.
        
        setinverse <- function(inverse) invrtd <<- inverse #Assign value to invrtd
                                                           #from the parent 
                                                           #environment and create 
                                                           #another environment.
        
        getinverse <- function() invrtd #Gets the value of invrtd when called, 
                                        #creates an environment that has invrtd.
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        #You need this ^ in order to refer to the functions with the $ operator 
        #and we like the $ operator.
        
}

#____________________________________________________________________________________


cacheSolve <- function(x, ...) #Supposed to return a function that is the inverse 
                               #matrix of the "matrix" object of the result 
                               #of makeCacheMatrix(x).
{
        invrtd <- x$getinverse()
        if (!is.null(invrtd)) #Our ~default~ for "invrtd" was NULL, this only 
                              #changes when "y" has a value/our "data" already 
                              #existed.
        {
                message("Getting cached data!") #This is executed if our 
                return(invrtd)                  #"invrtd" is not NULL.
        } 
        #Below is what is executed if "invrtd" is NULL.
        
        data_g <- x$get() #Getting our data to invert.
        invrtd <- solve(data_g, ...) #Inverting our data using solve(). 
        x$setinverse(invrtd) #Going through values.
        invrtd #Print our invrtd and we are super happy b/c we have our inverse.
}

#____________________________________________________________________________________


