################################################################################
# Guidelines:
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.
#
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
################################################################################

###############################################################################
# Orientações:                                                                #
# Inversão da matriz é geralmente um cálculo dispendioso e pode haver algum   #
# benefício se colocar o resultado em cache.                                  #
# As duas funções são usadas para armazenar em cache o inverso de uma matriz. #
#                                                                             #
# A função MakeCacheMatrix cria uma lista contendo uma função para:           #
# 1. definir o valor da matriz                                                #
# 2. obter o valor da matriz                                                  #
# 3. definir o valor da inversa da matriz                                     #
# 4. obter o valor do inverso da matriz                                       #
###############################################################################
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

################################################################################
# Guidelines:
# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.
#
# This function assumes that the matrix is always invertible.
################################################################################

###############################################################################
# Orientações:                                                                #
# A seguinte função retorna o inverso da matriz. Ele primeiro verifica se     #
# O inverso já foi calculado. Se sim, ele recebe o resultado e ignora o pro-  # 
# cessamento. Se não, ele calcula o inverso, define o valor no cache via      #
# Função setinverse.                                                          #
#                                                                             #
# Esta função assume que a matriz é sempre invertida.                         #
###############################################################################
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
