simulate <- function(x){
      UseMethod('simulate', x)      
}

simulate.shark <- function(x){NextMethod()}

simulate.default <- function(x){print('Doing nothing, some attributes may be missing')}