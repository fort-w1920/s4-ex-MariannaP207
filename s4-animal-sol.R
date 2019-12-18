### Animal Kingdom
library(methods)

## create class "animal" with three attributes:
# - name
# - weight
# - female 
setClass("animal",                       
         slots = c(
           name = "character",
           weight = "numeric",
           female = "logical"
         ),
         prototype = list(
           name = str_to_title(
             paste(
                   sample(letters, size = ceiling(runif(1, 3, 10))), 
                   collapse = "")
           ),
           weight = runif(1, 0, 100),
           female = TRUE
         )
)

## create underclass "prey" that inherits from class "animal":
setClass("prey",
         contains = "animal",
         slots = c(
           hide = "numeric"
         ),
         prototype = list(
           hide = runif(1, 0, 1)
         )
)

## create underclass "predator" that also inherits from class "animal":
setClass("predator",
         contains = "animal",
         slots = c(
           seek = "numeric"
         ),
         prototype = list(
           seek = runif(1, 0, 1)
         )
)

# now create underclasses for the class "prey":
setClass("mouse",
         contains = "prey",
         slots = c(
           weight = "numeric",
           hide = "numeric"
         ),
         prototype = list(
           weight = runif(1, 0.5, 1),
           hide = runif(1, 0.6, 1)
         )
)

setClass("rabbit",
         contains = "prey",
         slots = c(
           weight = "numeric",
           hide = "numeric"
         ),
         prototype = list(
           weight = runif(1, 1, 5),
           hide = runif(1, 0.3, 0.8)
         )
)

setClass("deer",
         contains = "prey",
         slots = c(
           weight = "numeric",
           hide = "numeric"
         ),
         prototype = list(
           weight = runif(1, 15, 30),
           hide = runif(1, 0.2, 0.7)
         )
)

# ... and underclasses for the class "predator":
setClass("hawk",
         contains = "predator",
         slots = c(
           weight = "numeric",
           seek = "numeric"
         ),
         prototype = list(
           weight = runif(1, 3, 8),
           seek = runif(1, 0.6, 1)
         )
)

setClass("lynx",
         contains = "predator",
         slots = c(
           weight = "numeric",
           seek = "numeric"
         ),
         prototype = list(
           weight = runif(1, 20, 60),
           seek = runif(1, 0.5, 0.9)
         )
)

### helper (constructor) functions for the preys:

mouse <- function(
  name = NA_character_,
  weight = NA_real_,
  female = NA,
  hide = NA_real_) {
  # validation
  new("mouse", name = name,
      weight = weight,
      female = female,
      hide = hide)
}

mouse()                          # why does not it work?
new("mouse")                     # and this does? but why does it always has the same values?

rabbit <- function(
  name = NA_character_,
  weight = NA_real_,
  female = NA,
  hide = NA_real_) {
  new("rabbit", name = name,
      weight = weight,
      female = female,
      hide = hide)
}

deer <- function(
  name = NA_character_,
  weight = NA_real_,
  female = NA,
  hide = NA_real_) {
  new("deer", name = name,
      weight = weight,
      female = female,
      hide = hide)
}

### helper (constructor) functions for the predators:
hawk <- function(
  name = NA_character_,
  weight = NA_real_,
  female = NA,
  seek = NA_real_) {
  new("hawk", name = name,
      weight = weight,
      female = female,
      seek = seek)
}

lynx <- function(
  name = NA_character_,
  weight = NA_real_,
  female = NA,
  seek = NA_real_) {
  new("lynx", name = name,
      weight = weight,
      female = female,
      seek = seek)
}


### validation functions
