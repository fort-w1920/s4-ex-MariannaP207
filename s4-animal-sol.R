Sys.setenv(LANGUAGE = "en")

### Animal Kingdom
library(methods)
library(tidyverse)


# helper function to simulate random names
make_name <- function(length = 7) {
  vowels <- c("a", "e", "i", "o", "u")
  consonants <- setdiff(letters, vowels)
  name <- character(length)
  name[1] <- sample(toupper(consonants), 1)
  name[seq(3, length, by = 2)] <-
    sample(consonants, size = ceiling(length / 2) - 1, replace = TRUE)
  name[seq(2, length, by = 2)] <-
    sample(vowels, size = floor(length / 2), replace = TRUE)
  paste(name, collapse = "")
}

################################################################################
#################### Create classes and constructor functions ##################

## create class "animal" with three attributes:
# @name
# @weight
# @female
setClass("animal",
  slots = list(
    name = "character",
    weight = "numeric",
    female = "logical"
  )
)


## create the underclass "prey" that inherits from class "animal"
# with argument @hide:
setClass("prey",
  contains = "animal",
  slots = list(hide = "numeric")
)


## create the underclass "predator" that also inherits from class "animal"
# but with argument @seek:
setClass("predator",
  contains = "animal",
  slots = c(
    seek = "numeric"
  )
)


# write a function that creates all underclasses
# based on the received class name, parent class
# and checks for validity:
set_class_animals <- function(class_name,
                              parent_class,
                              min_weight, max_weight,
                              min_param, max_param) {

  # define which parameter @seek or @hide must be used based on
  # the fact whether the animal is a prey or a predator:
  param <- ifelse(parent_class == "prey", "hide", "seek")

  # define class object with the corrsponding parent class:
  setClass(class_name,
    contains = parent_class,

    # validate if the attributes of the created object are correct
    # and within the appropriate range:
    validity = function(object) {
      invalids <- character(0)

      no_name <- nchar(object@name) == 0
      wrong_sex <- (is.na(object@female) || length(object@female) > 1)

      if (no_name) invalids <- "animals need a 'name'."
      if (wrong_sex) invalids <- c(invalids, "animals must have a gender of length 1.")
      if (!between(slot(object, param), min_param, max_param)) {
        invalids <- c(invalids, paste0(param, " must be in [", min_param, ", ", max_param, "]."))
      }
      if (!between(object@weight, min_weight, max_weight)) {
        invalids <- c(
          invalids,
          paste0("weight must be in [", min_weight, ", ", max_weight, "].")
        )
      }

      if (length(invalids)) invalids else TRUE
    }
  )
}

# define all animal classes with the corresponding ranges for their attributes:
set_class_animals("mouse",
  parent_class = "prey",
  min_weight = 0.5, max_weight = 1,
  min_param = 0.6, max_param = 1
)
set_class_animals("rabbit",
  parent_class = "prey",
  min_weight = 1, max_weight = 5,
  min_param = 0.3, max_param = 0.8
)
set_class_animals("deer",
  parent_class = "prey",
  min_weight = 15, max_weight = 30,
  min_param = 0.2, max_param = 0.7
)
set_class_animals("hawk",
  parent_class = "predator",
  min_weight = 3, max_weight = 8,
  min_param = 0.6, max_param = 1
)
set_class_animals("lynx",
  parent_class = "predator",
  min_weight = 20, max_weight = 60,
  min_param = 0.5, max_param = 0.9
)


# Write helper (constructor) functions for the preys.
# If no values provided the functions simulate random values
# based on the default settings:
mouse <- function(name = make_name(length = sample(3:10, size = 1)),
                  weight = runif(1, 0.5, 1),
                  female = TRUE,
                  hide = runif(1, 0.6, 1)) {
  new("mouse", name = name, weight = weight, female = female, hide = hide)
}

rabbit <- function(name = make_name(length = sample(3:10, size = 1)),
                   weight = runif(1, 1, 5),
                   female = TRUE,
                   hide = runif(1, 0.3, 0.8)) {
  new("rabbit", name = name, weight = weight, female = female, hide = hide)
}

deer <- function(name = make_name(length = sample(3:10, size = 1)),
                 weight = runif(1, 15, 30),
                 female = TRUE,
                 hide = runif(1, 0.2, 0.7)) {
  new("deer", name = name, weight = weight, female = female, hide = hide)
}


# helper (constructor) functions for the predators:
hawk <- function(name = make_name(length = sample(3:10, size = 1)),
                 weight = runif(1, 3, 8),
                 female = TRUE,
                 seek = runif(1, 0.6, 1)) {
  new("hawk", name = name, weight = weight, female = female, seek = seek)
}

lynx <- function(name = make_name(length = sample(3:10, size = 1)),
                 weight = runif(1, 20, 60),
                 female = TRUE,
                 seek = runif(1, 0.5, 0.9)) {
  new("lynx", name = name, weight = weight, female = female, seek = seek)
}



################################################################################
############################ Methods for animals################################
################################################################################

### Method "show" --------------------------------------------------------------
setMethod(
  "show", "animal",
  function(object) {
    prey <- is(object)[2] == "prey"

    if (prey) param <- "hide" else param <- "seek"
    if (slot(object, "female") == TRUE) sex <- "(f)" else sex <- "(m)"

    cat(is(object)[1], " '", object@name, "' ", sex, "\n",
      "   weight:  ", object@weight, "\n",
      "   ", param, ":  ", slot(object, param), "\n",
      sep = ""
    )
  }
)


### Method "meet" --------------------------------------------------------------

setGeneric(
  "meet",
  function(animal1, animal2) {
    standardGeneric("meet")
  }
)


## Method "meet" for two preys:
# three outcomes possible:
# - ignore each other
# - sniff each other
# - make babies

setMethod("meet",
  signature = c(animal1 = "prey", animal2 = "prey"),

  function(animal1, animal2) {

    # same animal
    if (identical(animal1, animal2)) {
      return(meet_identical(animal1))
    }

    # same class but different sex:
    else if (class(animal1) == class(animal2) &
      animal1@female != animal2@female) {
      output_message <- sample(c(
        " ignore each other ",
        " sniff each others' butts ",
        rep(" make sweet, sweet love ", 2)
      ), 1)
    }

    # otherwise:
    else {
      output_message <- sample(c(
        " ignore each other ",
        " sniff each others' butts "
      ), 1)
    }

    # output:
    return(make_output(animal1, animal2, output_message))
  }
)


## Method "meet" for a prey and a predator
# four outcomes possible:
# - predator kills the prey
# - prey escapes the predator
# - ignore each other
# - sniff each other

setMethod(
  "meet",

  signature("prey", "predator"),

  function(animal1, animal2) {

    # depending on the prey's and predator's weight different
    # probability for killing or escaping:
    if (between(animal1@weight, 0.05 * animal2@weight, 0.7 * animal2@weight)) {
      kill_prob <- min(1, max(0, 0.6 + animal2@seek - animal1@hide))
      output_message <- sample(c(
        " kills and eats ",
        " escapes from "
      ), 1, prob = c(kill_prob, 1 - kill_prob))
    }  
    
    else {
      # otherwise:
      output_message <- sample(c(
        " ignore each other ",
        " sniff each others' butts "
      ), 1) }
      return(make_output(animal1, animal2, output_message))
  }
)

## Method "meet" for predator & prey same as for prey & predator
# call previous method with changed arguments
setMethod(
  "meet",
  signature("predator", "prey"),
  function(animal1, animal2) {
    callGeneric(animal2, animal1)
  }
)


## Method "meet" for two predators
# four outcomes possible:
# - fight each other
# - make babies
# - ignore each other
# - sniff each other

setMethod("meet",
  signature = c(animal1 = "predator", animal2 = "predator"),

  function(animal1, animal2) {

    # same animal
    if (identical(animal1, animal2)) {
      return(meet_identical(animal1))
    }

    # same class but different sex:
    else if ((class(animal1) == class(animal2)) &
      (animal1@female != animal2@female)) {
      output_message <- sample(c(
        " fight for territory ",
        " make sweet, sweet love "
      ), 1)
    }

    # otherwise:
    else {
      output_message <- sample(c(
        " ignore each other ",
        " sniff each others' butts ",
        " fight for territory "
      ), 1)
    }

    return(make_output(animal1, animal2, output_message))
  }
)

make_output <- function(animal1 , animal2, output_message) {
  
  possible_outcomes <- c(" ignore each other ",
                         " sniff each others' butts ",
                         " make sweet, sweet love ",
                         " fight for territory ",
                         " kills and eats ",
                         " escapes from ")
  
  output_message <- match.arg(output_message, possible_outcomes)
  
  output <- switch(output_message,
                   " kills and eats " = paste0(
                     class(animal2), " '", animal2@name, "'", output_message,
                     class(animal1), " '", animal1@name, "'", "\n"
                   ),
                   " escapes from " = paste0(
                     class(animal1), " '", animal1@name, "'", output_message,
                     class(animal2), " '", animal2@name, "'", "\n"
                   ),
                   # default case
                   paste0(
                     class(animal1), " '", animal1@name, "' & ",
                     class(animal2), " '", animal2@name, "'", output_message, "\n"
                   ))
  output
}


meet_identical <- function(object) {
  paste0(
           class(object), " '", object@name,
           "' gazes at ", ifelse(object@female, "her", "his"),
           " reflection in a puddle", "\n")
}
