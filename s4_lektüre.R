Sys.setenv(LANG = "en")

library(methods)

# set S4 class 
setClass("Person",                # class name
         slots = c(               # attributes
           name = "character", 
           age = "numeric"
         )
)

john <- new("Person", name = "John Smith", age = NA_real_)    # new object of 
                                                              # class "Person"
str(john)
is(john)      # class
john@name     # slot
slot(john, "age")

setGeneric("age", function(x) standardGeneric("age"))
setGeneric("age<-", function(x, value) standardGeneric("age<-"))

setMethod("age", "Person", function(x) x@age)         # age: name of the function
                                                      # "Person": class
                                                      # function
setMethod("age<-", "Person", function(x, value) {
  x@age <- value
  x
})

age(john) <- 50   # method "age" applied
age(john)

?period

setClass("Person",                   # define new class "Person"
         slots = c(                  # characteristics of class
           name = "character", 
           age = "numeric"
         ), 
         prototype = list(           # default values for slots
           name = NA_character_,
           age = NA_real_
         )
)

me <- new("Person", name = "Hadley")  # new object "me" of class "Person"
str(me)

setClass("Employee", 
         contains = "Person",         # class "Employee" inherits from class "Person"
         slots = c(                   # class "Employee" consists of:
           boss = "Person"            # boss of class "Person" with name and age
         ),
         prototype = list(
           boss = new("Person")
         )
)

str(new("Employee"))

is(new("Employee"))           # what classes does "Employee" inherit from?
is(john, "Person")            # does john inherit from class "Person"?

new()                         # low-level constuctor

# helper
Person <- function(name, age = NA) {   # function must have the same name as class
  age <- as.double(age)
  
  new("Person", name = name, age = age)
}
Person("Hadley")

# validator
Person("Hadley", age = c(30, 37))   # actually wrong because age must be one value

setValidity("Person", method = function(object) {  # validation function for class "Person"
  if (length(object@name) != length(object@age)) { # message if error
    "@name and @age must be same length"
  } else {
    TRUE                                           # otherwise return TRUE
  }
})

Person("Hadley", age = c(30, 37))   # now it produces the error
Person("Hadley")
alex <- new("Person", name = "Alex", age = 45)
alex@age <- 1:10                    # still wrong if we just overwrite the previous value
str(alex)
validObject(alex)                   # now it shows error

utils::person()
?person

setClass("Person",                                # new class
         slots = c(
           name = "character",
           age = "numeric",
           given = "character",
           family = "character",
           email = "character",
           role = "character",
           comment = "character"
         ),
         prototype = list(
           name = NA_character_,
           age = NA_real_,
           given = NA_character_,
           family = NA_character_,
           email = NA_character_,
           role = NA_character_,
           comment = NA_character_
         )
)

Person <- function(name = NA_character_,           # helper
                   age = NA_real_,
                   given = NA_character_,
                   family = NA_character_,
                   email = NA_character_,
                   role = NA_character_,
                   comment = NA_character_) {
  age <- as.double(age)
  new("Person", name = name,
      age = age,
      given = given,
      family = family,
      email = email,
      role = role,
      comment = comment)
}

setValidity("Person", function(object) {          # validation
  
  if (length(object@name) != 1 |
    length(object@age) != 1 |
    length(object@given) != 1 |
    length(object@family) != 1 |
    length(object@email) != 1 |
    length(object@role) != 1 |
    length(object@comment) != 1 ) {
    stop("attributes must be of length 1")
  }
  
  if (!all(object@role %in% c(NA_character_,
                             "aut", "com", "cph", "cre", "ctb", "ctr",
                             "dtc", "fnd", "rev", "ths", "trl"))) {
    stop(paste("@role (s)  must be one of", 
          paste(c(NA_character_, "aut", "com", "cph", "cre", "ctb", "ctr",
                  "dtc", "fnd", "rev", "ths", "trl"),
                collapse = ", "), "."))
  }
  TRUE
})

maria <- new("Person", name = "maria", age = c(10, 15), role = c("cre"))  # error
validObject(maria)
str(maria)

Person("maria")


setClass("bullshit", slots = NULL)

# generic: to conduct method dispatch
# generic version of a function
setGeneric("myGeneric", function(x) standardGeneric("myGeneric"))

setGeneric("myGeneric",    # name of a generic function
           function(x, ..., verbose = TRUE) standardGeneric("myGeneric"),
           signature = "x" # can be any argument or everything except ...
)

setMethod("myGeneric", "Person", function(x) {
  # method implementation
})


args(getGeneric("show"))

setMethod("show", "Person", function(object) {  # method "show" for object from class "Person"
  cat(is(object)[[1]], "\n",
      "  Name: ", object@name, "\n",
      "  Age:  ", object@age, "\n",
      sep = ""
  )
})
john
maria
show(maria)

# to access slots one should not use @slot_name
# better use accessors:
setGeneric("name", function(x) standardGeneric("name"))   # why do we need it?
setMethod("name", "Person", function(x) x@name)
name(john)

setGeneric("name<-", function(x, value) standardGeneric("name<-"))
setMethod("name<-", "Person", function(x, value) {
  x@name <- value
  validObject(x)
  x
})

name(john) <- "Jon Smythe"
name(john)


name(john) <- letters

