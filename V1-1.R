setClass("Environment", contains = "matrix", slots = list(resistance = "numeric",
                                                          foodValue = "numeric",
                                                          foodDensity = "numeric",
                                                          foodType = "character",
                                                          height = "numeric"))

setClass("Axial", contains = "vector")
setClass("Cube", contains = "vector")
setClass("Odd_r", contains = "vector")

Axial <- function(q,r) {
  axialHex <- new("Axial", c(q,r))
  names(axialHex) <-  c("q","r")
  axialHex
}

Cube <- function(x,y,z) {
  cubeHex <- new("Cube", c(x,y,z))
  names(cubeHex) <- c("x","y","z")
  cubeHex
}

Odd_r <- function(col, row) {
  odd_rHex<- new("Odd_r", c(col, row))
  names(odd_rHex) <- c("col", "row")
  odd_rHex
}

setGeneric("toCube", function(object) stop("no toCube method for object of class", class(object), "\n"))
setGeneric("toOdd_r", function(object) stop("no toOdd_r method for object of class", class(object), "\n"))
setGeneric("toAxial", function(object) stop("no toAxial method for object of class", class(object), "\n"))

setMethod(toCube, "Axial", function(object) {
  x <- object["q"]
  z <- object["r"]
  y <- -x-z
  Cube(x,y,z)
})

setMethod(toCube, "Odd_r", function(object) {
  x <- object["col"] - (object["row"] - (bitwAnd(object["row"], 1))) / 2
  z <- object["row"]
  y <- -x-z
  Cube(x,y,z)
})

setMethod(toOdd_r, "Cube", function(object){
  col = object["x"] + (object["z"] - (bitwAnd(object["z"],1))) / 2
  row = object["z"]
  Odd_r(col, row)
})

setMethod(toOdd_r, "Axial", function(object){
  toOdd_r(toCube(object))
})

setMethod(toAxial, "Cube", function(object){
  q = object["x"]
  r = object["z"]
  Axial(q,r)
})

setMethod(toAxial, "Odd_r", function(object){
  toAxial(toCube(object))
})

Environment <- function(resistance, foodValue, foodDensity, foodType, height, ...){
  new("Environment", matrix(seq_along(resistance), ...), resistance = resistance, foodValue = foodValue, foodDensity = foodDensity, foodType = foodType, height = height)
}

setMethod(show, "Environment", function(object) {
  cat("class:", class(object), "\n")
  cat("dim:", dim(object), "\n")
  m <- object@.Data
  m[] <- object@resistance
  cat("resistance:\n"); print(m)
  m[] <- object@foodValue
  cat("foodValue:\n"); print(m)
  m[] <- object@foodDensity
  cat("foodDensity:\n"); print(m)
  m[] <- object@foodType
  cat("foodType:\n"); print(m)
  m[] <- object@height
  cat("height:\n"); print(m)
})

setMethod("[", "Environment", function(x, i, j, ..., drop=TRUE) {
  m <- callNextMethod()
  initialize(x, m, resistance=x@resistance[m], foodValue=x@foodValue[m], foodDensity=x@foodDensity[m], foodType=x@foodType[m], height=x@height[m])
})

test <- Environment(0, 0, 0, "A", 10, 3, 3)
