
errate = function(fun) {
  function(...) {
    warn <- err <- NULL
    result <- withCallingHandlers(
      tryCatch(
        fun(...),
        error=function(e) {
          err <<- conditionMessage(e)
          NULL
        }),
      warning=function(w) {
        warn <<- append(warn, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
    return(list(result=result, err=err, warn=warn))
  }
}

hasError = function(x) {
  !is.null(x[["err"]])
}

hasWarning = function(x) {
  !is.null(x[["warn"]])
}

hasErrata = function(x) {
  (hasError(x) || hasWarning(x))
}

errata = function(x) {
  list(err = x[["err"]],
       warn = x[["warn"]])
  # vs. x[["result"]] = NULL
}

unErrate = function(x) {
  x[["result"]]  
}

# factory <- function(fun)
#   function(...) {
#     warn <- err <- NULL
#     val <- withCallingHandlers(
#       tryCatch(fun(...), error=function(e) {
#         err <<- conditionMessage(e)
#         NULL
#       }), warning=function(w) {
#         warn <<- append(warn, conditionMessage(w))
#         invokeRestart("muffleWarning")
#       })
#     list(value=val, warn=warn, err=err)
#   }
# 
# .has <- function(x, what)
#   !sapply(lapply(x, "[[", what), is.null)
# hasWarning <- function(x) .has(x, "warn")
# hasError <- function(x) .has(x, "err")
# isClean <- function(x) !(hasError(x) | hasWarning(x))
# cleanv <- function(x) sapply(x[isClean(x)], "[[", 1)
# factoryValue <- function(x) sapply(x, "[[", 1)
# factoryStatus <- function(x) sapply(x, "[[", c(2, 3))
# 
# 
# withWarnings <- function(expr) {
#   myWarnings <- NULL
#   wHandler <- function(w) {
#     myWarnings <<- c(myWarnings, list(w))
#     invokeRestart("muffleWarning")
#   }
#   val <- withCallingHandlers(expr, warning = wHandler)
#   list(value = val, warn = myWarnings)
# }
# 
# 
# 
# .has <- function(x, what)
#   !sapply(lapply(x, "[[", what), is.null)
# hasWarning <- function(x) .has(x, "warn")
# hasError <- function(x) .has(x, "err")
# isClean <- function(x) !(hasError(x) | hasWarning(x))
# cleanv <- function(x) sapply(x[isClean(x)], "[[", 1)
# factoryValue <- function(x) sapply(x, "[[", 1)
# factoryStatus <- function(x) sapply(x, "[[", c(2, 3))


