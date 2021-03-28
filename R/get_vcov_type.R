#' internal function
#'
#' @param v a string or formula describing the standard error type
#' @keywords internal

get_vcov_type <- function(vcov) {

  get_vcov_type_inner <- function(v) {
    if (is.character(v)) {
       if (v %in% c("robust", "classical", "stata", "classical", "constant")) {
          out <- tools::toTitleCase(v)
       } else {
          out <- toupper(v)
       }
    } else if (inherits(v, "formula")) {
      out <- paste("C:", as.character(v)[2])
    } else {
      out <- ""
    }
    return(out)
  }

  vcov_type_flag <- !all(sapply(vcov, is.null))

  for (v in vcov) {
    if (!checkmate::test_formula(v) &&
        !checkmate::test_character(v, len = 1, null.ok = TRUE)) {
       vcov_type_flag <- FALSE 
    }
  }

  if (vcov_type_flag == TRUE) {
    vcov_type <- sapply(vcov, get_vcov_type_inner)
  } else {
    vcov_type <- NULL
  }

  return(vcov_type)
}

