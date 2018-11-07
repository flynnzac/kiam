#' return the dimension of a vector x as c(length(x),1)
#'
#' @param x the object to find the dimension of.  Does exactly what vanilla R does if x is not a vector.  If x is a vector, returns c(length(x),1).
#' @export
vdim <- function (x)
{
  if (is.vector(x))
    c(length(x),1)
  else
    .Primitive("dim")(x)
}

#' @export
vnrow <- function (x)
  vdim(x)[1L]

#' @export
vncol <- function (x)
  vdim(x)[2L]

#' subsets matrices, keeping the dimensionality intact
#'
#' When you subset a matrix or data frame and the condition ends up only grabbing one row, R returns a vector instead of a matrix with one row.  This function returns a matrix with one row.
#' @param x the matrix or data frame to subset
#' @param rows either a vector of row indicies or a logical vector giving which rows to select from \code{x}
#' @param cols either a vector of column indicies or a logical vector giving which rows to select from \code{x}
#' @return a matrix subsetted to include only the rows and columns indicated but with its dimensionality preserved.
#' @export
mind <- function (x, rows=NULL, cols=NULL)
{
  if (is.null(rows))
    rows <- 1:nrow(x)

  if (is.null(cols))
    cols <- 1:ncol(x)

  if (is.logical(rows))
    rows <- which(rows)

  if (is.logical(cols))
    cols <- which(cols)

  if (is.vector(x))
    x[rows]
  else
  {
    if (length(rows)==1)
      t(as.matrix(x[rows,cols]))
    else if (length(cols)==1)
      as.matrix(x[rows,cols])
    else
      x[rows,cols]
  }

}
