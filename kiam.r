dim <- function (x)
{
  if (is.vector(x))
    c(length(x),1)
  else
    .Primitive("dim")(x)
}

nrow <- function (x)
  dim(x)[1L]

ncol <- function (x)
  dim(x)[2L]

ind <- function (x, rows=NULL, cols=NULL)
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
