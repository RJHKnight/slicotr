% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mb05md.R
\name{mb05md}
\alias{mb05md}
\title{mb05md}
\usage{
mb05md(balanc, n, delta, a)
}
\description{
Matrix exponential for a real non-defective matrix
}
\examples{
  To compute exp(A*delta) where A is a real N-by-N non-defective
  matrix with real or complex eigenvalues and delta is a scalar
  value. The routine also returns the eigenvalues and eigenvectors
  of A as well as (if all eigenvalues are real) the matrix product
  exp(Lambda*delta) times the inverse of the eigenvector matrix
  of A, where Lambda is the diagonal matrix of eigenvalues.
  Optionally, the routine computes a balancing transformation to
  improve the conditioning of the eigenvalues and eigenvectors.

#'
}
\references{
\url{http://slicot.org/objects/software/shared/doc/MB05MD.html}
}
