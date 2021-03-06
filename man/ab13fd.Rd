% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ab13fd.R
\name{ab13fd}
\alias{ab13fd}
\title{ab13fd}
\usage{
ab13fd(n, a, tol)
}
\description{
Computing the distance from a real matrix to the nearest complex matrix with an eigenvalue on the imaginary axis, using SVD
}
\examples{
  To compute beta(A), the 2-norm distance from a real matrix A to
  the nearest complex matrix with an eigenvalue on the imaginary
  axis. If A is stable in the sense that all eigenvalues of A lie
  in the open left half complex plane, then beta(A) is the complex
  stability radius, i.e., the distance to the nearest unstable
  complex matrix. The value of beta(A) is the minimum of the
  smallest singular value of (A - jwI), taken over all real w.
  The value of w corresponding to the minimum is also computed.

#'
}
\references{
\url{http://slicot.org/objects/software/shared/doc/AB13FD.html}
}
