% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sb04qd.R
\name{sb04qd}
\alias{sb04qd}
\title{sb04qd}
\usage{
sb04qd(n, m, a, b, c, ldwork)
}
\description{
Solution of discrete-time Sylvester equations (Hessenberg-Schur method)
}
\examples{
  To solve for X the discrete-time Sylvester equation

     X + AXB = C,

  where A, B, C and X are general N-by-N, M-by-M, N-by-M and
  N-by-M matrices respectively. A Hessenberg-Schur method, which
  reduces A to upper Hessenberg form, H = U'AU, and B' to real
  Schur form, S = Z'B'Z (with U, Z orthogonal matrices), is used.

#'
}
\references{
\url{http://slicot.org/objects/software/shared/doc/SB04QD.html}
}
