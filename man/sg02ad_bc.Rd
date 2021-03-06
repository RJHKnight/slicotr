% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sg02ad_bc.R
\name{sg02ad_bc}
\alias{sg02ad_bc}
\title{sg02ad_bc}
\usage{
sg02ad_bc(dico, jobl, scal, sort, acc, n, m, p, a, e, b, q, r, l, tol, ldwork)
}
\description{
Solution of continuous- or discrete-time algebraic Riccati equations for descriptor systems
}
\examples{
  To solve for X either the continuous-time algebraic Riccati
  equation
                                -1
     Q + A'XE + E'XA - (L+E'XB)R  (L+E'XB)' = 0 ,              (1)

  or the discrete-time algebraic Riccati equation
                                     -1
     E'XE = A'XA - (L+A'XB)(R + B'XB)  (L+A'XB)' + Q ,         (2)

  where A, E, B, Q, R, and L are N-by-N, N-by-N, N-by-M, N-by-N,
  M-by-M and N-by-M matrices, respectively, such that Q = C'C,
  R = D'D and L = C'D; X is an N-by-N symmetric matrix.
  The routine also returns the computed values of the closed-loop
  spectrum of the system, i.e., the stable eigenvalues
  lambda(1),...,lambda(N) of the pencil (A - BF,E), where F is
  the optimal gain matrix,
          -1
     F = R  (L+E'XB)' ,        for (1),

  and
                 -1
     F = (R+B'XB)  (L+A'XB)' , for (2).
                           -1
  Optionally, matrix G = BR  B' may be given instead of B and R.
  Other options include the case with Q and/or R given in a
  factored form, Q = C'C, R = D'D, and with L a zero matrix.

  The routine uses the method of deflating subspaces, based on
  reordering the eigenvalues in a generalized Schur matrix pair.

  It is assumed that E is nonsingular, but this condition is not
  checked. Note that the definition (1) of the continuous-time
  algebraic Riccati equation, and the formula for the corresponding
  optimal gain matrix, require R to be nonsingular, but the
  associated linear quadratic optimal problem could have a unique
  solution even when matrix R is singular, under mild assumptions
  (see METHOD). The routine SG02AD works accordingly in this case.

#'
}
\references{
\url{http://slicot.org/objects/software/shared/doc/SG02AD.html}
}
