% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sb10hd.R
\name{sb10hd}
\alias{sb10hd}
\title{sb10hd}
\usage{
sb10hd(n, m, np, ncon, nmeas, a, b, c, d, tol, ldwork)
}
\description{
H2 optimal state controller for a continuous-time system
}
\examples{
  To compute the matrices of the H2 optimal n-state controller

           | AK | BK |
       K = |----|----|
           | CK | DK |

  for the system

                | A  | B1  B2  |   | A | B |
            P = |----|---------| = |---|---| ,
                | C1 |  0  D12 |   | C | D |
                | C2 | D21 D22 |

  where B2 has as column size the number of control inputs (NCON)
  and C2 has as row size the number of measurements (NMEAS) being
  provided to the controller.

  It is assumed that

  (A1) (A,B2) is stabilizable and (C2,A) is detectable,

  (A2) The block D11 of D is zero,

  (A3) D12 is full column rank and D21 is full row rank.

#'
}
\references{
\url{http://slicot.org/objects/software/shared/doc/SB10HD.html}
}
