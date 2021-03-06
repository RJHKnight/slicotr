% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sb10jd.R
\name{sb10jd}
\alias{sb10jd}
\title{sb10jd}
\usage{
sb10jd(n, m, np, a, b, c, d, e, ldwork)
}
\description{
Converting a descriptor state-space system into regular state-space form
}
\examples{
  To convert the descriptor state-space system

  E*dx/dt = A*x + B*u
        y = C*x + D*u

  into regular state-space form

   dx/dt = Ad*x + Bd*u
       y = Cd*x + Dd*u .

#'
}
\references{
\url{http://slicot.org/objects/software/shared/doc/SB10JD.html}
}
