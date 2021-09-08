#' tc01od_l
#'
#' Dual of a left/right polynomial matrix representation
#' @examples 

#'   To find the dual right (left) polynomial matrix representation of
#'   a given left (right) polynomial matrix representation, where the
#'   right and left polynomial matrix representations are of the form
#'   Q(s)*inv(P(s)) and inv(P(s))*Q(s) respectively.
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/TC01OD.html}
#' @export
tc01od_l <- function(m, p, indlim, pcoeff, qcoeff) {

    # In Parameters
    indlim <- as.integer(indlim)
    m <- as.integer(m)
    p <- as.integer(p)

    leri <- as.character("l")
    info <- as.integer(0)
    ldpco1 <- dim(pcoeff)[1]
    ldpco2 <- dim(pcoeff)[2]
    ldqco1 <- dim(qcoeff)[1]
    ldqco2 <- dim(qcoeff)[2]


    res <- .Fortran("TC01OD", LERI = leri, M = m, P = p, INDLIM = indlim, INFO = info, PCOEFF = pcoeff, QCOEFF = qcoeff, LDPCO1 = ldpco1, LDPCO2 = ldpco2, LDQCO1 = ldqco1, LDQCO2 = ldqco2)

    return(list(info = res$INFO, pcoeff = res$PCOEFF, qcoeff = res$QCOEFF))
}
