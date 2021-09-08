#' ab13dd
#'
#' L-infinity norm of a state space system
#' @examples 

#'   To compute the L-infinity norm of a continuous-time or
#'   discrete-time system, either standard or in the descriptor form,
#' 
#'                                   -1
#'      G(lambda) = C*( lambda*E - A ) *B + D .
#' 
#'   The norm is finite if and only if the matrix pair (A,E) has no
#'   eigenvalue on the boundary of the stability domain, i.e., the
#'   imaginary axis, or the unit circle, respectively. It is assumed
#'   that the matrix E is nonsingular.
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/AB13DD.html}
#' @export
ab13dd <- function(dico, jobe, equil, jobd, n, m, p, fpeak, a, e, b, c, d, tol) {

    # In Parameters
    dico <- as.character(dico)
    equil <- as.character(equil)
    jobd <- as.character(jobd)
    jobe <- as.character(jobe)
    m <- as.integer(m)
    n <- as.integer(n)
    p <- as.integer(p)
    tol <- as.double(tol)

    gpeak <- array(as.double(0), c(2))
    info <- as.integer(0)
    iwork <- array(as.integer(1), c(n))
    lcwork <- as.integer(max(1, (n + m) * (n + p) + 2 * min(p, m) + max(p, m)))
    ldwork <- as.integer(max(1, 15 * n * n + p * p + m * m + (6 * n + 3) * (p + m) + 4 * p * m + n * m + 22 * n + 7 * min(p, m)))
    dwork <- array(as.double(1), c(ldwork))
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]
    ldd <- dim(d)[1]
    lde <- dim(e)[1]


    res <- .Fortran("AB13DD", DICO = dico, JOBE = jobe, EQUIL = equil, JOBD = jobd, N = n, M = m, P = p, FPEAK = fpeak, A = a, LDA = lda, E = e, LDE = lde, B = b, LDB = ldb, C = c, LDC = ldc, D = d, LDD = ldd, GPEAK = gpeak, TOL = tol,
        IWORK = iwork, DWORK = dwork, LDWORK = ldwork, LCWORK = lcwork, INFO = info)

    return(list(fpeak = res$FPEAK, gpeak = res$GPEAK, info = res$INFO))
}
