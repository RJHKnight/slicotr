#' ab13bd
#'
#' H2 or L2 norm of a system
#' @examples 

#'   To compute the H2 or L2 norm of the transfer-function matrix G
#'   of the system (A,B,C,D). G must not have poles on the imaginary
#'   axis, for a continuous-time system, or on the unit circle, for
#'   a discrete-time system. If the H2-norm is computed, the system
#'   must be stable.
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/AB13BD.html}
#' @export
ab13bd <- function(dico, jobn, n, m, p, a, b, c, d, tol) {

    # In Parameters
    dico <- as.character(dico)
    jobn <- as.character(jobn)
    m <- as.integer(m)
    n <- as.integer(n)
    p <- as.integer(p)
    tol <- as.double(tol)

    # Out Parameters
    nq <- as.integer(0)
    iwarn <- as.integer(0)
    info <- as.integer(0)
    ab13bd <- as.double(0)

    # Hidden Parameters
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]
    ldd <- dim(d)[1]
    ldwork <- as.integer(max(1, max(m * (n + m) + max(n * (n + 5), max(m * (m + 2), 4 * p)), n * (max(n, p) + 4) + min(n, p))))
    dwork <- array(as.double(1), c(ldwork))

    res <- .Fortran("AB13BD", DICO = dico, JOBN = jobn, N = n, M = m, P = p, A = a, B = b, C = c, D = d, TOL = tol, NQ = nq, IWARN = iwarn, INFO = info, AB13BD = ab13bd, LDA = lda, LDB = ldb,
        LDC = ldc, LDD = ldd, LDWORK = ldwork, DWORK = dwork)

    return(list(nq = res$NQ, iwarn = res$IWARN, info = res$INFO, ab13bd = res$AB13BD))
}
