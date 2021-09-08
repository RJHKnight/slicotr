#' ab09md
#'
#' Balance & Truncate model reduction for the stable part of a system
#' @examples 

#'   To compute a reduced order model (Ar,Br,Cr) for an original
#'   state-space representation (A,B,C) by using either the square-root
#'   or the balancing-free square-root Balance & Truncate (B & T)
#'   model reduction method for the ALPHA-stable part of the system.
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/AB09MD.html}
#' @export
ab09md <- function(dico, job, equil, ordsel, n, m, p, nr, alpha, a, b, c, tol, ldwork) {

    # In Parameters
    alpha <- as.double(alpha)
    dico <- as.character(dico)
    equil <- as.character(equil)
    job <- as.character(job)
    ldwork <- as.integer(ldwork)
    m <- as.integer(m)
    n <- as.integer(n)
    nr <- as.integer(nr)
    ordsel <- as.character(ordsel)
    p <- as.integer(p)
    tol <- as.double(tol)

    ns <- as.integer(0)
    iwork <- array(as.integer(1), c(max(m, p)))
    dwork <- array(as.double(1), c(ldwork))
    iwarn <- as.integer(0)
    info <- as.integer(0)
    hsv <- array(as.double(0), c(n))
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]


    res <- .Fortran("AB09MD", DICO = dico, JOB = job, EQUIL = equil, ORDSEL = ordsel, N = n, M = m, P = p, NR = nr, ALPHA = alpha, A = a, LDA = lda, B = b, LDB = ldb, C = c, LDC = ldc, NS = ns, HSV = hsv, TOL = tol, IWORK = iwork, DWORK = dwork,
        LDWORK = ldwork, IWARN = iwarn, INFO = info)

    return(list(nr = res$NR, ns = res$NS, iwarn = res$IWARN, info = res$INFO, a = res$A, b = res$B, c = res$C, hsv = res$HSV))
}
