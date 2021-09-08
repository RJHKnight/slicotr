#' tb01pd
#'
#' Minimal, controllable or observable block Hessenberg realization for a given state-space representation
#' @examples 

#'   To find a reduced (controllable, observable, or minimal) state-
#'   space representation (Ar,Br,Cr) for any original state-space
#'   representation (A,B,C). The matrix Ar is in upper block
#'   Hessenberg form.
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/TB01PD.html}
#' @export
tb01pd <- function(job, equil, n, m, p, tol, a, b, c, ldwork) {

    # In Parameters
    equil <- as.character(equil)
    job <- as.character(job)
    m <- as.integer(m)
    n <- as.integer(n)
    p <- as.integer(p)
    tol <- as.double(tol)
    ldwork <- as.integer(ldwork)

    nr <- as.integer(0)
    info <- as.integer(0)
    iwork <- array(as.integer(1), c(n + max(m, p)))
    dwork <- array(as.double(1), c(ldwork))
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]


    res <- .Fortran("TB01PD", JOB = job, EQUIL = equil, N = n, M = m, P = p, NR = nr, TOL = tol, INFO = info, A = a, B = b, C = c, IWORK = iwork, LDWORK = ldwork, DWORK = dwork, LDA = lda, LDB = ldb, LDC = ldc)

    return(list(nr = res$NR, info = res$INFO, a = res$A, b = res$B, c = res$C))
}
