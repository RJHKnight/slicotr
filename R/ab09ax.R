#' ab09ax
#'
#' Balance & Truncate model reduction for stable systems with state matrix in real Schur canonical form
#' @examples 

#'   To compute a reduced order model (Ar,Br,Cr) for a stable original
#'   state-space representation (A,B,C) by using either the square-root
#'   or the balancing-free square-root Balance & Truncate model
#'   reduction method. The state dynamics matrix A of the original
#'   system is an upper quasi-triangular matrix in real Schur canonical
#'   form. The matrices of the reduced order system are computed using
#'   the truncation formulas:
#' 
#'        Ar = TI * A * T ,  Br = TI * B ,  Cr = C * T .
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/AB09AX.html}
#' @export
ab09ax <- function(dico, job, ordsel, n, m, p, nr, a, b, c, tol, ldwork) {

    # In Parameters
    dico <- as.character(dico)
    job <- as.character(job)
    ldwork <- as.integer(ldwork)
    m <- as.integer(m)
    n <- as.integer(n)
    nr <- as.integer(nr)
    ordsel <- as.character(ordsel)
    p <- as.integer(p)
    tol <- as.double(tol)

    # Out Parameters
    hsv <- array(as.double(0), c(n))
    t <- array(as.double(0), c(n, n))
    ti <- array(as.double(0), c(n, n))
    iwarn <- as.integer(0)
    info <- as.integer(0)

    # Hidden Parameters
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]
    ldt <- dim(t)[1]
    ldti <- dim(ti)[1]
    iwork <- array(as.integer(1), c(max(m, p)))
    dwork <- array(as.double(1), c(ldwork))

    res <- .Fortran("AB09AX", DICO = dico, JOB = job, ORDSEL = ordsel, N = n, M = m, P = p, NR = nr, A = a, B = b, C = c, TOL = tol, LDWORK = ldwork, HSV = hsv, T = t, TI = ti, IWARN = iwarn,
        INFO = info, LDA = lda, LDB = ldb, LDC = ldc, LDT = ldt, LDTI = ldti, IWORK = iwork, DWORK = dwork)

    return(list(nr = res$NR, a = res$A, b = res$B, c = res$C, hsv = res$HSV, t = res$T, ti = res$TI, iwarn = res$IWARN, info = res$INFO))
}
