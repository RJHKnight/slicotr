#' ab05nd
#'
#' Feedback inter-connection of two systems in state-space form
#' @examples 

#'   To obtain the state-space model (A,B,C,D) for the feedback
#'   inter-connection of two systems, each given in state-space form.
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/AB05ND.html}
#' @export
ab05nd <- function(n1, m1, p1, n2, alpha, a1, b1, c1, d1, a2, b2, c2, d2, ldwork) {

    # In Parameters
    alpha <- as.double(alpha)
    ldwork <- as.integer(ldwork)
    m1 <- as.integer(m1)
    n1 <- as.integer(n1)
    n2 <- as.integer(n2)
    p1 <- as.integer(p1)

    over <- as.character("n")
    n <- as.integer(0)
    info <- as.integer(0)
    a <- array(as.double(0), c(n1 + n2, n1 + n2))
    b <- array(as.double(0), c(n1 + n2, m1))
    c <- array(as.double(0), c(p1, n1, n2))
    d <- array(as.double(0), c(p1, m1))
    dwork <- array(as.double(1), c(ldwork))
    iwork <- array(as.integer(1), c(p1))
    lda <- dim(a)[1]
    lda1 <- dim(a1)[1]
    lda2 <- dim(a2)[1]
    ldb <- dim(b)[1]
    ldb1 <- dim(b1)[1]
    ldb2 <- dim(b2)[1]
    ldc <- dim(c)[1]
    ldc1 <- dim(c1)[1]
    ldc2 <- dim(c2)[1]
    ldd <- dim(d)[1]
    ldd1 <- dim(d1)[1]
    ldd2 <- dim(d2)[1]


    res <- suppressWarnings(.Fortran("AB05ND", OVER = over, N1 = n1, M1 = m1, P1 = p1, N2 = n2, ALPHA = alpha, A1 = a1, LDA1 = lda1,
        B1 = b1, LDB1 = ldb1, C1 = c1, LDC1 = ldc1, D1 = d1, LDD1 = ldd1, A2 = a2, LDA2 = lda2, B2 = b2, LDB2 = ldb2, C2 = c2,
        LDC2 = ldc2, D2 = d2, LDD2 = ldd2, N = n, A = a, LDA = lda, B = b, LDB = ldb, C = c, LDC = ldc, D = d, LDD = ldd, IWORK = iwork,
        DWORK = dwork, LDWORK = ldwork, INFO = info))

    return(list(n = res$N, info = res$INFO, a = res$A, b = res$B, c = res$C, d = res$D))
}
