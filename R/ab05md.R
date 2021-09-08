#' ab05md
#'
#' Cascade inter-connection of two systems in state-space form
#' @examples 

#'   To obtain the state-space model (A,B,C,D) for the cascaded
#'   inter-connection of two systems, each given in state-space form.
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/AB05MD.html}
#' @export
ab05md <- function(uplo, n1, m1, p1, n2, p2, ldwork, a1, a2, b1, b2, c1, c2, d1, d2) {

    # In Parameters
    ldwork <- as.integer(ldwork)
    m1 <- as.integer(m1)
    n1 <- as.integer(n1)
    n2 <- as.integer(n2)
    p1 <- as.integer(p1)
    p2 <- as.integer(p2)
    uplo <- as.character(uplo)

    over <- as.character("n")
    n <- as.integer(0)
    info <- as.integer(0)
    a <- array(as.double(0), c(n1 + n2, n1 + n2))
    b <- array(as.double(0), c(n1 + n2, m1))
    c <- array(as.double(0), c(p2, n1 + n2))
    d <- array(as.double(0), c(p2, m1))
    dwork <- array(as.double(1), c(ldwork))
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


    res <- .Fortran("AB05MD", UPLO = uplo, OVER = over, N1 = n1, M1 = m1, P1 = p1, N2 = n2, P2 = p2, N = n, LDWORK = ldwork, INFO = info, A = a, A1 = a1, A2 = a2, B = b, B1 = b1, B2 = b2, C = c, C1 = c1, C2 = c2, D = d, D1 = d1, D2 = d2,
        DWORK = dwork, LDA = lda, LDA1 = lda1, LDA2 = lda2, LDB = ldb, LDB1 = ldb1, LDB2 = ldb2, LDC = ldc, LDC1 = ldc1, LDC2 = ldc2, LDD = ldd, LDD1 = ldd1, LDD2 = ldd2)

    return(list(n = res$N, info = res$INFO, a = res$A, b = res$B, c = res$C, d = res$D))
}
