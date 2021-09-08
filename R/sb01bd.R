#' sb01bd
#'
#' Pole assignment for a given matrix pair (A,B)
#' @examples 

#'   To determine the state feedback matrix F for a given system (A,B)
#'   such that the closed-loop state matrix A+B*F has specified
#'   eigenvalues.
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/SB01BD.html}
#' @export
sb01bd <- function(dico, n, m, np, alpha, a, b, wr, wi, tol, ldwork) {

    # In Parameters
    alpha <- as.double(alpha)
    dico <- as.character(dico)
    ldwork <- as.integer(ldwork)
    m <- as.integer(m)
    n <- as.integer(n)
    tol <- as.double(tol)
    np <- as.integer(np)

    nfp <- as.integer(0)
    nap <- as.integer(0)
    nup <- as.integer(0)
    f <- array(as.double(0), c(m, n))
    z <- array(as.double(0), c(n, n))
    iwarn <- as.integer(0)
    info <- as.integer(0)
    dwork <- array(as.double(1), c(ldwork))
    ldf <- dim(f)[1]
    ldz <- dim(z)[1]
    lda <- dim(a)[1]
    ldb <- dim(b)[1]


    res <- .Fortran("SB01BD", DICO = dico, N = n, M = m, NP = np, ALPHA = alpha, A = a, LDA = lda, B = b, LDB = ldb, WR = wr, WI = wi, NFP = nfp, NAP = nap, NUP = nup, F = f, LDF = ldf, Z = z, LDZ = ldz, TOL = tol, DWORK = dwork, LDWORK = ldwork,
        IWARN = iwarn, INFO = info)

    return(list(nfp = res$NFP, nap = res$NAP, nup = res$NUP, f = res$F, z = res$Z, iwarn = res$IWARN, info = res$INFO, a = res$A, wi = res$WI, wr = res$WR))
}
