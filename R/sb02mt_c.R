#' sb02mt_c
#'
#' Conversion of optimal problems with coupling weighting terms to standard problems
#' @examples 

#'   To compute the following matrices
#' 
#'              -1
#'       G = B*R  *B',
#' 
#'       -          -1
#'       A = A - B*R  *L',
#' 
#'       -          -1
#'       Q = Q - L*R  *L',
#' 
#'   where A, B, Q, R, L, and G are N-by-N, N-by-M, N-by-N, M-by-M,
#'   N-by-M, and N-by-N matrices, respectively, with Q, R and G
#'   symmetric matrices.
#' 
#'   When R is well-conditioned with respect to inversion, standard
#'   algorithms for solving linear-quadratic optimization problems will
#'   then also solve optimization problems with coupling weighting
#'   matrix L. Moreover, a gain in efficiency is possible using matrix
#'   G in the deflating subspace algorithms (see SLICOT Library routine
#'   SB02OD) or in the Newton's algorithms (see SLICOT Library routine
#'   SG02CD).
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/SB02MT.html}
#' @export
sb02mt_c <- function(uplo, n, m, b, r) {

    # In Parameters
    m <- as.integer(m)
    n <- as.integer(n)
    uplo <- as.character(uplo)

    jobg <- as.character("g")
    jobl <- as.character("z")
    fact <- as.character("c")
    a <- array(as.double(0), c(1, 1))
    q <- array(as.double(0), c(1, 1))
    l <- array(as.double(0), c(1, 1))
    ipiv <- array(as.integer(0), c(1))
    oufact <- as.integer(0)
    iwork <- array(as.integer(1), c(m))
    ldwork <- as.integer(1)
    info <- as.integer(0)
    dwork <- array(as.double(1), c(ldwork))
    g <- array(as.double(0), c(n, n))
    lda <- dim(a)[1]
    ldl <- dim(l)[1]
    ldq <- dim(q)[1]
    ldb <- dim(b)[1]
    ldg <- dim(g)[1]
    ldr <- dim(r)[1]


    res <- suppressWarnings(.Fortran("SB02MT", JOBG = jobg, JOBL = jobl, FACT = fact, UPLO = uplo, N = n, M = m, A = a, LDA = lda,
        B = b, LDB = ldb, Q = q, LDQ = ldq, R = r, LDR = ldr, L = l, LDL = ldl, IPIV = ipiv, OUFACT = oufact, G = g, LDG = ldg,
        IWORK = iwork, DWORK = dwork, LDWORK = ldwork, INFO = info))

    return(list(a = res$A, q = res$Q, l = res$L, ipiv = res$IPIV, oufact = res$OUFACT, info = res$INFO, b = res$B, g = res$G, r = res$R))
}
