#' tb05ad_ng
#'
#' Frequency response matrix of a given state-space representation (A,B,C)
#' @examples 

#'   To find the complex frequency response matrix (transfer matrix)
#'   G(freq) of the state-space representation (A,B,C) given by
#'                                 -1
#'      G(freq) = C * ((freq*I - A)  ) * B
#' 
#'   where A, B and C are real N-by-N, N-by-M and P-by-N matrices
#'   respectively and freq is a complex scalar.
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/TB05AD.html}
#' @export
tb05ad_ng <- function(n, m, p, a, b, c, ldwork, lzwork) {

    # In Parameters
    m <- as.integer(m)
    n <- as.integer(n)
    p <- as.integer(p)
    ldwork <- as.integer(ldwork)
    lzwork <- as.integer(lzwork)

    baleig <- as.character("n")
    inita <- as.character("g")
    rcond <- iwork <- array(as.integer(1), c(n))
    info <- as.integer(0)
    evim <- array(as.double(1), c(n))
    evre <- array(as.double(1), c(n))
    ldg <- as.integer(p)
    ldhinv <- as.integer(n)
    dwork <- array(as.double(1), c(ldwork))
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]


    res <- .Fortran("TB05AD", BALEIG = baleig, INITA = inita, N = n, M = m, P = p, RCOND = rcond, IWORK = iwork, INFO = info, A = a, B = b, C = c, EVIM = evim, EVRE = evre, LDG = ldg, LDHINV = ldhinv, LDWORK = ldwork, LZWORK = lzwork,
        DWORK = dwork, LDA = lda, LDB = ldb, LDC = ldc)

    return(list(info = res$INFO, a = res$A, b = res$B, c = res$C))
}
