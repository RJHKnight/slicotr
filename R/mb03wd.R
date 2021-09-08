#' mb03wd
#'
#' Schur decomposition and eigenvalues of a product of matrices in periodic Hessenberg form
#' @examples 

#'   To compute the Schur decomposition and the eigenvalues of a
#'   product of matrices, H = H_1*H_2*...*H_p, with H_1 an upper
#'   Hessenberg matrix and H_2, ..., H_p upper triangular matrices,
#'   without evaluating the product. Specifically, the matrices Z_i
#'   are computed, such that
#' 
#'           Z_1' * H_1 * Z_2 = T_1,
#'           Z_2' * H_2 * Z_3 = T_2,
#'                  ...
#'           Z_p' * H_p * Z_1 = T_p,
#' 
#'   where T_1 is in real Schur form, and T_2, ..., T_p are upper
#'   triangular.
#' 
#'   The routine works primarily with the Hessenberg and triangular
#'   submatrices in rows and columns ILO to IHI, but optionally applies
#'   the transformations to all the rows and columns of the matrices
#'   H_i, i = 1,...,p. The transformations can be optionally
#'   accumulated.
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/MB03WD.html}
#' @export
mb03wd <- function(job, compz, n, h, ihi, ihiz, ilo, iloz, ldwork, z) {

    # In Parameters
    compz <- as.character(compz)
    job <- as.character(job)
    n <- as.integer(n)
    ihi <- as.integer(ihi)
    ihiz <- as.integer(ihiz)
    ilo <- as.integer(ilo)
    iloz <- as.integer(iloz)
    ldwork <- as.integer(ldwork)

    dwork <- array(as.double(1), c(ldwork))
    info <- as.integer(0)
    ldh1 <- dim(h)[1]
    ldh2 <- dim(h)[2]
    p <- dim(h)[3]
    wi <- array(as.double(0), c(n))
    wr <- array(as.double(0), c(n))
    ldz1 <- dim(z)[1]
    ldz2 <- dim(z)[2]


    res <- .Fortran("MB03WD", JOB = job, COMPZ = compz, N = n, H = h, DWORK = dwork, INFO = info, IHI = ihi, IHIZ = ihiz, ILO = ilo, LDH1 = ldh1, LDH2 = ldh2, P = p, WI = wi, WR = wr, ILOZ = iloz, LDWORK = ldwork, Z = z, LDZ1 = ldz1,
        LDZ2 = ldz2)

    return(list(h = res$H, info = res$INFO, wi = res$WI, wr = res$WR, z = res$Z))
}
