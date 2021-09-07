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
mb03wd <- function(job, compz, n, ilo, ihi, iloz, ihiz, h, z, ldwork) {

    # In Parameters
    compz <- as.character(compz)
    job <- as.character(job)
    n <- as.integer(n)
    ihi <- as.integer(ihi)
    ihiz <- as.integer(ihiz)
    ilo <- as.integer(ilo)
    iloz <- as.integer(iloz)
    ldwork <- as.integer(ldwork)

    # Out Parameters
    wr <- array(as.double(0), c(n))
    wi <- array(as.double(0), c(n))
    info <- as.integer(0)

    # Hidden Parameters
    p <- dim(h)[3]
    ldh1 <- dim(h)[1]
    ldh2 <- dim(h)[2]
    ldz1 <- dim(z)[1]
    ldz2 <- dim(z)[2]
    dwork <- array(as.double(1), c(ldwork))

    res <- .Fortran("MB03WD", JOB = job, COMPZ = compz, N = n, ILO = ilo, IHI = ihi, ILOZ = iloz, IHIZ = ihiz, H = h, Z = z, LDWORK = ldwork, WR = wr, WI = wi, INFO = info, P = p, LDH1 = ldh1,
        LDH2 = ldh2, LDZ1 = ldz1, LDZ2 = ldz2, DWORK = dwork)

    return(list(h = res$H, z = res$Z, wr = res$WR, wi = res$WI, info = res$INFO))
}
