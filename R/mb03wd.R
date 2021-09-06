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

    res <- .Fortran("MB03WD", JOB = job, COMPZ = compz, N = n, ILO = ilo, IHI = ihi, ILOZ = iloz, IHIZ = ihiz, H = h, Z = z, LDWORK = ldwork, WR = wr, WI = wi, INFO = info, P = p, LDH1 = ldh1, LDH2 = ldh2, LDZ1 = ldz1, LDZ2 = ldz2,
        DWORK = dwork)

    return(list(h = res$H, z = res$Z, wr = res$WR, wi = res$WI, info = res$INFO))
}
