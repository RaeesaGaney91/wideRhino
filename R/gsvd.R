#' Get GSVD
#' Get the components of the GSVD decomposition
#' @param A Matrix A
#' @param B Matrix B
#'
#' @returns Returns components from the GSVD decomposition
#' @export
#'
get.GSVD <- function (A, B)
{
  p <- ncol(A)
  n <- nrow(A)

  z <- geigen::gsvd(A,B)
  r <- Matrix::rankMatrix(rbind(A,B))[1]
  R <- geigen::gsvd.R(z)
  oR <- geigen::gsvd.oR(z)
  D1 <- geigen::gsvd.D1(z)
  D2 <- geigen::gsvd.D2(z)
  X <- oR %*% t(z$Q)
  bottom <- cbind(matrix(0,p-r,r),diag(1,p-r,p-r))
  C <- D1
  S <- D2
  U <- z$U
  V <- z$V

  Minv <- X

  list (Minv=Minv,bottom=bottom,C=C,S=S,U=U,V=V,r=r,k=z$k, l=z$l,Q=z$Q)
}
