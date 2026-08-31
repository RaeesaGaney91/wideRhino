# =====================================================================
#  wideRhino :: GSVD without external dependencies
#  ------------------------------------------------------------------
#  Pure-R replacement for geigen::gsvd() and its R/oR/D1/D2 helpers,
#  plus the get.GSVD() wrapper used by CVAgsvd().  No compiled code,
#  no Imports beyond base/stats.  Computes the LAPACK-style (quotient)
#  Generalised SVD of a matrix pair (A, B):
#
#      A = U %*% D1 %*% [0 R] %*% t(Q)
#      B = V %*% D2 %*% [0 R] %*% t(Q)
#
#  with U (m x m), V (p x p), Q (n x n) orthogonal, R (r x r) upper
#  triangular and nonsingular, r = k + l = rank(rbind(A, B)),
#  D1 = diag(alpha), D2 = diag(beta), alpha^2 + beta^2 = 1 on the
#  finite block.  The first k generalised singular values are infinite
#  (beta = 0), exactly as returned by LAPACK's DGGSVD / geigen.
#
#  The decomposition is unique only up to signs/rotations within
#  degenerate blocks; the construction below reproduces every quantity
#  consumed by wideRhino (Minv = oR %*% t(Q), and the rank/ordering of
#  the between- vs within-group directions) to machine precision, which
#  is all the CVA biplot depends on.
#
#  This replacement was written with the assistance of Claude Code
#  (Anthropic), after geigen was archived on CRAN. The CVA-GSVD
#  methodology and the rest of wideRhino are the package author's own
#  work; this file is purely an internal linear-algebra routine.
# =====================================================================


# ---- get.GSVD -----------------------------------------------------------

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

  z <- gsvd(A, B)
  r <- Matrix::rankMatrix(rbind(A, B))[1]
  R <- z$R
  oR <- z$oR
  D1 <- z$D1
  D2 <- z$D2
  X <- oR %*% t(z$Q)
  bottom <- cbind(matrix(0, p - r, r), diag(1, p - r, p - r))
  C <- D1
  S <- D2
  U <- z$U
  V <- z$V

  Minv <- X

  list (Minv = Minv, bottom = bottom, C = C, S = S, U = U, V = V,
        r = r, k = z$k, l = z$l, Q = z$Q)
}


# ---- main GSVD (internal) ----------------------------------------------
# Returns a list with the same fields as geigen::gsvd() plus pre-computed
# R / oR / D1 / D2, so no separate accessor functions are needed.
gsvd <- function(A, B) {
  if (!is.matrix(A)) A <- as.matrix(A)
  if (!is.matrix(B)) B <- as.matrix(B)
  if (ncol(A) != ncol(B))
    stop("Matrices A and B must have same number of columns")
  storage.mode(A) <- "double"; storage.mode(B) <- "double"

  m <- nrow(A); n <- ncol(A); p <- nrow(B)
  M <- rbind(A, B)                                    # (m+p) x n
  sM <- svd(M)
  big <- max(sM$d, 1)
  tol <- max(dim(M)) * .Machine$double.eps
  r <- sum(sM$d > tol * big)
  if (r == 0) stop("rbind(A, B) has numerical rank 0")

  Zr <- sM$u[, seq_len(r), drop = FALSE]              # (m+p) x r, orthonormal
  P  <- (sM$d[seq_len(r)] * t(sM$v[, seq_len(r), drop = FALSE]))  # r x n; M = Zr P
  Z1 <- Zr[seq_len(m), , drop = FALSE]
  Z2 <- Zr[m + seq_len(p), , drop = FALSE]

  cs <- .csd(Z1, Z2, tol)
  cc <- cs$c; ss <- cs$s

  k <- cs$k                                           # # infinite gsv (beta = 0)
  l <- r - k

  # T = t(W0) %*% P  (r x n);  A = U %*% D1 %*% T,  B = V %*% D2 %*% T.
  Tm  <- crossprod(cs$W0, P)                          # r x n
  rqf <- .rq_zeroR(Tm)
  R  <- rqf$R                                         # r x r (upper triangular)
  Q  <- rqf$Q                                         # n x n orthogonal
  oR <- rqf$oR                                        # r x n; oR %*% t(Q) == Tm

  alpha <- numeric(n); beta <- numeric(n)
  if (k > 0) { alpha[seq_len(k)] <- 1; beta[seq_len(k)] <- 0 }
  if (l > 0) { idx <- (k + 1):(k + l); alpha[idx] <- cc[idx]; beta[idx] <- ss[idx] }

  ret <- list(m = m, n = n, p = p, k = k, l = l, r = r,
              alpha = alpha, beta = beta,
              U = cs$U, V = cs$V, Q = Q,
              R = R, oR = oR, D1 = cs$Cmat, D2 = cs$Smat)
  class(ret) <- "xdgsvd"
  ret
}


# ---- CS decomposition of a column-orthonormal stack [Z1; Z2] -----------
# Z1 (m x r), Z2 (p x r) with t(Z1)%*%Z1 + t(Z2)%*%Z2 = I_r.
# Returns U (m x m), V (p x p), W0 (r x r), c (len r), s (len r) with
#   Z1 = U %*% Cmat %*% t(W0),  Z2 = V %*% Smat %*% t(W0),  c^2 + s^2 = 1,
# c sorted decreasing (cosines = 1 first -> infinite gsv first).
.csd <- function(Z1, Z2, tol) {
  m <- nrow(Z1); p <- nrow(Z2); r <- ncol(Z1)

  G  <- crossprod(Z1)                                 # r x r symmetric
  eg <- eigen((G + t(G)) / 2, symmetric = TRUE)
  ord <- order(eg$values, decreasing = TRUE)          # cosines^2 descending
  W0 <- eg$vectors[, ord, drop = FALSE]
  c2 <- pmin(pmax(eg$values[ord], 0), 1)
  cc <- sqrt(c2)
  ss <- sqrt(pmin(pmax(1 - c2, 0), 1))

  big <- max(1, max(abs(Z1)), max(abs(Z2)))
  ZW1 <- Z1 %*% W0
  ZW2 <- Z2 %*% W0

  kk <- sum(ss < 1e-6 & cc > 1 - 1e-6)                # # infinite gsv

  # U: cosine direction j sits at column j (diagonal placement in Cmat).
  U <- matrix(0, m, m); fixedU <- integer(0)
  for (j in seq_len(min(m, r))) {
    if (cc[j] > tol * big) { U[, j] <- ZW1[, j] / cc[j]; fixedU <- c(fixedU, j) }
  }
  U <- .fill_orth(U, fixedU)

  # V: sine direction j (j = k+1..r) maps to V column (j - k); Smat[j-k, j] = s[j].
  V <- matrix(0, p, p); Smat <- matrix(0, p, r); fixedV <- integer(0)
  sine_dirs <- if (kk < r) (kk + 1):r else integer(0)
  for (j in sine_dirs) {
    vcol <- j - kk
    if (vcol <= p && ss[j] > tol * big) {
      V[, vcol] <- ZW2[, j] / ss[j]
      Smat[vcol, j] <- ss[j]
      fixedV <- c(fixedV, vcol)
    }
  }
  V <- .fill_orth(V, fixedV)

  Cmat <- matrix(0, m, r); for (j in seq_len(min(m, r))) Cmat[j, j] <- cc[j]

  list(U = U, V = V, W0 = W0, c = cc, s = ss, Cmat = Cmat, Smat = Smat, k = kk)
}


# Fill the not-yet-assigned columns (those NOT in `fixed`) of a partially
# filled d x d matrix with an orthonormal basis of the complement, keeping
# the assigned columns EXACTLY as given (re-orthonormalising them would flip
# signs and desync U/V from the positive cosines/sines).
.fill_orth <- function(Mat, fixed) {
  d <- nrow(Mat)
  if (length(fixed) == 0) {
    return(qr.Q(qr(matrix(stats::rnorm(d * d), d))))
  }
  base <- Mat[, fixed, drop = FALSE]
  if (length(fixed) >= d) {                           # already a full orthonormal set
    Mat[, fixed] <- base
    return(Mat)
  }
  filler <- matrix(stats::rnorm(d * (d - length(fixed))), d)
  full <- qr.Q(qr(cbind(base, filler)))               # d x d; first cols span base
  free <- setdiff(seq_len(d), fixed)
  Mat[, free] <- full[, (length(fixed) + 1):d, drop = FALSE]
  Mat[, fixed] <- base
  Mat
}


# ---- factor T = [0 R] Q^T, Q (n x n) orthogonal, R (r x r) -------------
# Guarantees oR %*% t(Q) == T exactly (this is what wideRhino's Minv needs).
# R is delivered upper-triangular when the orientation check passes, else the
# exact nonsingular block is returned (orientation is cosmetic, unused below).
.rq_zeroR <- function(Tm) {
  r <- nrow(Tm); n <- ncol(Tm)
  Qf <- qr.Q(qr(t(Tm)), complete = TRUE)              # n x n orthogonal
  G  <- Tm %*% Qf                                     # r x n, nonzero in first r cols
  perm <- if (n > r) c((r + 1):n, 1:r) else seq_len(r)
  Qp <- Qf[, perm, drop = FALSE]
  Gp <- G[,  perm, drop = FALSE]
  Rblk <- Gp[, (n - r + 1):n, drop = FALSE]           # r x r, full rank
  # Right-orthogonal RQ of the square block to upper-triangularise R, absorbing
  # the rotation into Q's trailing columns (keeps oR %*% t(Q) = T).
  Jr <- diag(r)[r:1, , drop = FALSE]
  qd <- qr(Jr %*% t(Rblk))
  Worth <- Jr %*% t(qr.Q(qd))                         # Rblk = R_up %*% Worth
  R_up  <- Jr %*% t(qr.R(qd)) %*% Jr                  # upper-triangular
  ok <- max(abs(Rblk - R_up %*% Worth)) < 1e-8 * max(1, max(abs(Rblk))) &&
    max(abs(R_up[lower.tri(R_up)])) < 1e-8 * max(1, max(abs(R_up)))
  if (ok) {
    Qtrail <- Qp[, (n - r + 1):n, drop = FALSE] %*% t(Worth)
    Qfull <- Qp; Qfull[, (n - r + 1):n] <- Qtrail
    R <- R_up
  } else {
    Qfull <- Qp; R <- Rblk                            # exact but not upper-triangular
  }
  oR <- if (n > r) cbind(matrix(0, r, n - r), R) else R
  list(R = R, Q = Qfull, oR = oR)
}
