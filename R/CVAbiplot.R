#' CVA Biplot using the GSVD
#'
#' Create a CVA biplot using the generalised singular value decomposition when
#' number of variables (p) is larger than the number of samples (n).
#'
#' If p < n, then the solution defaults to the standard CVA biplot.
#'
#' @param X n x p data matrix
#' @param group vector of size n showing the groups
#'
#' @returns
#' @export
#'
CVAbiplot <- function(X,group)
{
  center=TRUE # always
  scaled=TRUE # does not matter in CVA, but matters in gsvd so must be set to TRUE

  X <- as.matrix(X)
  unscaled.X <- X
  group <- as.factor(group)
  g.names <-levels(group)

  means <- NULL
  sd <- NULL

  if(is.null(X))
  {  means <- NULL
  sd <- NULL
  } else
  {
    means <- apply(X, 2, mean)
    sd <- apply(X, 2, stats::sd)
    if (!center) {  X <- X
    means <- rep(0, ncol(X))
    sd <- rep(1, ncol(X))
    }
    else if (scaled) { X <- scale(X) }
    else { X <- scale(X, scale = FALSE)
    sd <- rep(1, ncol(X))
    }
    if (is.null(rownames(X))) rownames(X) <- paste(1:nrow(X))
    if (is.null(colnames(X))) colnames(X) <- paste("V", 1:ncol(X), sep = "")
  }

  p <- ncol(X)
  n <- nrow(X)
  K <- nlevels(group)

  G <- cds::indmat(group)
  X_bar <- solve(t(G) %*% G) %*% t(G) %*% X

  W <-  t(X) %*% X - t(X_bar) %*% t(G) %*% G %*% X_bar
  B <-  t(X_bar) %*% t(G) %*% G %*% X_bar
  Cmat <- t(G) %*% G

  J1 <- sum(diag(B))/sum(diag(W))

  if(n > p)
  {
    W_minhalf <- eigen(W)$vectors %*% diag(1/sqrt(eigen(W)$values)) %*%
      t(eigen(W)$vectors)
    eigenresult <- eigen(W_minhalf %*% B %*% W_minhalf)
    V <- eigenresult$vectors
    Mstar <- W_minhalf %*% V
    Lambda <- eigenresult$values

    XM <- as.matrix(X) %*% Mstar
    ax.one.unit <- solve(diag(diag(t(solve(Mstar)[1:2,]) %*%
                                     solve(Mstar)[1:2,]))) %*% t(solve(Mstar)[1:2,])

    XHat <- XM %*% solve(Mstar)
    if (scaled) XHat <- scale(XHat, center=F, scale=1/sd)
    if (center) XHat <- scale(XHat, center=-1*means, scale=F)
    Zmeans <- X_bar %*% Mstar[,1:2]
  }

  # gsvd
  groups <- unique(group)
  A <- lapply(groups, function(g) X[group == g,])
  group_means <- lapply(A, colMeans)
  overall_mean <- apply(X,2,mean)

  Hb1 <- matrix(0,K,p)
  for(k in 1:K)
  {
    group_mean <- group_means[[k]]
    group_size <- nrow(A[[k]])
    Hb1[k,] <- sqrt(group_size) * (group_mean - overall_mean)
  }

  Hw1 <- do.call(rbind, lapply(1:K, function(k) {
    group_matrix <- A[[k]]
    group_mean <- group_means[[k]]
    group_size <- nrow(group_matrix)
    e <- matrix(1,nrow=1,ncol=group_size)
    group_matrix - t(e) %*% group_mean
  }))

  Fmat <- Hb1
  Hmat <- Hw1


  r <- Matrix::rankMatrix(rbind(Fmat,Hmat))[1]
  s <- r - Matrix::rankMatrix(Hmat)[1]

  z <- get.GSVD(Fmat,Hmat)

  Minv <- z$Minv[(s+1):r,]
  N <- MASS::ginv(Minv)

  Y <- X %*% N
  Y_bar <- solve(t(G) %*% G) %*% t(G) %*% Y
  W_gsvd <- t(Y) %*% Y - t(Y_bar) %*% t(G) %*% G %*% Y_bar
  B_gsvd <- t(Y_bar) %*% t(G) %*% G %*% Y_bar

  J2 <- sum(diag(solve(W_gsvd)%*%B_gsvd))

  svd.out <- svd(W_gsvd)
  Wgsvd.sqrt <- svd.out$u %*% diag(sqrt(svd.out$d)) %*% t(svd.out$v)
  Amat <- solve(Wgsvd.sqrt) %*% B_gsvd %*% solve(Wgsvd.sqrt)
  svd.out <- svd(Amat)
  M_gsvd <- solve(Wgsvd.sqrt) %*% svd.out$u
  Lambda_gsvd <- svd.out$d

  YM <- as.matrix(Y) %*% M_gsvd
  ax.one.unit_gsvd <- solve(diag(diag(t(MASS::ginv(N %*% M_gsvd)[1:2,]) %*%
                                        MASS::ginv(N %*% M_gsvd)[1:2,]))) %*%
    t(MASS::ginv(N %*% M_gsvd)[1:2,])

  Zmeans_gsvd <- X_bar %*% N %*% M_gsvd[,1:2]

  if (n < p) {
    Mstar <- NULL ; XHat = NULL ; XM <- NULL ; ax.one.unit <- NULL ;
    Lambda <- NULL ; Zmeans <- NULL}

  object <- list(X = X,unscaled.X = unscaled.X, group=group,g.names = g.names, XHat = XHat,
                 means = means, sd = sd, K = K, n = n, p = p, center = center, scaled=scaled,
                 G=G, Xmeans=X_bar,Lambda = Lambda,Zmeans=Zmeans,Y=Y,YM=YM,N=N,
                 M_gsvd=M_gsvd,Zmeans_gsvd=Zmeans_gsvd,
                 Lambda_gsvd=Lambda_gsvd,W_gsvd=W_gsvd,
                 W = W, B = B,Cmat=Cmat, Mstar = Mstar, XM = XM, J1=J1,J2=J2,
                 ax.one.unit = ax.one.unit,
                 ax.one.unit_gsvd = ax.one.unit_gsvd)

  #class(object) <- "CVA"
  object

}
