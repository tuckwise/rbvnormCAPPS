#' rbvnorm
#'
#' @param iter number iterations to run in the sample
#' @param mu mean vector of distribution
#' @param sigma var/cov matrix of distribution
#'
#' @return invisible list of dataframe, iter, mu, sigma
#' @importFrom stats rnorm
#'
#' @importFrom ggplot2 ggplot geom_point stat_ellipse
#' @export
#'
#' @examples
#' \dontrun{rbvnorm(iter, mu, sigma)}
#'
rbvnorm <- function(iter, mu, sigma) {

  df <- data.frame(matrix(nrow = 0, ncol = 2)) # creating initial data frame
  x_2 <- rnorm(1, mean = mu[2], sd = sqrt(sigma[2, 2])) # generating seed value


  for (i in 1:iter) {

    x_1 <- rnorm(1, mean = mu[1] + sigma[1, 2] * (x_2 - mu[2])/sigma[2,2],
                 sd = sqrt(sigma[1, 1] - sigma[1, 2]^2 / sigma[2, 2]))

    x_2 <- rnorm(1, mean = mu[2] + sigma[2, 1] * (x_1 - mu[1])/sigma[1,1],
                 sd = sqrt(sigma[2, 2] - sigma[2, 1]^2 / sigma[1, 1]))

    df <- rbind(df, c(x_1, x_2))
  }
  colnames(df) <- c("X1", "X2")
  invisible(list(gibbs = df, iter = iter, mu = mu, sigma = sigma))
}
