library(dplyr)
library(tidyr)


mask_counts <- function(tb) {
  N <- sum(tb)
  masked_cells <- tb < 5 | (N - tb) < 3
  res <- as.character(tb)
  names(res) <- names(tb)
  
  # If total is <=7, just censor everything with a *
  if(N > 7) {
    res[tb > N - 3] <- paste0(">", N - 3)
    res[tb < 5] <- "<5"
  } else {
    res[tb > N - 3 | tb < 5] <- "*"
  }
  
  # complementary cell suppression
  
  if (sum(masked_cells) > 0 & sum(masked_cells) < length(masked_cells) & sum(tb[masked_cells]) < 5) {
    cs <- which(tb == min(tb[!masked_cells]))
    # Surplus count gets distributed across any cells with minimum number
    res[cs] <- paste0("<=", tb[cs] + ceiling(sum(tb[masked_cells])/ length(cs)))
  }
  
  res
}


tab_fct <- function(x, mask = TRUE) {
  if (!is.factor(x)) stop("Input variable must be a factor.")
  tb <- table(x)
  if (mask) tb <- mask_counts(tb)
  list(as.data.frame(t(as.matrix(tb)), stringsAsFactors = FALSE))
}


mask_pct <- function(pct, N, thresholds = NULL) {
  
  # default threshold table
  if (is.null(thresholds)) {
    thresholds <- data.frame(N = c(15, 20, 30, 50, 300, Inf),
                             low = c(.1, .07, .04, .03, .02, .01),
                             high = c(.9, .93, .96, .97, .98, .99))
  }
  
  n <- pct * N
  
  if (N < 10) {
    res <- rep("*", length(pct))
  } else {
    r <- which.max(N < thresholds$N)
    res <- paste0(as.character(round(pct * 100)), "%")
    masked_cells <- pct < thresholds$low[r] | pct > thresholds$high[r]
    res[pct < thresholds$low[r]] <- paste0("<=", as.character(thresholds$low[r] * 100), "%")
    res[pct > thresholds$high[r]] <- paste0(">=", as.character(thresholds$high[r] * 100), "%")
    
    # complementary cell suppression
    
    if (sum(masked_cells) > 0 & sum(pct[masked_cells]) < thresholds$low[r]) {
      cs <- which.min(pct[!masked_cells])
      res[!masked_cells][cs] <- paste0("<=", as.character(round(100 * (pct[!masked_cells][cs] + sum(pct[masked_cells])))), "%")
    }
  }
  names(res) <- names(pct)
  
  res
  
}

count_to_pct <- function(tb, mask = TRUE, ...) {
  pct <- tb / sum(tb)
  if (mask) pct <- mask_pct(pct, N = sum(tb), ...)
  pct
}

pct_fct <- function(x, mask = TRUE, ...) {
  if (!is.factor(x)) stop("Input variable must be a factor.")
  tb <- table(x)
  pct <- tb / sum(tb) 
  if (mask) pct <- mask_pct(pct, N = sum(tb), ...)
  list(as.data.frame(t(as.matrix(pct)), stringsAsFactors = FALSE))
}

M_SD <- function(x, mask = TRUE, cell_min = 5, na.rm = TRUE, ...) {
  if (mask & sum(!is.na(x)) < cell_min) {
    res <- data.frame(M = NA, SD = NA)
  } else {
    res <- data.frame(M = mean(x, na.rm = na.rm, ...), SD = sd(x, na.rm = na.rm, ...))
  }
  
  list(res)
}


