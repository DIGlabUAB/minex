# bench/scripts/03-tangled.R  — interdependent statements
x <- c(1, 2, NA)
m <- mean(x)
if (is.na(m)) stop("mean is NA")
