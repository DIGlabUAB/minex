# bench/scripts/06-nested-call.R  -- failure inside a nested call argument
d <- data.frame(a = 1:3)
res <- transform(d, b = nonexistent_col + a)
