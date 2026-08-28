# bench/run-benchmark.R
# Run from the package root: Rscript bench/run-benchmark.R
suppressMessages(devtools::load_all("."))
scripts <- list.files("bench/scripts", full.names = TRUE)
granularities <- c("statement", "expression")
rows <- list()
for (path in scripts) {
  code <- readLines(path, warn = FALSE)
  cond <- if (grepl("warning", path)) "warning" else "error"
  for (algo in c("cdd", "ddmin")) {
    for (gran in granularities) {
      res <- tryCatch(
        minex(code = code, condition = cond, algorithm = algo,
              backend = "callr", granularity = gran),
        error = function(e) NULL
      )
      if (!is.null(res)) {
        rows[[length(rows) + 1L]] <- data.frame(
          script = basename(path), algorithm = algo, granularity = gran,
          oracle_calls = res$oracle_calls, n_minimal = res$n_minimal,
          n_chars_minimal = res$n_chars_minimal
        )
      }
    }
  }
}
print(do.call(rbind, rows))
