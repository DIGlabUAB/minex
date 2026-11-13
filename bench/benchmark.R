# Helpers for the development benchmark harness. This directory is excluded
# from the built package; the helpers have no additional package dependencies.
benchmark_git_command <- function(args) {
  suppressWarnings(system2("git", args, stdout = TRUE, stderr = TRUE))
}

benchmark_git_provenance <- function(package_root, git = benchmark_git_command) {
  prefix <- c("-C", shQuote(normalizePath(package_root, mustWork = TRUE)))
  run <- function(args) {
    tryCatch(git(c(prefix, args)),
             error = function(e) structure(character(), status = 1L))
  }
  succeeded <- function(output) is.null(attr(output, "status")) ||
    identical(as.integer(attr(output, "status")), 0L)
  revision <- run(c("rev-parse", "HEAD"))
  if (!succeeded(revision) || !length(revision)) {
    return(list(git_revision = NA_character_, git_dirty = NA))
  }
  changes <- run(c("status", "--porcelain", "--untracked-files=normal"))
  list(git_revision = revision[[1L]],
       git_dirty = if (succeeded(changes)) any(nzchar(changes)) else NA)
}

benchmark_provenance <- function(package_root = ".", git = benchmark_git_command) {
  description <- read.dcf(file.path(package_root, "DESCRIPTION"))
  c(list(package_version = unname(description[1L, "Version"]),
         r_version = R.version.string, platform = R.version$platform,
         run_started_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
    benchmark_git_provenance(package_root, git = git))
}

benchmark_scripts <- function(paths, reducer, backend = "callr",
                              algorithms = c("cdd", "ddmin"),
                              granularities = c("statement", "expression"),
                              clock = function() unname(proc.time()[["elapsed"]]),
                              provenance = benchmark_provenance()) {
  if (!length(paths) || !length(algorithms) || !length(granularities)) {
    stop("Supply benchmark scripts, algorithms, and granularities.", call. = FALSE)
  }
  force(provenance)
  rows <- list()
  for (path in paths) {
    condition <- if (grepl("warning", basename(path))) "warning" else "error"
    for (algorithm in algorithms) {
      for (granularity in granularities) {
        warnings <- character()
        started <- clock()
        attempt <- tryCatch(
          withCallingHandlers({
            code <- readLines(path, warn = FALSE)
            result <- reducer(code = code, condition = condition,
                              algorithm = algorithm, backend = backend,
                              granularity = granularity)
            list(result = result, error = NULL)
          }, warning = function(w) {
            warnings <<- c(warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
          }),
          error = function(e) list(result = NULL, error = e)
        )
        elapsed <- clock() - started
        failed <- !is.null(attempt$error)
        result <- attempt$result
        complete <- if (failed) NA else isTRUE(result$complete)
        count <- function(name) if (failed) NA_real_ else as.numeric(result[[name]])
        rows[[length(rows) + 1L]] <- data.frame(
          script = basename(path), algorithm = algorithm,
          granularity = granularity, condition = condition, backend = backend,
          status = if (failed) "error" else if (complete) "success" else "incomplete",
          elapsed_seconds = elapsed, complete = complete,
          oracle_calls = count("oracle_calls"),
          n_original = count("n_original"), n_minimal = count("n_minimal"),
          n_chars_original = count("n_chars_original"),
          n_chars_minimal = count("n_chars_minimal"),
          evaluation_failures = if (failed) NA_integer_ else length(result$evaluation_failures),
          error_message = if (failed) conditionMessage(attempt$error) else NA_character_,
          error_classes = if (failed) paste(class(attempt$error), collapse = ";") else NA_character_,
          warning_messages = if (length(warnings)) paste(warnings, collapse = "\n") else NA_character_,
          package_version = provenance$package_version,
          r_version = provenance$r_version, platform = provenance$platform,
          run_started_at = provenance$run_started_at,
          git_revision = provenance$git_revision, git_dirty = provenance$git_dirty,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  do.call(rbind, rows)
}

write_benchmark_results <- function(results, path) {
  directory <- dirname(path)
  if (!dir.exists(directory) && !dir.create(directory, recursive = TRUE)) {
    stop("Cannot create benchmark output directory: ", directory, call. = FALSE)
  }
  utils::write.csv(results, file = path, row.names = FALSE, na = "")
  invisible(path)
}

benchmark_options <- function(args) {
  stamp <- gsub("\\.", "", format(Sys.time(), "%Y%m%d-%H%M%OS6", tz = "UTC"))
  options <- list(output = file.path("bench", "results", paste0("benchmark-", stamp, ".csv")),
                  backend = "callr")
  while (length(args)) {
    arg <- args[[1L]]
    if (grepl("^--(output|backend)=", arg)) {
      name <- sub("^--([^=]+)=.*$", "\\1", arg)
      value <- sub("^--[^=]+=", "", arg)
      args <- args[-1L]
    } else if (arg %in% c("--output", "--backend") && length(args) >= 2L) {
      name <- substring(arg, 3L)
      value <- args[[2L]]
      if (startsWith(value, "--")) {
        stop("Incomplete benchmark argument: ", arg, call. = FALSE)
      }
      args <- args[-c(1L, 2L)]
    } else {
      stop("Unknown or incomplete benchmark argument: ", arg, call. = FALSE)
    }
    if (!nzchar(value)) stop("Empty benchmark argument: --", name, call. = FALSE)
    options[[name]] <- value
  }
  if (!options$backend %in% c("callr", "inprocess")) {
    stop("`--backend` must be callr or inprocess.", call. = FALSE)
  }
  options
}
