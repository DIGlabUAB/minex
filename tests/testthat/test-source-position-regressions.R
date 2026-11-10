test_that("parser columns map tabs and Unicode to character positions", {
  text <- '\t\tf(\t"é🙂\t漢",\tbad)\n\tg(one, two)'
  pd <- pt_data(text)
  tokens <- pd[pd$terminal, , drop = FALSE]
  starts <- pt_line_offsets(text)
  from <- pt_char_index(tokens$line1, tokens$col1, starts)
  to <- pt_char_index(tokens$line2, tokens$col2, starts)
  expect_equal(substring(text, from, to), tokens$text)

  calls <- pt_call_args(text)
  expect_length(calls, 2L)
  results <- vapply(calls[[1]], function(s) pt_delete(text, list(s)), character(1))
  expect_setequal(results, c(
    '\t\tf(\tbad)\n\tg(one, two)',
    '\t\tf(\t"é🙂\t漢")\n\tg(one, two)'
  ))
  expect_true(all(vapply(results, pt_parses, logical(1))))
})

test_that("multiline pipe stages are ordered by their full source positions", {
  for (operator in c("|>", "%>%")) {
    text <- paste0("seq_len(10) ", operator, "\n\trev() ", operator, "\n\tlog()")
    stages <- pt_pipe_stages(text)[[1]]
    expect_length(stages, 3L)
    expect_equal(vapply(stages, function(s) substr(text, s$from, s$to), character(1)),
                 c("seq_len(10) ", paste0(operator, "\n\trev() "),
                   paste0(operator, "\n\tlog()")))
    expect_true(all(vapply(stages, function(s) s$from <= s$to, logical(1))))
    expect_lt(stages[[1]]$to, stages[[2]]$from)
    expect_lt(stages[[2]]$to, stages[[3]]$from)
    expect_true(pt_parses(pt_delete(text, stages[2L])))
    expect_true(pt_parses(pt_delete(text, stages[3L])))
    expect_equal(pt_delete(text, stages[2:3]), "seq_len(10) ")
  }
})

test_that("call detection and argument ordering handle lines beyond column 1000", {
  text <- paste0(strrep(" ", 1100L), "f(\n  a,\n  b)")
  calls <- pt_call_args(text)
  expect_length(calls, 1L)
  expect_length(calls[[1]], 2L)
  results <- vapply(calls[[1]], function(s) pt_delete(text, list(s)), character(1))
  parsed <- lapply(results, function(s) parse(text = s)[[1]])
  expect_equal(parsed, list(quote(f(b)), quote(f(a))))

  # A long argument and its comma must still precede the next line's argument.
  long <- strrep("a", 1100L)
  text <- paste0('f("', long, '",\n  b)')
  calls <- pt_call_args(text)
  expect_length(calls[[1]], 2L)
  results <- vapply(calls[[1]], function(s) pt_delete(text, list(s)), character(1))
  expect_equal(parse(text = results[1L])[[1]], quote(f(b)))
  expect_equal(parse(text = results[2L])[[1]], call("f", long))
})

test_that("simultaneous argument deletion merges shared commas in original coordinates", {
  text <- "f(a,b,c,d)"
  spans <- pt_call_args(text)[[1]]
  expect_equal(pt_delete(text, spans), "f()")
  expect_equal(pt_delete(text, spans[1:2]), "f(c,d)")
  expect_equal(pt_delete(text, spans[2:3]), "f(a,d)")
  expect_equal(pt_delete(text, spans[c(1L, 3L)]), "f(b,d)")
  expect_equal(pt_delete(text, c(spans, spans)), "f()")
  expect_equal(pt_delete(text, list()), text)

  # An outer argument and its nested argument overlap; neither may be deleted
  # twice, which would remove characters from the surviving call.
  text <- "f(g(a),b)"
  spans <- unlist(pt_call_args(text), recursive = FALSE)
  expect_equal(pt_delete(text, spans), "f()")
})

test_that("every argument deletion subset preserves the remaining call arguments", {
  texts <- c("f(a,b,c,d)", "\tf(\ta,\n\tb,\tc,\n\td)",
             'f("é🙂\t漢",b,c,d)', paste0(strrep(" ", 1100L), "f(a,\nb,c,d)"))
  for (text in texts) {
    spans <- pt_call_args(text)[[1]]
    args <- as.list(parse(text = text)[[1]])[-1L]
    for (mask in 0:15) {
      drop <- which(as.logical(intToBits(mask)[1:4]))
      kept <- setdiff(seq_along(args), drop)
      expected <- as.call(c(list(as.name("f")), args[kept]))
      reduced <- pt_delete(text, spans[drop])
      expect_equal(parse(text = reduced)[[1]], expected)
    }
  }
})

test_that("calls with missing argument slots are retained conservatively", {
  for (text in c("f(a,,b,named=1)", "f(,a,b)", "f(a,b,)", "f(a,,b)")) {
    expect_length(pt_call_args(text), 0L)
    expect_equal(hdd_star(text, function(s) TRUE, max_oracle_calls = 20L), text)
  }
  expect_length(pt_call_args("f(,g(a,b))"), 1L)
})

test_that("invalid deletion spans fail instead of duplicating or retaining source", {
  for (span in list(list(from = 3L, to = 2L), list(from = 0L, to = 1L),
                    list(from = 1L, to = 5L), list(from = NA_real_, to = 1L),
                    list(from = 1.5, to = 2L))) {
    expect_error(pt_delete("f(a)", list(span)), "valid character positions")
  }
})

test_that("HDD completes multiline pipelines without cycling through malformed spans", {
  text <- "seq_len(10) |>\n  rev() |>\n  log()"
  info <- new.env()
  out <- hdd_star(text, function(s) {
    grepl("log(", s, fixed = TRUE) && grepl("10", s, fixed = TRUE)
  }, max_oracle_calls = 100L, info = info)
  expect_true(info$complete)
  expect_lt(info$oracle_calls, 100L)
  expect_equal(parse(text = out)[[1]], quote(log(seq_len(10))))
})

test_that("HDD drops nested call arguments with tab indentation", {
  info <- new.env()
  out <- hdd_star("\tf(g(a, bad), c)", function(s) grepl("bad", s, fixed = TRUE),
                  max_oracle_calls = 100L, info = info)
  expect_true(info$complete)
  expect_equal(parse(text = out)[[1]], quote(f(g(bad))))
})
