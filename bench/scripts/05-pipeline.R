# bench/scripts/05-pipeline.R  -- failure inside a native pipe
x <- 1:10
result <- x |> rev() |> sqrt() |> sum() |> log("oops not a base")
