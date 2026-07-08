# minex 0.1.0

* Initial release.
* `minex()` reduces a failing R script to a one-minimal reproducible example,
  evaluating candidates in a separate R process by default.
* `ddmin()` exposes the underlying delta debugging algorithm for reuse on any
  collection.
* `reduce_rows()` reduces a data frame to the rows that reproduce a failure.
