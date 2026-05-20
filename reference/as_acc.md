# Convert to acc

In many cases the `as_acc` function will directly create an acceleration
vector from input data

## Usage

``` r
as_acc(x, ...)

# Default S3 method
as_acc(x, ...)

# S3 method for class 'move2'
as_acc(
  x,
  colset = NULL,
  min_freq = 1,
  merge_continuous = TRUE,
  drop = FALSE,
  ...
)
```

## Arguments

- x:

  A `move2` containing acceleration data as collected by EOBS, Ornitela,
  or similar tracking devices. Most of the time this will be either
  loaded from disk using
  [move2::mt_read](https://bartk.gitlab.io/move2/reference/mt_read.html)
  or downloaded using
  [move2::movebank_download_study](https://bartk.gitlab.io/move2/reference/movebank_download_study.html).

- ...:

  currently not used

- colset:

  An `imu_colset` object or list of `imu_colset` objects specifying the
  columns of `x` that contain acceleration data. By default, constructs
  bursts for all column sets that are detected in `x` that also contain
  data (see
  [`active_acc_colsets()`](https://robe2037.github.io/move2Acc/reference/active_colsets.md)).

  Several common colsets are listed under
  [`movebank_acc_colsets()`](https://robe2037.github.io/move2Acc/reference/movebank_colsets.md).
  To specify a custom set of columns, use
  [`imu_colset()`](https://robe2037.github.io/move2Acc/reference/imu_colset.md).

- min_freq:

  Numeric value indicating the minimum allowable within-burst data
  collection frequency when identifying bursts in long-format data. Any
  two adjacent timestamps that fall outside of the period defined by
  this frequency will be split into separate bursts. If no units are
  provided, this value is assumed to be in Hz.

  Ignored if data are already in predefined bursts.

- merge_continuous:

  Logical value indicating whether to merge adjacent bursts. Two
  adjacent bursts can be merged if the first burst ends at the same time
  that the second starts and the burst frequency is identical between
  the two. This is useful for processing continuous data that have been
  stored in chunks split at regular intervals.

- drop:

  Logical indicating whether empty bursts should be dropped from the
  output. If `drop = FALSE`, then the length of the output will match
  the number of rows in the input data `x` and bursts will be stored at
  the index location corresponding to the start time of the burst.

## Details

The resulting vector will be as long as the input. This means it can,
for example, be added as a column to a `data.frame`. For some tags this
means `NA` values are inserted when one burst is stored over multiple
rows of a `data.frame`.
