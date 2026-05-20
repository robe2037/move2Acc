# Apply a function to each element of an IMU vector

This function provides a general framework to apply an arbitrary
function to each element of an IMU vector while providing access to each
element's burst, frequency, and start time metadata.

Note that some [common
operations](https://robe2037.github.io/move2Acc/reference/explore-functions.md)
have already been implemented as stand-alone functions.

## Usage

``` r
map_imu(x, .f, simplify = FALSE, .progress = FALSE)
```

## Arguments

- x:

  An IMU vector (e.g. `acc`, `mag`, `gyro`).

- .f:

  A function to be applied to each element of `x`. This can be supplied
  in one of the following ways:

  - A named function

  - An anonymous function (e.g., `function(.br) nrow(.br) / .fq`)

  - A formula (e.g., `~ nrow(.br) / .fq`)

  In all cases, use `.br` to refer to the burst matrix of each element,
  `.fq` to refer to the frequency of each element, and `.st` to refer to
  the start time of each element. See examples.

- simplify:

  Logical. If `TRUE`, attempts to simplify the output to a vector.
  Otherwise, the output will be a list. If the output cannot be
  simplified while maintaining a one-to-one correspondence with the
  input, an error will be thrown.

- .progress:

  Whether to show a progress bar. Use `TRUE` to turn on a basic progress
  bar, use a string to give it a name.

## Details

This function behaves similarly to the
[`purrr::map()`](https://purrr.tidyverse.org/reference/map.html) family
of functions. However, `map_imu()` only matches arguments by name, not
position. Thus, the input to `.f` must use the specified terminology
(`.br`, `.fq`, and/or `.st`) to access specific data from each element.
For a given vector `x`:

- `.br` accesses each element of the list returned by
  [`bursts()`](https://robe2037.github.io/move2Acc/reference/explore-functions.md)

- `.fq` accesses each element of the vector returned by
  [`freqs()`](https://robe2037.github.io/move2Acc/reference/explore-functions.md)

- `.st` accesses each element of the vector returned by
  [`starts()`](https://robe2037.github.io/move2Acc/reference/explore-functions.md)

## Examples

``` r
a <- acc_example()

# Use `.br` to access the burst matrix for each element:
n_samp <- map_imu(a, function(.br) nrow(.br))

n_samp
#> [[1]]
#> [1] 30
#> 
#> [[2]]
#> [1] 20
#> 

# Use `.fq` to access the frequency value for each element:
burst_len <- map_imu(a, function(.br, .fq) nrow(.br) / .fq)

burst_len
#> [[1]]
#> 1.5 [1/Hz]
#> 
#> [[2]]
#> 1 [1/Hz]
#> 

# Use `.st` to access the start time for each element:
burst_end <- map_imu(
  a,
  function(.br, .fq, .st) as.numeric(nrow(.br) / .fq) + .st
)

burst_end
#> [[1]]
#> [1] "1970-01-01 00:00:01 UTC"
#> 
#> [[2]]
#> [1] "1970-01-01 00:00:11 UTC"
#> 

# You can also provide a separately defined function
get_burst_end <- function(.br, .fq, .st, offset = 2) {
  as.numeric(nrow(.br) / .fq) + .st + offset
}

map_imu(a, get_burst_end)
#> [[1]]
#> [1] "1970-01-01 00:00:03 UTC"
#> 
#> [[2]]
#> [1] "1970-01-01 00:00:13 UTC"
#> 

map_imu(a, function(.br, .fq, .st) get_burst_end(.br, .fq, .st, offset = 5))
#> [[1]]
#> [1] "1970-01-01 00:00:06 UTC"
#> 
#> [[2]]
#> [1] "1970-01-01 00:00:16 UTC"
#> 

# Use simplify to reduce to a vector format:
map_imu(a, get_burst_end, simplify = TRUE)
#> [1] "1970-01-01 00:00:03 UTC" "1970-01-01 00:00:13 UTC"

# Note that this will fail if the result cannot be simplified to the same
# length as the input vector
try(
  map_imu(a, function(.br) .br, simplify = TRUE)
)
#> Error in purrr::list_simplify(out) : 
#>   `x[[1]]` must have size 1, not size 30.
```
