# Functions to explore an IMU vector

Functions to explore an IMU vector

## Usage

``` r
is_acc(x)

n_axis(x)

is_uniform(x)

bursts(x)

bursts(x) <- value

freqs(x)

freqs(x) <- value

starts(x)

starts(x) <- value

burst_dur(x)

n_samples(x)

imu_units(x)

is_gyro(x)

is_mag(x)
```

## Arguments

- x:

  An IMU vector (e.g. `acc`, `mag`, `gyro`).

- value:

  Replacement value.

## Examples

``` r
x <- acc(
  bursts = list(
    cbind(X = sin(1:30 / 10), Y = cos(1:30 / 10), Z = 1),
    cbind(X = sin(1:20 / 10 + 2), Y = cos(1:20 / 10 + 3))
  ),
  frequency = units::as_units(c(20, 30), "Hz")
)
x <- c(x, NA)
n_axis(x)
#> [1]  3  2 NA
n_samples(x)
#> [1] 30 20 NA
is_uniform(x)
#> [1] FALSE
length(x)
#> [1] 3
is.na(x)
#> [1] FALSE FALSE  TRUE
na.omit(x)
#> <acceleration[2]>
#> [1] (0.67 0.01 1) (0.08 -0.52) 
#> # frequency: 20 [Hz] - 30 [Hz]
 y <- acc(
  bursts = list(
    cbind(X = sin(1:20 / 10), Y = cos(1:20 / 10)),
    cbind(X = sin(1:20 / 10 + 2), Y = cos(1:20 / 10 + 3))
  ),
  frequency = units::as_units(c(20, 20), "Hz")
)
is_uniform(y)
#> [1] TRUE
```
