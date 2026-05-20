# Convert raw acceleration values to physical units in an `acc` vector

Applies calibration function(s) to bursts of raw acceleration ADC values
in an `acc` vector. Specify a set of calibration functions with
[`acc_calibration()`](https://robe2037.github.io/move2Acc/reference/acc_calibration.md).

## Usage

``` r
acc_calibrate(x, calibration)
```

## Arguments

- x:

  An `acc` vector.

- calibration:

  An `acc_calibration` object containing the calibration function(s) to
  apply to each burst in `x`. See
  [`acc_calibration()`](https://robe2037.github.io/move2Acc/reference/acc_calibration.md)
  to specify calibration functions.

## Value

An `acc` vector of the same length as the input with calibrated burst
matrices.

## See also

[`acc_calibration()`](https://robe2037.github.io/move2Acc/reference/acc_calibration.md)
and
[`as_acc_calibration()`](https://robe2037.github.io/move2Acc/reference/acc_calibration.md)
to construct calibrations for use with `acc_calibrate()`.

## Examples

``` r
a <- acc_example()

acc_calibrate(a, acc_calibration("ornitela"))
#> <acceleration[2]>
#> [1] (0.01 0 0.01) [m/s^2]  (0 -0.01 0.01) [m/s^2]
#> # frequency: 20 [Hz]

acc_calibrate(a, acc_calibration("eobs", tag_id = 1000))
#> <acceleration[2]>
#> [1] (-54.21 -54.23 -54.2) [m/s^2] (-54.22 -54.24 -54.2) [m/s^2]
#> # frequency: 20 [Hz]

acc_calibrate(a, acc_calibration(offset = 2048, slope = 0.001))
#> <acceleration[2]>
#> [1] (-20.08 -20.08 -20.07) [m/s^2] (-20.08 -20.09 -20.07) [m/s^2]
#> # frequency: 20 [Hz]

# Specify different calibration parameters for each burst.
# Calibrations will be mapped to the input `acc` object by index.
acc_calibrate(
  a, 
  acc_calibration(offset = c(2048, 2046), slope = c(0.001, 0.002))
)
#> <acceleration[2]>
#> [1] (-20.08 -20.08 -20.07) [m/s^2] (-40.13 -40.14 -40.11) [m/s^2]
#> # frequency: 20 [Hz]
```
