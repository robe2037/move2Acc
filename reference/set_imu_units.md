# Set or convert units on IMU burst data

Methods to attach or convert units on the burst matrices of `acc`,
`mag`, and `gyro` vectors. Each method validates that the target unit is
dimensionally compatible with its IMU class:

- `acc`: acceleration units (e.g., `"m/s^2"`, `"standard_free_fall"`)

- `mag`: magnetic flux density units (e.g., `"tesla"`, `"uT"`,
  `"gauss"`)

- `gyro`: angular velocity units (e.g., `"rad/s"`, `"degree/s"`)

To calibrate raw accelerometer values rather than simply attaching or
converting units, use
[`acc_calibrate()`](https://robe2037.github.io/move2Acc/reference/acc_calibrate.md).

## Usage

``` r
set_imu_units(x, value, ...)
```

## Arguments

- x:

  An `acc`, `mag`, or `gyro` vector.

- value:

  Character specifying the target units (e.g., `"m/s^2"`). For units in
  terms of gravitational acceleration, use `"standard_free_fall"`.

- ...:

  Unused.

## Value

The input vector with units attached to each burst matrix.

## See also

[`acc_calibrate()`](https://robe2037.github.io/move2Acc/reference/acc_calibrate.md)
to calibrate raw acceleration values.

## Examples

``` r
a <- acc_example()

# Attach units to unitless bursts
set_imu_units(a, "m/s^2")
#> <acceleration[2]>
#> [1] (0.67 0.01 1) [m/s^2]  (0.08 -0.52 1) [m/s^2]
#> # frequency: 20 [Hz]

# Convert between units
a_ms2 <- set_imu_units(a, "m/s^2")
set_imu_units(a_ms2, "standard_free_fall")
#> <acceleration[2]>
#> [1] (0.07 0 0.1) [standard_free_fall]     (0.01 -0.05 0.1) [standard_free_fall]
#> # frequency: 20 [Hz]

# Dimensionally incompatible units error
try(set_imu_units(a, "kg"))
#> Error in set_imu_units_(x, value, reference = "m/s^2", sensor = "acc") : 
#>   kg units not valid for `acc` vector.
#> ℹ Units must be convertible to m/s^2
```
