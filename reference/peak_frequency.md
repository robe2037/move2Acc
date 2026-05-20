# Calculate the peak frequency per axis for bursts

Calculate the peak frequency per axis for bursts

## Usage

``` r
peak_frequency(x, resolution = NA)
```

## Arguments

- x:

  An IMU vector (e.g. `acc`, `mag`, `gyro`).

- resolution:

  A scalar with the
  [units](https://r-quantities.github.io/units/reference/units.html)
  Hertz

## Value

returns a list with the same length as `x` with the peak frequency per
axis

## Details

To increase the resolution of the result zero padding can be used. This
can be controlled using the resolution argument. Note that increasing
resolution without increasing the number of samples in a burst has a
limited ability to get closer to the true frequency.

## Examples

``` r
a <- acc(
  list(
    cbind(
      X = sin(1:200 / (5  / (pi * 2))),
      Z = cos(1:200 / (80 / (pi * 2)))
    )
  ), 
  units::set_units(400, "Hz")
)

peak_frequency(a)
#> [[1]]
#> Units: [Hz]
#>  X  Z 
#> 80  6 
#> 

peak_frequency(a, units::set_units(.25, "Hz"))
#> [[1]]
#> Units: [Hz]
#>  X  Z 
#> 80  5 
#> 

# Increasing resolution more
peak_frequency(a, units::set_units(.005, "Hz"))
#> [[1]]
#> Units: [Hz]
#>      X      Z 
#> 79.995  5.115 
#> 

a <- acc(
  list(
    cbind(
      X = sin((1:200) / (5 / (pi * 2))),
      Z = cos(80 + 1:200 / (80 / (pi * 2)))
    )
  ), 
  units::set_units(400, "Hz")
)

peak_frequency(a, units::set_units(.005, "Hz"))
#> [[1]]
#> Units: [Hz]
#>      X      Z 
#> 79.995  4.875 
#> 
```
