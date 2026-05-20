# Create a `acc` vector

Create a `acc` vector

## Usage

``` r
acc(
  bursts = list(),
  frequency = units::set_units(double(), "Hz"),
  start = NULL
)
```

## Arguments

- bursts:

  a list of matrices

- frequency:

  The sampling frequency of the recordings in `bursts`. Either the same
  length of `bursts` or it will be recycled. If no units are specified,
  the frequency is assumed to be in Hz.

- start:

  Start time of the burst, in POSIXct format
