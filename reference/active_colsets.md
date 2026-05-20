# Identify IMU columns present in a `move2` object

Determine the column sets that will be used by default when extracting
IMU data from a `move2` object. Column sets are processed independently,
but a single `move2` may contain multiple active column sets for one IMU
sensor.

- `active_acc_colsets()` — column sets used by
  [`as_acc()`](https://robe2037.github.io/move2Acc/reference/as_acc.md).

- `active_mag_colsets()` — column sets used by
  [`as_mag()`](https://robe2037.github.io/move2Acc/reference/as_mag.md).

- `active_gyro_colsets()` — column sets used by
  [`as_gyro()`](https://robe2037.github.io/move2Acc/reference/as_gyro.md).

## Usage

``` r
active_acc_colsets(x)

active_mag_colsets(x)

active_gyro_colsets(x)
```

## Arguments

- x:

  A `move2` object.

## Value

A list of `imu_colset` objects.

## Details

If no active colsets are found, use
[`imu_colset()`](https://robe2037.github.io/move2Acc/reference/imu_colset.md)
to specify the columns that contain IMU data.

## See also

[`movebank_acc_colsets()`](https://robe2037.github.io/move2Acc/reference/movebank_colsets.md),
[`movebank_mag_colsets()`](https://robe2037.github.io/move2Acc/reference/movebank_colsets.md),
[`movebank_gyro_colsets()`](https://robe2037.github.io/move2Acc/reference/movebank_colsets.md)
for the supported default colsets.

[`as_acc()`](https://robe2037.github.io/move2Acc/reference/as_acc.md),
[`as_mag()`](https://robe2037.github.io/move2Acc/reference/as_mag.md),
[`as_gyro()`](https://robe2037.github.io/move2Acc/reference/as_gyro.md)
to extract IMU data from a `move2` object.

## Examples

``` r
active_acc_colsets(albatrosses())
#> $eobs
#> burst-format [axes=eobs_acceleration_axes, frequency=eobs_acceleration_sampling_frequency_per_axis, bursts=eobs_accelerations_raw]
#> 

# Multiple colsets may be available
active_acc_colsets(move2::mt_stack(albatrosses(), gulls()))
#> $eobs
#> burst-format [axes=eobs_acceleration_axes, frequency=eobs_acceleration_sampling_frequency_per_axis, bursts=eobs_accelerations_raw]
#> 
#> $raw_xyz
#> long-format [X=acceleration_raw_x, Y=acceleration_raw_y, Z=acceleration_raw_z]
#> 

# Missing long-format axes are not included in the set
g <- gulls()
g$acceleration_raw_x <- NULL
active_acc_colsets(g)
#> $raw_xyz
#> long-format [=acceleration_raw_y, =acceleration_raw_z]
#> 

# Columns with no data are also removed
g$acceleration_raw_y <- NA
active_acc_colsets(g)
#> $raw_xyz
#> long-format [=acceleration_raw_z]
#> 

# Some column sets must be present in their entirety
alb <- albatrosses()
alb$eobs_acceleration_axes <- NULL

if (FALSE) { # \dontrun{
  active_acc_colsets(alb)
} # }
```
