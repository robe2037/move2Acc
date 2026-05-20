# Create calibration functions for raw acceleration values

Generate an `acc_calibration` object containing a list of functions with
various calibration parameters to be used in
[`acc_calibrate()`](https://robe2037.github.io/move2Acc/reference/acc_calibrate.md).

Use `acc_calibration()` to specify calibration parameters manually.
Arguments are vectorized and matched by index.

Use `as_acc_calibration()` to convert a data.frame containing row-wise
burst calibration parameters to an `acc_calibration` object.

This allows you to specify burst-specific calibration functions to
flexibly convert raw acceleration values to physical units in `acc`
vectors that contain data from heterogeneous sources.

## Usage

``` r
acc_calibration(
  manufacturer = NULL,
  tag_id = NULL,
  sensitivity = NULL,
  offset = NULL,
  offset_x = offset,
  offset_y = offset,
  offset_z = offset,
  slope = NULL,
  slope_x = slope,
  slope_y = slope,
  slope_z = slope,
  orientation = NULL,
  orientation_x = orientation,
  orientation_y = orientation,
  orientation_z = orientation,
  units = "m/s^2",
  axes = "XYZ"
)

as_acc_calibration(df)
```

## Arguments

- manufacturer:

  Manufacturer of the tag. Currently, `"eobs"` and `"ornitela"` are
  supported. For other manufacturers, leave `NULL` and manually specify
  the calibration parameters below.

- tag_id:

  If `manufacturer = "eobs"`, the e-obs tag ID for the tag.

- sensitivity:

  If `manufacturer = "eobs"`, the sensitivity of the tag. Defaults to
  `"low"` if none provided. Note that only e-obs generation 1 tags have
  a sensitivity setting.

- offset, offset_x, offset_y, offset_z:

  Custom offset to use when calibrating. To specify axis-specific
  offsets, use `offset_x`, `offset_y`, and/or `offset_z`.

  Required if no `manufacturer` is specified.

- slope, slope_x, slope_y, slope_z:

  Custom slope to use when calibrating. To specify axis-specific slope,
  use `slope_x`, `slope_y`, and/or `slope_z`.

  Required if no `manufacturer` is specified.

- orientation, orientation_x, orientation_y, orientation_z:

  Either `1` or `-1` indicating the orientation of the tag's axes. To
  specify axis-specific orientations, use `orientation_x`,
  `orientation_y`, and/or `orientation_z`. Defaults to `1`.

  This is useful to standardize orientations across tags of different
  manufacturers or generations.

- units:

  Output units. Either `"m/s^2"` (default) or `"standard_free_fall"`.

- axes:

  Character string specifying which axes to calibrate, e.g., `"XYZ"`
  (default), `"XY"`, or `"Z"`. Only these axes will appear in the
  calibrated output.

- df:

  data.frame containing columns corresponding to the available arguments
  in `acc_calibration()`

## Value

An `acc_calibration` object.

## See also

[`acc_calibrate()`](https://robe2037.github.io/move2Acc/reference/acc_calibrate.md)
to apply calibration functions to the entries in an `acc` vector.

## Examples

``` r
# Calibration for ornitela tags:
acc_calibration(manufacturer = "ornitela")
#> <acc_calibration[1]>

# E-obs tag defaults vary by tag_id and sensitivity (default `"low"`)
acc_calibration(manufacturer = "eobs", tag_id = 1000, sensitivity = "high")
#> <acc_calibration[1]>
acc_calibration(manufacturer = "eobs", tag_id = 4000)
#> <acc_calibration[1]>

# Provide vector arguments to generate element-wise calibrations:
acc_calibration(
  manufacturer = c("eobs", "ornitela"),
  tag_id = c(1000, NA)
)
#> <acc_calibration[2]>

# Calibration with explicit offset and slope
acc_calibration(offset = 2048, slope = 1 / 512)
#> <acc_calibration[1]>

# Calibrate specific axes with axis-specific args:
cal <- acc_calibration(
  offset_x = 2048, 
  offset_y = 2046,
  offset_z = 2048,
  slope = 1 / 512, 
  orientation_y = -1 # Flip y axis orientation
)

# Apply calibration with acc_calibrate()
acc_calibrate(acc_example(), cal)
#> <acceleration[2]>
#> [1] (-39.21 39.19 -39.21) [m/s^2] (-39.23 39.2 -39.21) [m/s^2] 
#> # frequency: 20 [Hz]
```
