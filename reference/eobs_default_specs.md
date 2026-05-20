# Default e-obs tag configuration table

Returns a data.frame of known e-obs tag generations with their tag ID
ranges and default calibration parameters.

## Usage

``` r
eobs_default_specs()
```

## Value

A data.frame with columns `tag_gen`, `min_tag_id`, `max_tag_id`,
`sensitivity`, `orientation_x`, `orientation_y`, `orientation_z`,
`offset`, and `slope`.

## See also

[`acc_calibration()`](https://robe2037.github.io/move2Acc/reference/acc_calibration.md)
to set up tag-specific calibration specifications and
[`acc_calibrate()`](https://robe2037.github.io/move2Acc/reference/acc_calibrate.md)
to calibrate eobs acceleration values.
