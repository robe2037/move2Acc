# Specify IMU data columns present in a `move2` object

Define which columns in a `move2` object contain IMU data. Pass the
result as the `colset` argument of
[`as_acc()`](https://robe2037.github.io/move2Acc/reference/as_acc.md),
[`as_mag()`](https://robe2037.github.io/move2Acc/reference/as_mag.md),
or
[`as_gyro()`](https://robe2037.github.io/move2Acc/reference/as_gyro.md)
to convert those columns into an IMU vector.

`move2` objects store IMU data in two ways:

- **Long-format** columns store one measurement (possibly for multiple
  axes) in a single row.

- **Burst-format** columns store a burst of measurements as a
  space-delimited string. This string must be segmented into
  axis-specific measurements using an associated column that indicates
  the axes present for the bursted data. A further column provides the
  sampling frequency of the burst. All three of these columns must be
  present to form a valid burst-format column set.

## Usage

``` r
imu_colset(
  x = NULL,
  y = NULL,
  z = NULL,
  bursts = NULL,
  axes = NULL,
  frequency = NULL
)
```

## Arguments

- x, y, z:

  (Long-format) Column name(s) for the X, Y, and/or Z axes.

- bursts:

  (Burst-format) Column name containing the raw burst strings.

- axes:

  (Burst-format) Column name containing the axis labels for each burst.

- frequency:

  (Burst-format) Column name containing the sampling frequency for each
  burst.

## Value

An `imu_colset` object of type `"long"` or `"burst"`.

## See also

[`as_acc()`](https://robe2037.github.io/move2Acc/reference/as_acc.md),
[`as_mag()`](https://robe2037.github.io/move2Acc/reference/as_mag.md),
[`as_gyro()`](https://robe2037.github.io/move2Acc/reference/as_gyro.md)
to extract IMU data from a move2 object.

[`active_acc_colsets()`](https://robe2037.github.io/move2Acc/reference/active_colsets.md),
[`active_mag_colsets()`](https://robe2037.github.io/move2Acc/reference/active_colsets.md),
[`active_gyro_colsets()`](https://robe2037.github.io/move2Acc/reference/active_colsets.md)
to identify IMU colsets present in a move2 object.

[`movebank_acc_colsets()`](https://robe2037.github.io/move2Acc/reference/movebank_colsets.md),
[`movebank_mag_colsets()`](https://robe2037.github.io/move2Acc/reference/movebank_colsets.md),
[`movebank_gyro_colsets()`](https://robe2037.github.io/move2Acc/reference/movebank_colsets.md)
to see column sets provided by Movebank.

## Examples

``` r
# Long-format: one or more axes
imu_colset(x = "my_x", y = "my_y", z = "my_z")
#> long-format [X=my_x, Y=my_y, Z=my_z]
imu_colset(x = "my_x", y = "my_y")
#> long-format [X=my_x, Y=my_y]

# Burst-format: all three columns required
imu_colset(bursts = "my_raw", axes = "my_axes", frequency = "my_freq")
#> burst-format [bursts=my_raw, axes=my_axes, frequency=my_freq]

# Use a colset to extract IMU data from those columns in a move2 object
as_acc(gulls(), colset = imu_colset(x = "acceleration_raw_x"))
#> <acceleration[1499]>
#>    [1] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>    [8] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>   [15] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>   [22] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>   [29] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>   [36] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>   [43] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>   [50] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>   [57] <NA>      <NA>      <NA>      <NA>      <NA>      (-97.75)  <NA>     
#>   [64] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>   [71] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>   [78] <NA>      <NA>      <NA>      <NA>      <NA>      (-95)     <NA>     
#>   [85] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>   [92] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>   [99] <NA>      <NA>      <NA>      <NA>      <NA>      (7.1)     <NA>     
#>  [106] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [113] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [120] <NA>      <NA>      <NA>      <NA>      <NA>      (77.65)   <NA>     
#>  [127] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [134] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [141] <NA>      <NA>      <NA>      <NA>      <NA>      (46.9)    <NA>     
#>  [148] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [155] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [162] <NA>      <NA>      <NA>      <NA>      <NA>      (-29.15)  <NA>     
#>  [169] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [176] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [183] <NA>      <NA>      <NA>      <NA>      <NA>      (119.8)   <NA>     
#>  [190] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [197] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [204] <NA>      <NA>      <NA>      <NA>      <NA>      (142)     <NA>     
#>  [211] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [218] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [225] <NA>      <NA>      <NA>      <NA>      <NA>      (11.45)   <NA>     
#>  [232] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [239] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [246] <NA>      <NA>      <NA>      <NA>      <NA>      (0.4)     <NA>     
#>  [253] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [260] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [267] <NA>      <NA>      <NA>      <NA>      <NA>      (-12.1)   <NA>     
#>  [274] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [281] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [288] <NA>      <NA>      <NA>      <NA>      <NA>      (336)     <NA>     
#>  [295] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [302] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [309] <NA>      <NA>      <NA>      <NA>      <NA>      (-168.85) <NA>     
#>  [316] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [323] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [330] <NA>      <NA>      <NA>      <NA>      <NA>      (-280.5)  <NA>     
#>  [337] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [344] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [351] <NA>      <NA>      <NA>      <NA>      <NA>      (-186.1)  <NA>     
#>  [358] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [365] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [372] <NA>      <NA>      <NA>      <NA>      <NA>      (-113.85) <NA>     
#>  [379] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [386] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [393] <NA>      <NA>      <NA>      <NA>      <NA>      (-221.35) <NA>     
#>  [400] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [407] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [414] <NA>      <NA>      <NA>      <NA>      <NA>      (-202)    <NA>     
#>  [421] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [428] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [435] <NA>      <NA>      <NA>      <NA>      <NA>      (-191.1)  <NA>     
#>  [442] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [449] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [456] <NA>      <NA>      <NA>      <NA>      <NA>      (710.15)  <NA>     
#>  [463] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [470] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [477] <NA>      <NA>      <NA>      <NA>      <NA>      (-123.35) <NA>     
#>  [484] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [491] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [498] <NA>      <NA>      <NA>      <NA>      <NA>      (-211.1)  <NA>     
#>  [505] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [512] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [519] <NA>      <NA>      <NA>      <NA>      <NA>      (-168.95) <NA>     
#>  [526] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [533] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [540] <NA>      <NA>      <NA>      <NA>      <NA>      (511.8)   <NA>     
#>  [547] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [554] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [561] <NA>      <NA>      <NA>      <NA>      <NA>      (328.2)   <NA>     
#>  [568] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [575] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [582] <NA>      <NA>      <NA>      <NA>      <NA>      (-169.35) <NA>     
#>  [589] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [596] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [603] <NA>      <NA>      <NA>      <NA>      <NA>      (-157.7)  <NA>     
#>  [610] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [617] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [624] <NA>      <NA>      <NA>      <NA>      <NA>      (353)     <NA>     
#>  [631] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [638] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [645] <NA>      <NA>      <NA>      <NA>      <NA>      (-77.75)  <NA>     
#>  [652] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [659] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [666] <NA>      <NA>      <NA>      <NA>      <NA>      (-213.55) <NA>     
#>  [673] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [680] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [687] <NA>      <NA>      <NA>      <NA>      <NA>      (548.8)   <NA>     
#>  [694] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [701] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [708] <NA>      <NA>      <NA>      <NA>      <NA>      (102.95)  <NA>     
#>  [715] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [722] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [729] <NA>      <NA>      <NA>      <NA>      <NA>      (-167.15) <NA>     
#>  [736] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [743] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [750] <NA>      <NA>      <NA>      <NA>      <NA>      (160.3)   <NA>     
#>  [757] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [764] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [771] <NA>      <NA>      <NA>      <NA>      <NA>      (75.7)    <NA>     
#>  [778] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [785] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [792] <NA>      <NA>      <NA>      <NA>      <NA>      (64.65)   <NA>     
#>  [799] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [806] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [813] <NA>      <NA>      <NA>      <NA>      <NA>      (180.35)  <NA>     
#>  [820] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [827] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [834] <NA>      <NA>      <NA>      <NA>      <NA>      (-96.05)  <NA>     
#>  [841] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [848] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [855] <NA>      <NA>      <NA>      <NA>      <NA>      (376.3)   <NA>     
#>  [862] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [869] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [876] <NA>      <NA>      <NA>      <NA>      <NA>      (-26.5)   <NA>     
#>  [883] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [890] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [897] <NA>      <NA>      <NA>      <NA>      <NA>      (300.3)   <NA>     
#>  [904] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [911] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [918] <NA>      <NA>      <NA>      <NA>      <NA>      (145.15)  <NA>     
#>  [925] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [932] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [939] <NA>      <NA>      <NA>      <NA>      <NA>      (-303.65) <NA>     
#>  [946] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [953] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [960] <NA>      <NA>      <NA>      <NA>      <NA>      (127.3)   <NA>     
#>  [967] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [974] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [981] <NA>      <NA>      <NA>      <NA>      <NA>      (126.15)  <NA>     
#>  [988] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#>  [995] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1002] <NA>      <NA>      <NA>      <NA>      <NA>      (232.95)  <NA>     
#> [1009] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1016] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1023] <NA>      <NA>      <NA>      <NA>      <NA>      (101.5)   <NA>     
#> [1030] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1037] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1044] <NA>      <NA>      <NA>      <NA>      <NA>      (187.05)  <NA>     
#> [1051] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1058] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1065] <NA>      <NA>      <NA>      <NA>      <NA>      (125.6)   <NA>     
#> [1072] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1079] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1086] <NA>      <NA>      <NA>      <NA>      <NA>      (412.05)  <NA>     
#> [1093] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1100] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1107] <NA>      <NA>      <NA>      <NA>      <NA>      (219.25)  <NA>     
#> [1114] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1121] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1128] <NA>      <NA>      <NA>      <NA>      <NA>      (104.1)   <NA>     
#> [1135] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1142] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1149] <NA>      <NA>      <NA>      <NA>      <NA>      (67.7)    <NA>     
#> [1156] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1163] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1170] <NA>      <NA>      <NA>      <NA>      <NA>      (65.9)    <NA>     
#> [1177] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1184] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1191] <NA>      <NA>      <NA>      <NA>      <NA>      (81.7)    <NA>     
#> [1198] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1205] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1212] <NA>      <NA>      <NA>      <NA>      <NA>      (69.6)    <NA>     
#> [1219] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1226] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1233] <NA>      <NA>      <NA>      <NA>      <NA>      (147.1)   <NA>     
#> [1240] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1247] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1254] <NA>      <NA>      <NA>      <NA>      <NA>      (67)      <NA>     
#> [1261] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1268] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1275] <NA>      <NA>      <NA>      <NA>      <NA>      (19.1)    <NA>     
#> [1282] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1289] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1296] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1303] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1310] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1317] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1324] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1331] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1338] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1345] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1352] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1359] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1366] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1373] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1380] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1387] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1394] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1401] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1408] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1415] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1422] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1429] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1436] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1443] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1450] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1457] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1464] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1471] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1478] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1485] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1492] <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>     
#> [1499] <NA>     
#> # frequency: 20 [Hz]
```
