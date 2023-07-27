# Test that package works and data can be loaded
test_that('Load data and do InSiGHTS on single layer and summarize', {

  suppressWarnings( requireNamespace("terra", quietly = TRUE) )
  suppressWarnings( requireNamespace("ibis.iSDM", quietly = TRUE) )

  # Load files
  range <- terra::rast(system.file('extdata/example_range.tif', package='insights',mustWork = TRUE))

  # Also load land-use layers
  lu <- c(
    terra::rast(system.file('extdata/Grassland.tif', package='insights',mustWork = TRUE)),
    terra::rast(system.file('extdata/Sparsely.vegetated.areas.tif', package='insights',mustWork = TRUE))
  )
  # Convert to fractions
  lu <- lu / 10000

  # Input checks
  expect_true(ibis.iSDM::is.Raster(range))
  expect_true(ibis.iSDM::is.Raster(lu))
  expect_true(terra::nlyr(lu)==2)

  # Do basic clipping
  expect_no_error(
    out <- insights_fraction(range = range,
                           lu = lu)
  )
  expect_true(ibis.iSDM::is.Raster(out))
  expect_gt(terra::global(out, "max", na.rm = TRUE), 0.3)

  # With a single layer
  expect_no_error(
    out <- insights_fraction(range = range,
                             lu = lu$Grassland)
  )
  expect_true(ibis.iSDM::is.Raster(out))

  # See if it fails correctly if non-fractional layers are supplied
  expect_error(
    out <- insights_fraction(range = range,
                             lu = lu*2)
  )
  expect_error(
    out <- insights_fraction(range = range*2,
                             lu = lu)
  )

  # ------ #
  # Load an additional layer to supply for
  dem <- terra::rast(system.file('extdata/DEM.tif', package='insights',mustWork = TRUE))

  expect_no_error(
    out <- insights_fraction(range = range,
                             lu = lu,
                             dem)
  )
  expect_true(ibis.iSDM::is.Raster(out))
  expect_gt(terra::global(out, "max", na.rm = TRUE), 0.3)

  # Also with possible error in other
  expect_error(
    out <- insights_fraction(range = range,
                             lu = lu,
                             dem+2)
  )

  # --- #
  expect_no_error(
    out <- insights_fraction(range = range,
                             lu = lu,
                             dem)
  )

  # Now summarize
  df <- insights_summary(out)
  expect_s3_class(df, "data.frame")
  expect_gt(df$suitability, 10)

  # Directly on the range should also work
  expect_no_error(insights_summary(range))

  }
)

# Multiple time steps
# This test ensures that the refinement works even when multiple layers are supplied.
test_that('Do InSiGHTS on multiple layers and summarize', {

  suppressWarnings( requireNamespace("terra", quietly = TRUE) )
  suppressWarnings( requireNamespace("ibis.iSDM", quietly = TRUE) )

  # Load files
  range <- terra::rast(system.file('extdata/example_range.tif', package='insights',mustWork = TRUE))

  # Also load land-use layers
  lu <- c(
    terra::rast(system.file('extdata/Grassland.tif', package='insights',mustWork = TRUE)),
    terra::rast(system.file('extdata/Sparsely.vegetated.areas.tif', package='insights',mustWork = TRUE))
  )
  # Convert to fractions
  lu <- lu / 10000

  # Load an additional layer to supply for
  dem <- terra::rast(system.file('extdata/DEM.tif', package='insights',mustWork = TRUE))

  # ---- #
  # Sample patches and set them to 0
  pp <- terra::patches( terra::which.lyr(range))

  # Make a time series of the range
  range_ts <- c(range,
                terra::as.int(pp>5),
                terra::as.int(pp>10),
                terra::as.int(pp>15))
  range_ts <- terra::na.omit(range_ts)
  terra::time(range_ts, tstep="years") <- seq(2000, 2050, 15)

  # Input checks
  expect_true(ibis.iSDM::is.Raster(range_ts))
  expect_true(ibis.iSDM::is.Raster(lu))
  expect_true(terra::nlyr(lu)==2)

  # Do basic clipping
  expect_no_error(
    out <- insights_fraction(range = range_ts,
                             lu = lu,
                             dem)
  )
  expect_true(ibis.iSDM::is.Raster(out))
  expect_equal(terra::nlyr(out),4)
  expect_true(all(terra::global(out, "max", na.rm = TRUE)[,1]> 0))

  # Now summarize
  df <- insights_summary(out)
  expect_s3_class(df, "data.frame")
  expect_equal(df$suitability[1],0)
  df <- insights_summary(out,toArea = FALSE)
  expect_s3_class(df, "data.frame")
  df <- insights_summary(out,toArea = FALSE, relative = FALSE)

  # Directly on the range should also work
  expect_no_error(insights_summary(range_ts))
}
)
