# Test that package works and data can be loaded
test_that('Load data', {

  suppressWarnings( requireNamespace("terra", quietly = TRUE) )
  suppressWarnings( requireNamespace("ibis.iSDM", quietly = TRUE) )

  # Load files
  range <- terra::rast(system.file('extdata/example_range.tif', package='insights',mustWork = TRUE))

  expect_true(ibis.iSDM::is.Raster(range))
  }
)
