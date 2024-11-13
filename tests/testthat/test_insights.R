# Manually test insights using loaded raster and stars objects
test_that('Directly apply InSiGHTS on rasters and stars', {

  skip_if_not_installed("stars")
  skip_if_not_installed("terra")

  suppressWarnings( requireNamespace("terra", quietly = TRUE) )
  suppressWarnings( requireNamespace("stars", quietly = TRUE) )

  # Load range
  range <- insights:::load_exampledata(timeperiod = "current")
  testthat::expect_s4_class(range, "SpatRaster")

  # Load land-use layers
  lu <- c(
    terra::rast(system.file('extdata/Grassland.tif', package='insights',mustWork = TRUE)),
    terra::rast(system.file('extdata/Sparsely.vegetated.areas.tif', package='insights',mustWork = TRUE))
  )
  # Convert to fractions
  lu <- lu / 10000
  testthat::expect_true(
    all(terra::global(lu, "max", na.rm = TRUE)[,1] <= 1)
  )

  # --- #
  # Now apply insights
  # range = current | lu = current
  expect_no_error(
    suppressMessages(
      out <- insights_fraction(range = range, lu = lu)
    )
  )
  expect_s4_class(out, "SpatRaster")
  expect_gt(terra::global(out,"max",na.rm=T)[,1], 0)
  # Habitat clips should be smaller/equal as they are a subset
  expect_gte(terra::global(range,"max",na.rm=T)[,1],
             terra::global(out,"max",na.rm=T)[,1],)

  # --------- #
  # Load future layer and repeat
  range <- insights:::load_exampledata(timeperiod = "future")
  testthat::expect_s3_class(range, "stars")
  testthat::expect_length(range, 1)

  # Now apply insights
  # range = future | lu = current
  expect_no_error(
    suppressMessages(
      out <- insights_fraction(range = range, lu = lu)
    )
  )
  testthat::expect_s3_class(out, "stars")
  testthat::expect_s3_class(insights_summary(out), "data.frame")

  # --- #
  # Apply with future lu
  ll <- list.files(system.file('extdata/predictors_presfuture/',package = "ibis.iSDM",mustWork = TRUE),
                   full.names = T)
  # Load the same files future ones
  suppressWarnings(
    lu <- stars::read_stars(ll) |> stars:::slice.stars('Time', seq(1, 86, by = 10))
  )
  sf::st_crs(lu) <- sf::st_crs(4326)

  # Normalize here
  lu <- lu |> stars:::select.stars(crops, secdf)
  suppressMessages(
    lu <- ibis.iSDM::predictor_transform(lu, "norm") |> round(2)
  )

  # range = future | lu = future
  expect_no_error(
    suppressMessages(
      out <- insights_fraction(range = range, lu = lu)
    )
  )
  testthat::expect_s3_class(out, "stars")

})

# ----------------- #
# Exact tests
test_that('Exact INSIGHTS test', {

  skip_if_not_installed("stars")
  skip_if_not_installed("terra")

  suppressWarnings( requireNamespace("terra", quietly = TRUE) )
  suppressWarnings( requireNamespace("stars", quietly = TRUE) )

  # Define raster
  range <- terra::rast(nrow = 10, ncol = 10,
                       vals = stats::rbinom(100,1,.5))
  testthat::expect_s4_class(range, "SpatRaster")

  val <- stats::rlnorm(100,0,.5)
  val <- val/max(val)
  lu <- terra::rast(nrow = 10, ncol = 10, vals = val)

  # --- #
  # Now apply insights
  # range = current | lu = current
  expect_no_error(
    suppressMessages(
      out <- insights_fraction(range = range, lu = lu)
    )
  )
  expect_s4_class(out, "SpatRaster")
  expect_gt(terra::global(out,"max",na.rm=T)[,1], 0)
})

