# Test that package works and data can be loaded
test_that('Train a ibis.iSDM model and apply inSights on it', {

  skip_if_not_installed("glmnet")
  skip_if_not_installed("ibis.iSDM")

  suppressWarnings( requireNamespace("terra", quietly = TRUE) )
  suppressWarnings( requireNamespace("ibis.iSDM", quietly = TRUE) )
  suppressWarnings( requireNamespace("glmnet", quietly = TRUE) )
  suppressPackageStartupMessages(
    require("ibis.iSDM")
  )
  suppressPackageStartupMessages(
    require("glmnet")
  )

  options("ibis.setupmessages" = FALSE)

  # Load ibis test data
  background <- terra::rast(system.file('extdata/europegrid_50km.tif', package='ibis.iSDM',mustWork = TRUE))
  virtual_points <- sf::st_read(system.file('extdata/input_data.gpkg', package='ibis.iSDM',mustWork = TRUE),'points',quiet = TRUE)
  ll <- list.files(system.file('extdata/predictors',package = 'ibis.iSDM',mustWork = TRUE),full.names = T)
  predictors <- terra::rast(ll);names(predictors) <- tools::file_path_sans_ext(basename(ll))

  # Also load land-use layers
  lu <- c(
    terra::rast(system.file('extdata/Grassland.tif', package='insights',mustWork = TRUE)),
    terra::rast(system.file('extdata/Sparsely.vegetated.areas.tif', package='insights',mustWork = TRUE))
  )
  # Convert to fractions
  lu <- lu / 10000

  # Now train a small little model
  fit <- ibis.iSDM::distribution(background) |>
    ibis.iSDM::add_biodiversity_poipo(virtual_points) |>
    ibis.iSDM::add_predictors(predictors) |>
    ibis.iSDM::engine_glmnet() |>
    ibis.iSDM::train() |>
    ibis.iSDM::threshold(method = "perc", value = .33)

  expect_s3_class(fit, "DistributionModel")
  tr <- fit$get_data("threshold_percentile")
  expect_s4_class(tr, "SpatRaster")

  # --- #
  # Now apply insights
  expect_no_error(
    suppressMessages(
      out <- insights_fraction(range = fit,lu = lu)
    )
  )
  expect_s4_class(out, "SpatRaster")
  expect_gt(terra::global(out,"max",na.rm=T)[,1], 0)

  # Also apply on the threshold directly
  expect_no_error(
    suppressMessages(
      out2 <- insights_fraction(range = tr,lu = lu)
    )
  )
  expect_equal(round(terra::global(out, "max", na.rm = TRUE)[,1],3),
               round(terra::global(out2, "max", na.rm = TRUE)[,1],3))

  }
)
