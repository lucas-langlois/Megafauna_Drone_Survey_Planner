repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)

test_that("both apps provide metric distance measurement", {
  for (app_file in c(
    file.path(repo_root, "..", "app.R"),
    file.path(repo_root, "app.R")
  )) {
    lines <- readLines(app_file, warn = FALSE)
    text <- paste(lines, collapse = "\n")
    expect_true(grepl("addMeasure", text, fixed = TRUE), info = app_file)
    expect_true(grepl('primaryLengthUnit = "meters"', text, fixed = TRUE), info = app_file)
    expect_true(grepl('secondaryLengthUnit = "kilometers"', text, fixed = TRUE), info = app_file)
  }
})
