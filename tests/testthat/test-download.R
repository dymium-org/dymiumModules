test_that("download", {
  tmpdir <- tempdir()
  dir.create(tmpdir)
  download_module(name = "test", force = T,  .basedir = fs::path(tmpdir))
  checkmate::expect_names(
    x = list.files(fs::path(tmpdir, "modules"), all.files = T),  
    permutation.of = c("temp-module.zip", "test")
  )
})
