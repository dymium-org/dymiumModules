modules::import('data.table')
modules::import('magick')
modules::import('purrr')
modules::import('here')
modules::expose(here::here('modules/matsim/logger.R')) # import lgr's logger. To use the logger use 'lg' (default logger's name).

make_legHistogram_all <- function(search_path, outdir = here::here()) {
  make_plot(pattern = 'legHistogram_all', search_path = search_path, outdir = outdir)
}

make_plot <- function(pattern, search_path, outdir) {
  .start_time <- Sys.time()
  files <-
    list.files(
      search_path,
      pattern = pattern,
      recursive = T,
      full.names = T
    )
  filenames <-
    list.files(
      search_path,
      pattern = pattern,
      recursive = T,
      include.dirs = FALSE
    )

  files_dt <- data.table(
    path = files,
    order = gsub("\\/.*", "", filenames) %>% gsub("it.", "", .) %>% as.numeric()
  ) %>%
    .[order(order)]

  lg$info('Combining {nrow(files_dt)} images.')

  outfile <- file.path(outdir, paste0(pattern, "_", format(Sys.time(),'%Y_%d_%m__%H_%M'), ".gif"))

  files_dt$path %>%
    map(image_read) %>% # reads each path file
    image_join(.) %>% # joins image
    image_animate(fps = 10) %>% # animates, can opt for number of loops
    image_write(outfile)
  lg$info("Saved gif to: '{outfile}'")
  lg$info("Finished in {format(Sys.time() - .start_time)}.")
}

# make_legHistogram_all(search_path = "inputs/matsim/output/ITERS")