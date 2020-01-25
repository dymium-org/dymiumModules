# This script creates a logger to be used across the module.
# To customise and learn more out the logger package used here please read the
# vignette of 'lgr' package (https://github.com/s-fleck/lgr).
# To use the logger inside your event script you may import the constants script
# as the following 'modules::expose('modules/matsim/logger.R')' the logger
# A neat modification that I like to do is to colorize the
# name of my module's logger by changing '{.logger$name}' to '{crayon::blue(.logger$name)}'
# this will make your module's logger name more standout on your R console. To make
# the color modification you will need to have 'crayon' package installed.
#
#
# TL;DR - to use logger put this 'modules::expose('modules/matsim/logger.R')' in your
#         event script. Then use the following commands.
#
# > lg$info('I am the info-level logger')
# > lg$warn('I am the warn-level logger')
# > lg$error('I am the error-level logger')
#
# !! The codes below should work without any modifications, however if you are
# comfortable with how the 'lgr' and 'modules' packages work you may modify
# the codes below.
modules::import('lgr')
modules::import("crayon", "magenta")
modules::export('lg')
lg <- lgr::get_logger_glue(name = 'matsim')
lg$set_appenders(list(cons = lgr::AppenderConsole$new()))
lg$appenders$cons$set_layout(
  lgr::LayoutGlue$new(
    fmt = '[{format(timestamp, \'%H:%M:%S\')}] \\
           {pad_right(colorize_levels(toupper(level_name)), 5)}\\
           {crayon::magenta(.logger$name)} {caller}: {msg}'
  )
)
logfile <- file.path(dymiumCore::get_active_scenario()$scenario_dir, "log")
fs::file_create(logfile)
lg$add_appender(lgr::AppenderFile$new(logfile, layout = lgr::LayoutJson$new()))
lg$set_propagate(FALSE)
