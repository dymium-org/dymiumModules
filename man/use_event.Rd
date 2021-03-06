% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/event-utils.R
\name{use_event}
\alias{use_event}
\title{Create an event.}
\usage{
use_event(name, module, with_comments)
}
\arguments{
\item{name}{(\code{character(1)})\cr
Name of an event to create. This is also be used as its script name.}

\item{module}{(\code{character(1)})\cr
Name of the module folder to add a event file to. The function
looks for a folder inside the \code{modules} folder at the root folder of the active
R project. If the module folder is not found or has not been created this will
return an error.}

\item{with_comments}{(\code{logical(1)})\cr
If \code{TRUE} the generated event script will contain
comments about what each component inside the script does and some recommendations
for the user to follow when authoring an event. For advance users, you may not need
this hence you may specify \code{FALSE}. If missing, it will be prompted in the console
for you to decide.}
}
\description{
This function creates an event script from the provided event template inside
a module along with a testtthat test script.
}
\examples{

\dontrun{
  # Note: running this will create a folder called "modules" and a sub-folder
  #       to your working directory within the folder called "demography"
  use_module(name = "demography")

  # create an event called 'birth' inside the 'demography' module.
  use_event(name = "birth", module = 'demography')
}
}
