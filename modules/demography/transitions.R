modules::import('dymiumCore')
modules::import("R6", "R6Class")

TransitionGroupHousehold <- R6Class(
  classname = "TransitionGroupHousehold",
  inherit = TransitionClassification,
  public = list(
  )
)

TransitionChildCustody <- R6Class(
  classname = "TransitionChildCustody",
  inherit = TransitionClassification,
  public = list(
  )
)