# The script is where helper functions of the module should reside.
# Sometimes you may have a function that are used across the module, in many events,
# this is the central place for you to store this type of function. Hence, in every
# event scrips that you create using 'dymiumCore::create_new_event' this script, helpers.R,
# will be imported. If not needed, you may remove the import line.
#
# To use these helper functions inside your event script I suggest you import the helper script
# as the following 'helpers <- modules::import('modules/matsim/helpers.R')'.

# If the package dymimCore is not needed you may remove the line below which imports it
modules::import('dymiumCore')
modules::import('xml2')
modules::import('data.table')
modules::import('sf')
modules::import('furrr')
modules::import('glue')
modules::import('here')
modules::expose(here::here('modules/matsim/logger.R'))

# If you need your constants here uncomment the line below
# constants <- modules::use('modules/matsim/constants.R')

# this class modify some basic config params where 'x' is a path to a MATSim config file.
# see https://www.w3schools.com/xml/xpath_syntax.asp for xpath syntax
MatsimConfig <- R6::R6Class(
  classname = "MatsimConfig",
  public = list(
    config = NULL,
    initialize = function(x) {
      self$config <- xml2::read_xml(x)
    },

    set_controler = function(outputDirectory, firstIteration, lastIteration) {
      controler_node <- xml_find_first(self$config, "//module[@name='controler']")

      if (!missing(outputDirectory)) {
        n <- xml_find_first(controler_node, "param[@name='outputDirectory']")
        xml_attr(x = n, attr = "value") <- outputDirectory
      }

      if (!missing(firstIteration)) {
        n <- xml_find_first(controler_node, "param[@name='firstIteration']")
        xml_attr(x = n, attr = "value") <- firstIteration
      }

      if (!missing(lastIteration)) {
        n <- xml_find_first(controler_node, "param[@name='lastIteration']")
        xml_attr(x = n, attr = "value") <- lastIteration
      }

      invisible(self)
    },

    get_controler = function() {
      xml_find_first(self$config, "//module[@name='controler']")
    }
  )
)

create_population_v6 <- function(population) {
  data <- dymiumCore::toy_individuals
  # create a root doc with the first node being dtd file
  root <- xml_new_root(xml_dtd("plans", "", "http://www.matsim.org/files/dtd/plans_v4.dtd"),
                       version = "1.0", encoding = "utf-8")
  # create a person nodeset
  indData <- xml_add_child(root, "plans", .where = "after")
  s_time <- Sys.time()
  for (i in 1:5) {

    #' add person node
    #' ** adding a sibling node is faster than adding a child node hence the code below was implemented **
    if (i == 1) {
      ind <- xml_add_child(indData, "person")
    } else {
      ind <- xml_add_sibling(ind, "person")
    }
    xml_attr(ind, attr = "id") <- data[i, pid]
    xml_attr(ind, attr = "hid") <- data[i, hid]
    xml_attr(ind, attr = "age") <- data[i, age]
    xml_attr(ind, attr = "sex") <- data[i, sex]

    # add plan
    plan <- xml_add_child(ind, "plan")

    # add leg
    n_legs <- sample(1:10, 1)
    for (i in seq_len(n_legs)) {
      if (i == 1) {
        leg <- xml_add_child(plan, "leg")
      } else {
        leg <- xml_add_sibling(leg, "leg")
      }
      xml_attr(leg, attr = "trip_distance") <- sample(1:100, 1)
      xml_attr(leg, attr = "mode") <- sample(c('driving', 'biking', 'walking'), 1)
    }
  }
  e_time <- Sys.time()
  e_time - s_time # time taken

  # write to disk
  write_xml(root, "modules/matsim/plan.xml")

  # load in the xml again to validate againts its DTD
  doc <- read_xml("modules/matsim/plan.xml", options = c("DTDLOAD", "DTDVALID"))
}

#' This function was used to assign random geographical coordinates to the destination
#' and origins in the VISTA trips table. This takes ~10mins to run using a single core.
#' Consider using the future package `plan(multiprocess, workers = 10)` to speed up the function.
assign_location <- function(trips, zones, zone_col = 'CD_CODE06') {
  .start_time <- Sys.time()
  checkmate::assert_data_table(trips)
  checkmate::assert_names(names(trips), must.include = c(zone_col, "TRIPID"))
  checkmate::assert_class(zones, 'sf')
  checkmate::assert_names(names(zones), must.include = c(zone_col))
  zones <- st_transform(zones, 28355)
  n_grp <- 10
  zone_grp <- data.table::copy(trips) %>%
    .[, .(zone = unique(get(zone_col)))] %>%
    .[, grp := sample(1:n_grp, .N, replace = TRUE)] %>%
    .[, .(zone = list(zone)), by = grp]

  trips_new <-
    furrr::future_map_dfr(.x = 1:n_grp, .f = ~ {
      trips2 <- trips %>%
        .[get(zone_col) %in% zone_grp[.x, zone][[1]]] %>%
        .[, .(N = .N, TRIPID = list(TRIPID)), by = c(zone_col)]
      zones2 <- merge(zones, trips2, by = zone_col)
      sampled_points <-
        st_sample(zones2, size = zones2[['N']]) %>%
        st_coordinates() %>%
        as.data.table()
      trips2_zone_order <-
        tidyr::unnest(st_drop_geometry(zones2)[, c(zone_col, "TRIPID")],
                      cols = c(zone_col, 'TRIPID')) %>%
        as.data.table()
      trips2_zone_order[, `:=`(X = sampled_points[['X']], Y = sampled_points[['Y']])]
    })

  print(Sys.time() - .start_time)
  return(trips_new)
}

# for converting shp to xml network as required by MATSim's network_v2.dtd
shp2xml <- function(links, nodes, matsim_check_network = FALSE, matsim_jar = NULL) {
  # ID = "id", lanes = "nb_lanes", freespeed = "speed", fromID = "fnode", toID = "tnode"
  checkmate::assert_data_table(links, min.rows = 1, null.ok = FALSE)
  checkmate::assert_data_table(nodes, min.rows = 1, null.ok = FALSE)
  checkmate::assert_names(names(links), type = 'strict',
                          must.include = c("ID", "lanes", "freespeed", "fromID", "toID", "capacity", "oneway", "modes"))
  checkmate::assert_names(names(nodes), type = 'strict',
                          must.include = c("id", "x", "y"))

  if (sf::st_crs(nodes) != sf::st_crs(links)) {
    stop("links and nodes do not have the same CSR! tips: use sf::st_crs(x) to check.")
  }


  # create a root doc with the first node being dtd file
  root <- xml_new_root(xml_dtd("network", "", "http://www.matsim.org/files/dtd/network_v2.dtd"),
                       version = "1.0", encoding = "utf-8")
  network_node <- xml_add_child(root, "network", .where = "after")
  # create main nodesets
  node_node <- xml_add_child(network_node, "nodes", .where = "after")
  link_node <- xml_add_child(network_node, "links", .where = "after")
  s_time <- Sys.time()
  # add node nodes
  for (i in 1:nrow(nodes)) {

    # report the progress
    if (i %% 1000 == 1) {
      message("node ", i, " -- ", format(Sys.time() - s_time))
    }

    #' add person node
    #' ** adding a sibling node is faster than adding a child node hence the code below was implemented **
    if (i == 1) {
      .node <- xml_add_child(node_node, "node")
    } else {
      .node <- xml_add_sibling(.node, "node")
    }
    xml_attr(.node, attr = "id") <- nodes[['id']][i]
    xml_attr(.node, attr = "x") <- nodes[['x']][i]
    xml_attr(.node, attr = "y") <- nodes[['y']][i]
  }

  s_time <- Sys.time()
  # add link nodes
  for (i in 1:nrow(links)) {

    # report the progress
    if (i %% 1000 == 1) {
      message("link ", i, " -- ", format(Sys.time() - s_time))
    }
    #' add person node
    #' ** adding a sibling node is faster than adding a child node hence the code below was implemented **
    if (i == 1) {
      .link <- xml_add_child(link_node, "link")
    } else {
      .link <- xml_add_sibling(.link, "link")
    }

    xml_attr(.link, attr = "id") <- links[['ID']][i]
    xml_attr(.link, attr = "from") <- links[['fromID']][i]
    xml_attr(.link, attr = "to") <- links[['toID']][i]
    xml_attr(.link, attr = "length") <- links[['length']][i]
    xml_attr(.link, attr = "freespeed") <- links[['freespeed']][i]
    xml_attr(.link, attr = "capacity") <- links[['capacity']][i]
    xml_attr(.link, attr = "permlanes") <- links[['lanes']][i]
    xml_attr(.link, attr = "oneway") <- links[['oneway']][i]
    xml_attr(.link, attr = "modes") <- links[['modes']][i]
  }

  # write to disk
  tmpfile <- paste0(tempdir(), "/matsim-network.xml")
  write_xml(x = root, file = tmpfile)

  if (!is.null(matsim_jar) && matsim_check_network == TRUE) {
    cleaned_network_tmpfile <- paste0(tempdir(), "/cleaned-matsim-network.xml.gz")
    system(glue::glue("java -cp '{matsim_jar}' org.matsim.run.NetworkCleaner {tmpfile} cleaned_network_tmpfile"))
    return(read_xml(cleaned_network_tmpfile, options = c("DTDLOAD", "DTDVALID")))
  }

  # load the xml to validate againts its DTD and return
  return(read_xml(tmpfile, options = c("DTDLOAD", "DTDVALID")))
}