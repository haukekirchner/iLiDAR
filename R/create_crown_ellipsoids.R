#' @import data.table
create_crown_ellipsoids <- function(las){
  # Use data.table grouping to calculate ellipse data for each crown ID
  ellipsoid_hull_data <- las_dbh@data[,create_ellipsoid_data(X, Y), by = treeID]

  # Create ellipse polygons (or convex ones if the fitting was unsuccessful) with
  # the correct CRS
  ellipsoid_polygons <- map2(.x = ellipsoid_hull_data$geometry,
                             .y = ellipsoid_hull_data$is_invalid_ellipse_probably,
                             ~ if (.y) {
                               .x %>%
                                 sf::st_multipoint() %>%
                                 sf::st_convex_hull(.)
                             } else {
                               .x %>%
                                 list() %>%
                                 sf::st_polygon()
                             }) %>%
    sf::st_sfc(crs = lidR::projection(las))

  # Calculate the area and mean diameter of the polygons
  ellipsoids <- sf::st_set_geometry(ellipsoid_hull_data %>% select(-geometry),
                                    ellipsoid_polygons) %>%
    mutate(
      area_ellipsoid = sf::st_area(geometry),
      diameter_ellipsoid_mean = sf::st_cast(geometry, to = "LINESTRING") %>%
        sf::st_length() / pi)

  return(ellipsoids)
}

create_ellipsoid_data <- function(X, Y) {

  # Fit an ellipse to the X and Y coordinates
  ellipsoid_fit <- cluster::ellipsoidhull(cbind(X, Y))

  # Set up a preliminary result list
  res_list <- list(
    ellipsoid_fit_converged = ellipsoid_fit[["conv"]],
    ellipsoid_fit_num_iterations = ellipsoid_fit[["it"]]
  )

  # If the fitting was (probably) not successful:
  if (!ellipsoid_fit[["conv"]] || ellipsoid_fit[["it"]] == 0) {

    # Add the corresponding values to the result list
    res_list <- purrr::list_merge(res_list,
                                  is_invalid_ellipse_probably = TRUE,
                                  diameter_ellipsoid_min_manual = NA_real_,
                                  diameter_ellipsoid_max_manual = NA_real_,
                                  diameter_ellipsoid_mean_manual = NA_real_,
                                  geometry = list(cbind(X, Y))
    )

  } else {# if the fitting was successful:

    # Predict points on the ellipse border for radius estimation and polygon
    # creation
    ellipsoid_border <- cluster::predict.ellipsoid(ellipsoid_fit, n.out = 40)

    # Calculate distances between the ellipse center and its border
    distances <- apply(
      ellipsoid_border[seq_len(nrow(ellipsoid_border) / 2), ],
      MARGIN = 1,
      FUN = function(xy_coords) {
        dist(rbind(xy_coords, ellipsoid_fit[["loc"]]))
      }
    )

    # Calculate a mean radius from the distances and store the border points in
    # the result list for polygon creation
    #
    # Why mean(range(distances)) for the mean radius instead of mean(distances)?
    # -> Because the border points are not equally spaced. See
    #     plot(ellipsoid_hull_data[treeID == 1, geometry][[1]])
    # after processing the data for a visual demonstration of the issue.
    distances_min_max <- range(distances)

    res_list <- purrr::list_merge(res_list,
                                  is_invalid_ellipse_probably = FALSE,
                                  diameter_ellipsoid_min_manual = min(distances_min_max) * 2,
                                  diameter_ellipsoid_max_manual = max(distances_min_max) * 2,
                                  diameter_ellipsoid_mean_manual = mean(distances_min_max) * 2,
                                  geometry = list(ellipsoid_border)
    )
  }

  return(res_list)
}
