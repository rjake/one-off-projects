library(sf)
library(tidyverse)

#saveRDS(color_clusters, "color_clusters.Rds")

color_clusters <-
  readRDS("color_clusters.Rds") |>
  filter(hex != "#FFFFFF")

# THE LOOP ----------------------------------------------
# a place to catch the results
color_circle_all <-
  color_clusters |>
  slice(0)

area_allowed <- 80#floor(min(color_clusters$area))
buffer_min <- sqrt(area_allowed) / pi
get_iteration <- 1

system.time(
  for (cluster in 1:nrow(color_clusters)) {
    color_polygon <-
      color_clusters |>
      slice(cluster) |>
      filter(area > area_allowed)

    # plot(color_clusters$geometry, col = NA, add = F)
    # plot(color_circle_all$geometry, col = "red", add = T)
    # plot(color_polygon$geometry, col = color_polygon$hex, add = F)

    cluster_part <- 1
    area_left <- TRUE
    rows_left <- nrow(color_polygon)

    while (area_left || rows_left == 1) {
      if (!area_left && rows_left > 1) {
        # drop current row and go to next row
        color_polygon <-
          color_polygon |>
          slice(-1)

        # try next row
        next() # repeat current if logic
      }
      else {
        # update variable values
        get_area <- color_polygon$area[1]
        rows_left <- nrow(color_polygon)
        area_left <- get_area > area_allowed

        # take the first row from the data
        color_polygon_single <-
          color_polygon |>
          slice(1)

        # check to see if the buffer leaves enough space to plot
        color_buffer <-
          color_polygon_single |>
          st_buffer(-buffer_min) %>%
          mutate(area = st_area(.))

        # plot(color_buffer$geometry, add = T)

        color_polygon <-
          color_polygon |>
          slice(-1)

        if (nrow(color_buffer) == 0 || color_buffer$area == 0) {
          print("too small")
          area_left <- FALSE
          next()
        } else { # THE BULK OF THE MADNESS -------------------------------------
          # from calc, there is enough space to draw a circle
          # polygon needs to be a string for distance calc to work
          color_line <-
            color_polygon_single |>
            st_cast("LINESTRING")
          # plot(color_line$geometry, add = F)

          # find centroid of buffered region
          color_centroid <-
            color_buffer |>
            st_point_on_surface()

          # plot(color_centroid$geometry, add = T)

          # get the distance of this point
          color_dist <-
            st_distance(color_centroid[1, 1], color_line)

          # create a circle of this size (color_dist)
          color_circle <-
            color_centroid |>
            st_buffer(color_dist + .01)

          # plot(color_circle$geometry, add = T)

          # build dataset of circles
          color_circle_all <-
            rbind(color_circle, color_circle_all) |>
            st_cast("MULTIPOLYGON")

          color_new <-
            color_polygon_single |>
            st_difference(st_union(st_buffer(color_circle, 0.8))) |>
            st_cast("POLYGON") %>%
            mutate(area = st_area(.))

          # plot(color_new$geometry, add = F)
          if (nrow(color_new) == 0) {
            print("nothing left")
            area_left <- FALSE
          } else {
            # add new polygon shape/shapes to the color_polygon table
            color_polygon <-
              rbind(color_new, color_polygon)  |>
              arrange(desc(area)) |>
              filter(area > area_allowed) |>
              st_difference(st_union(color_circle))


            # conditions met
            cat(
              "cluster (row):", cluster,
              " ///  cluster_part:", cluster_part,
              " ///  iteration:", get_iteration,
              " ///  remaining:", nrow(color_polygon),
              "pieces left\n"
            )
          }
        }
        cluster_part <- cluster_part + 1
        get_iteration <- get_iteration + 1
        next()
      }
    }
  }
)


plot(
  color_circle_all$geometry,
  col = color_circle_all$hex,
  border = NA,
  add = F
)
