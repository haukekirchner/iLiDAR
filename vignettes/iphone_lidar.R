library(lidR)
library(tidyverse)

las <- lidR::readLAS("~/Desktop/20201228_spielplatz_hackescher_markt_small.las")

# fix wrong orientation
z <- las@data$Z

las@data$Z <- las@data$Y
las@data$Y <- z

lidR::plot(las, color = "RGB", axis=T)
lidR::plot(las)

# point density
las_d <- grid_density(las, res = 1)
plot(las_d)

# classify ground
las <- classify_ground(las, algorithm = csf(sloop_smooth = T))

las <- lidR::normalize_height(las       = las,
                              algorithm = lidR::tin(),
                              use_class = c(2))

plot(las, color = "Classification")

# tree segmentation
las <- lidR::filter_poi(las, Classification != 2)

# point density above 3m is very sparse
# points above 3m are assumed to be outliers
las2 <- filter_poi(las, Z < 3)

las2 <- segment_trees(las2, li2012(hmin =1.3), attribute = "treeID")

plot(las2, color = "treeID", axis = T,
     colorPalette = lidR::random.colors(data.table::uniqueN(las@data$treeID)))

# calculate tree hulls

hulls <- lidR::delineate_crowns(las,
                       type = "convex",
                       attribute = "treeID")

plot(hulls)

las2 <- filter_poi(las, Z > 1.3)
las2 <- filter_poi(las2, Z < 1.31)

ttops <- find_trees(las2, lmf(ws = 1))
plot(ttops)
plot(hulls, add =T)

chm <- grid_canopy(las, 0.1, p2r())


algo <- dalponte2016(chm, ttops)
las <- segment_trees(las, algo) # segment point cloud
