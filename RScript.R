# Please change the paths

setwd("D:/DATEN ZWEI/Wue/WS22_23/IntroductionToProgramming/Assignment/FloodPopulation/")

library(sf)
library(sp)
library(mapedit)
library(raster)
library(mapview)
library(caret)
library(RStoolbox)
library(devtools)
library(stars)
library(rayshader)
library(rayrender)
library(MetBrewer)
library(colorspace)
library(magick)

# data import and pre-processing
studyarea <-  raster::shapefile("D:/DATEN ZWEI/Wue/WS22_23/IntroductionToProgramming/Assignment/FloodPopulation/Data/StudyArea.shp")

sat_large <- brick("D:/DATEN ZWEI/Wue/WS22_23/IntroductionToProgramming/Assignment/FloodPopulation/Data/LC08_L2SP_090083_20210323_20210402_02_T1.tif")
bands <- c("SR_B1", "SR_B2", "SR_B3", "SR_B4", "SR_B5", "SR_B6", "SR_B7")
names(sat_large) <- bands


# crop raster to study area
sat <- crop(sat_large, studyarea)




## CLASSIFY & EXTRACT WATER/FLOODED AREA USING INDICES
## The NDWI and the modified NDWI is a good and helpful tool to classify and
## extract water in satellite images and a (faster) alternative to the normal classification


## NDWI - Normalized Difference Water Index

# Band Green (B3)
green <- sat[[3]]

# Band NIR (B5)
NIR <- sat[[5]]

# The layout( ) function has the form layout(mat) where mat is a matrix object specifying the location of the figures
nf <- layout(matrix(c(1,2), 1,2, byrow = TRUE))

# Indicate the 4 images that will be plotted in the  1X2 matrix
plot(green, main="Green Band")
plot(NIR, main="NIR Band")

## NDWI (very good for detecting water)
ndwi = (green-NIR)/(green+NIR)

# define a threshold to mask out the values smaller than 0 -> are non-water (0,2 – 1 –> Water surface,
# 0.0 – 0,2 – Flooding, humidity) -> that's the extracted water area
ndwi_water <- calc(ndwi, function(x){x[x < 0] <- NA;return(x)})

# plot results
# With 'par()' set in how many columns or rows the images should be arranged
par(mfrow=c(1,3))

plot(ndwi, main="NDWI")
plot(ndwi_water, main="NDWI - Water pixels")

cuts=c(-1, 0, 1)
pal= colorRampPalette(c("black", "white"))
plot(ndwi, breaks=cuts, col=pal(2), main= "NDWI - Water mask")




## COMPARISON WITH MNDWI - Modified Normalized Difference Water Index

# Band 6. SWIR enhances open water features and can better differentiate between open
# water features and built area features (which are often correlated/confused)
SWIR <- sat[[6]]

# calculate MNDWI
mndwi = (green-SWIR)/(green+SWIR)

# define a threshold to mask out the values smaller than 0 -> are non-water
mndwi_water <- calc(mndwi, function(x){x[x < 0] <- NA;return(x)})

# plot results
par(mfrow=c(1,3))

plot(mndwi, main="MNDWI")
plot(mndwi_water, main="MNDWI - Water pixels")

cuts=c(-1, 0, 1)
pal= colorRampPalette(c("black", "white"))
plot(mndwi, breaks=cuts, col=pal(2), main= "MNDWI - Water mask")


# plot the water areas derived from both indices next to each other with the original satellite image in between
# to compare them visually and choose which one classified the water better
# or on top of each other

par(mfrow=c(1,2))

# Plot satellite image first in a band combination suitable for water
plotRGB(sat, r = 5, g = 6, b = 4)

# Overlay ndwi on top of the sat image
plot(ndwi_water, add = TRUE, legend = FALSE)
title(main = "Sat. image & NDWI Water Area")

plotRGB(sat, r = 5, g = 6, b = 4)

plot(mndwi_water, add = TRUE, legend = FALSE)
title(main = "Sat. image & MNDWI Water Area")

# it can be seen, that the normal ndwi had a little more misclassified pixels/a little too many "holes"
# and the analysis with the mndwi achieved better results

# only display the flood area
par()
plot(mndwi_water, col = "blue", legend = FALSE, axes = FALSE, box = FALSE)



### COMPARISON WITH SUPERVISED CLASSIFICATION RESULT - Which method detects the water area better?
# a typical approach to detect and classify different landcover classes is the supervised classification of a raster
# image, for which training samples are needed. Here I will just run quickly a classification to find out, which approach
# will get the better result, can better detect the flooded area


file_samples <- "TD.gpkg"
if(!file.exists(file_samples)){
  # sampling:
  # water
  water <- drawFeatures(
    map = mapview(mndwi, maxpixels = 1000000)
  )

  # vegetation
  vegetation <- drawFeatures(
    map = viewRGB(sat, r=5, g=6, b=4, maxpixels = 58536901)
  )

  # herbeceous
  herbeceous <- drawFeatures(
    map = viewRGB(sat, r=5, g=6, b=4, maxpixels = 58536901)
  )

  # urban
  urban <- drawFeatures(
    map = viewRGB(sat, r=5, g=6, b=4, maxpixels = 58536901)
  )


  # add landuse attribute
  water$landuse <- "water"
  vegetation$landuse <- "vegetation"
  herbeceous$landuse <- "herbeceous"
  urban$landuse <- "urban"


  # codify landuse as integers using mapvalues
  labeled_poly <- rbind(water, vegetation, herbeceous, urban)
  labeled_poly$classid <- as.numeric(
    plyr::mapvalues(labeled_poly$landuse,
                    from = unique(labeled_poly$landuse),
                    to = 1:length(unique(labeled_poly$landuse)))
  )

  # save
  st_write(labeled_poly, "D:/DATEN ZWEI/Wue/WS22_23/IntroductionToProgramming/Assignment/FloodPopulation/TD.gpkg")
}else{
  labeled_poly <- st_read(file_samples)
}

# load polygons
labeled_poly <- st_transform(labeled_poly, st_crs(sat))

# what do you want to be your response variable? numeric!
labeled_poly$classid
labeled_poly$resp_var <- labeled_poly$classid

# to get labeled features, we need points to extract features for
# let's randomly select some points in our polygons and save there labels to them
labeled_points <- list()
for(i in unique(labeled_poly$resp_var)){
  message(paste0("Sampling points from polygons with resp_var=", i))

  # sample points for polygons of resp_var = i
  labeled_points[[i]] <- st_sample(
    x = labeled_poly[labeled_poly$resp_var == i,],
    size = 100
  )
  labeled_points[[i]] <- st_as_sf(labeled_points[[i]])
  labeled_points[[i]]$resp_var <- i
}
labeled_points <- do.call(rbind, labeled_points)

# now that we have points we need to extract features
# we can use our RS data for this since we do not have any ground-truth from site
sat_classi <- normImage(sat)
sat_classi <- rescaleImage(sat_classi, ymin = 0, ymax = 1)

# extract features and label them with our response variable!
unlabeled_features <- raster::extract(sat, labeled_points, df = T)
unlabeled_features <- unlabeled_features[,-1] # no ID column needed
labeled_features <- cbind(
  resp_var = labeled_points$resp_var,
  unlabeled_features
)

# remove duplicates (in case multiple points fall into the same pixel)
dupl <- duplicated(labeled_features)
which(dupl)
length(which(dupl)) # number of duplicates in labeled features that we need to remove!
labeled_features <- labeled_features[!dupl,]

# x = features
x <- labeled_features[,2:ncol(labeled_features)] # remove ID column
y <- as.factor(labeled_features$resp_var) #we want caret to treat this as categories, thus factor
levels(y) <- paste0("class_", levels(y))


# fit the ranodm forest model
model <- train(
  x = x,
  y = y,
  trControl = trainControl(
    p = 0.75, # percentage of samples used for training
    method  = "cv", # cross validation
    number  = 5, # 5-fold
    verboseIter = TRUE,
    classProbs = TRUE
  ),
  method = "rf" # used algorithm (random forest)
)

# performance
model
confusionMatrix(model)

# predict
sat_class <- predict(sat, model, type='raw')

# write
writeRaster(sat_class, filename = "flood_landcover.tif",
            datatype = "INT1U", overwrite = T)

cols <- c("blue", "darkgreen", "sandybrown", "red")

# show on map (it can be seen, that it had troubles distinguishing between the water and vegetation and could also
# not differentiating well between urban and the other vegetational classes. But we only focus on the water area
# so the classification of the other classes is not relevant. But it can be used to see how well the classification worked generally)
mapview(sat_class, col.regions = cols)

# Extract the water class
water_class <- sat_class == 1 # water class is labeled as 1

# Create a new raster for water class
water_raster <- sat_class
water_raster[!water_class] <- NA

# Plot the water class
plot(water_raster)


# display both extracted flood areas together for comparison

par(mfrow=c(1,2))

# Plot satellite image
plotRGB(sat, r = 5, g = 6, b = 4)

# Overlay mndwi on top of the sat image
plot(mndwi_water, add = TRUE, col = "cornflowerblue", legend = FALSE, axes = FALSE, box = FALSE)
title(main = "Sat. image & MNDWI")

plotRGB(sat, r = 5, g = 6, b = 4)

plot(water_raster, add = TRUE, col = "indianred1", legend = FALSE, axes = FALSE, box = FALSE)
title(main = "Sat. image & Classification")

# it can be seen, that the the (M-)NDWI approach classified and masked the water pixels much better than the
# classification.
# So for the next step, we will use the flood area derived from the mndwi approach





## POPULATION DENSITY -> IN FLOODED AREA -> EXPOSED POPULATION
## PERCENTAGE OF PEOPLE AFFECTED BY THE FLOOD IN THE STUDYAREA


### ## unfortunately, my computer couldn't handle the conversion of the raster population data to a vector file
### ## so I used the already vectorized data. But that would be the "pre-processing" steps:
### # import raster
### population_raster <- raster("path/to/raster.tif")
### # reproject raster to wanted crs (from sat)
### population_raster_proj <- projectRaster(population_raster, ndwi_polygons_merged, method = "bilinear")
### # convert raster to vector polygons
### population_poly <- rasterToPolygons(population_raster_proj, dissolve = T)
### # the crs is lost during that process, so set it again
### crs(population_poly) = crs(ndwi_polygons_merged)
### # to sf oject to clip
### population_sf <- st_as_sf(population_poly)
### # clip
### population_clipped <- st_intersection(population_poly, mndwi_polygons_merged)


# convert study area to sf
studyarea_sf <- st_as_sf(studyarea)



# pre-process mndwi to only have flood area
# Convert NDWI raster to polygons
mndwi_polygons <- rasterToPolygons(mndwi_water, dissolve = TRUE)

# convert to sf object
mndwi_polygons_sf <- st_as_sf(mndwi_polygons)

# Merge the polygons into a single polygon
mndwi_polygons_merged <- st_union(mndwi_polygons_sf)




# import population data
pop_large <- st_read("D:/DATEN ZWEI/Wue/WS22_23/IntroductionToProgramming/Assignment/FloodPopulation/Data/Population.gpkg")


# reproject population data

# Check the current CRS of the population GeoPackage file
st_crs(pop_large)

# Reproject the population GeoPackage file to match the CRS of the shapefile
pop_large_reproj <- st_transform(pop_large, st_crs(mndwi_polygons_merged))

# Check the new CRS of the population GeoPackage file
st_crs(pop_large_reproj)


# Population in study area
# Clip the population to the study area
pop_stud <- st_intersection(pop_large_reproj, studyarea_sf)
plot(pop_stud)


# Population in flood area

# Clip the population to the flood area
pop_flood <- st_intersection(pop_large_reproj, mndwi_polygons_merged)

plot(pop_flood)



# Calculate the percentage of affected people with by the flood in the study area
# the unit of the population is the number of people per square kilometer

# calculate the total number of people affected by the flood
total_affected <- sum(pop_flood$population)
# around 20 thousand people are affected by the flood

# calculate the total population in the study area
total_population <- sum(pop_stud$population)

# calculate the percentage of affected people in the study area
percentage_affected <- (total_affected / total_population) * 100
# there are around 75% percent of the people in the study area affected by the flood



# Visualisation of this affected population with the density

# will be using rayshader so converting to matrix
# how many columns and rows for matrix
# define aspect ratio based on bounding box (returns the same area as the study area)
# (calculate ratio between width and height)
bbox <- st_bbox(pop_flood)

# convert numbers of bbox to spatial point coordinates
bottom_left <- st_point(c(bbox[["xmin"]], bbox[["ymin"]])) %>%
  st_sfc(crs = st_crs(pop_flood))

bottom_right <- st_point(c(bbox[["xmax"]], bbox[["ymin"]])) %>%
  st_sfc(crs = st_crs(pop_flood))

top_left <- st_point(c(bbox[["xmin"]], bbox[["ymax"]])) %>%
  st_sfc(crs = st_crs(pop_flood))

width <- st_distance(bottom_left, bottom_right)

height <- st_distance(bottom_left, top_left)

# different conditions, which side is longer
if (width > height) {
  width_ratio <- 1
  height_ratio <- height / width
  # other condition: when height is bigger or equal
} else {
  height_ratio <- 1
  width_ratio <- width / height # when equal, this would be 1
}

# define matrix. At the moment the pop data is in a spatial format. So first to convert it to a raster
# (using stars) and then to a matrix

# rasterize
# width and height is important (number of cells in x and y direction) in integer numbers (=floor)
base_size <- 1000 # size of the base -> so the longer side will be the hole amount of base_size
pop_raster <- st_rasterize(pop_flood,
                           nx = floor(base_size * width_ratio),
                           ny = floor(base_size * height_ratio))

# convert it to matrix with same size as raster and only use the population
pop_mat <- matrix(pop_raster$population,
                  nrow = floor(base_size * width_ratio),
                  ncol = floor(base_size * height_ratio))


# create and plot 3d object
dev.off()

# check the range of the population (ranges from 1 person to 2421 people)
range(pop_mat, na.rm = TRUE)

# create colour palette
colpal <- met.brewer("OKeeffe2")
swatchplot(colpal)

# with bias we can switch the colour palette
textu <- grDevices::colorRampPalette(colpal, bias = 2)(256)
swatchplot(textu)

# plot
pop_mat %>%
  height_shade(texture = textu) %>%
  plot_3d(heightmap = pop_mat,
          zscale = 80, # zscale for height exageration -> the higher the number, the less exagerated
          solid = FALSE, # remove base under the hexagons
          shadowdepth = 0 # to remove the gap between the map and the shadow
  )


# set viewing parameters (theta is rotation angle, phi is azimuth angle)
render_camera(theta = -20, phi = 20, zoom = 0.8)


# visualisation and save as png (set parameters for light, where it comes from, how many sources, how strong)
render_highquality(
 filename = "pop_image.png",
 interactive = FALSE,
 lightdirection = 270,
 lightaltitude = c(20, 80),
 lightcolor = c(colpal[2], "white"),
 width = 800, height = 800,
 lightintensity = c(600, 100),
 )

# import png to work with magick
pic <- image_read("pop_image.png")

# first crop, to "center" it
# in annotate we set the text parameters, like size, colour (with alpha make it transparent), bold (weight)
# and position (gravity) (for each text field one)
pic %>%
  image_crop(gravity = "center",
             geometry = "800x500") %>%

  image_annotate("Population Density in the Flooded Area",
                 gravity = "north",
                 location = "+0+35",
                 size = 30,
                 color = "firebrick4",
                 font = "TimesNewRoman",
                 weight = 500,
                 ) %>%

  image_annotate("The distribution of the population affected by the flood (2021) in NSW, Australia",
                 gravity = "southwest",
                 location = "+20+50",
                 size = 15,
                 color = alpha("firebrick4", 0.75),
                 font = "TimesNewRoman",
                 ) %>%

  image_write("image_git.png")

## just as a comment: the visualisation of the population distribution would look much nicer, if the area was larger
## but I chose this small area to decrease the computing time