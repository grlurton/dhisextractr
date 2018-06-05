#'Extracting GPS coordinates
#'
#' \code{format_GPS} extracts GPS coordinates for extracted organization units
#'
#' @param org_units_description A data frame of organization descriptions as extracted .
#' @return Returns a list of data frames in which the columns are the organisation unit ID,
#' the longitude and the latitude of the organisation units. Each element of the list is an
#' organization unit.
format_GPS <- function(org_units_description){
  org_units_description$coordinates <- gsub('\\[|\\]', '' ,
                                            org_units_description$coordinates)
  splitcoord <- unlist(strsplit(org_units_description$coordinates ,
                                "," , fixed = TRUE))
  coords <- length(splitcoord)
  org_units_coords <- data.frame(long = character() ,
                                 lat = character()
  )
  if(coords == 2){
    org_units_coords <- data.frame(long = splitcoord[1] ,
                                   lat = splitcoord[2])
  }
  if(coords > 2){
    org_units_coords <- data.frame(long = splitcoord[2*(1:(length(splitcoord)/2))-1] ,
                                   lat = splitcoord[2*(1:(length(splitcoord)/2))]
    )
  }
  org_units_coords
}

#'Make shapefiles for organization units
#'
#' \code{make_shapefiles} makes shapefiles from a list of organization units coordinates.
#'
#' @param formatted_coordinates A list of organization units and their GPS coordinates.
#' \link{format_GPS}.
#' @return Returns a list of two elements. The first element of the list is a shapefile of
#' point organization units. The second element is a shapefile of polygon organization
#' units.
make_shapefiles <- function(formatted_coordinates){
  points_shapefile <- poly_shapefile<-  data.frame(id = character() ,
                                                   long = character() ,
                                                   lat = character()
  )

  for(id in unique(formatted_coordinates$id)){
    if(sum(formatted_coordinates$id == id) == 1){
      points_shapefile <- rbind(points_shapefile , formatted_coordinates[formatted_coordinates$id == id,])
    }
    if(sum(formatted_coordinates$id == id) > 1){
      poly_shapefile <- rbind(poly_shapefile , formatted_coordinates[formatted_coordinates$id == id,])
    }
  }

  if(nrow(points_shapefile) > 1){
    print('making the points shapefile')
    points_shapefile$lat <- as.numeric(as.character(points_shapefile$lat))
    points_shapefile$long <- as.numeric(as.character(points_shapefile$long))
    ShapeData <- data.frame(org_unit_ID = unique(points_shapefile$id) )
    points_shapefile <- shapefiles::convert.to.shapefile(points_shapefile, ShapeData, "org_unit_ID", 1)
  }

  if(nrow(poly_shapefile) > 1){
  print('making the polygons shapefile')
  poly_shapefile$lat <- as.numeric(as.character(poly_shapefile$lat))
  poly_shapefile$long <- as.numeric(as.character(poly_shapefile$long))
  ShapeData <- data.frame(org_unit_ID = unique(poly_shapefile$id) )
  poly_shapefile <- shapefiles::convert.to.shapefile(poly_shapefile, ShapeData, "org_unit_ID", 5)
  }

  list(points_shapefile , poly_shapefile)
}

#'Make shapefiles for organization units
#'
#' \code{extract_geolocalisation} makes shapefiles from extracted organization units
#'
#' @param org_units_description A data frame of organization descriptions as extracted.
#' @return Returns a list of two elements. The first element of the list is a shapefile of
#' point organization units. The second element is a shapefile of polygon organization
#' units.
extract_geolocalisation <- function(metadata){
  formatted_gps <- metadata$organisationUnits %>% dplyr::group_by(id) %>% dplyr::do(format_GPS(.))
  out <- make_shapefiles(data.frame(formatted_gps))
  out
}



#'Writing shapefiles for organization units
#'
#' \code{write_geolocalisation} makes and writes shapefiles from a list of organization units coordinates.
#'
#' @param shapefiles A list of organization units and their GPS coordinates extracted from \link{extract_geolocalisation}.
write_geolocalisation <- function(shapefiles){
  shapefiles::write.shapefile(shapefiles[[1]], 'map_points', arcgis=T)
  shapefiles::write.shapefile(shapefiles[[2]], 'map_polygons', arcgis=T)
  point_read <- rgdal::readOGR('map_points.shp')
  poly_read <- rgdal::readOGR('map_polygons.shp')
  rgdal::writeOGR(poly_read, 'map_points.shp', driver="ESRI Shapefile", layer = 'points')
  rgdal::writeOGR(poly_read, 'map_points.geoJson', driver="GeoJSON", layer = 'points')
  rgdal::writeOGR(poly_read, 'map_polygons.shp', driver="ESRI Shapefile", layer = 'polygons')
  rgdal::writeOGR(poly_read, 'map_polygons.geoJson', driver="GeoJSON", layer = 'polygons')
}
