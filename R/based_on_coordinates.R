#' Crop the bbox
#'
#' Crop the bbox as retrieved by `osmdata::getbb` so
#'   you can zoom in.
#' @param bbox A 2x2 matrix, returned by `osmdata::getbb`.
#' @param left,right,top,bottom The proportion to crop from
#'   with respect to this side of the bbox. Cannot be
#'   smaller than 0 or exceed 1.
#' @return A 2x2 matrix that is cropped.
#' @examples
#' make_sexy_map("Leiden", "Leiden", monochrome = TRUE)
#' # not so nice, we just want the old city center
#' # not all the outskirts
#' leiden_bbox <- osmdata::getbb("Leiden")
#' leiden_cropped <- crop_bbox(leiden_bbox, .45, .15, .25, .5)
#' make_sexy_map(leiden_cropped, "Leiden", monochrome = TRUE)
#' @export
crop_bbox <- function(bbox,
                      left   = 0,
                      right  = 0,
                      top    = 0,
                      bottom = 0) {
  stopifnot(left >= 0, left < 1, right >= 0, right < 1,
            top >= 0, top < 1, bottom >= 0, bottom < 1,
            left + right < 1, top + bottom < 1)
  bbox <- as.numeric(bbox)
  lat  <- bbox[c(1, 3)]
  long <- bbox[c(2, 4)]
  lat_length  <- lat[2] - lat[1]
  long_length <- long[2] - long[1]
  create_bbox(c(lat[1] + left * lat_length, lat[2] - right * lat_length),
              c(long[1] + bottom * long_length, long[2] - top * long_length))
}

#' Create a bbox
#'
#' Crop the bbox as retrieved by `osmdata::getbb` so
#'   you can zoom in.
#' @param lat A vector of length 2 of the latitudes
#' @param long A vector of length 2 of the longitudes
#' @return A 2x2 matrix that can be used for `make_sexy_map`
#' @examples
#'create_bbox(c(4.438865, 4.524072),
#'            c(52.118952, 52.184626))
#' @export
create_bbox <- function(lat,
                        long) {
  bbox <- matrix(c(sort(lat), sort(long)),
                 nrow = 2,
                 byrow = TRUE)
  colnames(bbox) <- c("min", "max")
  rownames(bbox) <- c("x", "y")
  bbox
}
