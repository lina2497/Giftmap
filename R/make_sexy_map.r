
#' Download highways data from the Open Streetmaps API for a specified location and plot a map.
#'
#' @return Returns a aesthitically pleasing plot .
#' @param location A character string describing the target, gets passed to `osmdata::getbb`.
#'  Or a matrix of latitudes and longitudes, created by `create_bbox` or `crop_bbox`,
#'  that can be passed to `osmdata::opq`.
#' @param short_name A character string to be used as a caption.
#' @param monochrome Logical, if TRUE output is black and white.
#' @export
#' @import dplyr ggplot2 osmdata viridis purrr sf
#' @examples
#' make_sexy_map(location = "Tuckenhay in England", short_name = "Tuckenhay", monochrome = TRUE)
#' make_sexy_map(location = create_bbox(c(4.438865, 4.524072),
#'                                      c(52.118952, 52.184626)),
#'               short_name = "Leiden", monochrome = TRUE)
#' leiden_bbox <- osmdata::getbb("Leiden")
#' leiden_cropped <- crop_bbox(leiden_bbox, .45, .15, .25, .5)
#' make_sexy_map(location = leiden_cropped,
#'               short_name = "Leiden", monochrome = TRUE)
make_sexy_map <- function(location,
                          short_name,
                          monochrome = FALSE) {
  print("Downloading Data")
  if (is.character(location)) {
    bb <- getbb(location)
  } else {
    bb <- location
  }

  highway_data<-bb%>%
    opq()%>%
    add_osm_feature(key = "highway") %>%
    osmdata_sf()

  print("Data Downloaded")

  print("Extracting Lines")

  osm_lines_list<-highway_data$osm_lines%>%
    split(.$highway)

  print("Extracting Polygons")
  polygons_list<-highway_data$osm_polygons%>%
    split(.$highway)





  plot_osm_lines<-function(osm_line_data,color="#7fc0ff", size=0.4, alpha=0.8){
    geom_sf(data = osm_line_data ,
            inherit.aes = FALSE,
            color = color,
            size = size,
            alpha = alpha)
  }


  print("Creating Style Table")
  style_table<-tibble(tags=names(osm_lines_list),
                      pallete=viridis::viridis(length(osm_lines_list)),
                      size=0.4,
                      alpha=0.8,
                      number_of_lines= map_int(osm_lines_list, nrow))




  style_table$size[style_table$tags%in%c("primary","secondary","tertiary","trunk")]<-1.2
  style_table$pallete[style_table$tags%in%c("primary","secondary","tertiary","trunk")]<-"lightblue"
  style_table$alpha[style_table$tags%in%c("primary","secondary","tertiary","trunk")]<-0.8
  style_table$pallete[!(style_table$tags%in%c("primary","secondary","tertiary","trunk"))]<-"#ffbe7f"
  style_table$size[!(style_table$tags%in%c("primary","secondary","tertiary","trunk"))]<-0.6
  style_table$alpha[!(style_table$tags%in%c("primary","secondary","tertiary","trunk"))]<-0.3
  style_table$size[style_table$tags%in%c("track","path","bridleway","footway","cycleway")]<-0.3
  style_table$pallete[style_table$tags%in%c("track","path","bridleway","footway","cycleway")]<-"gray48"
  style_table$alpha[style_table$tags%in%c("track","path","bridleway","footway","cycleway")]<-0.05





  polygon_style_table<-filter(style_table, tags %in% names(polygons_list))

  if(monochrome){
    if(nrow(style_table)>0){
      style_table$pallete<-"black"
    }
    if(nrow(polygon_style_table)>0){
      polygon_style_table$pallete<-"black"
    }
  }

  polygons_list<-polygons_list[names(polygons_list)[names(polygons_list) %in% names(osm_lines_list)]]

  print("Generating plot")
  p<-ggplot() +
    pmap(
      list(
        osm_line_data = osm_lines_list,
        color = style_table$pallete,
        size = style_table$size,
        alpha = style_table$alpha
      ),
      plot_osm_lines)

  q<-p+pmap(
    list(
      osm_line_data = polygons_list,
      color = polygon_style_table$pallete,
      size = polygon_style_table$size,
      alpha = polygon_style_table$alpha
    ),
    plot_osm_lines)

  trim_x<-0.001
  trim_y <- 0.001


  r<-q+
    coord_sf(
      xlim = c(bb[1, 1] + trim_x, bb[1, 2] - trim_x),
      ylim = c(bb[2, 1] + trim_y, bb[2, 2]) - trim_y,
      expand = FALSE
    )


  s<-r+theme(axis.text = element_blank(), plot.margin=unit(c(1.1,2.4,1.1,2.4),"cm"),
             panel.grid.major = element_line(colour = ifelse(monochrome,"white","#282828")),
             panel.grid.minor = element_line(colour = ifelse(monochrome,"white","#282828")),
             plot.background = element_rect(fill = ifelse(monochrome,"white","#282828")),
             panel.background = element_rect(fill = ifelse(monochrome,"white","#282828")),
             plot.caption = element_text(hjust = 0.5, color =   ifelse(monochrome,"#282828","white"), size = 18),
             panel.border = element_rect(colour =   ifelse(monochrome,"gray48","white"), fill=NA, size=2),
             axis.ticks = element_blank())

  output<-s+labs(caption=paste0(paste(rep("_",nchar(short_name)+4),collapse = ""),"\n",toupper(short_name)))
  print("Rendering Final Output... If you've picked a city this may take a while")
  print(output)

}


