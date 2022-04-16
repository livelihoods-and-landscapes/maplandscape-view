#' Add layer to \code{Leaflet} web map
#'
#' @importFrom magrittr %>%
#'

add_layers_leaflet <-
  function(map_object,
           map_active_df,
           map_var,
           map_colour,
           waiter,
           mask_zero, 
           base_group) {
    supported_geometries <-
      c("POINT",
        "LINESTRING",
        "MULTILINESTRING",
        "POLYGON",
        "MULTIPOLYGON")
    
    if ("sf" %in% class(map_active_df) &
        is.atomic(map_active_df[[map_var]]) & nrow(map_active_df) > 0) {
      waiter$show()
      
      # Catch GeoPackages with non-spatial tables that GeoPandas has added empty
      # GeometryCollection column to.
      if (any(is.na(sf::st_crs(map_active_df)))) {
        waiter$hide()
        return()
      }
      
      # make map active layer epsg 4326
      # make this an if statement
      map_df <- try(map_active_df %>%
                      sf::st_transform(4326))
      
      if ("try-error" %in% class(map_df)) {
        waiter$hide()
        return()
      }
      
      if (mask_zero == TRUE) {
        try(map_df[[map_var]][map_df[[map_var]] == 0] <- NA)
      }
      
      # get geometry type of map active layer
      geometry_type <-
        sf::st_geometry_type(map_df, by_geometry = FALSE)
      
      if (geometry_type %in% supported_geometries) {
        # get bounding box for the map
        bbox <- sf::st_bbox(map_df) %>%
          as.vector()
        
        # make colour palette
        if (class(map_df[[map_var]]) != "numeric" &
            class(map_df[[map_var]]) != "integer") {
          pal <- leaflet::colorFactor(map_colour, map_df[[map_var]])
        } else {
          pal <- leaflet::colorNumeric(map_colour, map_df[[map_var]])
        }
        
        # draw polygon layers
        if (geometry_type == "POLYGON" | geometry_type == "MULTIPOLYGON") {
          
          if (geometry_type == "MULTIPOLYGON") {
            # cast MULTIPOLYGON to POLYGON as leafgl does not support multi*
            map_df <- sf::st_cast(map_df, "POLYGON")
          }
          
          proxy_map <- leaflet::leafletProxy(map_object, data = map_df) %>%
            leafgl::clearGlLayers() %>%
            leaflet::clearControls() %>%
            leaflet::clearMarkers() %>%
            leaflet::clearShapes() %>%
            leafgl::addGlPolygons(
              data = map_df,
              layerId = map_df$layer_id,
              opacity = 1,
              fillColor = ~ pal(map_df[[map_var]]),
              group = "layer"
            ) %>%
            leaflet::flyToBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%
            leaflet::addLayersControl(
              baseGroups = base_group,
              overlayGroups = "layer",
              options = leaflet::layersControlOptions(collapsed = FALSE),
              position = c("bottomright")
            )
        } else if (geometry_type == "LINESTRING" | geometry_type == "MULTILINESTRING") {
          
          if (geometry_type == "MULTILINESTRING") {
            # cast MULTILINESTRING to LINESTRING as leafgl does not support multi*
            map_df <- sf::st_cast(map_df, "LINESTRING")
          }
          
          proxy_map <- leaflet::leafletProxy(map_object, data = map_df) %>%
            leafgl::clearGlLayers() %>%
            leaflet::clearControls() %>%
            leaflet::clearMarkers() %>%
            leaflet::clearShapes() %>%
            leafgl::addGlPolylines(
              data = map_df,
              layerId = map_df$layer_id,
              opacity = 1,
              color = ~ pal(map_df[[map_var]]),
              group = "layer"
            ) %>%
            leaflet::flyToBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%
            leaflet::addLayersControl(
              baseGroups = base_group,
              overlayGroups = "layer",
              options = leaflet::layersControlOptions(collapsed = FALSE),
              position = c("bottomright")
            )
        } else {
          
          # cast MULTIPOINT to POINT as Leaflet does not support multipoint
          map_df <- sf::st_cast(map_df, "POINT")
          
          proxy_map <- leaflet::leafletProxy(map_object, data = map_df) %>%
            leafgl::clearGlLayers() %>%
            leaflet::clearControls() %>%
            leaflet::clearMarkers() %>%
            leaflet::clearShapes() %>%
            leafgl::addGlPoints(
              data = map_df,
              layerId = map_df$layer_id,
              fillColor = ~ pal(map_df[[map_var]]),
              group = "layer"
            ) %>%
            leaflet::flyToBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%
            leaflet::addLayersControl(
              baseGroups = base_group,
              overlayGroups = "layer",
              options = leaflet::layersControlOptions(collapsed = FALSE),
              position = c("bottomright")
            )
        }
        
        waiter$hide()
        
        proxy_map
      }
    }
    
    
  }
