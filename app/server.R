shinyServer(function(input, output, session) {
  # waiting screen
  waiter <- waiter::Waiter$new(
    html = loading_screen,
    color = "rgba(255,255,254,.5)"
  )

  # waiting screen for loading map
  map_waiter <- waiter::Waiter$new(
    html = map_screen,
    color = transparent(0.25)
  )

  # app data
  data_file <-
    reactiveValues(
      items = NULL, # files in S3 bucket associated with QFieldCloud project
      project_files = NULL, # files in S3 bucket associated with QFieldCloud project
      data_file = data.frame(), # data frame of paths to GeoPackages from S3 (QFieldCloud) and layers
      data_table = data.frame(),
      data_report = data.frame(),
      map_drawn = 0,
      base_groups = NULL,
      report_map = NULL,
      report_chart = NULL,
      report_summary_table = NULL,
      report_raw_table = NULL,
      report_raw_gpkg = NULL,
      report_raw_table_dir = NULL,
      report_raw_gpkg_dir = NULL,
      report_summary_table_dir = NULL,
      token = NULL # use this to keep track of login status
    )


  # login -------------------------------------------------------------------

  # token is TRUE if the user is logged in successfully
  # get QFieldCloud token
  observeEvent(input$login, {
    username <- input$qfieldcloud_username
    password <- input$qfieldcloud_password
    endpoint <- qfieldcloud_url
    
    token <- qfieldcloudR::qfieldcloud_login(
      username,
      password,
      endpoint
    )

    if (token$status == "success") {
      data_file$token <- token$token

      login_message <- paste0("logged in as ", username)

      output$login_status <- renderUI({
        tags$p(login_message)
      })
    } else {
      data_file$token <- NULL
      login_message <-
        paste0("login failed - check username and password.")
      output$login_status <- renderUI({
        tags$p(login_message)
      })
    }
  })


  # Get project files -------------------------------------------------------
  observe({
    req(data_file$token)

    # get project files
    tryCatch(
      error = function(cnd) {
        showNotification("Could not load project files.", type = "error")
      },
      {
        
        # get project files
        data_file$project_files <- qfieldcloudR::get_qfieldcloud_files(
          data_file$token,
          qfieldcloud_url,
          project_id
        )
        
        if (!is.null(data_file$project_files) & nrow(data_file$project_files) > 0) {
          items <- data_file$project_files$name
          
          # keep only GeoPackages - we don't want to view .qgs files
          items <- stringr::str_subset(items, ".gpkg$")
          
          # these are checks specific to working with Tonga Crop Survey data
          # avoid showing some Tonga Crop Survey data files to the user
          items <- stringr::str_subset(items, "^sync", negate = TRUE)
          items <-
            stringr::str_subset(items, "-outline.gpkg$", negate = TRUE)
          items <-
            stringr::str_subset(items, "-boundaries.gpkg$", negate = TRUE)
          data_file$items <- items
        }
        
        
      }
    )

  })

  # update select input on map tab with list of objects in S3 bucket
  observe({
    data_file$items

    if (length(data_file$items) > 0 &
      !"no items returned" %in% data_file$items) {
      updateSelectInput(session,
        "s3_bucket_objects",
        choices = data_file$items
      )
    } else {
      updateSelectInput(session,
        "s3_bucket_objects",
        choices = ""
      )
    }
  })

  # get user selected S3 object as a layer, write GeoPackage retrieved from S3 to
  # data_file$data_file and unpack layers in the GeoPackage
  observeEvent(input$s3_bucket_objects, {
    req(input$s3_bucket_objects)

    waiter$show()

    selected_s3_object <- input$s3_bucket_objects

    tryCatch(
      error = function(cnd) {
        showNotification("Could not load project files.", type = "error")
      },
      {
        s3_gpkg <- qfieldcloudR::get_qfieldcloud_file(
          data_file$token,
          qfieldcloud_url,
          project_id,
          selected_s3_object
        )

        f_lyrs <- purrr::map2(
          s3_gpkg$tmp_file,
          s3_gpkg$filename,
          list_layers
        ) %>%
          dplyr::bind_rows()

        # clear existing layers
        data_file$data_file <- data.frame()

        df <- dplyr::bind_rows(data_file$data_file, f_lyrs)

        # this line is redundant but other functions need to be refactored to
        # remove dependency on layer_disp_name_idx
        df$layer_disp_name_idx <- paste0(df$layer_disp_name)
        data_file$data_file <- df
      }
    )

    waiter$hide()
  })


  # Select layer and render on web map --------------------------------------

  # select one layer as active layer from GeoPackage
  observe({
    df <- data_file$data_file
    choices <- unique(df$layer_disp_name_idx)
    updateSelectInput(session,
      "map_active_layer",
      choices = choices
    )
  })

  # read selected layer into reactive object
  map_active_df <- reactive({
    req(input$map_active_layer)

    df <- data_file$data_file

    # read selected layer from GeoPackage and return if error reading GeoPackage layer
    map_active_df <- try(read_tables(df, input$map_active_layer))

    if ("try-error" %in% class(map_active_df)) {
      # shiny::showNotification("", type = "error")
      return()
    }

    # create feature id to use when mapping user clicks on the map to features in a dataframe
    if (nrow(map_active_df) > 0) {
      map_active_df$layer_id <- as.character(1:nrow(map_active_df))
    }

    if (nrow(map_active_df) > 10000) {
      map_active_df <- map_active_df[1:10000, ]
      id <-
        showNotification(
          "Only drawing first 10000 features!",
          duration = 5,
          type = c("warning")
        )
    }

    # show warning if map active layer has no records
    shinyFeedback::feedbackWarning(
      "map_active_layer", !(nrow(map_active_df) > 0),
      "Not updating map - no records in selected layer"
    )

    # show warning if map active layer is not spatial (class sf)
    shinyFeedback::feedbackWarning(
      "map_active_layer", !("sf" %in% class(map_active_df)),
      "Not updating map - not a spatial layer"
    )

    # don't update options if selected layer has no records
    req(nrow(map_active_df) > 0, "sf" %in% class(map_active_df))

    # remove legend on selecting new layer
    leaflet::leafletProxy("map") %>%
      leaflet::clearControls()

    updateCheckboxInput(session,
      "legend",
      value = FALSE
    )

    # reset map drawn state to 0
    data_file$map_drawn <- 0

    # round numeric values for clearer display
    map_active_df <- map_active_df %>%
      dplyr::mutate(dplyr::across(is.numeric,
        round,
        digits = 4
      ))

    map_active_df
  })

  # update select input to choose column in layer to map to fill colour palette
  observe({
    df <- map_active_df()
    choices <- colnames(df)

    # prevent user from having the option to map geometry columns to fill colour palettes
    choices <- choices[choices != "geom"]
    choices <- choices[choices != "geometry"]
    choices <- choices[choices != "layer_id"]

    updateSelectInput(session,
      "map_var",
      choices = choices
    )
  })

  # select columns to display in map popup
  observe({
    df <- map_active_df()
    choices <- colnames(df)

    # prevent user from displaying geometry columns in popups
    choices <- choices[choices != "geom"]
    choices <- choices[choices != "geometry"]
    choices <- choices[choices != "layer_id"]

    updateSelectInput(session,
      "label_vars",
      choices = choices
    )
  })

  # Create web map
  output$map <- leaflet::renderLeaflet({
    use_custom_basemap <- nchar(custom_xyz)

    if (nchar(use_custom_basemap) > 0) {
      base_map <- leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::addTiles(
          urlTemplate = custom_xyz,
          group = custom_xyz_name,
          options = leaflet::tileOptions()
        ) %>%
        leaflet::addTiles(group = "OSM (default)") %>%
        leaflet::addProviderTiles(
          providers$Esri.WorldImagery,
          options = providerTileOptions(maxZoom = 17),
          group = "ESRI Satellite"
        ) %>%
        leaflet::setView(0, 0, 3) %>%
        leaflet::addLayersControl(
          baseGroups = c("OSM (default)", custom_xyz_name, "ESRI Satellite"),
          options = leaflet::layersControlOptions(collapsed = FALSE),
          position = c("bottomright")
        )

      data_file$base_group <- c("OSM (default)", custom_xyz_name, "ESRI Satellite")
    } else {
      base_map <- leaflet::leaflet() %>%
        leaflet::addTiles(group = "OSM (default)") %>%
        leaflet::addProviderTiles(
          providers$Esri.WorldImagery,
          options = providerTileOptions(maxZoom = 17),
          group = "ESRI Satellite"
        ) %>%
        leaflet::setView(0, 0, 3) %>%
        leaflet::addLayersControl(
          baseGroups = c("OSM (default)", "ESRI Satellite"),
          options = leaflet::layersControlOptions(collapsed = FALSE),
          position = c("bottomright")
        )

      data_file$base_group <- c("OSM (default)", "ESRI Satellite")
    }

    base_map
  })

  # add user selected spatial layers to the map
  observeEvent(input$draw_map, {
    req(map_active_df())
    req(input$map_var)

    if (data_file$map_drawn == 0) {
      tryCatch(
        error = function(cnd) {
          shiny::showNotification("Could not render map.", type = "error")
        },
        {
          add_layers_leaflet(
            map_object = "map",
            map_active_df = map_active_df(),
            map_var = input$map_var,
            map_colour = input$map_colour,
            opacity = 0.8,
            map_line_width = input$map_line_width,
            map_line_colour = input$map_line_colour,
            waiter = map_waiter,
            mask_zero = input$mask_zero,
            base_group = data_file$base_group
          )

          data_file$map_drawn <- 1

          # Catch GeoPackages with non-spatial tables that GeoPandas has added an empty
          # GeometryCollection column to.
          if (any(is.na(sf::st_crs(map_active_df())))) {
            data_file$map_drawn <- 0
          }
        }
      )
    } else if (data_file$map_drawn == 1) {
      tryCatch(
        error = function(cnd) {
          shiny::showNotification("Could not render map.", type = "error")
        },
        {
          add_layers_leaflet_no_zoom(
            map_object = "map",
            map_active_df = map_active_df(),
            map_var = input$map_var,
            map_colour = input$map_colour,
            opacity = 0.8,
            map_line_width = input$map_line_width,
            map_line_colour = input$map_line_colour,
            waiter = map_waiter,
            mask_zero = input$mask_zero,
            base_group = data_file$base_group
          )

          data_file$map_drawn <- 1

          # Catch GeoPackages with non-spatial tables that GeoPandas has added
          # empty GeometryCollection column to.
          if (any(is.na(sf::st_crs(map_active_df())))) {
            data_file$map_drawn <- 0
          }
        }
      )
    }
    # set legend to false on adding new data to the map
    updateCheckboxInput(session,
      "legend",
      value = FALSE
    )
  })

  # update fill colour
  observeEvent(input$map_colour, {
    req(map_active_df())
    req(input$map_var)

    if (data_file$map_drawn == 1) {
      tryCatch(
        error = function(cnd) {
          shiny::showNotification("Could not render map.", type = "error")
        },
        {
          add_layers_leaflet_no_zoom(
            map_object = "map",
            map_active_df = map_active_df(),
            map_var = input$map_var,
            map_colour = input$map_colour,
            opacity = 0.8,
            map_line_width = input$map_line_width,
            map_line_colour = input$map_line_colour,
            waiter = map_waiter,
            mask_zero = input$mask_zero,
            base_group = data_file$base_group
          )
        }
      )
    }
  })

  # update line width
  observeEvent(input$map_line_width, {
    req(map_active_df())
    req(input$map_var)

    if (data_file$map_drawn == 1) {
      tryCatch(
        error = function(cnd) {
          shiny::showNotification("Could not render map.", type = "error")
        },
        {
          add_layers_leaflet_no_zoom(
            map_object = "map",
            map_active_df = map_active_df(),
            map_var = input$map_var,
            map_colour = input$map_colour,
            opacity = 0.8,
            map_line_width = input$map_line_width,
            map_line_colour = input$map_line_colour,
            waiter = map_waiter,
            mask_zero = input$mask_zero,
            base_group = data_file$base_group
          )
        }
      )
    }
  })

  # update line colour
  observeEvent(input$map_line_colour, {
    req(map_active_df())
    req(input$map_var)

    if (data_file$map_drawn == 1) {
      tryCatch(
        error = function(cnd) {
          shiny::showNotification("Could not render map.", type = "error")
        },
        {
          add_layers_leaflet_no_zoom(
            map_object = "map",
            map_active_df = map_active_df(),
            map_var = input$map_var,
            map_colour = input$map_colour,
            opacity = 0.8,
            map_line_width = input$map_line_width,
            map_line_colour = input$map_line_colour,
            waiter = map_waiter,
            mask_zero = input$mask_zero,
            base_group = data_file$base_group
          )
        }
      )
    }
  })

  # update mask zeros - whether to make zero values grey or give zero a fill colour
  observeEvent(input$mask_zero, {
    req(map_active_df())
    req(input$map_var)

    if (data_file$map_drawn == 1) {
      tryCatch(
        error = function(cnd) {
          shiny::showNotification("Could not render map.", type = "error")
        },
        {
          add_layers_leaflet_no_zoom(
            map_object = "map",
            map_active_df = map_active_df(),
            map_var = input$map_var,
            map_colour = input$map_colour,
            opacity = 0.8,
            map_line_width = input$map_line_width,
            map_line_colour = input$map_line_colour,
            waiter = map_waiter,
            mask_zero = input$mask_zero,
            base_group = data_file$base_group
          )
        }
      )
    }

    updateCheckboxInput(session,
      "legend",
      value = FALSE
    )
  })

  # add popup labels
  observeEvent(input$map_shape_click, {
    leaflet::leafletProxy("map") %>% clearPopups()

    # capture click events
    # event_shape captures a user click on a shape object
    # event_marker captures a user click on a marker object
    event_shape <- input$map_shape_click
    event_marker <- input$map_marker_click

    # if a user has not clicked on a marker or object leave event as null if a
    # user has clicked on a shape or marker update event and pass it into
    # fct_add_popups to create popup for clicked object
    event <- NULL

    if (!is.null(event_shape)) {
      event <- event_shape
    }

    if (!is.null(event_marker)) {
      event <- event_marker
    }

    if (is.null(event)) {
      return()
    }

    isolate({
      req(input$label_vars)

      content <-
        add_popups(
          in_df = map_active_df,
          layer_id = event$id,
          label_vars = input$label_vars
        )

      leaflet::leafletProxy("map") %>%
        leaflet::addPopups(event$lng, event$lat, content, layerId = event$id)
    })
  })

  # remove legend and reset map on selecting a new column
  observeEvent(input$map_var, {
    updateCheckboxInput(session,
      "legend",
      value = FALSE
    )

    leaflet::leafletProxy("map") %>%
      leaflet::clearControls() %>%
      leaflet::clearShapes() %>%
      leaflet::clearMarkers()
  })

  # add legend on top of leaflet object
  observe({
    req(map_active_df())

    if ("sf" %in% class(map_active_df()) &
      is.atomic(map_active_df()[[input$map_var]]) &
      nrow(map_active_df()) > 0) {

      # Catch GeoPackages with non-spatial tables that GeoPandas has added empty
      # GeometryCollection column to.
      if (any(is.na(sf::st_crs(map_active_df())))) {
        return()
      }

      # make map active layer epsg 4326
      # make this an if statement
      map_df <- try(map_active_df() %>%
        sf::st_transform(4326))

      if ("try-error" %in% class(map_df)) {
        return()
      }

      # update legend to reflect masking zero values
      if (input$mask_zero == TRUE) {
        try(map_df[[input$map_var]][map_df[[input$map_var]] == 0] <- NA)
      }

      if (class(map_df[[input$map_var]]) != "numeric" &
        class(map_df[[input$map_var]]) != "integer") {
        pal <-
          leaflet::colorFactor(input$map_colour, map_df[[input$map_var]])
      } else {
        pal <-
          leaflet::colorNumeric(input$map_colour, map_df[[input$map_var]])
      }

      if (input$legend == TRUE) {
        leaflet::leafletProxy("map") %>%
          leaflet::clearControls() %>%
          leaflet::addLegend(
            pal = pal,
            values = map_df[[input$map_var]],
            position = "topright",
            title = input$map_legend_title
          )
      } else {
        leaflet::leafletProxy("map") %>%
          leaflet::clearControls()
      }
    }
  })

  # Tables ------------------------------------------------------------------

  # update select input with list of objects in S3bucket
  observe({
    data_file$items

    if (length(data_file$items) > 0 &
      !"no items returned" %in% data_file$items) {
      updateSelectInput(session,
        "table_s3_bucket_objects",
        choices = data_file$items
      )
    } else {
      updateSelectInput(session,
        "table_s3_bucket_objects",
        choices = ""
      )
    }
  })

  # get user selected S3 object and unpack layers in GeoPackage
  observeEvent(input$table_s3_bucket_objects, {
    req(input$table_s3_bucket_objects)

    waiter$show()

    selected_s3_object <- input$table_s3_bucket_objects

    tryCatch(
      error = function(cnd) {
        showNotification("Could not load project files.", type = "error")
      },
      {
        s3_gpkg <- qfieldcloudR::get_qfieldcloud_file(
          data_file$token,
          qfieldcloud_url,
          project_id,
          selected_s3_object
        )

        f_lyrs <- purrr::map2(
          s3_gpkg$tmp_file,
          s3_gpkg$filename,
          list_layers
        ) %>%
          dplyr::bind_rows()

        # clear existing layers
        data_file$data_table <- data.frame()

        df <- dplyr::bind_rows(data_file$data_table, f_lyrs)

        # this line is redundant but other functions need to be refactored to
        # remove dependency on layer_disp_name_idx
        df$layer_disp_name_idx <- paste0(df$layer_disp_name)
        data_file$data_table <- df
      }
    )

    waiter$hide()

  })

  # select one table as active layer from files loaded to the server
  observe({
    df <- data_file$data_table
    choices <- unique(df$layer_disp_name_idx)
    updateSelectInput(session,
      "table_active_layer",
      choices = choices
    )
  })

  # select columns to display in data table
  observeEvent(table_active_df(), {
    req(table_active_df())

    df <- table_active_df()
    choices <- colnames(df)

    choices <- choices[choices != "geom"]
    choices <- choices[choices != "geometry"]

    updateSelectInput(session,
      "table_vars",
      choices = choices
    )
  })

  # read selected layer into reactive object
  table_active_df <- reactive({
    req(input$table_active_layer)

    dt <- try(data_file$data_table)

    if ("try-error" %in% class(dt)) {
      # shiny::showNotification("", type = "error")
      return()
    }

    table_active_df <- NULL

    table_active_df <-
      try(read_tables(dt, input$table_active_layer))

    #  check table read was OK
    req(nrow(table_active_df) > 0)

    colnames <- colnames(df)

    if ("sf" %in% class(table_active_df)) {
      table_active_df <- table_active_df %>%
        st_drop_geometry() %>%
        as.data.frame()
    }

    if ("geom" %in% colnames) {
      table_active_df <- table_active_df %>%
        select(-c("geom"))
    }

    if ("geometry" %in% colnames) {
      table_active_df <- table_active_df %>%
        select(-c("geometry"))
    }

    table_active_df
  })

  view_df <- reactive({
    req(table_active_df())

    view_df <- table_active_df()

    if (!is.null(input$table_vars) & length(input$table_vars) > 0) {
      view_df <- try(view_df %>%
        dplyr::select(tidyselect::all_of(input$table_vars)))
    }

    if ("try-error" %in% class(view_df)) {
      shiny::showNotification("Cannot select columns", type = "error")
      return()
    }

    view_df
  })

  output$data_table <- DT::renderDataTable({
    req(view_df())
    view_df <- view_df()
    view_df <- view_df %>%
      dplyr::mutate(dplyr::across(is.numeric,
        round,
        digits = 4
      ))
    DT::datatable(data = view_df)
  })


  # Data Download -----------------------------------------------------------

  # Date stamp for downloading files
  dt <- reactive({
    d <- Sys.time()
    d <- stringr::str_replace_all(d, ":", "-")
    d <- stringr::str_replace(d, " ", "-")
    d
  })

  # download table data
  output$download_csv_data <- downloadHandler(
    filename = function() {
      paste("table_data_", dt(), ".csv", sep = "")
    },
    content = function(file) {
      req(view_df())

      readr::write_csv(view_df(), file)
    }
  )


  # Report ------------------------------------------------------------------

  # update select input with list of objects in S3 bucket
  observe({
    data_file$items

    if (length(data_file$items) > 0 &
      !"no items returned" %in% data_file$items) {
      updateSelectInput(session,
        "report_s3_bucket_objects",
        choices = data_file$items
      )
    } else {
      updateSelectInput(session,
        "report_s3_bucket_objects",
        choices = ""
      )
    }
  })

  # get user selected S3 object as layer
  observeEvent(input$report_s3_bucket_objects, {
    req(input$report_s3_bucket_objects)

    waiter$show()

    selected_s3_object <- input$report_s3_bucket_objects

    tryCatch(
      error = function(cnd) {
        showNotification("Could not load project files.", type = "error")
      },
      {
        s3_gpkg <- qfieldcloudR::get_qfieldcloud_file(
          data_file$token,
          qfieldcloud_url,
          project_id,
          selected_s3_object
        )

        f_lyrs <- purrr::map2(
          s3_gpkg$tmp_file,
          s3_gpkg$filename,
          list_layers
        ) %>%
          dplyr::bind_rows()

        # clear existing layers
        data_file$data_report <- data.frame()

        df <- dplyr::bind_rows(data_file$data_report, f_lyrs)

        # this line is redundant but other functions need to be refactored to
        # remove dependency on layer_disp_name_idx
        df$layer_disp_name_idx <- paste0(df$layer_disp_name)
        data_file$data_report <- df
      }
    )

    waiter$hide()
  })

  # select one layer as active layer from files loaded to the server
  observe({
    df <- data_file$data_report
    choices <- unique(df$layer_disp_name_idx)
    updateSelectInput(session,
      "report_active_layer",
      choices = choices
    )
  })

  # read selected layer into reactive object
  report_active_df <- reactive({
    req(input$report_active_layer)

    df <- try(data_file$data_report)

    report_active_df <-
      try(read_tables(df, input$report_active_layer))

    if ("try-error" %in% class(report_active_df)) {
      # shiny::showNotification("", type = "error")
      return()
    }

    # show warning if map active layer has no records
    shinyFeedback::feedbackWarning(
      "report_active_layer", !(nrow(report_active_df) > 0),
      "No records in selected layer"
    )

    # show warning if map active layer is not spatial (class sf)
    shinyFeedback::feedbackWarning(
      "report_active_layer", !("sf" %in% class(report_active_df)),
      "Not a spatial layer"
    )

    # don't update options if selected layer has no records or is not spatial
    req(
      nrow(report_active_df) > 0,
      "sf" %in% class(report_active_df)
    )

    report_active_df
  })

  # select columns to use in report
  observe({
    req(report_active_df())
    df <- report_active_df()
    choices <- colnames(df)

    choices <- choices[choices != "geom"]
    choices <- choices[choices != "geometry"]

    updateSelectInput(session,
      "report_vars",
      choices = choices
    )
  })

  # select group by columns to display in report
  observe({
    req(report_active_df())

    df <- report_active_df()
    choices <- colnames(df)

    choices <- choices[choices != "geom"]
    choices <- choices[choices != "geometry"]
    choices <- choices[choices != input$report_vars]

    updateSelectInput(session,
      "report_group_vars",
      choices = choices
    )
  })

  # show colour palette
  observeEvent(input$carto_colour, {
    colour_ramp <- make_colour_ramp_viridis(input$carto_colour)

    output$colour_ramp <- renderPlot({
      colour_ramp
    })
  })

  # preview map
  observeEvent(input$preview_map, {
    req(input$report_vars)

    # get layer to map
    map_df <- report_active_df() %>%
      dplyr::select(tidyselect::all_of(input$report_vars[1]))

    # get basemap
    bbox <- unname(sf::st_bbox(map_df))
    basemap <-
      ggmap::get_map(
        location = bbox,
        source = "osm",
        color = "bw",
        force = TRUE,
        zoom = 10
      )

    basemap <- ggmap_bbox(basemap)

    map_df_3857 <- map_df %>%
      sf::st_transform(3857)

    if (input$carto_mask_zeros == TRUE &
      is.numeric(map_df[[input$report_vars[1]]])) {
      map_df_3857 <- map_df_3857 %>%
        dplyr::filter(.data[[input$report_vars[1]]] != 0)
    } else if (input$carto_mask_zeros == TRUE &
      (is.character(map_df[[input$report_vars[1]]]) |
        is.factor(map_df[[input$report_vars[1]]]))) {
      map_df_3857 <- na.omit(map_df_3857)
    }

    # generate cartographic output
    if (is.numeric(map_df[[input$report_vars[1]]])) {
      gg_map <- ggmap::ggmap(basemap) +
        ggplot2::coord_sf(crs = sf::st_crs(3857)) + # force the ggplot2 map to be in 3857
        ggplot2::geom_sf(
          data = map_df_3857,
          ggplot2::aes(fill = .data[[input$report_vars[1]]], color = .data[[input$report_vars[1]]]),
          size = input$carto_line_width,
          inherit.aes = FALSE
        ) +
        ggplot2::scale_fill_viridis_c(option = input$carto_colour) +
        ggplot2::scale_color_viridis_c(option = input$carto_colour, guide = "none") +
        ggplot2::theme_bw() +
        ggplot2::labs(fill = input$report_legend_title) +
        ggplot2::theme(
          panel.grid.major = ggplot2::element_line(
            color = gray(0.5),
            linetype = "dashed",
            size = 0.5
          ),
          axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.1)
        )
    } else if (is.character(map_df[[input$report_vars[1]]]) |
      is.factor(map_df[[input$report_vars[1]]])) {
      gg_map <- ggmap::ggmap(basemap) +
        ggplot2::coord_sf(crs = sf::st_crs(3857)) + # force the ggplot2 map to be in 3857
        ggplot2::geom_sf(
          data = map_df_3857,
          ggplot2::aes(fill = .data[[input$report_vars[1]]], color = .data[[input$report_vars[1]]]),
          size = input$carto_line_width,
          inherit.aes = FALSE
        ) +
        ggplot2::scale_fill_viridis_d(option = input$carto_colour) +
        ggplot2::scale_color_viridis_d(option = input$carto_colour, guide = "none") +
        ggplot2::theme_bw() +
        ggplot2::labs(fill = input$report_legend_title) +
        ggplot2::theme(
          panel.grid.major = ggplot2::element_line(
            color = gray(0.5),
            linetype = "dashed",
            size = 0.5
          ),
          axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.1)
        )
    }

    output$map_view <- renderPlot(gg_map)

    # preview map
    showModal(modalDialog(
      title = "Map Preview",
      size = "l",
      plotOutput("map_view")
    ))
  })

  # popup window with interactive plotly map
  # do not map attribute values to colours so only fill colour popup shows in map
  observeEvent(input$custom_map, {
    req(input$report_vars)

    waiter$show()

    # get layer to map
    map_df <- report_active_df() %>%
      dplyr::select(tidyselect::all_of(input$report_vars[1]))
    
    # get basemap
    bbox <- unname(sf::st_bbox(map_df))
    basemap <-
      ggmap::get_map(
        location = bbox,
        source = "osm",
        color = "bw",
        force = TRUE,
        zoom = 10
      )

    basemap <- ggmap_bbox(basemap)

    map_df_3857 <- map_df %>%
      sf::st_transform(3857)

    if (input$carto_mask_zeros == TRUE &
      is.numeric(map_df[[input$report_vars[1]]])) {
      map_df_3857 <- map_df_3857 %>%
        dplyr::filter(.data[[input$report_vars[1]]] != 0)
    } else if (input$carto_mask_zeros == TRUE &
      (is.character(map_df[[input$report_vars[1]]]) |
        is.factor(map_df[[input$report_vars[1]]]))) {
      map_df_3857 <- na.omit(map_df_3857)
    }

    # generate cartographic output
    if (is.numeric(map_df[[input$report_vars[1]]])) {
      gg_map <- ggmap::ggmap(basemap) +
        ggplot2::coord_sf(crs = sf::st_crs(3857)) + # force the ggplot2 map to be in 3857
        ggplot2::geom_sf(
          data = map_df_3857,
          ggplot2::aes(fill = .data[[input$report_vars[1]]]),
          size = input$carto_line_width,
          inherit.aes = FALSE
        ) +
        ggplot2::scale_fill_viridis_c(option = input$carto_colour) +
        ggplot2::theme_bw() +
        ggplot2::labs(fill = input$report_legend_title) +
        ggplot2::theme(
          panel.grid.major = ggplot2::element_line(
            color = gray(0.5),
            linetype = "dashed",
            size = 0.5
          ),
          axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.1)
        )
    } else if (is.character(map_df[[input$report_vars[1]]]) |
      is.factor(map_df[[input$report_vars[1]]])) {
      gg_map <- ggmap::ggmap(basemap) +
        ggplot2::coord_sf(crs = sf::st_crs(3857)) + # force the ggplot2 map to be in 3857
        ggplot2::geom_sf(
          data = map_df_3857,
          ggplot2::aes(fill = .data[[input$report_vars[1]]]),
          size = input$carto_line_width,
          inherit.aes = FALSE
        ) +
        ggplot2::scale_fill_viridis_d(option = input$carto_colour) +
        ggplot2::theme_bw() +
        ggplot2::labs(fill = input$report_legend_title) +
        ggplot2::theme(
          panel.grid.major = ggplot2::element_line(
            color = gray(0.5),
            linetype = "dashed",
            size = 0.5
          ),
          axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.1)
        )
    }

    waiter$hide()

    output$plotly_map <- plotly::renderPlotly({
      plotly::ggplotly(gg_map) %>%
        plotly::layout(
          xaxis = list(autorange = TRUE),
          yaxis = list(autorange = TRUE)
        ) %>%
        plotly::toWebGL()
    })

    # preview map
    showModal(modalDialog(
      title = "Map Preview",
      size = "l",
      plotly::plotlyOutput(
        "plotly_map",
        height = "75vh"
      )
    ))
  })

  # preview chart
  observeEvent(input$preview_chart, {
    req(input$report_vars)
    
    # get layer to map
    chart_df <- report_active_df() %>%
      dplyr::select(tidyselect::all_of(c(
        input$report_vars[1], input$report_group_vars
      )))

    # generate summary table
    summary_df <- group_by_summarise(
      chart_df,
      input$report_group_vars,
      input$report_vars[1]
    )

    if (ncol(summary_df) == 2) {
      col_chart_df <- data.frame(summary_df[, 1], summary_df[, 2])
    } else if (input$bar_plot_type == "count_records") {
      col_chart_df <- data.frame(summary_df[, 1], summary_df[, 4])
    } else if (input$bar_plot_type == "mean") {
      col_chart_df <- data.frame(summary_df[, 1], summary_df[, 2])
    } else if (input$bar_plot_type == "sum_values") {
      col_chart_df <- data.frame(summary_df[, 1], summary_df[, 3])
    }

    gg_chart <-
      ggplot2::ggplot(data = col_chart_df, ggplot2::aes(col_chart_df[, 1], col_chart_df[, 2])) +
      ggplot2::geom_col(color = "#000000", fill = "#000000") +
      ggplot2::xlab(input$x_lab) +
      ggplot2::ylab(input$y_lab) +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = NA, colour = NA),
        panel.background = ggplot2::element_rect(fill = NA, colour = "#000000"),
        axis.text.x = ggplot2::element_text(
          angle = -90,
          vjust = 1,
          hjust = 0,
          size = input$font
        ),
        axis.text.y = ggplot2::element_text(size = input$font),
        axis.title.x = ggplot2::element_text(size = input$font),
        axis.title.y = ggplot2::element_text(size = input$font)
      )

    output$chart_view <- renderPlot(gg_chart)

    # preview map
    showModal(modalDialog(
      title = "Chart Preview",
      size = "l",
      plotOutput("chart_view")
    ))
  })


  observeEvent(input$generate_report, {
    req(input$report_vars)
    
    waiter$show()

    data_file$report_raw_table <- report_active_df() %>%
      sf::st_drop_geometry() %>%
      as.data.frame()

    # generate outputs
    data_file$report_raw_table_dir <- NULL
    tmp_report_table_dir <- paste0(tempdir(), "/report_raw_data_table.csv")
    data_file$report_raw_table_dir <- tmp_report_table_dir

    readr::write_csv(
      data_file$report_raw_table,
      tmp_report_table_dir
    )

    # make maps
    # get layer to map
    map_df <- report_active_df() %>%
      dplyr::select(tidyselect::all_of(input$report_vars))

    data_file$report_raw_gpkg <- map_df

    # generate outputs
    data_file$report_raw_gpkg_dir <- NULL
    tmp_report_gpkg_dir <- paste0(tempdir(), "/report_raw_spatial_data.gpkg")
    data_file$report_raw_gpkg_dir <- tmp_report_gpkg_dir

    sf::st_write(
      data_file$report_raw_gpkg,
      tmp_report_gpkg_dir,
      delete_dsn = TRUE
    )

    # get basemap
    bbox <- unname(sf::st_bbox(map_df))
    basemap <-
      ggmap::get_map(
        location = bbox,
        source = "osm",
        color = "bw",
        force = TRUE,
        zoom = 10
      )

    basemap <- ggmap_bbox(basemap)

    map_df_3857 <- map_df %>%
      sf::st_transform(3857)

    # remove paths to previous saved maps
    data_file$report_map <- NULL
    # remove paths to previous saved charts
    data_file$report_chart <- NULL
    # remove paths to previous saved summary tables
    data_file$report_summary_table <- NULL

    for (i in seq_along(input$report_vars)) {
      report_var <- input$report_vars[i]

      if (input$carto_mask_zeros == TRUE &
        is.numeric(map_df[[report_var]])) {
        map_df_3857 <- map_df_3857 %>%
          dplyr::filter(.data[[report_var]] != 0)
      } else if (input$carto_mask_zeros == TRUE &
        (is.character(map_df[[report_var]]) |
          is.factor(map_df[[report_var]]))) {
        map_df_3857 <- na.omit(map_df_3857)
      }

      # generate cartographic output
      if (is.numeric(map_df[[report_var]])) {
        gg_map <- ggmap::ggmap(basemap) +
          ggplot2::coord_sf(crs = sf::st_crs(3857)) + # force the ggplot2 map to be in 3857
          ggplot2::geom_sf(
            data = map_df_3857,
            ggplot2::aes(fill = .data[[report_var]], color = .data[[report_var]]),
            size = input$carto_line_width,
            inherit.aes = FALSE
          ) +
          ggplot2::scale_fill_viridis_c(option = input$carto_colour) +
          ggplot2::scale_color_viridis_c(option = input$carto_colour, guide = "none") +
          ggplot2::theme_bw() +
          ggplot2::labs(fill = input$report_legend_title) +
          ggplot2::theme(
            panel.grid.major = ggplot2::element_line(
              color = gray(0.5),
              linetype = "dashed",
              size = 0.5
            ),
            axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.1)
          )
      } else if (is.character(map_df[[report_var]]) |
        is.factor(map_df[[report_var]])) {
        gg_map <- ggmap::ggmap(basemap) +
          ggplot2::coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
          ggplot2::geom_sf(
            data = map_df_3857,
            ggplot2::aes(fill = .data[[report_var]], color = .data[[report_var]]),
            size = input$carto_line_width,
            inherit.aes = FALSE
          ) +
          ggplot2::scale_fill_viridis_d(option = input$carto_colour) +
          ggplot2::scale_color_viridis_d(option = input$carto_colour, guide = "none") +
          ggplot2::theme_bw() +
          ggplot2::labs(fill = input$report_legend_title) +
          ggplot2::theme(
            panel.grid.major = ggplot2::element_line(
              color = gray(0.5),
              linetype = "dashed",
              size = 0.5
            ),
            axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.1)
          )
      }

      tmp_map_dir <- paste0(tempdir(), "/", report_var, "_report_map.png")
      ggplot2::ggsave(
        tmp_map_dir,
        gg_map,
        dpi = 300,
        units = "cm",
        width = 30,
        height = 30
      )

      # temporary location to store current ggmap
      data_file$report_map <- c(data_file$report_map, tmp_map_dir)

      # make charts
      # get layer to chart
      if (input$make_chart == TRUE) {
        chart_df <- report_active_df() %>%
          dplyr::select(tidyselect::all_of(c(
            report_var, input$report_group_vars
          )))

        # generate summary table
        summary_df <- group_by_summarise(
          chart_df,
          input$report_group_vars,
          report_var
        )

        if (ncol(summary_df) == 2) {
          col_chart_df <- data.frame(summary_df[, 1], summary_df[, 2])
        } else if (input$bar_plot_type == "count_records") {
          col_chart_df <- data.frame(summary_df[, 1], summary_df[, 4])
        } else if (input$bar_plot_type == "mean") {
          col_chart_df <- data.frame(summary_df[, 1], summary_df[, 2])
        } else if (input$bar_plot_type == "sum_values") {
          col_chart_df <- data.frame(summary_df[, 1], summary_df[, 3])
        }

        gg_chart <-
          ggplot2::ggplot(data = col_chart_df, ggplot2::aes(col_chart_df[, 1], col_chart_df[, 2])) +
          ggplot2::geom_col(color = "#000000", fill = "#000000") +
          ggplot2::xlab(input$x_lab) +
          ggplot2::ylab(input$y_lab) +
          ggplot2::theme(
            plot.background = ggplot2::element_rect(fill = NA, colour = NA),
            panel.background = ggplot2::element_rect(fill = NA, colour = "#000000"),
            axis.text.x = ggplot2::element_text(
              angle = -90,
              vjust = 1,
              hjust = 0,
              size = input$font
            ),
            axis.text.y = ggplot2::element_text(size = input$font),
            axis.title.x = ggplot2::element_text(size = input$font),
            axis.title.y = ggplot2::element_text(size = input$font)
          )

        tmp_chart_dir <- paste0(tempdir(), "/", report_var, "_report_chart.png")
        ggplot2::ggsave(
          tmp_chart_dir,
          gg_chart,
          dpi = 300,
          units = "cm",
          width = 30,
          height = 20
        )


        data_file$report_chart <- c(data_file$report_chart, tmp_chart_dir)

        # save summary table

        tmp_sum_table_dir <- paste0(tempdir(), "/", report_var, "_summary_table.csv")
        data_file$report_summary_table_dir <- c(data_file$report_summary_table_dir, tmp_sum_table_dir)

        readr::write_csv(
          summary_df,
          tmp_sum_table_dir
        )
      }
    }

    waiter$hide()
  })

  # download table data
  output$download_report <- downloadHandler(
    filename = function() {
      paste("report_", dt(), ".zip", sep = "")
    },
    content = function(file) {
      raw_table <- data_file$report_raw_table_dir
      map <- data_file$report_map
      gpkg <- data_file$report_raw_gpkg_dir

      if (input$make_chart == TRUE) {
        chart <- data_file$report_chart
        summary_table <- data_file$report_summary_table_dir
        zip(
          zipfile = file,
          files = c(summary_table, raw_table, map, chart, gpkg),
          flags = "-r9Xj"
        )
      } else {
        zip(
          zipfile = file,
          files = c(raw_table, map, gpkg),
          flags = "-r9Xj"
        )
      }
    },
    contentType = "application/zip"
  )
})
