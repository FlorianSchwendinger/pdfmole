##  ----------------------------------------------------------------------------
#  select_area
#  ============
#' @title select_area
#' @description A function creating a shiny miniUI for select the coordinates
#' for \code{\link{select_bounds}}.
#' @param x an object inheriting from \code{data.frame}.
#' @param ... additional arguments (currently not used).
#' @details Some details to be written.
#' @return TODO
#' @export
##  ----------------------------------------------------------------------------

select_area <- function(x, ...) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package \"shiny\" needed for this function to work. Please install it.",
      call. = FALSE)
  }

  if (!requireNamespace("miniUI", quietly = TRUE)) {
    stop("Package \"miniUI\" needed for this function to work. Please install it.",
      call. = FALSE)
  }


  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Please select"),
    miniUI::miniContentPanel(
      shiny::plotOutput("plot1", brush = "plot_brush", height = "100%"),
      shiny::verbatimTextOutput("info")
    )
  )

  server <- function(input, output) {
    output$plot1 <- shiny::renderPlot({
      textplot(x)
    })

    brush_bounds <- function(e) {
      if(is.null(e)) return(NULL)
      c(round(e$xmin, 1), round(e$xmax, 1), round(e$ymin, 1), round(e$ymax, 1))
    }

    output$info <- shiny::renderText({
      xy_range_str <- function(e) {
        if(is.null(e)) return("NULL\n")
        paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
               " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
      }

      paste0("brush: ", xy_range_str(input$plot_brush))

    })

    shiny::observeEvent(input$done, {
      shiny::stopApp(brush_bounds(input$plot_brush))
    })
    shiny::observeEvent(input$cancel, {
      shiny::stopApp(stop("No coords selected.", call. = FALSE))
    })
  }

  shiny::runGadget(ui, server)
}