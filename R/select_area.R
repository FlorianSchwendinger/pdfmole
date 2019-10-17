##  ----------------------------------------------------------------------------
#  select_area
#  ============
#' @title select_area
#' @description A function creating a shiny miniUI for select the coordinates
#' for \code{\link{rm_bound}}.
#' @param x an object inheriting from \code{data.frame}.
#' @param ... additional arguments (currently not used).
#' @details Some details to be written.
#' @return TODO
#' @export
##  ----------------------------------------------------------------------------

select_area <- function(x) {
  ui <- miniPage(
    gadgetTitleBar("Please select"),
    miniContentPanel(
      plotOutput("plot1", brush = "plot_brush", height = "100%"),
      verbatimTextOutput("info")
    )
  )

  server <- function(input, output) {
    output$plot1 <- renderPlot({
      textplot(x)
    })

    brush_bounds <- function(e) {
      if(is.null(e)) return(NULL)
      c(round(e$xmin, 1), round(e$xmax, 1), round(e$ymin, 1), round(e$ymax, 1))
    }

    output$info <- renderText({
      xy_range_str <- function(e) {
        if(is.null(e)) return("NULL\n")
        paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
               " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
      }

      paste0("brush: ", xy_range_str(input$plot_brush))

    })

    observeEvent(input$done, {
      stopApp(brush_bounds(input$plot_brush))
    })
    observeEvent(input$cancel, {
      stopApp(stop("No coords select.", call. = FALSE))
    })
  }

  runGadget(ui, server)
}