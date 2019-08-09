#' Annotate a png file or directory of png files
#'
#' These function annotates a single png file or all files within a
#' directory.
#'
#' If a png file has been annotated once, this function will not
#' annotate it again.  Therefore, you can run this function on
#' directories with different input script names and it will label
#' each file based on when each file was run.
#' 
#' Based on code from MrFlick on
#' \href{https://stackoverflow.com/a/23816416}{Stack Overflow}.
#'
#' @param file_or_dir The png file to annotate or directory location for
#'   annotating png files.  Note this will annotate just once, so if
#'   you generate multiple png files and then annotate at the end of
#'   your script it will have the correct script name on it. Then if
#'   you create new images in a different script in the same directory
#'   and then annotate with the script name the second script, the PNG
#'   files will show the correct script location for each file.
#' @param script Script name to add as a footnote; By default this is
#'   empty, though it could name the script that
#' @param status Draft or other status; If \code{status="Final"} or
#'   \code{status=""} the status overlay will be removed.  By default
#'   the status is DRAFT.
#' @param date_format Date format for adding the time the png was
#'   annotated.
#' @param col Color for annotating the draft status
#' @param font Font to use for the annotation function
#' @param cex_status_mult Multiplication factor for the status
#'   annotation.  By default 7
#' @param cex_footnote_mult Multiplication factor for the footnote
#'   annotation. By default 0.8
#' @param status_angle Angle to rotate status
#' @param x11 Display on the X11/Windows device
#' 
#' @return nothing
#' 
#' @examples
#' # using the examples from plot()
#' file.name <- tempfile()
#' grDevices::png(file.name)
#' graphics::plot(cars)
#' graphics::lines(stats::lowess(cars))
#' grDevices::dev.off()
#' # annotate one file
#' xgx_annotate_status_png(file.name, "/tmp/script1.R")
#'
#' @author Matthew Fidler, Alison M, ....
#' @importFrom grDevices grey
#' @importFrom assertthat has_extension
#' @importFrom png readPNG
#' @importFrom png writePNG
#' @importFrom grDevices png
#' @importFrom graphics lines
#' @importFrom graphics par
#' @importFrom graphics plot
#' @importFrom graphics plot.new
#' @importFrom graphics plot.window
#' @importFrom graphics rasterImage
#' @importFrom graphics text
#' @importFrom grDevices dev.off
#' @importFrom magrittr "%>%"
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom stats lowess
#' @importFrom stats rnorm
#' @importFrom stats sd
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @export
xgx_annotate_status_png <- function(file_or_dir, script = "", status = "DRAFT",
                                    date_format = "%a %b %d %X %Y",
                                    col = grDevices::grey(0.8, alpha = 0.7),
                                    font = 2,
                                    cex_status_mult = 7,
                                    cex_footnote_mult = 0.8,
                                    status_angle = 45,
                                    x11 = FALSE) {
  # read file
  if (assertthat::has_extension(file_or_dir, "png")) {
    files <- c(file_or_dir)
  } else {
    files <- list.files(file_or_dir, pattern = ".png$", full.names = TRUE)
  }
  for (file in files) {
    img <- png::readPNG(file, info = TRUE)
    info <- attr(img, "info")
    dpi <- round(mean(c(0, info$dpi), na.rm = TRUE), 0)
    if (dpi < 10) {
      dpi <- 75
    }
    metadata <- attr(img, "metadata")
    if (!identical(metadata, "annotated by xGx")) {
      message(sprintf("Add footnote to %s\n", file))

      # get size
      h <- dim(img)[1]
      w <- dim(img)[2]

      # open file for output
      # make it slightly taller to add the text at the bottom
      grDevices::png(file, width = w, height = h * 1.05)

      # par is for setting graphical parameters
      # here, you're initializing a "state machine" setting all the
      # graphical parameters
      # from the state machine.  you can just set a few parameters differently.
      # it is extremely fast and takes no memory.
      #
      # grid, lattice, ggplot are friendlier to use, but they are much slower
      # and they require more memory
      #
      # mar <- c(0, 0, 0, 0): sets margins to zero
      # xpd <- NA: all plotting is clipped to the device region
      # mgp <- c(0, 0, 0): margin line (in mex units) for the axis title.
      # oma <- c(0, 0, 0, 0): more margins # ann = FALSE: do not add extra
      # annotation to the plot
      old_par <- graphics::par(no.readonly =TRUE)
      on.exit(graphics::par(old_par))
      
      graphics::par(mar = c(0, 0, 0, 0),
                                     xpd = NA,
                                     mgp = c(0, 0, 0),
                                     oma = c(0, 0, 0, 0), ann = FALSE)

      # creates new plot - uses what was set with par()?
      graphics::plot.new()
      graphics::plot.window(0:1, 0:1)

      # fill plot with image
      # gives the extremes of the user coordinates
      usr <- graphics::par("usr")
      # shifted up by 0.1 to make space for text
      graphics::rasterImage(img, usr[1], usr[3] + 0.1, usr[2], usr[4])

      # add draft status to text if status isn't "Final"
      # could be boolean, too.
      cx <- dpi / 75

      if (!any(status == c("Final", ""))) {
        graphics::text(0.5, 0.5, status, cex = cex_status_mult * cx,
                       col = col, font = font, srt = status_angle)
      }

      # add path to the bottom of the graphs
      bottom_txt <- paste0(script, ifelse(script == "", "", "\n"),
                          "PNG: ", file,
                          ifelse(date_format == "", "",
                                 paste0("\n", "Date: ",
                                        format(Sys.time(), date_format))))
      graphics::text(0.5, 0.025, bottom_txt, cex = cx * cex_footnote_mult)

      # close image
      invisible(grDevices::dev.off())
      img <- png::readPNG(file)
      png::writePNG(img, file, metadata = "annotated by xGx")
      if (x11) {
        graphics::par(mar = c(0, 0, 0, 0), xpd = NA, mgp = c(0, 0, 0),
                      oma = c(0, 0, 0, 0), ann = FALSE)
        lim <- graphics::par()
        graphics::plot.new()
        graphics::plot.window(0:1, 0:1)
        graphics::rasterImage(img, lim$usr[1], lim$usr[3],
                              lim$usr[2], lim$usr[4])
      }
    } else {
      message(sprintf("Already annotated %s; Need to regenerate figure to annotate again\n", file))
    }
  }
  return(invisible())
}
