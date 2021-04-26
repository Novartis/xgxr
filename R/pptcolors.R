##' Colors in the powerpoint document
##'
##' This is based on the information about the powerpoint color scheme
##' given in the msdn article
##' https://msdn.microsoft.com/en-us/library/cc964302(v=office.12).aspx
##' The article is unclear what "very light" or "very dark" is, so it
##' is assumed the HSL lightness score is below 10% for very light
##' colors and the HSL lightness score is above 90% for very dark
##' colors. These values can be tuned.
##'
##' @param doc Powerpoint object
##'
##' @param full boolean to return full color matrix
##'
##' @param veryLight a number betweeen 0 and 1 representing the cutoff
##'     for very light colors.  This defaults to 0.1
##'
##' @param veryDark a number between 0 and 1 representing the cutoff
##'     for very dark colors.  This defaults to 0.9
##'
##' @return a list of named powerpoint based on the powerpoint theme
##'
##' @author Matthew L. Fidler
##' @export
xgx_ppt_colors <- function(doc, full=FALSE, veryLight = 0.1, veryDark  = 0.9){
  ret <- pptxColors.character.single(doc);
  if (full){
    tmp <- pptxFullColors(ret, veryLight = veryLight, veryDark  = veryDark)
    return(tmp)
  } else {
    return(ret)
  }
}

pptxColors.character.single <- function(doc){ #ppt_template Object
  ## ppt_get_template_colors returns a updated template with the
  ## powerpoint's colors.  This will also update the R
  ## meta-information file about the presentation.
  ## Number of layouts
  default.colors <- structure(c("#5B9BD5", "#ED7D31", "#A5A5A5", "#FFC000", "#4472C4",  "#70AD47", "#000000", "#FFFFFF", "#44546A", "#E7E6E6", "#0563C1",  "#954F72"), .Names = c("accent1", "accent2", "accent3", "accent4",  "accent5", "accent6", "dk1", "lt1", "dk2", "lt2", "hlink", "folHlink" ));
  tmpd <- tempfile();
  on.exit({unlink(tmpd, TRUE)})
  i <- 1;
  files <- unzip(doc,files=paste0("ppt/theme/theme",i,".xml"),junkpaths=TRUE, exdir=tmpd)
  while(file.exists(paste0("theme", i, ".xml"))){
    i <- i + 1;
    files <- c(files, suppressWarnings({unzip(doc,files=paste0("ppt/theme/theme",i,".xml"),junkpaths=TRUE, exdir=tmpd)}));
  }
  i <- i - 1;
  color.names <- c(paste0("accent",1:6),sapply(data.frame(t(expand.grid(c("dk","lt"),1:2))),paste0,collapse=""),c("hlink","folHlink"));
  colors <- list();
  use.xml2 <- requireNamespace("xml2", quietly = TRUE);
  for (f in files){
    if (!use.xml2){
      con <- file(f,open="r");
      lst <- readLines(con)
      close(con);
      unlink(f);
    }
    tmp <- c();
    for (i in color.names){
      if (!use.xml2){
        ## Use regular expression matching (should parse the xml.)
        clr <- strsplit(strsplit(lst,paste0("<a:",i,"><a:srgbClr val=\""))[[2]][2],"\"")[[1]][1];
        if (!is.na(clr)){
          tmp[i] <- paste0("#",clr);
        } else {
          clr <- strsplit(strsplit(lst,paste0("<a:",i,"><a:sysClr val=\"window[^\"]*\" lastClr=\""))[[2]][2],"\"")[[1]][1];
          tmp[i] <- paste0("#",clr);
        }
      } else {
        xml <- xml2::read_xml(f);
        tag <- xml2::xml_find_all(xml, paste0(".//a:", i));
        if (length(tag) == 1){
          clr.tag <- xml2::xml_find_all(tag,".//a:srgbClr");
          if (length(clr.tag) == 1){
            clr <- xml2::xml_attrs(clr.tag)[[1]];
            if (length(clr) == 1){
              tmp[i] <- paste0("#", clr)
            } else {
              tmp[i] <- default.colors[i]
            }
          } else {
            clr.tag <- xml2::xml_find_all(tag,".//a:sysClr");
            if (length(clr.tag) == 1){
              clr <- xml2::xml_attrs(clr.tag)[[1]];
              tmp[i] <- paste0("#", clr["lastClr"])
            } else {
              tmp[i] <- default.colors[i]
            }
          }
        } else {
          tmp[i] <- default.colors[i]
        }

      }
    }
    colors[[length(colors) + 1]] <- tmp;
  }
  return(colors);
}

pptxFullColors <- function(col, ...){
  if (class(col) == "character"){
    return(pptxFullColors.single(col, ...));
  } else if (class(col) == "list"){
    return(lapply(col, pptxFullColors));
  }
}
pptxFullColors.single <- function(col,
                                  veryLight = 0.1,
                                  veryDark  = 0.9){
  ## ppt_full_colors returns full powerpoint color table
  ##
  ## From https://msdn.microsoft.com/en-us/library/cc964302(v=office.12).aspx
  ##

  ## - For mid-range colors (commonly the accent colors), tints are
  ##   80%, 60%, and 40% lighter than the original, and shades are
  ##   25% and 50% darker than the original.

  ## - Very light colors (typically the light 2 text/background
  ##   color) use shades of 10%, 25%, 50%, 75%, and 90%.

  ## - Very dark colors (typically the dark 2 text/background color)
  ##   use tints of 90%, 75%, 50%, 25%, and 10%.

  ## - White, which is usually used for the Light 1 color position,
  ##   uses shades of 5%, 15%, 25%, 35%, and 50%.

  ## - Black, which is usually used for the Dark 1 color position,
  ##   uses tints of 50%, 35%, 25%, 15%, and 5%.

  ## First grab the colors
  colDf <- data.frame(grDevices::col2rgb(col));

  tmp <- sapply(colDf,function(x){return(max(x)/510+min(x)/510)})
  colDf <- data.frame(t(data.frame(t(colDf),l=tmp)))
  ret <- sapply(colDf,function(x){
    l <- x[4];
    col <- data.frame(t(x[1:3]));
    names(col) <- c("red","green","blue");
    tintCol <- function(tint){
      f <- function(x){
        return(round(x+(255-x)*tint));
      }
      for (v in c("red", "green", "blue")){
        col[[v]] <- f(col[[v]]);
      }
      col$col <- with(col, grDevices::rgb(red, green, blue, maxColorValue=255))
      return(col$col);
    }
    shadeCol <- function(shade){
      f <- function(x){
        return(round(x*(1-shade)));
      }
      for (v in c("red", "green", "blue")){
        col[[v]] <- f(col[[v]]);
      }
      col$col <- with(col, grDevices::rgb(red, green, blue, maxColorValue=255))
      return(col$col)
    }
    col$col <- with(col, grDevices::rgb(red, green, blue, maxColorValue=255))
    ret <- col$col;
    if (all(x[1:3] == 0)){
      ## Black
      ## - Black, which is usually used for the Dark 1 color position,
      ##   uses tints of 50%, 35%, 25%, 15%, and 5%.
      return(c(ret,sapply(c(0.5,0.35,0.25,0.15,0.05),tintCol)));
    } else if (all(x[1:3] == 255)){
      ## White
      ## - White, which is usually used for the Light 1 color position,
      ##   uses shades of 5%, 15%, 25%, 35%, and 50%.
      return(c(ret,sapply(c(0.05,0.15,0.25,0.35,0.5),shadeCol)));
    } else if (x[4] < veryLight){
      ## Very Light colors
      ## - Very light colors (typically the light 2 text/background
      ##   color) use shades of 10%, 25%, 50%, 75%, and 90%.
      return(c(ret,sapply(c(0.1,0.25,0.5,0.75,0.9),shadeCol)));
    } else if (x[4] > veryDark){
      ## Very Dark Colors
      ## - Very dark colors (typically the dark 2 text/background color)
      ##   use tints of 90%, 75%, 50%, 25%, and 10%.
      return(c(ret,sapply(c(0.9,0.75,0.5,0.25,0.1),tintCol)));
    } else {
      ## - For mid-range colors (commonly the accent colors), tints are
      ##   80%, 60%, and 40% lighter than the original, and shades are
      ##   25% and 50% darker than the original.
      return(c(ret,sapply(c(0.8,0.6,0.4),tintCol),sapply(c(0.25,0.5),shadeCol)))
    }
  })
  ret <- ret[,c(paste0(rep(c("lt","dk"),2),rep(1:2,each=2)),paste0("accent",1:6),c("hlink","folHlink"))];
  rownames(ret) <- c("Hue", "Tint3", "Tint2", "Tint1", "Shade1", "Shade2");
  return(ret)
}

ggFill <- function(pptx, name, extra, layout=1, ..., fill=TRUE){
  if (missing(name)){
    clr <- pptxColors(pptx, ...)[[layout]];
  } else {
    clr <- pptxColors(pptx, full=TRUE, ...)[[layout]];
    if (any(rownames(clr) == name)){
      clr <- clr[name, ];
      byRow <- TRUE;
    } else if (any(colnames(clr) == name)){
      byRow <- FALSE;
      clr <- clr[, name];
    } else {
      stop(sprintf("Cannot figure out what you mean by the the color '%s'", name));
    }
  }
  if (!missing(extra)){
    if (class(clr) == "character"){
      clr <- clr[extra];
    } else if (byRow){
      clr <- clr[, extra];
    } else {
      clr <- clr[extra, ];

    }
  }
  if (fill){
    return(scale_fill_manual(values=as.vector(clr)));
  } else {
    return(scale_color_manual(values=as.vector(clr)));
  }
}

ggColor <- function(pptx, name, extra, layout=1, ...){
  args <- as.list(match.call())[-1];
  args$fill = FALSE
  do.call("ggFill", args)
}

ggColour <- ggColor; ## UK spelling.


##This is taken from http://research.stowers.org/efg/R/Color/Chart/
SetTextContrastColor <- function(color)
{
  ifelse( mean(col2rgb(color)) > 127, "black", "white")
}
## This is adapted from http://research.stowers.org/efg/R/Color/Chart/
plotPptColors <- function(pptx, ...){
  if (file.exists(pptx)){
    clr <- pptxColors(pptx, full=TRUE);
    if (length(clr) == 1){
      plotPptColors(clr[[1]]);
      title(sprintf("Colors for %s", pptx))
    } else {
      for (i in 1:length(clr)){
        plotPptColors(clr[[1]]);
        title(sprintf("Colors for %s layout=%d", pptx, i))
      }
    }
  } else {
    ColorsToPrint <- pptx;
    TextContrastColor <- unlist( lapply(ColorsToPrint, SetTextContrastColor) )
    colCount <- length(ColorsToPrint[1,]) # number per row
    rowCount <- length(ColorsToPrint[,1])
    plot( c(-0.5, colCount + 0.5), c(-0.5, rowCount + 0.5), type = "n", ylab = "", xlab="",
         axes =- FALSE, ylim = c(rowCount,0))
    for (j in 0:(rowCount)){
      if(j==0){
        colori <- "white"
        contrasti <- "black"
        texti <- labels(ColorsToPrint)[[2]][1:colCount]
      }else{
        colori <- ColorsToPrint[j, 1:colCount]
        contrasti <- unlist( lapply(colori, SetTextContrastColor) )
        texti = colori

        rect( -0.5, j - 0.5, 0.5, j + 0.5,
             border = "black",
             col="white")
        text(0, j, paste(labels(ColorsToPrint)[[1]][j]), cex=0.7,
             col="black")
      }
      rect((1:colCount) -0.5, j - 0.5, (1:colCount) + 0.5, j + 0.5,
           border = "black",
           col=colori)
      text((1:colCount), j, paste(texti), cex=0.7,
           col=contrasti)
    }
  }
}
