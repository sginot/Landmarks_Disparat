#----------------------------------------------
# Remove the n last characters from a vector of character strings
#-----------------------------------------------

rm.last.chr <- function(x, n) {
  nc <- nchar(x)
  brk <- nc - n
  substr(x, 1, brk)
}

#----------------------------------------------
# Keep the n last characters from a vector of character strings
#-----------------------------------------------

keep.last.chr <- function(x, n) {
  nc <- nchar(x)
  brk <- nc - n
  substr(x, brk + 1, nc)
}


#-------------------------------------------------------
# Add CI polygon to a biplot
#-------------------------------------------------------

plot.lm.CI <- function(x, y, 
                       border = NULL,
                       color = "gray", 
                       alpha = NA, 
                       level = 0.95,
                       line = T) {
  
  mod <- lm(y ~ x)
  
  minx <- min(x, na.rm = T)
  maxx <- max(x, na.rm = T)
  rg <- abs(minx - maxx)
  
  newx <- seq(minx, 
              maxx, 
              by = rg/1000) 
  
  prd <- predict(object = mod,
                 newdata = data.frame(x = newx),
                 interval = "confidence",
                 level = level)
  
  if (line) {
    clip(minx, maxx, min(y)/2, max(y)*2)
    abline(mod, col = color)}
  
  if (!is.na(alpha)) {
    require(scales)
    polygon(x = c(newx, rev(newx)),
          y = c(prd[,2], rev(prd[,3])),
          col = alpha(color, alpha = alpha), 
          border = border)
  } else {
    polygon(x = c(newx, rev(newx)),
            y = c(prd[,2], rev(prd[,3])),
            col = color)
  }
  
} 

#--------------------------------------------------------
# Check if a number is even (return TRUE or FALSE)
#--------------------------------------------------------

is.even <- function(x, verbose = T) {
  if (verbose) {
    print(x/2 == round(x/2))
    x/2 == round(x/2)
  } else {
    x/2 == round(x/2)
  }
}

#--------------------------------------------------------
# Match two vectors of different length based on the names 
# of their elements, output data frame with common elements
# for each vector
#--------------------------------------------------------

vec.mtch <- function(vec1, vec2) {
  if (is.null(names(vec1)) | is.null(names(vec2))) {
    stop("Both vectors must have names")
  }
  
  s1 <- vec1[names(vec1) %in% names(vec2)]
  s2 <- vec2[names(vec2) %in% names(vec1)]
  
  d <- data.frame(s1, s2)
  rownames(d) <- names(s1)
  colnames(d)[1] <- deparse(substitute(vec1))
  colnames(d)[2] <- deparse(substitute(vec2))
  d
}

#--------------------------------------------------------
# Function to shorten labels (e.g. tree tips or rownames), 
# based on strplit of the vectors into the different components. 
# Positions argument defines which element(s) of the 
# strsplit output to keep (e.g. only generic / specific name)
#--------------------------------------------------------

shorten.labels <- function(x, positions, split) {
  splt <- strsplit(x = x, split = split)
  newlab <- list()  
  for (i in 1:length(splt)) {
    newlab[[i]] <- paste(splt[[i]][positions], collapse = " ")
  }
  return(unlist(newlab))
}

#--------------------------------------------------------
# Function to average matrix based on factor, 
# i.e. apply / tapply combo
#--------------------------------------------------------

avg.matrix.fac <- function(mat, MARGIN = 2, INDEX) {
  mav <- apply(mat, 2, tapply, INDEX = INDEX, mean)
return(mav)}
