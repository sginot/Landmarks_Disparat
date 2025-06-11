source("CUSTOM_R_FUNCTIONS.R")

################################################
#Import STV landmark files

input_dir <- "LMs_STV/"
fil <- list.files(input_dir, full.names = T)
fi <- list.files(input_dir, full.names = F)
sp_short <- shorten.labels(fi, c(1,2), split = "_")

imp_lm <- lapply(fil, scan, what = "charater")

lstab <- list()

for (i in 1:length(imp_lm)) {
d <- imp_lm[[i]]
lim <- grep(pattern="CurveNode",d)[1]-3
dd <- d[3:lim]
nr <- ceiling(length(dd)/7)
m <- matrix(dd, nrow = nr, ncol = 7, byrow= T)
lstab[[i]] <- m 
}

#################################################
# Import lmk files

input_dir <- "LM_and_CURVES/"
fil <- list.files(input_dir, full.names = T)
fi <- list.files(input_dir, full.names = F)
sp_short <- shorten.labels(fi, c(1,2), split = "_")

imp_lm <- lapply(fil, scan, what = "charater")
lslm <- lapply(X = imp_lm, FUN = matrix, ncol = 4, byrow = T)

nlm <- unlist(lapply(X=lslm, FUN= myfun <- function(x) {length(grep(x[,1], pattern =  "Landmark"))}))

for (i in 1:length(lslm)) {
  coo <- apply(lslm[[i]][,2:4], 2, as.numeric)
  rownames(coo) <- lslm[[i]][,1]
  lslm[[i]] <- coo}
  
#############################################
# Combine landmarks from stv files with curves from lmk files

lsco <- list()

for (i in 1:length(lstab)) {
lm <- matrix(as.numeric(lstab[[i]][,2:4]), ncol=3)
nlm <- dim(lm)[1]
rownames(lm) <- paste(rep("Landmark", nlm), 0:(nlm-1), rep(":", nlm), sep= "")
cur <- lslm[[i]][grep(pattern = "Curve_segment", rownames(lslm[[i]])),]
lsco[[i]] <- rbind(lm, cur)
}

if (!dir.exists("landmarks_final")) {dir.create("landmarks_final")}

for (i in 1:length(lsco)) {
write.table(file = paste("landmarks_final/", fi[i], sep = ""), x = lsco[[i]], quote = FALSE, col.names = FALSE)
}


