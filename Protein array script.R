#Protein array script.

def_path <- "*.txt"
filtri <- matrix(c("Text files", "*.txt", "All files", "*"), 2, 2, byrow = TRUE)
files <- character()

repeat {
  files <- append(files,choose.files(default=def_path, caption="Scegli le slide", filters=filtri, index=1))
  if (length(files) == 0) stop("Azione annullata dall'utente")
  if (regexpr("^[YySs]", readline(prompt="\n\nVuoi aggiungere altre slides della stessa SS? ")) < 0) break
}

cat("\n\nN. slides:", length(files), "\n\n")

i <- 1
for (f in files) {
  
  cat(i,")")
  slide<-basename(f)
  l <- readLines(f)
  sk <- match("Begin Raw Data", l)
  
  dati <- read.delim(f, skip=sk, nrow=length(l)-sk-3)
  dati <- dati[c("Gene.ID", "Signal.Mean", "Background.Mean", "Flag")]
  dati <- dati[dati$Flag == 0, 1:3]
  
  medie <- round(tapply((dati[, 2] - dati[, 3]), dati[, 1], mean))
  medie[medie < 0] <- 0
  sd <- round(tapply((dati[, 2] - dati[, 3]), dati[, 1], sd))
  cv <- round(sd / (medie+1), 2)
  
  if (i > 1) {
    totmedie <- cbind(totmedie, medie)
    totsd <- cbind(totsd, sd)
    totcv <- cbind(totcv, cv)
  } else {
    totmedie <- as.data.frame(list(Nomi=names(medie), medie), row.names=NULL)
    totsd <- as.data.frame(list(Nomi=names(sd), sd), row.names=NULL)
    totcv <- as.data.frame(list(Nomi=names(cv), cv), row.names=NULL)
  }
  names(totmedie)[dim(totmedie)[2]] <- substr(slide, 1, nchar(slide) - 4)
  names(totsd)[dim(totsd)[2]] <- substr(slide, 1, nchar(slide) - 4)
  names(totcv)[dim(totcv)[2]] <- substr(slide, 1, nchar(slide) - 4)
  
  cat(" ", slide, "\n")
  i <- i + 1
}


f <- paste0(dirname(f), .Platform$file.sep, "R - Medie.csv")
write.table(totmedie, file=f, sep=";", qmethod="d", row.names=F)
if (regexpr("^[YySs]", readline(prompt="\n\nVuoi informazioni sull'errore? \n")) > 0) {
  f <- paste0(dirname(f), .Platform$file.sep, "R - SD.csv")
  write.table(totsd, file=f, sep=";", qmethod="d", row.names=F)
  f <- paste0(dirname(f), .Platform$file.sep, "R - CV.csv")
  write.table(totcv, file=f, sep=";", qmethod="d", row.names=F)  
}
cat("\n\nFatto\n\n-------------------------------------\n\n")

rm(l,sk,dati,filtri,totmedie,totsd,def_path,f,files,i,medie,sd,slide,cv,totcv)