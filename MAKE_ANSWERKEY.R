
rmd2rscript_labanswers <- function(infile="LECTURE2.Rmd"){    # function for converting markdown to scripts
  outfile <- gsub(".Rmd","_key.R",infile)
  close( file( outfile, open="w" ) )   # clear output file
  con1 <- file(infile,open="r")
  con2 <- file(outfile,"w")
  stringToFind <- "```{r*"
  stringToFind2 <- c("answer","test","solution")
  isrblock <- FALSE
  #count=0
  blocknum=0

  while(length(input <- readLines(con1, n=1)) > 0){   # while there are still lines to be read
    isrblock <- grepl(input, pattern = stringToFind, perl = TRUE)   # is it the start of an R block?
    showit <- grepl(input, pattern = stringToFind2[1], perl = TRUE) | grepl(input, pattern = stringToFind2[2]) | grepl(input, pattern = stringToFind2[3])
    if(isrblock){
      blocknum=blocknum+1
      while(!grepl(newline<-readLines(con1, n=1),pattern="```",perl=TRUE)){
        if((blocknum>1)&((showit)|(blocknum==2))) write(newline,file=con2,append=TRUE)
        #count=count+1
      }
      isrblock=FALSE
    }
  }
  closeAllConnections()
}

#getwd()

rmd2rscript_labanswers(infile="LAB1.Rmd")
rmd2rscript_labanswers("LAB2.Rmd")
rmd2rscript_labanswers("LAB3.Rmd")
rmd2rscript_labanswers("LAB4.Rmd")
