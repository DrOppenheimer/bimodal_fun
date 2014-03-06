bimodal.test <- function(file_in="test_data.txt", max_mix_proportion=0.95, var_filter=1.5, method="npEM",file_out="bimodal.test.output.txt", produce_hists=TRUE){

# supported methods, npEM, spEM, #normalmixEM#
# require necessary package
require(mixtools)||install.packages("mixtools")

# script uses defaults from npEM{mixtools}
# A nonparametric EM algorithm:
# Benaglia, T., Chauveau, D., and Hunter, D. R.,
# An EM-like algorithm for semi- and non-parametric estimation in multivariate mixtures,
# Journal of Computational and Graphical Statistics, 18, 505-526, 2009
# The non parametric EM seems to tolerate sparse count data reasoanbly well

# with mu0 = 2
# decides if data are unimodal or bimodal + based on max mix proportion

cat(
    paste(
        "# file_in: ", "\t", file_in, "\n",   
        "# method: ",  "\t", method,  "\n",
        "# var_filter", "\t", var_filter, "\t", "probably needs tweaking", "\n",
        "# max_mix_proportion: ", "\t", max_mix_proportion, "\t", "probably needs tweaking","\n",
         sep="",collapse=""
        ),
    file=file_out
    )

my_data <<- as.matrix(read.table(file_in, row.names=1, header=FALSE, sep="\t", comment.char="", quote="", check.names=FALSE, fill=TRUE))

num_rows <- nrow(my_data)
num_cols <- ncol(my_data)

output <- matrix("", num_rows, 6)
dimnames(output)[[1]] <- dimnames(my_data)[[1]]
dimnames(output)[[2]] <- c("length", "var", "max_proportion", "min_proportion", "modality_guess", "guess_on")

if ( produce_hists==TRUE ){
  hists_dir <- paste(getwd(), "/histograms", sep="", collapse="" )
  dir.create(hists_dir)
}


for (i in 1:num_rows){

    print(paste("processing row ( ", i, " ) of ",num_rows," rows",sep="",collapse=""))

    length_i <- sum(!is.na(my_data[i,]))
    output[i,1] <- length_i
    
    my_var <- var(my_data[i,1:length_i])
    output[i,2] <- my_var

    if ( length_i < 3 ){

      output[i,3:6]<-NA

    }else{
    
      if ( my_var <= var_filter ){
        output[i,3:4] <- rep("NA",2)
        output[i,5] <- "1"
        output[i,6] <- "var_filter"
      }else{
        
        if ( identical( method, "npEM" ) ){
          my_npEM <<- npEM(my_data[i,1:length_i], mu0=2, verb=FALSE)
          my_lambdahat <- sort(my_npEM$lambdahat,decreasing=TRUE)
          output[i,3:4] <- my_lambdahat
          if  ( my_lambdahat[1] >= max_mix_proportion ){
            output[i,5] <- "1"
            output[i,6] <- "max_mix_proportion"
          } else {
            output[i,5] <- "2 or more"
            output[i,6] <- "max_mix_proportion"
          }

          if ( produce_hists==TRUE){
            png(
                filename = paste( hists_dir, "/", dimnames(output)[[1]][i], ".histogram.png", sep="", collapse=""),
                width = 6,
                height = 3,
                res = 150,
                units = 'in'
                )
            plot(my_npEM, xlab=paste( dimnames(output)[[1]][i], ".histogram.png", sep="", collapse=""))
            dev.off()    
          }
          
        }
        
        if ( identical( method, "spEM" ) ){
          my_spEM <- spEM(my_data[i,1:length_i], mu0=2, verb=FALSE)
          my_lambdahat <- sort(my_spEM$lambdahat,decreasing=TRUE)
          output[i,3:4] <- my_lambdahat
          if  ( my_lambdahat[1] >= max_mix_proportion ){
            output[i,5] <- "1"
            output[i,6] <- "max_mix_proportion"
          } else {
            output[i,5] <- "2 or more"
            output[i,6] <- "max_mix_proportion"
          }
        }

        if ( produce_hists==TRUE){
            png(
                filename = paste( hists_dir, "/", dimnames(output)[[1]][i], ".histogram.png", sep="", collapse=""),
                width = 6,
                height = 3,
                res = 150,
                units = 'in'
                )
            plot(my_spEM, xlab=paste( dimnames(output)[[1]][i], ".histogram.png", sep="", collapse=""))
            dev.off()    
          }
        
      }
      

        #if ( identical( method, "normalmixEM" ) ){
        #    my_normalmixEM <- normalmixEM(my_data[i,], k=2, verb=FALSE)
        #    my_lambda <- sort(my_normalmixEM$lambda,decreasing=TRUE)
        #    output[i,2:3] <- my_lambda
        #    if  ( my_lambda[1] >= max_proportion ){
        #        output[i,1] <- "1"
        #    } else {
        #        output[i,1] <- "2 or more"
        #    }
        #}

    }




}

suppressWarnings(write.table(output, file=file_out, col.names=NA, row.names=TRUE, append=TRUE, sep="\t"))


}
