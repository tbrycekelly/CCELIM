#' Convergence Analysis
#'
#' This script is designed analyze the set of runs <l> for convergence (i.e. stationary solution).
#' @param l A list of the data sets to analyze. For each plot, the data saved in the files specified by l will be loaded and plotted.
#' @param image.dir The prefered output directory for the generated images. Include the final '/' on linux/mac and '\' on windows.
#' @param hash The code used for the unique identification of the composite images. Default is CRC32 hash of l.
#' @import Matrix lattice ggplot2 RColorBrewer diagram digest
#' @importFrom XLConnect loadWorkbook saveWorkbook createSheet writeWorksheet
#' @export
#' @examples Analysis.Convergence()
Analysis.Convergence = function(l=NULL, image.dir='/Users/TKelly/Dropbox/Images/', hash=digest(runif(1),algo='crc32') ) {
    
    # TODO: TBK
    #   Allow these plots to be made for any flow, not just GPP.
    
    ######################## Individual Analyses ############################
    
    for (index in c(1:length(l))) {
        
        ## Load Data
        load(paste('./data/Solution-', l[index], '.RData', sep=''))
        load(paste('./data/Model-', l[index], '.RData', sep=''))
        
        ## GPP History plot
        #   Plot of GPP over the course of the model run. Upon convergence the GPP signal should have no obvious trends or patterns (i.e. random).
        #
        tempname = paste(image.dir, "Convergence-", l[index], ".eps", sep='')
        postscript(onefile=FALSE, tempname)
        plot(c(1:length(solution$X[,1])), solution$X[,1], xlim=c(1,length(solution$X[,1])) 
             ,type='l', main=paste("History of GPP: cycle", model$cycle), 
             xlab=paste("Solution number (x",solution$iter.length/solution$out.length,")"), 
             ylab="GPP to Phy" )
        dev.off()
        
        ## Density of GPP
        #   This plot shows the density plots of the first and second half of the data sets of GPP. The two densities should be quite similar and overlapping.
        #
        tempname = paste(image.dir, 'Convergence2', l[index], ".eps",sep='')
        postscript(onefile=FALSE, tempname)
        col.count = ncol(solution$X)
        xx = density(solution$X[1:(col.count/2),1])
        xy = density(solution$X[(col.count/2):col.count,1])
        plot(xy, main=paste("Distribution of GPP values: cycle", model$cycle), 
             xlab="Carbon Flux", ylim=c(0,max(xy$y,xx$y)), 
             xlim=c(min(xy$x,xx$x),max(xy$x,xx$x)), cex.lab = 1.6)
        polygon(xy, col="black")
        polygon(xx, col="red")
        lines(xy, col="black")
        legend(max(max(xy$x)*.95, max(xx$x)*.95),max(max(xy$y),max(xx$y)),
               c("First Set","Second Set"), 
               lty=c(1,1), col=c("red","black"), bty="n")
        dev.off()
    }
    
    ######################## Group Analyses ############################
    
}