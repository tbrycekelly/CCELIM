#' Raw Analysis
#'
#' This script is designed to run and generate the analyses based on saved data. 
#' @param l A list of the data sets to analyze. For each plot, the data saved in the files specified by l will be loaded and plotted.
#' @param dir The prefered output directory for the generated images. Include the final '/' on linux/mac and '\' on windows.
#' @param hash The code used for the unique identification of the composite images. Default is CRC32 hash of l.
#' @import Matrix lattice ggplot2 RColorBrewer diagram digest
#' @importFrom XLConnect loadWorkbook saveWorkbook createSheet writeWorksheet
#' @export
#' @examples Analysis.Raw()
Analysis.Raw = function(l=NULL, image.dir='/Users/TKelly/Dropbox/Images/', hash=digest(runif(1),algo='crc32')) {
    for (index in c(1:length(l))) {
        load(paste('./data/Solution-', l[index], '.RData', sep=''))
        load(paste('./data/Model-', l[index], '.RData', sep=''))
        
        ## Surface Flow Dotplot
        tempname = paste(image.dir, 'Flows-', l[index], ".eps", sep='')
        postscript(onefile=FALSE, tempname, height=1280, width=720)
        dotchart(x = as.vector(solution$avg), labels=model$flows$flow, xlab="Carbon Flux (mg C /m^2 /d)", main=paste("Flows in Upper Layer: cycle", model$cycle) , pch=16, xlim=c(0,max(solution$avg[1:54]+solution$sd[1:54])) )
        segments(solution$avg-solution$sd, 1:length(solution$avg), solution$avg+solution$sd, 1:length(solution$avg))
        dev.off()
    }
}