#' Correlation Analysis
#'
#' This script is designed to run and generate the analyses based on correlation between flows in the solution sets.
#' @param l A list of the data sets to analyze. For each plot, the data saved in the files specified by l will be loaded and plotted.
#' @param dir The prefered output directory for the generated images. Include the final '/' on linux/mac and '\' on windows.
#' @param hash The code used for the unique identification of the composite images. Default is CRC32 hash of l.
#' @import Matrix lattice ggplot2 RColorBrewer diagram digest
#' @importFrom XLConnect loadWorkbook saveWorkbook createSheet writeWorksheet
#' @export
#' @examples Analysis.Correlation()
Analysis.Correlation = function(l=NULL, image.dir='/Users/TKelly/Dropbox/Images/', hash=digest(runif(1),algo='crc32')) {
    
    ######################## Individual Analyses ############################
    
    for (index in c(1:length(l))) {
        load(paste('./data/Solution-', l[index], '.RData', sep=''))
        load(paste('./data/Model-', l[index], '.RData', sep=''))
        flow = ReadFlows(model,solution)
        
        ## Correlation in Compartments
        tempname = paste(image.dir, 'Correlation_Net-', l[index], ".eps", sep='')
        postscript(onefile=FALSE, tempname)
        rgb.palette <- colorRampPalette(c("blue","white", "red"), space = "rgb")
        thisplot=levelplot(cor(flow), main=paste("Correlation Between Compartments: cycle", model$cycle), xlab="Flow", ylab="Flow", 
                           col.regions=rgb.palette(250), cuts=500, at=seq(-1,1,0.05))
        print(thisplot)
        dev.off()
        
        ## Correlation in carbon flows
        tempname <- paste(image.dir, 'Correlation_Flows-', l[index], ".eps", sep='')
        postscript(onefile=FALSE, tempname)
        rgb.palette = colorRampPalette(c("blue","white", "red"), space = "rgb")  ## TODO
        thisplot = levelplot(cor(solution$X[]), main=paste("Correlation Between Flows: cycle", model$cycle), xlab="Flow", ylab="Flow", col.regions=rgb.palette(250), cuts=500, at=seq(-1,1,0.01))
        print(thisplot)
        dev.off()
    }
    
    ######################## Comparison Analyses ############################
    
    if (length(l) == 2) {
        
        ## Differences between two simulation's carbon flows.
        tempname = paste(image.dir, 'Correlation_Difference-', hash, ".eps", sep='')
        postscript(onefile=FALSE, tempname)
        load(paste("./data/Solution-", l[1], ".RData", sep=''))
        cor1 = cor(solution$X[])
        load(paste("./data/Solution-", l[2], ".RData", sep=''))
        cor2 = cor(solution$X[])
        rgb.palette <- colorRampPalette(c("blue","white", "red"), space = "rgb")
        thisplot=levelplot(cor1-cor2, main=paste("Correlation Differences: hashes"), xlab="Flow", ylab="Flow", col.regions=rgb.palette(250), cuts=500, at=seq(-1,1,0.01))
        print(thisplot)
        dev.off()
    }
}