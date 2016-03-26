#' Indirect Analysis
#'
#' This script is designed analyze the set of runs <l> against ecosystem metrics.
#' @param l A list of the data sets to analyze. For each plot, the data saved in the files specified by l will be loaded and plotted.
#' @param dir The prefered output directory for the generated images. Include the final '/' on linux/mac and '\' on windows.
#' @param hash The code used for the unique identification of the composite images. Default is CRC32 hash of l.
#' @import Matrix lattice ggplot2 RColorBrewer diagram XLConnect
#' @export
#' @examples Analysis.Indirect()
#' 
Analysis.Indirect = function(l=NULL, image.dir='/Users/TKelly/Dropbox/Images/', hash=digest(runif(1),algo='crc32')) {
    for ( index in 1:length(l) ) {
        ## Load seperate data
        load(paste('./data/Solution-', l[index], '.RData', sep=''))
        load(paste('./data/Model-', l[index], '.RData', sep=''))
        
        flow = ReadFlows(model, solution)
        wb = loadWorkbook(paste("./data/Indirect-", l[index],".xlsx", sep=''), create=TRUE)
        
        #Normalize
        for (i in 1:length(flow[1,])) {
            flow[,i] = flow[,i]/sum(flow[,i])
        }
        flow[is.na(flow)] = 0
        
        indirect = solve(diag( length(flow[,1]) ) - flow)   
        indirect[diag(length(indirect[,1])) == 1] = 0   ## Set diagonal to zero
        indirect[flow > 0] = 0  ## Remove direct flows
        createSheet(wb, name="Indirect")
        writeWorksheet(wb, cbind(colnames(flow),indirect), sheet="Indirect")
        
        createSheet(wb, name="Indirect NC")
        indirectNC = apply(indirect, 2, function (x) x/max(x))
        writeWorksheet(wb, cbind(colnames(flow),indirectNC), sheet="Indirect NC")
        saveWorkbook(wb)
    }
    
}