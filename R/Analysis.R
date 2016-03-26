#' Analysis Suite
#'
#' This script is designed to run and generate the analyses based on saved data. 
#' @param l A list of the data sets to analyze. For each plot, the data saved in the files specified by l will be loaded and plotted.
#' @param dir The prefered output directory for the generated images. Include the final '/' on linux/mac and '\' on windows.
#' @param hash The code used for the unique identification of the composite images. Default is CRC32 hash of l.
#' @import Matrix lattice ggplot2 RColorBrewer diagram digest
#' @importFrom XLConnect loadWorkbook saveWorkbook createSheet writeWorksheet
#' @export
#' @examples Analysis
Analysis = function(l=NULL, image.dir='/Users/TKelly/Dropbox/Images/', hash=digest(runif(1),algo='crc32')) {
    if (length(l) < 1)
        stop("Analysis requires a list of simulation names to continue.")
    
    if (is.null(hash))
        hash = digest(paste(l), algo='crc32')
    
    cat('The output name for compilation images is: ', hash, '\n')
    cat('Analyzing data for <Raw>...\n')
    Analysis.Raw(l, image.dir, hash)
    cat('Analyzing data for <Convergence>...\n')
    Analysis.Convergence(l, image.dir, hash)
    cat('Analyzing data for <Grazing>...\n')
    Analysis.Grazing(l, image.dir, hash)
    cat('Analyzing data for <Deep>...\n')
    Analysis.Deep(l, image.dir, hash)
    cat('Analyzing data for <Ecosystem>...\n')
    Analysis.Ecosystem(l, image.dir, hash)
    #cat('Analyzing data for <Correlation>...\n')
    #Analysis.Correlation(l, image.dir, hash)
    cat('Analyzing data for <Approximate>...\n')
    Analysis.Approximate(l, image.dir, hash)
    cat('Analyzing data for <Indirect>...\n')
    Analysis.Indirect(l, image.dir, hash)
}






    


