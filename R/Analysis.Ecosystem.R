#' Ecosystem Analysis
#'
#' This script is designed analyze the set of runs <l> against ecosystem metrics.
#' @param l A list of the data sets to analyze. For each plot, the data saved in the files specified by l will be loaded and plotted.
#' @param dir The prefered output directory for the generated images. Include the final '/' on linux/mac and '\' on windows.
#' @param hash The code used for the unique identification of the composite images. Default is CRC32 hash of l.
#' @import Matrix lattice ggplot2 RColorBrewer diagram
#' @export
#' @examples Analysis.Ecosystem()
#' 
Analysis.Ecosystem = function(l=NULL, image.dir='/Users/TKelly/Dropbox/Images/', hash=digest(runif(1),algo='crc32')) {
    for (index in c(1:length(l))) {
        ## Load seperate data
        load(paste('./data/Solution-', l[index], '.RData', sep=''))
        load(paste('./data/Model-', l[index], '.RData', sep=''))
        flow = ReadFlows(model, solution)
        
        ## Food Web ##
        #tempname = paste(image.dir, "FoodWeb-", l[index], ".eps", sep='')
        #postscript(onefile=FALSE, tempname)
        #plotweb(flow, legend=FALSE)
        #dev.off()
    }
    
#     ## GGE Set ##
#     tempname = paste(image.dir, "GGE_Set-", hash, ".eps", sep='')
#     postscript(onefile=FALSE, tempname, height=640, width=1200)
#     par(mar=c(8,6,6,5))
#     plot(x=c(1:9), y=NULL,cex.lab=1.8, ylim=c(0,0.5), col="red",yaxt='n', xaxt="n", xlab="", ylab="GGE ± SD", main=paste("Gross Growth Efficiencies"))
#     GGElab = c("Hnf", "Mic", "Smz", "Lmz", "Deep Hnf", "Deep Mic", "Gel", "Bac", "Deep Bac")
#     Gmin = c( .1, .1,.1,.1,.1,.1,.1,.05,.05)
#     Gmax = c( .4,.4,.4,.4,.4,.4,.4,.3,.3)
#     axis(side=1, labels=GGElab, cex.axis=1.8 ,at=c(1:9), las=2)
#     axis(side=2, at=c(0.1,0.2, 0.3, 0.4, 0.5), cex.axis=2)
#     rect(1:9-.2, Gmin, 1:9+.2, Gmax,density=5, col="white", border="black", lty=4)
#     
#     for (i in c(1:length(l))) {
#         load(paste('./data/Solution-', l[i], '.RData', sep=''))
#         t1 = as.vector((solution$X[,2]+solution$X[,7]+solution$X[,8] )/solution$X[,1]) ##GGE = (gppTophy-phyToRes) / gppTOphy
#         #HNF
#         t2 = 1-(solution$X[,11]+solution$X[,12]+solution$X[,13] )/(solution$X[,3]+solution$X[,43]+solution$X[,48]+solution$X[,45])
#         #MIC
#         t3 = 1-(solution$X[,14]+solution$X[,17]+solution$X[,18])/(solution$X[,4] + solution$X[,9] +solution$X[,49] + solution$X[,44] + solution$X[,46])
#         #SMZ
#         t4 = 1-(solution$X[,21]+solution$X[,22]+solution$X[,23])/(solution$X[,5] + solution$X[,10] + solution$X[,15]+solution$X[,50] )
#         #LMZ
#         t5 = 1-(solution$X[,27] + solution$X[,28]+solution$X[,29])/(solution$X[,6] + solution$X[,19] + solution$X[,16] + solution$X[,51] + solution$X[113])
#         #dHNF
#         t6 = 1- (solution$X[,60] + solution$X[,61] + solution$X[,62])/(solution$X[,93]  +solution$X[,95] + solution$X[,98])
#         #dMIC
#         t7 = 1- (solution$X[,65] + solution$X[,66] + solution$X[,67]) / (solution$X[,63] + solution$X[,94] + solution$X[,96] + solution$X[,99])
#         #GEL
#         t8 = 1- (solution$X[,30] + solution$X[,31] + solution$X[,32] + solution$X[,80] + solution$X[,81] + solution$X[,82])/(solution$X[,24] + solution$X[,77] + solution$X[,120])
#         #BAC
#         t9 = 1- (solution$X[,42])/(solution$X[,53])
#         #dBAC
#         t10 = 1- (solution$X[,92])/(solution$X[,103])
#         GGE = data.frame(t2,t3,t4,t5,t6,t7,t8,t9,t10)
#         names(GGE) = GGElab[1:9]
#         GGEavg = apply(GGE, 2, mean)
#         GGEsd = apply(GGE, 2, sd)
#         segments(seq(1-0.20+0.35*i/length(l),9-0.2+0.35*i/length(l)),GGEavg-GGEsd , seq(1-0.20+0.35*i/length(l),9-0.2+0.35*i/length(l)), GGEavg+GGEsd, col="red", lty=1)
#     }
#     dev.off()
    
    ## GGE Set ##
    tempname = paste(image.dir, "GGE_Set_Ex-", hash, ".eps", sep='')
    postscript(onefile=FALSE, tempname, height=640, width=1200)
    par(mar=c(8,6,6,5))
    plot(x=c(1:11), y=NULL,cex.lab=1.8, ylim=c(0,0.5), col="red",yaxt='n', xaxt="n", xlab="", ylab="GGE ± SD", main=paste("Gross Growth Efficiencies"))
    GGElab = c("Hnf", "Mic", "Smz", "Lmz", "Deep Hnf", "Deep Mic", "Gel", "Bac", "Deep Bac", "vmSMZ", "vmLMZ")
    Gmin = c( .1, .1,.1,.1,.1,.1,.1,.05,.05, .1, .1)
    Gmax = c( .4,.4,.3,.3,.4,.4,.4,.3,.3, .3, .3)
    axis(side=1, labels=GGElab, cex.axis=1.8 ,at=c(1:11), las=2)
    axis(side=2, at=c(0.1,0.2, 0.3, 0.4, 0.5), cex.axis=2)
    rect(1:11-.2, Gmin, 1:11+.2, Gmax,density=5, col="white", border="black", lty=4)
    
    for (i in c(1:length(l))) {
        load(paste('./data/Solution-', l[i], '.RData', sep=''))
        t1 = as.vector((solution$X[,2]+solution$X[,7]+solution$X[,8] )/solution$X[,1]) ##GGE = (gppTophy-phyToRes) / gppTOphy
        #HNF
        t2 = 1-(solution$X[,11]+solution$X[,12]+solution$X[,13] )/(solution$X[,3]+solution$X[,43]+solution$X[,48]+solution$X[,45])
        #MIC
        t3 = 1-(solution$X[,14]+solution$X[,17]+solution$X[,18])/(solution$X[,4] + solution$X[,9] +solution$X[,49] + solution$X[,44] + solution$X[,46])
        #SMZ
        t4 = 1-(solution$X[,21]+solution$X[,22]+solution$X[,23])/(solution$X[,5] + solution$X[,10] + solution$X[,15]+solution$X[,50] )
        #LMZ
        t5 = 1-(solution$X[,27] + solution$X[,28]+solution$X[,29]) / (solution$X[,6] + solution$X[,19] + solution$X[,16] + solution$X[,51] + solution$X[,113])
        #dHNF
        t6 = 1- (solution$X[,60] + solution$X[,61] + solution$X[,62])/(solution$X[,93]  +solution$X[,95] + solution$X[,98])
        #dMIC
        t7 = 1- (solution$X[,65] + solution$X[,66] + solution$X[,67]) / (solution$X[,94] + solution$X[,96] + solution$X[,99])
        #GEL
        t8 = 1- (solution$X[,30] + solution$X[,31] + solution$X[,32] + solution$X[,80] + solution$X[,81] + solution$X[,82])/(solution$X[,24] + solution$X[,77] + solution$X[,120])
        #BAC
        t9 = 1- (solution$X[,42])/(solution$X[,53])
        #dBAC
        t10 = 1- (solution$X[,92])/(solution$X[,103])
        #vmSMZ
        t11 = 1- (solution$X[,70]+solution$X[,71]+solution$X[,72]+solution$X[,116]+solution$X[,117]+solution$X[,119])/(solution$X[,106]+solution$X[,108]+solution$X[,109]+solution$X[,110]+solution$X[,100]+solution$X[,68]+solution$X[,64])
        #vmLMZ
        t12 = 1- (solution$X[,74]+solution$X[,75]+solution$X[,76]+solution$X[,123]+solution$X[,124]+solution$X[,125])/(solution$X[,111]+solution$X[,112]+solution$X[,107]+solution$X[,114]+solution$X[,118]+solution$X[,101]+solution$X[,73]+solution$X[,69])

        GGE = data.frame(t2,t3,t4,t5,t6,t7,t8,t9,t10, t11, t12)
        names(GGE) = GGElab[1:11]
        GGEavg = apply(GGE, 2, mean)
        GGEsd = apply(GGE, 2, sd)
        segments(seq(1-0.20+0.35*i/length(l),11-0.2+0.35*i/length(l)),GGEavg-GGEsd , seq(1-0.20+0.35*i/length(l),11-0.2+0.35*i/length(l)), GGEavg+GGEsd, col="red", lty=1)
    }
    dev.off()
}