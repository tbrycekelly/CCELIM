#' Deep Analysis
#'
#' This script is designed analyze the set of runs <l> for aspects of the 2 layer ecosystem.
#' @param l A list of the data sets to analyze. For each plot, the data saved in the files specified by l will be loaded and plotted.
#' @param dir The prefered output directory for the generated images. Include the final '/' on linux/mac and '\' on windows.
#' @param hash The code used for the unique identification of the composite images. Default is CRC32 hash of l.
#' @import Matrix lattice ggplot2 RColorBrewer diagram digest
#' @export
#' @examples Analysis.Deep()
Analysis.Deep = function(l=NULL, image.dir='/Users/TKelly/Dropbox/Images/', hash=digest(runif(1),algo='crc32')) {
    
    ######################## Individual Analyses ############################
    
    ######################## Group Analyses ############################
    
    ## Source of Carbon to Deep ##
    tempname = paste(image.dir, "Deep_Sources-", hash,".eps",sep='')
    postscript(onefile=FALSE, tempname)
    smz = NULL
    lmz = NULL
    gel = NULL
    myc = NULL
    sdt = NULL
    ldt = NULL
    labels = c("Smz", "Lmz", "Gel", "VM Myc", "Sdt", "Ldt")
    for (i in c(1:length(l))) {
        load(paste("./data/Solution-", l[i], ".RData", sep=''))
        smz[i] = solution$avg[56]
        lmz[i] = solution$avg[57]
        gel[i] = solution$avg[58]
        myc[i] = solution$avg[59]
        sdt[i] = solution$avg[54]
        ldt[i] = solution$avg[55]
    }
    #colors = RColorBrewer::brewer.pal(6, 'RdYlGn')
    colors = c("#FF5050", "#F08080", "#e0e0e0", "#f0f0f0", "#3030f0", "#3535d3")
    par(mar=c(5,5,5,4))
    barplot(rbind(smz,lmz,gel,myc,sdt,ldt), col=colors, main="Source of Carbon to Deep", ylim=c(0,2*max(smz, lmz, gel, myc, sdt, ldt)) ,density=NULL, xlab="Equation", ylab="Carbon Flux (mg C m^-2 d^-1)")
    par(mar=c(3,3,3,4))
    legend(5,max(smz,lmz,gel,myc,sdt,ldt)*1.2,labels,lty=1, col=colors, lwd=15)
    #points(Ra$avg, pch=5, col="red")
    dev.off()
    
    ## Source of Carbon to Deep ##
    tempname = paste(image.dir, "Deep_SourcesRel-", hash,".eps",sep='')
    postscript(onefile=FALSE, tempname)
    smz = NULL
    lmz = NULL
    gel = NULL
    myc = NULL
    sdt = NULL
    ldt = NULL
    labels = c("Smz", "Lmz", "Gel", "VM Myc", "Sdt", "Ldt")
    for (i in c(1:length(l))) {
        if (!is.na(l[i])) {
            load(paste("./data/Solution-", l[i], ".RData", sep=''))
            smz[i] = solution$avg[56]
            lmz[i] = solution$avg[57]
            gel[i] = solution$avg[58]
            myc[i] = solution$avg[59]
            sdt[i] = solution$avg[54]
            ldt[i] = solution$avg[55]
        }
    }
    dat=NULL
    dat = as.matrix(data.frame(smz,lmz,gel,myc,sdt,ldt))
    dat2=dat
    for (i in c(1:length(l))) {
        dat2[i,] = dat2[i,]/sum(dat2[i,])
    }
    #colors = RColorBrewer::brewer.pal(6, 'RdYlGn')
    par(mar=c(5,5,5,4))
    barplot(t(dat2), col=colors, main="Source of Carbon to Deep" ,density=NULL, xlab="Equation", ylab="Rel Carbon Flux")
    par(mar=c(3,3,3,4))
    axis(1, at=c(1:length(l)), c(1:length(l)))
    dev.off()
    
    ## Source of Carbon to Deep ##
    tempname = paste(image.dir, "Deep_SourcesRel2-", hash,".eps",sep='')
    postscript(onefile=FALSE, tempname)
    dat2=dat
    for (i in c(1:6)) {
        dat2[,i] = dat2[,i]/sum(dat2[,i])
    }
    #colors = RColorBrewer::brewer.pal(length(l), 'RdYlGn')
    par(mar=c(5,5,5,4))
    barplot(dat2, col=colors, main="Source of Carbon to Deep" ,density=NULL, xlab="Equation", ylab="Rel Carbon Flux")
    par(mar=c(3,3,3,4))
    dev.off()
}