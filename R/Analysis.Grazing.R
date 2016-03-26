#' Grazing Analysis
#'
#' This script is designed analyze the set of runs <l> against the approimate equations of the model.
#' @param l A list of the data sets to analyze. For each plot, the data saved in the files specified by l will be loaded and plotted.
#' @param dir The prefered output directory for the generated images. Include the final '/' on linux/mac and '\' on windows.
#' @param hash The code used for the unique identification of the composite images. Default is CRC32 hash of l.
#' @import Matrix lattice ggplot2 RColorBrewer diagram digest
#' @importFrom XLConnect loadWorkbook saveWorkbook createSheet writeWorksheet
#' @export
#' @examples Analysis.Grazing()
Analysis.Grazing = function(l=NULL, image.dir='/Users/TKelly/Dropbox/Images/', hash=digest(runif(1),algo='crc32')) {
    
    ######################## Individual Analyses ############################
    
    ######################## Group Analyses ############################
    
    ## NPP Grazing ~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    tempname = paste(image.dir, 'NPP-', hash,".eps",sep='')
    postscript(onefile=FALSE, tempname)
    par(mar=c(5.1, 7.1, 4.1, 2.1))
    
    ## Determine the axis limits
    max = 0
    min = 0
    for (i in 1:length(l)) {
        load(paste("./data/Solution-", l[i], ".RData", sep=''))
        load(paste("./data/Model-", l[i], ".RData", sep=''))
        
        temp = model$ba[1] + model$sdb[1]
        if (max < temp) max = temp
        temp = model$ba[1] - model$sdb[1]
        if (min > temp) min = temp
        temp = (model$Aa %*% solution$avg + model$Aa %*% solution$sd)[1]
        if (max < temp) max = temp 
        temp = (model$Aa %*% solution$avg - model$Aa %*% solution$sd)[1]
        if (min > temp) min = temp 
    }
    min = 1.1*min
    max = 1.1*max
    
    plot(x=c(min:max), y=c(min:max), type='l',lty=2 , pch=16, ylim=c(min,max),xlim = c(min,max),cex.lab=1.4, 
         ylab=expression(Model~Carbon~Flux~(mg~C~m^{-2}~d^{-1})),
         xlab=expression(Measured~Carbon~Flux~(mg~C~m^{-2}~d^{-1})),
         main="Net Primary Production +/- SD")
    
    for (i in c(1:length(l))) {
        load(paste("./data/Solution-", l[i], ".RData", sep=''))
        load(paste("./data/Model-", l[i], ".RData", sep=''))
        RX = model$Aa %*% solution$avg
        RS = model$Aa %*% solution$sd
        points(model$ba[1], RX[1])
        text(model$ba[1]-50, RX[1]*1.07+70, labels=paste(l[i]))
        segments(model$ba[1]+model$sdb[1], RX[1], model$ba[1]-model$sdb[1], RX[1])
        segments(model$ba[1]+model$sdb[1], RX[1]*0.98, model$ba[1]+model$sdb[1], RX[1]*1.02)
        segments(model$ba[1]-model$sdb[1], RX[1]*0.98, model$ba[1]-model$sdb[1], RX[1]*1.02)
        segments(model$ba[1], RX[1]+RS[1], model$ba[1], RX[1]-RS[1], col="red")
        segments(model$ba[1]*0.98, RX[1]+RS[1], model$ba[1]*1.02, RX[1]+RS[1], col="red")
        segments(model$ba[1]*0.98, RX[1]-RS[1], model$ba[1]*1.02, RX[1]-RS[1], col="red")
    }
    dev.off()
    
    ## MIC Grazing ~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    tempname = paste(image.dir, 'GrMic-', hash, ".eps", sep='')
    postscript(onefile=FALSE, tempname)
    par(mar=c(5.1, 7.1, 4.1, 2.1))
    
    ## Determine the axis limits
    max = 0
    min = 0
    for (i in c(1:length(l))) {
        load(paste("./data/Solution-", l[i], ".RData", sep=''))
        load(paste("./data/Model-", l[i], ".RData", sep=''))
        
        temp = model$ba[2] + model$sdb[2]
        if (max < temp) max = temp
        temp = model$ba[2] - model$sdb[2]
        if (min > temp) min = temp
        
        temp = (model$Aa %*% solution$avg + model$Aa %*% solution$sd)[2]
        if (max < temp) max = temp 
        temp = (model$Aa %*% solution$avg - model$Aa %*% solution$sd)[2]
        if (min > temp) min = temp
    }
    max = 1.1*max
    min = 1.1*min
    
    plot(x=c(min:max), y=c(min:max), type='l',lty=2 , pch=16, ylim=c(min,max),xlim = c(min,max),cex.lab=1.4, 
         ylab=expression(Model~Grazing~Rate~(mg~C~m^{-2}~d^{-1})),
         xlab=expression(Measured~Grazing~Rate~(mg~C~m^{-2}~d^{-1})),
         main="Mic Grazing Rate +/- SD")
    
    for (i in c(1:length(l))) {
        load(paste("./data/Solution-", l[i], ".RData", sep=''))
        load(paste("./data/Model-", l[i], ".RData", sep=''))
        RX = model$Aa %*% solution$avg
        RS = model$Aa %*% solution$sd
        
        points(model$ba[2], RX[2])
        text(model$ba[2]+40, RX[2]-150, labels=paste(l[i]))
        segments(model$ba[2]+model$sdb[2], RX[2]*0.98, model$ba[2]+model$sdb[2], RX[2]*1.02)
        segments(model$ba[2]+model$sdb[2], RX[2], model$ba[2]-model$sdb[2], RX[2])
        segments(model$ba[2]-model$sdb[2], RX[2]*0.98, model$ba[2]-model$sdb[2], RX[2]*1.02)
        segments(model$ba[2], RX[2]+RS[2], model$ba[2], RX[2]-RS[2], col="red")
        segments(model$ba[2]*0.98, RX[2]+RS[2], model$ba[2]*1.02, RX[2]+RS[2], col="red")
        segments(model$ba[2]*0.98, RX[2]-RS[2], model$ba[2]*1.02, RX[2]-RS[2], col="red")
    }
    dev.off()
    
    ## SMZ Grazing ~~~~~~~~~~~~~~~~~~~~~~~~~~
    tempname = paste(image.dir, "GrSMZ", hash,".eps",sep='')
    postscript(onefile=FALSE, tempname)
    par(mar=c(5.1, 7.1, 4.1, 2.1))
    
    ## Determine the axis limits
    max = 0
    min = 0
    for (i in c(1:length(l))) {
        load(paste("./data/Solution-", l[i], ".RData", sep=''))
        load(paste("./data/Model-", l[i], ".RData", sep=''))
        
        temp = model$ba[3] + model$sdb[3]
        if (max < temp) max = temp
        temp = model$ba[3] - model$sdb[3]
        if (min > temp) min = temp
        
        temp = (model$Aa %*% solution$avg + model$Aa %*% solution$sd)[3]
        if (max < temp) max = temp 
        temp = (model$Aa %*% solution$avg - model$Aa %*% solution$sd)[3]
        if (min > temp) min = temp
    }
    max = 1.1*max
    min = 1.1*min
    
    plot(x=c(min:max), y=c(min:max), type='l',lty=2 , pch=16, ylim=c(min,max),xlim = c(min,max),cex.lab=1.4, 
         ylab=expression(Model~Grazing~Rate~(mg~C~m^{-2}~d^{-1})),
         xlab=expression(Measured~Grazing~Rate~(mg~C~m^{-2}~d^{-1})),
         main="SMZ Grazing Rate +/- SD")
    
    for (i in c(1:length(l))) {
        load(paste("./data/Solution-", l[i], ".RData", sep=''))
        load(paste("./data/Model-", l[i], ".RData", sep=''))
        RX = model$Aa %*% solution$avg
        RS = model$Aa %*% solution$sd
        
        points(model$ba[3], RX[3])
        text(model$ba[3]+50, RX[3]*0.9-70, labels=paste(l[i]))
        segments(model$ba[3]+model$sdb[3], RX[3]*0.98, model$ba[3]+model$sdb[3], RX[3]*1.02)
        segments(model$ba[3]+model$sdb[3], RX[3], model$ba[3]-model$sdb[3], RX[3])
        segments(model$ba[3]-model$sdb[3], RX[3]*0.98, model$ba[3]-model$sdb[3], RX[3]*1.02)
        segments(model$ba[3], RX[3]+RS[3], model$ba[3], RX[3]-RS[3], col="red")
        segments(model$ba[3]*0.98, RX[3]+RS[3], model$ba[3]*1.02, RX[3]+RS[3], col="red")
        segments(model$ba[3]*0.98, RX[3]-RS[3], model$ba[3]*1.02, RX[3]-RS[3], col="red")
    }
    dev.off()
    
    ## LMZ Grazing ~~~~~~~~~~~~~~~~~~~~~~~~~~
    tempname = paste(image.dir, "GrLMZ", hash,".eps",sep='')
    postscript(onefile=FALSE, tempname)
    par(mar=c(5.1, 7.1, 4.1, 2.1))
    
    ## Determine the axis limits
    max = 0
    min = 0
    for (i in 1:length(l)) {
        load(paste("./data/Solution-", l[i], ".RData", sep=''))
        load(paste("./data/Model-", l[i], ".RData", sep=''))
        
        temp = model$ba[4] + model$sdb[4]
        if (max < temp) max = temp
        temp = model$ba[4] - model$sdb[4]
        if (min > temp) min = temp
        temp = (model$Aa %*% solution$avg + model$Aa %*% solution$sd)[4]
        if (max < temp) max = temp 
        temp = (model$Aa %*% solution$avg - model$Aa %*% solution$sd)[4]
        if (min > temp) min = temp
    }
    max = 1.1*max
    min = 1.1*min
    
    plot(x=c(min:max), y=c(min:max), type='l',lty=2 , pch=16, ylim=c(min,max),xlim = c(min,max),cex.lab=1.4, 
         ylab=expression(Model~Grazing~Rate~(mg~C~m^{-2}~d^{-1})),
         xlab=expression(Measured~Grazing~Rate~(mg~C~m^{-2}~d^{-1})),
         main="LMZ Grazing Rate +/- SD")
    
    for (i in c(1:length(l))) {
        load(paste("./data/Solution-", l[i], ".RData", sep=''))
        load(paste("./data/Model-", l[i], ".RData", sep=''))
        RX = model$Aa %*% solution$avg
        RS = model$Aa %*% solution$sd
        
        points(model$ba[4], RX[4])
        text(model$ba[4]-25, RX[4]*1.0+30, labels=paste(l[i]))
        segments(model$ba[4]+model$sdb[4], RX[4]*0.98, model$ba[4]+model$sdb[4], RX[4]*1.02)
        segments(model$ba[4]+model$sdb[4], RX[4], model$ba[4]-model$sdb[4], RX[4])
        segments(model$ba[4]-model$sdb[4], RX[4]*0.98, model$ba[4]-model$sdb[4], RX[4]*1.02)
        segments(model$ba[4], RX[4]+RS[4], model$ba[4], RX[4]-RS[4], col="red")
        segments(model$ba[4]*0.98, RX[4]+RS[4], model$ba[4]*1.02, RX[4]+RS[4], col="red")
        segments(model$ba[4]*0.98, RX[4]-RS[4], model$ba[4]*1.02, RX[4]-RS[4], col="red")
    }
    dev.off()
}