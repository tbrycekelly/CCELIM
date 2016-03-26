#' Approimate Analysis
#'
#' This script is designed analyze the set of runs <l> against the approimate equations of the model.
#' @param l A list of the data sets to analyze. For each plot, the data saved in the files specified by l will be loaded from <./data/Model-...> and plotted.
#' @param image.dir The prefered output directory for the generated images. Include the final '/' on linux/mac and '\' on windows.
#' @param hash The code used for the unique identification of the composite images. Default is CRC32 hash of l.
#' @import Matrix lattice ggplot2 RColorBrewer diagram digest
#' @importFrom XLConnect loadWorkbook saveWorkbook createSheet writeWorksheet
#' @export
#' @examples Analysis.Approximate('Test01')
#' 
Analysis.Approximate = function(l=NULL, image.dir='/Users/TKelly/Dropbox/Images/', hash=digest(runif(1),algo='crc32')) {
    
    ######################## Individual Analyses ############################
    
    ######################## Group Analyses ############################
    
    load(paste("./data/Model-", l[1], ".RData", sep=''))
    num.approx = length(model$ba)
    
    ## SDs from measured value.
    tempname = paste(image.dir, "AppSD_Set_8s-", hash, ".eps", sep='')
    postscript(onefile=FALSE, tempname)
    par(mar=c(5,5,5,5))
    
    ## Setup plotting area
    plot(NULL, pch=16, ylim=c(-8,8), xlim=c(1,num.approx),xaxt='n', ylab="SD in Measurement", main="SD away from Data")
    axis(side=1, at=c(1:length(model$ba)), las=2, labels=c("C14 PP", "Mic Gr", "SMZ Gr", "vmSMZ Gr", "LMZ Gr", "vmLMZ Gr", "ST Flux", "Thorium", "Bac Prod", "Myc Res", "dMyc Res", "Myc Fecal", "Myc Mort", "nvm Res", "nvm Fecal", "nvm Mort"))
    max = rep(0,length(model$ba))
    min = max
    
    for (i in c(1:length(l))) { # Loop for each run in l
        ## Load data
        load(paste("./data/Solution-", l[i], ".RData", sep=''))
        load(paste("./data/Model-", l[i], ".RData", sep=''))
        
        ## Calculate model b vectors and find mean and SD.
        Ra=NULL
        Ra$X = apply(solution$X, 1, function(x) model$Aa %*% x)
        Ra$avg = (apply(Ra$X, 1, mean)-model$ba)/model$sdb
        Ra$sd = apply(Ra$X, 1, sd)/model$sdb
        
        ## Plot results
        points(x=(1:length(model$ba))-0.5+0.7*i/length(l),y=Ra$avg, pch=16)
        segments(1:length(model$ba)-0.5+0.7*i/length(l), Ra$avg+Ra$sd, 1:length(model$ba)-0.5+0.7*i/length(l), Ra$avg-Ra$sd)
        
        if (i ==1) {
            max = Ra$avg+Ra$sd
            min = Ra$avg-Ra$sd
        }
        else {
            larger = which(Ra$avg+Ra$sd > max)
            max[larger] = (Ra$avg+Ra$sd)[larger]
            
            smaller = which(Ra$avg-Ra$sd < min)
            min[smaller] = (Ra$avg-Ra$sd)[smaller]
        }
    }
    
    ## Draw solution bounding boxes
    for (i in 1:length(model$ba)) {
        lines(c(-0.5+i,-0.5+i, 0.45+i,0.45+i,-0.5+i),c(min[i],max[i],max[i],min[i],min[i]), col="red")
    }
    
    ## Draw horizontal lines at +-2 SD and 0
    lines(c(0,num.approx+1),c(0,0), lty=3)
    lines(c(0,num.approx+1),c(2,2), lty=3, col='red')
    lines(c(0,num.approx+1),c(-2,-2), lty=3, col='red')
    dev.off()
    
    
    
    ## SDs from model of Measured values
#     tempname = paste(image.dir, "AppSD2_Set-", hash, ".eps", sep='')
#     postscript(onefile=FALSE, tempname)
#     par(mar=c(5,5,5,5))
#     plot(NULL, pch=12, ylim=c(-12,12), xlim=c(1,14), ylab="SD in Model" ,xlab="Approximate Equation" ,main="SD away from Model")
#     axis(side=1, at=c(seq(1,13,2)))
#     for (i in c(1:length(l))) {
#         load(paste("./data/Solution-", l[i], ".RData", sep=''))
#         load(paste("./data/Model-", l[i], ".RData", sep=''))
#         Ra=NULL
#         Ra$X = apply(solution$X, 1, function(x) model$Aa %*% x)
#         Ra$sd = apply(Ra$X, 1, sd)
#         Ra$avg = (model$ba-apply(Ra$X, 1, mean))/Ra$sd
#         Ra$sd = model$sdb/Ra$sd
#         points(x=(1:length(model$ba))-0.5+0.7*i/length(l),y=Ra$avg, pch=4, col='red')
#         segments(1:length(model$ba)-0.5+0.7*i/length(l), Ra$avg+Ra$sd, 1:length(model$ba)-0.5+0.7*i/length(l), Ra$avg-Ra$sd)
#     }
#     lines(c(0,15),c(0,0), lty=3)
#     lines(c(0,15),c(2,2), lty=3, col='red')
#     lines(c(0,15),c(-2,-2), lty=3, col='red')
#     dev.off()
    
    ## SDs from measured value Zoom
#     tempname = paste(image.dir, "AppSD_SetZoom-", hash, ".eps", sep='')
#     postscript(onefile=FALSE, tempname)
#     par(mar=c(5,5,5,5))
#     plot(NULL, pch=12, xaxt='n', ylim=c(-1,1), xlim=c(1,7), ylab="SD in Measurement" ,xlab="Approximate Equation" ,main="SD away from Data")
#     axis(side=1, at=c(seq(1,7,1)), labels=c(8:14))
#     for (i in c(1:length(l))) {
#         if (length(model$ba)==14) {
#             load(paste("./data/Solution-", l[i], ".RData", sep=''))
#             load(paste("./data/Model-", l[i], ".RData", sep=''))
#             Ra=NULL
#             Ra$X = apply(solution$X, 1, function(x) model$Aa %*% x)
#             
#             Ra$avg = (apply(Ra$X, 1, mean)-model$ba)/model$sdb
#             Ra$sd = apply(Ra$X, 1, sd)/model$ba
#             points(x=(1:7)-0.2+0.35*i/length(l),y=Ra$avg[8:14], pch=16)
#             segments(1:7-0.2+0.35*i/length(l), Ra$avg[8:14]+Ra$sd[8:14], 1:7-0.2+0.35*i/length(l), Ra$avg[8:14]-Ra$sd[8:14])
#         }
#     }
#     lines(c(0,9),c(0,0), lty=3)
#     lines(c(0,15),c(2,2), lty=3, col='red')
#     lines(c(0,15),c(-2,-2), lty=3, col='red')
#     dev.off()
#     
#     ## SDs from model of Measured values Zoom
#     tempname = paste(image.dir, "AppSD_SetZoom2-", hash, ".eps", sep='')
#     postscript(onefile=FALSE, tempname)
#     par(mar=c(5,5,5,5))
#     plot(x=NULL,y=NULL, pch=12, xaxt='n', ylim=c(-3,3), xlim=c(1,7), ylab="SD in Model" ,xlab="Approximate Equation" ,main="SD away from Model")
#     axis(side=1, at=c(seq(1,7,1)), labels=c(8:14))
#     for (i in c(1:length(l))) {
#         if (length(model$ba) == 14) {
#             load(paste("./data/Solution-", l[i], ".RData", sep=''))
#             load(paste("./data/Model-", l[i], ".RData", sep=''))
#             Ra=NULL
#             Ra$X = apply(solution$X, 1, function(x) model$Aa %*% x)
#             Ra$sd = apply(Ra$X, 1, sd)
#             Ra$avg = (model$ba-apply(Ra$X, 1, mean))/Ra$sd
#             Ra$sd = model$sdb/Ra$sd
#             points(x=(1:7)-0.2+0.35*i/length(l),y=Ra$avg[8:14], pch=16)
#             segments(1:7-0.2+0.35*i/length(l), Ra$avg[8:14]+Ra$sd[8:14], 1:7-0.2+0.35*i/length(l), Ra$avg[8:14]-Ra$sd[8:14])
#         }
#     }
#     lines(c(0,9),c(0,0), lty=3)
#     lines(c(0,15),c(2,2), lty=3, col='red')
#     lines(c(0,15),c(-2,-2), lty=3, col='red')
#     dev.off()
}