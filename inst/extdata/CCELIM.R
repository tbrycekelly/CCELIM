#' LIM
#'
#' This script is designed to run and generate the analyses based on saved data. 
#' @param name A list of the data sets to analyze. For each plot, the data saved in the files specified by l will be loaded and plotted.
#' @param iter The prefered output directory for the generated images. Include the final '/' on linux/mac and '\' on windows.
#' @export
#' @examples LIM
LIM = function(name = 'temp', iter = '1e6', out.length='1e4', burn.length = '1e5', jmp='1.3') {

    model = ReadModel('/Users/TKelly/Dropbox/Documents/EndToEnd2Layer_01.xls', 'Sensativity5', 126, 25, 16, 89)
    SaveModel(model, name)
    res = RunModel(model, iter=iter, out.length=out.length, burn.length=burn.length, jmp=jmp)
    SaveSolution(res, name)

<<<<<<< HEAD
    SaveSpread(model, res, name)
    Analysis(as.vector(name))
}

=======
name = 'test01'
iter = 1e6
out.length = 2500
burn.length = 0.2*iter
jmp = 1.3

model = ReadModel('/Users/TKelly/Dropbox/Documents/EndToEnd2Layer_01.xls', 'Sensativity3', 125, 25, 16, 79)
SaveModel(model, name)
res = RunModel(model, iter=iter, out.length=out.length, burn.length=burn.length, jmp=jmp)
SaveSolution(res, name)

SaveSpread(model, res, name)
Analysis(as.vector(name))

## Run Sequential Simulations across cycles

Analysis(c('001S1','001','03S2','003'))
#Analysis(c('001S1','001','03S2','004'))

## Run Sequential Sensativity tests
>>>>>>> parent of 8d9dc15... csv data dump
