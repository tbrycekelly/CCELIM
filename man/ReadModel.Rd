% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ReadModel.R
\name{ReadModel}
\alias{ReadModel}
\title{ReadModel()}
\usage{
ReadModel(file = "./inst/extdata/Example.xls",
  file2 = ".inst/extdata/Example_data.xls", cycle = 1, sheet.name = NULL,
  num.flows, num.exact = NULL, num.approx = NULL, num.ineq = NULL)
}
\arguments{
\item{file}{The name with path to the excel spreadsheet containing the model.}

\item{file2}{The name (including path) of the excel file containing the b vectors.}

\item{cycle}{The cycle number indicating the rows within the excel file2 with the appropriate data.}

\item{sheet.name}{The name of the sheet in excel.}

\item{num.flows}{The count of the model flows.}

\item{num.exact}{The count of the exact equations used in the model.}

\item{num.approx}{The count of the approximate equations used in the model.}

\item{num.ineq}{The count of the inequality relations used in the model.}
}
\description{
This script is the first stage of the inverse modeling workflow. Here the excel model is imported and the matricies and arrays are generated. The function returns a dataframe <model> that can then be passed on to the RunModel() function.
A model consists of a dataframe with the folloing fields: $flows <list of flow names>, $Aa <Approximate Matrix>, $ba <Solution of Aa>, $Ae <Exact Matrix>, $be <Solution of Ae>, $Gg <Inequalities Matrix>, $h <ineqality Solutions> ,$sdb <Uncertainty in the measurements (SD)>.
}
\examples{
ReadModel(7, 4, 32)

}
\keyword{CCELIM}
\keyword{Ecosystem,}
\keyword{Inverse}
\keyword{MCMC,}
\keyword{Modeling,}

