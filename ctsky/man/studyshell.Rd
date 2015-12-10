\name{studyshell}
\alias{studyshell}
\title{shell function for studying the data}
\usage{
studyshell(data=c(), resultpath= c(),CX2=0.95,minN = 30,
                      dependent="f_int",independent="freq_eff",group="band",id="runcat",
                      timecol="taustart_ts",mcmc=FALSE,include_ext1=FALSE,do.res=FALSE,
                      do.log=TRUE,imidcol="imageid",xtrsrc="xtrsrc",
                      error.low=c(),error.up=c(),prec=c(),
                      modeltime=TRUE,do.bar=TRUE,
                      deplabel="Intensity (Jy)")
}
\arguments{
\item{data}{dataframe including the variables (columns): source identifiers (runcat, xtrsrc),
independent variable (freq_eff), timestamps (taustart_ts), data type (extract_type),
independent variables (f_int), 1-sigma error in the independent variable (f_int_err),
grouping variable (band), image id (id)
}
\item{resultpath}{directory where code stores the results}
\item{CX2}{Confidence interval for the chi-square test}
\item{minN}{Minimum number of measurements per frequency band}
\item{dependent}{Name of the dependent variable in the model (f_int in our case)}
\item{independent}{Name of the independent variable in the model (freq_eff in our case)}
\item{group}{Grouping variable (band in our case)}
\item{id}{Identifier of the image, this is mainly used as a key to aid tracing back the
origin of the data}
\item{timecol}{Name of variable to indicate time}
\item{mcmc}{If TRUE then use Markov Chain Monte Carlo regression instead
of ordinary least squares}
\item{include_ext1}{If true then keep the datapoints in radio astronomy for which
the extract type equals 1. Extract type equals 1 data points are interpolations and
not real data. A model is likely to be more accurate if these datapoints are ommited}
\item{do.res}{if true then also output the function residuals as a seperate dataframe}
\item{do.log}{whether to take the log from the indpenedent and dependent variables}
\item{imidcol}{Name of column in which image id is stored}
\item{xtrsrc}{Name of column in which xtrsrc is stored}
\item{prec}{Number of decimal places at which the data needs to be roudned. If left empty
then function will estimate this from 10 percent of standard deviation in dependent
variable}
\item{error.low}{lower boundary of error range or uncertainty itself if error.up
is not provided (see error.up)}
\item{error.up}{upper boundary of error range (if not provided then it will assume that
error.low equals the uncertainty itself and calculates from this the upper and lower
boundary}
\item{modeltime}{Whether to use constant of time series as a model (TRUE) or
to use one model per source based on the assuming of a exponential relationship between
source intensity and frequency (FALSE)}
\item{do.bar}{whether to plot the error bars}
\item{deplabel}{label name for the dependent variable in the visualisations}
}
\description{
Shell function for investigating the impact of model representations on compression,
reproduction of residuals, approximation of original data. This function is mainly
as a sanity check that the procedure is doing what it is supposed to do
}
\examples{
\dontrun{
studyshell(data=D,resultpath= "~/CTS/results/",CX2=0.95)
}
}
