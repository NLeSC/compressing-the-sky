\name{getmodel}
\alias{getmodel}
\title{derive models from data}
\usage{
getmodel(data=c(),dependent=c(),independent=c(),group=c(),id=c(),
                    mcmc=FALSE,include_ext1=FALSE,minN=30,do.res=FALSE,CX2=0.95,
                    do.log=TRUE,imidcol="imageid",xtrsrc="xtrsrc",modeltime=TRUE)
}
\arguments{
\item{data}{dataframe}
\item{dependent}{name of dependent variable as stored in data (source intensity)}
\item{independent}{name of independent variable as stored in data (frequency)}
\item{group}{name of grouping variable, band (frequency band) in the case
radio astronomy}
\item{id}{name of variable as stored in data for identifying the subsets of
data for which a model needs to be developed. In radio astronomy this is the source
identifier named runcat}
\item{mcmc}{If TRUE then use Markov Chain Monte Carlo regression instead
of ordinary least squares}
\item{include_ext1}{If true then keep the datapoints in radio astronomy for which
the extract type equals 1. Extract type equals 1 data points are interpolations and
not real data. A model is likely to be more accurate if these datapoints are ommited
}
\item{minN}{minimum number of measurements required per group (frequency band)}
\item{do.res}{if true then also output the function residuals as a seperate dataframe}
\item{CX2}{Conficende interval for for Chi-square test, default = 0.95}
\item{do.log}{whether to take the log from the indpenedent and dependent variables}
\item{imidcol}{Name of column in which image id is stored}
\item{xtrsrc}{Name of column in which xtrsrc is stored}
\item{modeltime}{Whether to use constant of time series as a model (TRUE) or
to use one model per source based on the assuming of a exponential relationship between
source intensity and frequency (FALSE)}
}
\description{
This function takes the data and derives one models per source (the subset of data
identified by id). Here, the function converts the data to log space
and then fits a linear model. This is what is assumed to work in radio astronomogy.
Additionally the function tests whether the dependent values are normally distributed
per frequency band.
}
\value{
Dataframe with models and if do.res is set to TRUE it also includes the model residuals
}
\examples{
\dontrun{
V = getmodel(data=D,dependent="f_int",independent="freq_eff",group="band",id="runcat",
                 mcmc=FALSE,include_ext1=FALSE,minN=minN,doinR=doin.R,CX2=CX2)
}
}
