\name{approxdata}
\alias{approxdata}
\title{approximate data based on the models as derived with function getmodel}
\usage{
approxdata(models=c(),do.log=TRUE, dependent="f_int",independent="freq_eff",
group="band",id="runcat",modeltime=TRUE)
}
\arguments{
\item{models}{dataframe of models as produced by function getmodel}
\item{do.log}{Turn independent and devependent variables into log}
\item{dependent}{name of dependent variable as stored in data (source intensity)}
\item{independent}{name of independent variable as stored in data (frequency)}
\item{group}{name of grouping variable, band (frequency band) in the case
radio astronomy}
\item{id}{name of variable as stored in data for identifying the subsets of
data for which a model needs to be developed. In radio astronomy this is the source
identifier named runcat}
\item{modeltime}{Whether to use constant of time series as a model (TRUE) or
to use one model per source based on the assuming of a exponential relationship between
source intensity and frequency (FALSE)}
}
\description{
This function takes the models dataframe as produced by getmodel
and uses it to replicate the original data based on the model, which will
be an approximation.
}
\value{
A dataframe with the same size as the dataframe from which models was derived
}
\examples{
\dontrun{
rep.data = approxdata(models=models)
}
}
