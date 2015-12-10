\name{getvis}
\alias{getvis}
\title{generate visualisation from the data as store these in a pdf}
\usage{
getvis(data=c(),resultpath  = c(),dependent="f_int",independent="freq_eff",
group="band",id="runcat",timecol="taustart_ts",do.log=TRUE,
do.bar=TRUE,error.low="err_low",error.up="err_up",deplabel="Intensity (Jy)")
}
\arguments{
\item{data}{dataframe based on a merge between original data and the models}
\item{dependent}{name of dependent variable as stored in data (source intensity)}
\item{independent}{name of independent variable as stored in data (frequency)}
\item{group}{name of grouping variable, band (frequency band) in the case
radio astronomy}
\item{id}{name of variable as stored in data for identifying the subsets of
data for which a model needs to be developed. In radio astronomy this is the source
identifier named runcat}
\item{resultpath}{directory where where the pdf with visualisations will be stored}
\item{timecol}{Name of variable to indicate time}
\item{do.log}{Turn independent and devependent variables into log}
\item{do.bar}{whether to plot the error bars}
\item{error.low}{lower boundary of error range}
\item{error.up}{upper boundary of error range}
\item{deplabel}{label name for the dependent variable in the visualisations}
}
\description{
This function takes a merge between the original data and the models to
create a visualation of both models and data
}
\value{
No output is stored
}
\examples{
\dontrun{
getvis(data=mydata2)
}
}
