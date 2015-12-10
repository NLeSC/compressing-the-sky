\name{testcompr}
\alias{testcompr}
\title{test data compression}
\usage{
testcompr(data=c(),path=c())
}
\arguments{
\item{data}{vector of data}
\item{path}{directory that is used for temporarily creating files}
}
\description{
This function takes a vector and then tests the compression of the
vector using nine different compression techniques:
 gzip low (level 1), high (level 9) or default (level NA)
 bzip2 low (level 1), high (level 9) or default (level NA)
 xz low (level 1), high (level 9) or default (level NA)
}
\examples{
\dontrun{
result = testcompr(data=D,path=filepath)
}
}
