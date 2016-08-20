\name{discrete_va_plot}
\alias{discrete_va_plot}
\title{Discrete plot for StatCast data}
\description{
 Graphs Statcast data for different categories of fitted hit probability.
}
\usage{
discrete_va_plot(df, br=seq(0, 1, length.out=10), pal="YlOrBr")
}
\arguments{
  \item{df}{data frame containing Statcast data}
  \item{br}{vector of breaks of probabilities}
  \item{pal}{palette to use with ColorBrewer scale}
}

\author{Jim Albert}

\examples{
statcast_graphs(trout2015)
}

\keyword{models}
