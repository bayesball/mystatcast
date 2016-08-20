\name{discrete_va_plot}
\alias{discrete_va_plot}
\title{Discrete plot for StatCast data}
\description{
 Graphs Statcast data for different categories of fitted hit probability or linear weights.
}
\usage{
discrete_va_plot(df, br=seq(0, 1, length.out=10), pal="YlOrBr", method="hit")
}
\arguments{
  \item{df}{data frame containing Statcast data}
  \item{br}{vector of breaks of probabilities or linear weights}
  \item{pal}{palette to use with ColorBrewer scale}
  \item{method}{response is hit/out ("hit") or linear weight ("lw")}
}

\author{Jim Albert}

\examples{
discrete_va_plot(scdata)
}

\keyword{models}
