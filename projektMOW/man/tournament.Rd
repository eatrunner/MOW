\name{tournament}
\alias{tournament}
\title{Tournament of individuals}
\usage{
tournament(it, df)
}
\arguments{
\item{it}{iterator}
{
enables parallel excecution of tournament
}

\item{df}{dataframe}
{
dataframe with columns accuracy and index of each individual from population
}
}
\value{
winner of tournament
}
\description{
Tournament of individuals for evolution algorithm
}
