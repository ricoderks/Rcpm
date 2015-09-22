\name{summary_se}
\alias{summary_se}
\title{Gives a summary of a data frame}
\usage{
summary_se(data = NULL, measurevar, groupvars = NULL, na.rm = FALSE,
  conf.interval = 0.95, .drop = TRUE)
}
\arguments{
\item{data}{data frame}

\item{measurevar}{the name of a column that contains the variable to be summariezed}

\item{groupvars}{a vector containing names of columns that contain grouping variables}

\item{na.rm}{a boolean that indicates whether to ignore NA's}

\item{conf.interval}{the percent range of the confidence interval (default is 0.95)}
\item{.drop}{should empty fields be dropped}
}
\value{
Data frame with count, mean, standard devation, standard error of the mean and confidence interval.
}
\description{
Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 0.95).
}
\references{
www.cookbook-r.com
}

