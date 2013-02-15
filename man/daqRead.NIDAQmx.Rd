\name{daqRead.NIDAQmx}
\alias{daqRead.NIDAQmx}
\title{Read data.}
\usage{
  daqRead.NIDAQmx(dev, timeout = -1)
}
\arguments{
  \item{dev}{Device.}

  \item{timeout}{Timeout.}
}
\value{
  ts matrix where each column correspond to one channel.
}
\description{
  Read data.
}

