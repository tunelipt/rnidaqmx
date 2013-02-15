\name{daqAcquire.NIDAQmx}
\alias{daqAcquire.NIDAQmx}
\title{Synchronous data acquisition.}
\usage{
  daqAcquire.NIDAQmx(dev, timeout = -1)
}
\arguments{
  \item{dev}{Device.}

  \item{timeout}{Timeout.}
}
\value{
  ts matrix where each column correspond to one channel.
}
\description{
  Synchronous data acquisition.
}

