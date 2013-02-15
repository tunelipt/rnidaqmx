\name{NIDAQnew}
\alias{NIDAQnew}
\title{Create a new NIDAQmx device.}
\usage{
  NIDAQnew(dev = "dev1", channels = 1, rge = c(0, 5),
    terminal = -1, taskname = "")
}
\arguments{
  \item{dev}{Device name.}

  \item{channels}{Integer array containing channel numbers
  starting in 1.}

  \item{rge}{Range.}

  \item{terminal}{Terminal type.}

  \item{taskname}{}
}
\description{
  Create a new NIDAQmx device.
}

