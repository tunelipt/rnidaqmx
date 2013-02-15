#' @useDynLib rnidaqmx
NULL



#' Create a new NIDAQmx device.
#'
#' New NIDAQmx device.
#'
#' @param dev Device name.
#' @param channels Integer array containing channel numbers starting in 1.
#' @param rge Range.
#' @param terminal Terminal type.
#' @param taskname
#' @export
NIDAQnew <- function(dev="dev1", channels=1, rge=c(0,5),
                     terminal=-1, taskname=""){

  task <- .Call("Rnidaq_create_task", taskname)
  terminalList <- list(Default=-1, RSE=10083, NRSE=10078, Diff=10106, PseudoDiff=12529)
  if (terminal!=-1){
    terminal <- terminalList[[terminal]]
    if (is.null(terminal)) terminal <- -1
  }

  # Get the channels physical configuration - NIDAQmx syntax:
  chanName <- paste(dev, '/', 'ai', channels-1, sep='', collapse=',')
  .Call("Rnidaq_create_voltage_chan", task, chanName, "", terminal,
        rge[1], rge[2], NULL, "")
  freq <- NULL
  nsamples <- NULL
  open <- TRUE
  env <- environment()
  device <- list(env=env)
  class(device) <- "NIDAQmx"
  reg.finalizer(env, daqClose.NIDAQmx, onexit=TRUE)
  return(device)
}


#' Verify whether an NIDAQmx device is open.
#'
#' @param dev Device.
#' @return Logical vector.
#' @export
isDaqOpen.NIDAQmx <- function(dev){
  return(get("open", envir=dev$env))
}

#' Configure NIDAQmx device.
#'
#' @param dev Device.
#' @param freq Sampling reate in Hz.
#' @param nsamples Number of samples to read.
#' @export
daqConfig.NIDAQmx <- function(dev, freq=1000.0, nsamples=100){
  if (!isDaqOpen(dev)){
    stop("NIDAQmx task not open!")
    return(NULL)
  }

  task <- get("task", envir=dev$env)
  assign("freq", freq, envir=dev$env)
  assign("nsamples", nsamples, envir=dev$env)
  
  .Call("Rnidaq_config_sampl_clk_timing", task, "", freq, NULL, NULL, nsamples)
}


#' Start asynchronoud data acquisition.
#'
#' @param NIDAQ device.
#' @export
daqStart.NIDAQmx <- function(dev){
  if (!isDaqOpen(dev)){
    stop("NIDAQmx task not open!")
    return(NULL)
  }
  
  task <- get("task", envir=dev$env)
  .Call("Rnidaq_start_task", task)
}


#' Close NIDAQmx device.
#'
#' @param dev Device.
#' @export
daqClose.NIDAQmx <- function(dev){
  if (!isDaqOpen(dev)){
    stop("NIDAQmx task not open!")
    return(NULL)
  }
  task <- get("task", envir=dev$env)
  .Call("Rnidaq_clear_task", task)
  assign("open", FALSE, envir=dev$env)
  
}


#' Verify whether asynchronous data acquisition is over.
#'
#' @param dev Device.
#' @return Logical vector.
#' @export
isDaqFinished.NIDAQmx <- function(dev){
  if (!isDaqOpen(dev)){
    stop("NIDAQmx task not open!")
    return(NULL)
  }
  task <- get("task", envir=dev$env)
  .Call("Rnidaq_is_finished", task)
}


#' Read data.
#'
#' @param dev Device.
#' @param timeout Timeout.
#' @return ts matrix where each column correspond to one channel.
#' @export
daqRead.NIDAQmx <- function(dev, timeout=-1){
  if (!isDaqOpen(dev)){
    stop("NIDAQmx task not open!")
    return(NULL)
  }
  on.exit(.Call("Rnidaq_stop_task", task))
  task <- get("task", envir=dev$env)
  freq <- get("freq", envir=dev$env)
  nsamples <- get("nsamples", envir=dev$env)

  nchan <- length(get("channels", envir=dev$env))
  dados <- .Call("Rnidaq_read_analog_f64", task, nsamples, timeout, NULL, nchan)
  dim(dados) <- c(nsamples, nchan)
  return(ts(dados, start=0, freq=freq))
}

#' Synchronous data acquisition.
#' @param dev Device.
#' @param timeout Timeout.
#' @return ts matrix where each column correspond to one channel.
#' @export
daqAcquire.NIDAQmx <- function(dev, timeout=-1){
  daqStart(dev)
  daqRead(dev, timeout)
}


#' Return error code message.
#'
#' @param errorCode Numeric value of the error code.
#' @return Character vector describing the error code.
#' @export
nidaqErrorMsg <- function(errorCode){
  .Call("Rnidaq_get_error_string", errorCode)
}

#' Stop data acquisition.
#'
#' @param dev Device
#' @export
nidaqStop <- function(dev){
  if (!isDaqOpen(dev)){
    stop("NIDAQmx task not open!")
    return(NULL)
  }

  task <- get("task", envir=dev$env)
  .Call("Rnidaq_stop_task", task)
}

#' Clear NIDAQmx task.
#'
#' @param dev Device.
nidaqClear <- function(dev){
  if (!isDaqOpen(dev)){
    stop("NIDAQmx task not open!")
    return(NULL)
  }
  task <- get("task", envir=dev$env)
  .Call("Rnidaq_clear_task", task)
  
}


