  
NIDAQnew <- function(dev="dev1", channels=1, rge=c(0,5),
                     terminal=-1, taskname=""){

  task <- .Call("Rcreate_task", taskname)
  terminalList <- list(Default=-1, RSE=10083, NRSE=10078, Diff=10106, PseudoDiff=12529)
  if (terminal!=-1){
    terminal <- terminalList[[terminal]]
    if (is.null(terminal)) terminal <- -1
  }

  # Get the channels physical configuration - NIDAQmx syntax:
  chanName <- paste(dev, '/', 'ai', channels-1, sep='', collapse=',')
  .Call("Rcreate_voltage_chan", task, chanName, "", terminal,
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

  
isDaqOpen.NIDAQmx <- function(dev){
  return(get("open", envir=dev$env))
}

daqConfig.NIDAQmx <- function(dev, freq=1000.0, nsamples=100){
  if (!isDaqOpen(dev)){
    stop("NIDAQmx task not open!")
    return(NULL)
  }

  task <- get("task", envir=dev$env)
  assign("freq", freq, envir=dev$env)
  assign("nsamples", nsamples, envir=dev$env)
  
  .Call("Rconfig_sampl_clk_timing", task, "", freq, NULL, NULL, nsamples)
}


daqStart.NIDAQmx <- function(dev){
  if (!isDaqOpen(dev)){
    stop("NIDAQmx task not open!")
    return(NULL)
  }
  
  task <- get("task", envir=dev$env)
  .Call("Rstart_task", task)
}


daqClose.NIDAQmx <- function(dev){
  if (!isDaqOpen(dev)){
    stop("NIDAQmx task not open!")
    return(NULL)
  }
  task <- get("task", envir=dev$env)
  .Call("Rclear_task", task)
  assign("open", FALSE, envir=dev$env)
  
}


isDaqFinished.NIDAQmx <- function(dev){
  if (!isDaqOpen(dev)){
    stop("NIDAQmx task not open!")
    return(NULL)
  }
  task <- get("task", envir=dev$env)
  .Call("Ris_finished", task)
}


daqRead.NIDAQmx <- function(dev, timeout=-1){
  if (!isDaqOpen(dev)){
    stop("NIDAQmx task not open!")
    return(NULL)
  }
  on.exit(.Call("Rstop_task", task))
  task <- get("task", envir=dev$env)
  freq <- get("freq", envir=dev$env)
  nsamples <- get("nsamples", envir=dev$env)

  nchan <- length(get("channels", envir=dev$env))
  dados <- .Call("Rread_analog_f64", task, nsamples, timeout, NULL, nchan)
  dim(dados) <- c(nsamples, nchan)
  return(ts(dados, start=0, freq=freq))
}

daqAcquire.NIDAQmx <- function(dev, timeout=-1){
  daqStart(dev)
  daqRead(dev, timeout)
}
.First.lib <- function(lib, pkg){
  library.dynam("rnidaqmx", pkg, lib)
}

nidaqErrorMsg <- function(errorCode){
  .Call("Rget_error_string", errorCode)
}

nidaqStop <- function(dev){
  if (!isDaqOpen(dev)){
    stop("NIDAQmx task not open!")
    return(NULL)
  }

  task <- get("task", envir=dev$env)
  .Call("Rstop_task", task)
}


nidaqClear <- function(dev){
  if (!isDaqOpen(dev)){
    stop("NIDAQmx task not open!")
    return(NULL)
  }
  task <- get("task", envir=dev$env)
  .Call("Rclear_task", task)
  
}
