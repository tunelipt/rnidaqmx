
NIDAQnew <- function(dev="dev1", channels=1, rge=c(0,5),
                     terminal=-1, taskname=""){

  task <- .Call("Rcreate_task", taskname)
  terminalList <- list(Default=-1, RSE=10083, NRSE=10078, Diff=10106, PseudoDiff=12529)
  if (terminal!=-1){
    terminal <- terminalList[[terminal]]
    is (is.null(terminal)) terminal <- -1
  }

  # Get the channels physical configuration - NIDAQmx syntax:
  chanName <- paste(dev, '/', 'ai', channels-1, sep='')
  .Call("Rcreate_voltage_chan", task, chanName, "", terminal,
        rge[1], rge[2], NULL, "")
  env <- environment()
  device <- list(env=env)
  class(device) <- "NIDAQmx"
  reg.finalizer(env, daqClose.NIDAQmx, onexit=TRUE)
  return(device)
}

  
    
daqConfig.NIDAQmx <- function(dev, rate=1000.0, nsamples=100){

  task <- get("task", envir=dev$env)
  .Call("Rconfig_sampl_clk_timing", task, "", rate, NULL, NULL, nsamples)
}


daqStart.NIDAQmx <- function(dev){
  
  task <- get("task", envir=dev$env)
  .Call("Rstart_task", task)
}


  
.First.lib <- function(lib, pkg){
  library.dynam("rscanivalve", pkg, lib)
}
