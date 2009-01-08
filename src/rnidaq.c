/** \file \brief Implementation of R interface to NIDAQmx.

    This file implements the R interface to NIDAQmx. For now it is very
    simple and allows for analog input of voltage signals. 
 */
#include <string.h>

#include <NIDAQmx.h>
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>

static SEXP
Rcreate_task(SEXP task_name)
{
  PROTECT(task_name = coerceVector(task_name, STRSXP));
  const char *name = CHAR(STRING_ELT(task_name, 0));
  TaskHandle task;
  int32 error_code = DAQmxCreateTask(name, &task);
  
  if (error_code < 0)
    {
      UNPROTECT(1);
      error("NIDAQmx error code: %d", error_code);
    }
  else if (error_code > 0)
    {
      warning("NIDAQmx warning code: %d", error_code);
    }
  
  SEXP tag;
  PROTECT(tag = allocVector(STRSXP, 1));
  const char *tag_name = "NIDAQmxTask";
  SET_STRING_ELT(tag, 0, mkChar(tag_name));
  
  SEXP task_ptr = R_MakeExternalPtr((void*) task, tag, NULL);

  UNPROTECT(2);

  return task_ptr;
}


static SEXP
Rstart_task(SEXP r_task)
{
  
  TaskHandle task = (TaskHandle) R_ExternalPtrAddr(r_task);
  int32 ret = DAQmxStartTask(task);

  if (ret < 0)
    {
      error("NIDAQmx error code: %d", ret);
    }
  else if (ret > 0)
    {
      warning("NIDAQmx warning code: %d", ret);
    }

  return R_NilValue;
}

static SEXP
Radd_global_chans_to_task(SEXP r_task, SEXP r_chans)
{
  TaskHandle task = (TaskHandle) R_ExternalPtrAddr(r_task);
  PROTECT(r_chans = AS_CHARACTER(r_chans));
  const char *chans = CHAR(STRING_ELT(r_chans, 0));

  int32 ret = DAQmxAddGlobalChansToTask(task, chans);

  if (ret < 0)
    {
      UNPROTECT(1);
      error("NIDAQmx error code: %d", ret);
    }
  else if (ret > 0)
    {
      warning("NIDAQmx warning code: %d", ret);
    }

  UNPROTECT(1);
  return R_NilValue;
}

static SEXP
Rstop_task(SEXP r_task)
{
  TaskHandle task = (TaskHandle) R_ExternalPtrAddr(r_task);

  int32 ret = DAQmxStopTask(task);

  if (ret < 0)
    {
      error("NIDAQmx error code: %d", ret);
    }
  else if (ret > 0)
    {
      warning("NIDAQmx warning code: %d", ret);
    }

  return R_NilValue;
}

static SEXP
Rclear_task(SEXP r_task)
{
  TaskHandle task = (TaskHandle) R_ExternalPtrAddr(r_task);

  int32 ret = DAQmxClearTask(task);

  if (ret < 0)
    {
      error("NIDAQmx error code: %d", ret);
    }
  else if (ret > 0)
    {
      warning("NIDAQmx warning code: %d", ret);
    }

  return R_NilValue;
}

static SEXP
Rcreate_voltage_chan(SEXP r_task, SEXP r_phys_chan, SEXP r_chan_name,
		     SEXP r_terminal_config,
		     SEXP r_minVal, SEXP r_maxVal, 
		     SEXP r_units, SEXP r_custom_scale)
{
  TaskHandle task = (TaskHandle) R_ExternalPtrAddr(r_task);
  PROTECT(r_phys_chan = AS_CHARACTER(r_phys_chan));
  const char *phys_chan = CHAR(STRING_ELT(r_phys_chan, 0));
  PROTECT(r_chan_name = AS_CHARACTER(r_chan_name));
  const char *chan_name = CHAR(STRING_ELT(r_chan_name, 0));
  int terminal_config = INTEGER(AS_INTEGER(r_terminal_config))[0];
  double minVal = REAL(AS_NUMERIC(r_minVal))[0];
  double maxVal = REAL(AS_NUMERIC(r_maxVal))[0];
  int32 units;
  char *null_string = "";
  char *scale;
  
  if (Rf_isNull(r_units))
    {
      units = DAQmx_Val_Volts;
      scale = null_string;
    }
  else
    {
      units = INTEGER(AS_INTEGER(r_units))[0];
      PROTECT(r_custom_scale = AS_CHARACTER(r_custom_scale));
      scale = CHAR(STRING_ELT(r_custom_scale, 0));
    }
  
  
  int32 ret = DAQmxCreateAIVoltageChan(task, phys_chan, chan_name,
				       terminal_config, minVal, maxVal,
				       units, scale);
  
  if (ret < 0)
    {
      UNPROTECT(3);
      error("NIDAQmx error code: %d", ret);
    }
  else if (ret > 0)
    {
      warning("NIDAQmx warning code: %d", ret);
    }

  UNPROTECT(3);
  return R_NilValue;
}


static SEXP
Rconfig_sampl_clk_timing(SEXP r_task, SEXP r_source, SEXP r_rate,
			 SEXP r_active_edge, SEXP r_sample_mode, SEXP r_nsamples)
{
  TaskHandle task = (TaskHandle) R_ExternalPtrAddr(r_task);
  double rate = REAL(AS_NUMERIC(r_rate))[0];
  int32 active_edge;
  if (Rf_isNull(r_active_edge))
    {
      active_edge = DAQmx_Val_Rising;
    }
  else
    {
      active_edge = INTEGER(AS_INTEGER(r_active_edge))[0];
    }
  
  int32 sample_mode;

  if (Rf_isNull(r_sample_mode))
    {
      sample_mode = DAQmx_Val_FiniteSamps;
    }
  else
    {
      sample_mode = INTEGER(AS_INTEGER(r_sample_mode))[0];
    }
  
  int nsamples = INTEGER(AS_INTEGER(r_nsamples))[0];
  
  PROTECT(r_source = AS_CHARACTER(r_source));
  const char *source = CHAR(STRING_ELT(r_source, 0));
  
  int32 ret = DAQmxCfgSampClkTiming(task,source, rate, active_edge,
				    sample_mode, nsamples);

  
  if (ret < 0)
    {
      UNPROTECT(1);
      error("NIDAQmx error code: %d", ret);
    }
  else if (ret > 0)
    {
      warning("NIDAQmx warning code: %d", ret);
    }

  UNPROTECT(1);
  return R_NilValue;

}



static SEXP
Ris_finished(SEXP r_task)
{

  TaskHandle task = (TaskHandle) R_ExternalPtrAddr(r_task);
  bool32 is_task_done;
  
  int32 ret = DAQmxIsTaskDone(task, &is_task_done);

  if (ret < 0)
    {
      error("NIDAQmx error code: %d", ret);
    }
  else if (ret > 0)
    {
      warning("NIDAQmx warning code: %d", ret);
    }
  SEXP ans;
  PROTECT(ans = NEW_LOGICAL(1));
  LOGICAL(ans)[0] = is_task_done;
  UNPROTECT(1);
  return ans;
}


R_CallMethodDef callMethods[] = {
  {"Rcreate_task", (DL_FUNC) &Rcreate_task, 1},
  {"Rstart_task", (DL_FUNC) &Rstart_task, 1},
  {"Rclear_task", (DL_FUNC) &Rclear_task, 1},
  {"Rstop_task", (DL_FUNC) &Rstop_task, 1},
  {"Radd_global_chans_to_task", (DL_FUNC) &Radd_global_chans_to_task, 2},
  {"Rcreate_voltage_chan", (DL_FUNC) &Rcreate_voltage_chan, 8},
  {"Rconfig_sampl_clk_timing",(DL_FUNC)  &Rconfig_sampl_clk_timing, 3},
  {"Ris_finished", (DL_FUNC) &Ris_finished, 1},
  {NULL, NULL, 0}
};

void 
R_init_rnidaqmx(DllInfo *info)
{
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}
