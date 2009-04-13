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
Rnidaq_create_task(SEXP task_name)
{
  PROTECT(task_name = coerceVector(task_name, STRSXP));
  const char *name = CHAR(STRING_ELT(task_name, 0));
  TaskHandle task;
  int32 error_code = DAQmxCreateTask(name, &task);
  
  if (error_code < 0)
    {
      UNPROTECT(1);
      error("%d: NIDAQmx error code.", error_code);
    }
  else if (error_code > 0)
    {
      warning("%d: NIDAQmx warning code.", error_code);
    }
  
  SEXP tag;
  PROTECT(tag = allocVector(STRSXP, 1));
  const char *tag_name = "NIDAQmxTask";
  SET_STRING_ELT(tag, 0, mkChar(tag_name));
  
  SEXP task_ptr = R_MakeExternalPtr((void*) task, tag, R_NilValue);

  UNPROTECT(2);

  return task_ptr;
}


static SEXP
Rnidaq_start_task(SEXP r_task)
{
  
  TaskHandle task = (TaskHandle) R_ExternalPtrAddr(r_task);
  int32 ret = DAQmxStartTask(task);

  if (ret < 0)
    {
      error("%d: NIDAQmx error code.", ret);
    }
  else if (ret > 0)
    {
      warning("%d: NIDAQmx warning code.", ret);
    }

  return R_NilValue;
}

static SEXP
Rnidaq_add_global_chans_to_task(SEXP r_task, SEXP r_chans)
{
  TaskHandle task = (TaskHandle) R_ExternalPtrAddr(r_task);
  PROTECT(r_chans = AS_CHARACTER(r_chans));
  const char *chans = CHAR(STRING_ELT(r_chans, 0));

  int32 ret = DAQmxAddGlobalChansToTask(task, chans);

  if (ret < 0)
    {
      UNPROTECT(1);
      error("%d: NIDAQmx error code.", ret);
    }
  else if (ret > 0)
    {
      warning("%d: NIDAQmx warning code.", ret);
    }

  UNPROTECT(1);
  return R_NilValue;
}

static SEXP
Rnidaq_stop_task(SEXP r_task)
{
  TaskHandle task = (TaskHandle) R_ExternalPtrAddr(r_task);

  int32 ret = DAQmxStopTask(task);

  if (ret < 0)
    {
      error("%d: NIDAQmx error code.", ret);
    }
  else if (ret > 0)
    {
      warning("%d: NIDAQmx warning code.", ret);
    }

  return R_NilValue;
}

static SEXP
Rnidaq_clear_task(SEXP r_task)
{
  TaskHandle task = (TaskHandle) R_ExternalPtrAddr(r_task);

  int32 ret = DAQmxClearTask(task);

  if (ret < 0)
    {
      error("%d: NIDAQmx error code.", ret);
    }
  else if (ret > 0)
    {
      warning("%d: NIDAQmx warning code.", ret);
    }

  return R_NilValue;
}

static SEXP
Rnidaq_create_voltage_chan(SEXP r_task, SEXP r_phys_chan, SEXP r_chan_name,
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
  int num_protect = 2;
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
      ++num_protect;
    }
  
  
  int32 ret = DAQmxCreateAIVoltageChan(task, phys_chan, chan_name,
				       terminal_config, minVal, maxVal,
				       units, scale);
  
  if (ret < 0)
    {
      UNPROTECT(num_protect);
      error("%d: NIDAQmx error code.", ret);
    }
  else if (ret > 0)
    {
      warning("%d: NIDAQmx warning code.", ret);
    }

  UNPROTECT(num_protect);
  return R_NilValue;
}


static SEXP
Rnidaq_config_sampl_clk_timing(SEXP r_task, SEXP r_source, SEXP r_rate,
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
      error("%d: NIDAQmx error code.", ret);
    }
  else if (ret > 0)
    {
      warning("%d: NIDAQmx warning code.", ret);
    }

  UNPROTECT(1);
  return R_NilValue;

}



static SEXP
Rnidaq_is_finished(SEXP r_task)
{

  TaskHandle task = (TaskHandle) R_ExternalPtrAddr(r_task);
  bool32 is_task_done;
  
  int32 ret = DAQmxIsTaskDone(task, &is_task_done);

  if (ret < 0)
    {
      error("%d: NIDAQmx error code.", ret);
    }
  else if (ret > 0)
    {
      warning("%d: NIDAQmx warning code.", ret);
    }
  SEXP ans;
  PROTECT(ans = NEW_LOGICAL(1));
  LOGICAL(ans)[0] = is_task_done;
  UNPROTECT(1);
  return ans;
}

static SEXP
Rnidaq_read_analog_f64(SEXP r_task, SEXP r_nsamples, SEXP r_timeout, SEXP r_fill_mode, SEXP r_num_chans)
{
  TaskHandle task = (TaskHandle) R_ExternalPtrAddr(r_task);
  int nsamples = -1;
  if (!Rf_isNull(r_nsamples))
    {
      nsamples = INTEGER(AS_INTEGER(r_nsamples))[0];
    }
  double timeout = 10.0;
  if (!Rf_isNull(r_timeout))
    {
      timeout = INTEGER(AS_INTEGER(r_timeout))[0];
    }
  bool32 fill_mode = DAQmx_Val_GroupByChannel;
  
  if (!Rf_isNull(r_fill_mode))
    {
      fill_mode = (bool32) LOGICAL(AS_LOGICAL(r_fill_mode))[0];
    }
  int num_chans = INTEGER(AS_INTEGER(r_num_chans))[0];
  int32 array_size = num_chans * nsamples;

  // Allocate the array
  SEXP dados;

  PROTECT(dados = NEW_NUMERIC(array_size));
  int32 samples_read;
  int32 ret = DAQmxReadAnalogF64(task, nsamples, timeout, fill_mode, REAL(dados), array_size, &samples_read, NULL);

  if (ret < 0)
    {
      UNPROTECT(1);
      error("%d: NIDAQmx error code.", ret);
    }
  else if (ret > 0)
    {
      warning("%d: NIDAQmx warning code.", ret);
    }
  UNPROTECT(1);
  if (samples_read != nsamples)
    {
      error("Number of samples not what is expected");
    }
  return dados;
  
}

static SEXP
Rnidaq_get_error_string(SEXP r_error_code)
{
  const int buff_len = 4096;
  char buffer[buff_len];
  
  int32 error_code = INTEGER(AS_INTEGER(r_error_code))[0];

  int32 ret = DAQmxGetErrorString(error_code, buffer, buff_len-1);
  if (ret < 0)
    {
      error("%d: NIDAQmx error code.", ret);
    }
  else if (ret > 0)
    {
      warning("%d: NIDAQmx warning code.", ret);
    }
  SEXP msg;
  PROTECT(msg = NEW_CHARACTER(1));
  SET_STRING_ELT(msg, 0, mkChar(buffer));
  UNPROTECT(1);
  return msg;
}

R_CallMethodDef callMethods[] = {
  {"Rnidaq_create_task", (DL_FUNC) &Rnidaq_create_task, 1},
  {"Rnidaq_start_task", (DL_FUNC) &Rnidaq_start_task, 1},
  {"Rnidaq_clear_task", (DL_FUNC) &Rnidaq_clear_task, 1},
  {"Rnidaq_stop_task", (DL_FUNC) &Rnidaq_stop_task, 1},
  {"Rnidaq_add_global_chans_to_task", (DL_FUNC) &Rnidaq_add_global_chans_to_task, 2},
  {"Rnidaq_create_voltage_chan", (DL_FUNC) &Rnidaq_create_voltage_chan, 8},
  {"Rnidaq_config_sampl_clk_timing",(DL_FUNC)  &Rnidaq_config_sampl_clk_timing, 6},
  {"Rnidaq_is_finished", (DL_FUNC) &Rnidaq_is_finished, 1},
  {"Rnidaq_read_analog_f64", (DL_FUNC) &Rnidaq_read_analog_f64, 5},
  {"Rnidaq_get_error_string", (DL_FUNC) &Rnidaq_get_error_string, 1},
  {NULL, NULL, 0}
};

void 
R_init_rnidaqmx(DllInfo *info)
{
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}
