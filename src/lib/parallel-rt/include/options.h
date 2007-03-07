/* options.h
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#ifndef _OPTIONS_H_
#define _OPTIONS_H_

#include "manticore-rt.h"

Options_t *InitOptions (int argc, const char **argv);

bool GetFlagOpt (Options_t *opts, const char *flg);
int GetIntOpt (Options_t *opts, const char *opt, int dflt);

#endif /* !_OPTIONS_H_ */
