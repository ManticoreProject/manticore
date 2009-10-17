/* options.h
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#ifndef _OPTIONS_H_
#define _OPTIONS_H_

#include "manticore-rt.h"

extern Options_t *InitOptions (int argc, const char **argv);

extern bool GetFlagOpt (Options_t *opts, const char *flg);
extern int GetIntOpt (Options_t *opts, const char *opt, int dflt);
extern const char *GetStringOpt (Options_t *opts, const char *opt, const char *dflt);
extern const char *GetStringEqOpt (Options_t *opts, const char *opt, const char *dflt);

/* get a size option; the suffixes "k" and "m" are supported */
extern Addr_t GetSizeOpt (Options_t *opts, const char *opt, Addr_t dfltScale, Addr_t dflt);

extern int NumOptions ();
extern const char **Options ();

#endif /* !_OPTIONS_H_ */
