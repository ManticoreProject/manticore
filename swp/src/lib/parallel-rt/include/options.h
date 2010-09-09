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

/***** Configuration-file support *****/

extern void InitConfiguration (Options_t *opts);

extern int GetIntConfig (const char *key, int dflt);
extern Addr_t GetSizeConfig (const char *key, Addr_t dfltScale, Addr_t dflt);


/***** Utility functions *****/
extern int64_t GetSizeValue (const char *s, Addr_t dfltScale);

#endif /* !_OPTIONS_H_ */
