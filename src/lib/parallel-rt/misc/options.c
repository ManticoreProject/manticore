/*! \file options.c
 * \brief Support for command-line options.
 *
 * \author John Reppy
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#include "manticore-rt.h"
#include <string.h>
#include <stdio.h>
#include "options.h"

struct struct_opts {
    bool	errors;		/* set to true if there were errors */
				/* in process options */
    const char	*cmd;		/* the name of the executable */
    int		argc;		/* number of arguments in argv */
    const char	**argv;		/* array of argument pointers */
};

Options_t *InitOptions (int argc, const char **argv)
{
    Options_t *opts = NEW(Options_t);
    opts->errors = false;
    opts->cmd = argv[0];
    opts->argc = argc-1;
    opts->argv = NEWVEC(const char *, argc-1);
    for (int i = 1;  i < argc;  i++) {
	opts->argv[i-1] = argv[i];
    }

    return opts;
}


/* remove the n options starting at location i */
static void CompressOpts (Options_t *opts, int i, int n)
{
    assert (i+n <= opts->argc);
    opts->argc -= n;
    for (int j = i;  j < opts->argc;  j++)
	opts->argv[j] = opts->argv[j+n];

}


bool GetFlagOpt (Options_t *opts, const char *flg)
{
    for (int i = 0;  i < opts->argc;  i++) {
	if (strcmp(flg, opts->argv[i]) == 0) {
	    CompressOpts (opts, i, 1);
	    return true;
	}
    }

    return false;

}

int GetIntOpt (Options_t *opts, const char *opt, int dflt)
{
    for (int i = 0;  i < opts->argc;  i++) {
	if (strcmp(opt, opts->argv[i]) == 0) {
	    if (++i < opts->argc) {
		long arg = strtol (opts->argv[i], 0, 10);
		CompressOpts (opts, i-1, 2);
		return arg;
	    } else {
		CompressOpts (opts, i-1, 1);
		Error("%s: missing argument for `%s' option\n", opts->cmd, opt);
		opts->errors = true;
		return dflt;
	    }
	}
    }

    return dflt;
}

/* get a size option; the suffixes "k" and "m" are supported */
Addr_t GetSizeOpt (Options_t *opts, const char *opt, Addr_t dfltScale, Addr_t dflt)
{
    for (int i = 0;  i < opts->argc;  i++) {
	if (strcmp(opt, opts->argv[i]) == 0) {
	    if (++i < opts->argc) {
		char *endp;
		long arg = strtol (opts->argv[i], &endp, 10);
		CompressOpts (opts, i-1, 2);
		if (arg < 0) {
		    Error("%s: size should be positive\n", opts->cmd);
		if (*endp == 'k') dfltScale = ONE_K;
		else if (*endp == 'm') dfltScale = ONE_MEG;
		return arg * dfltScale;
	    } else {
		CompressOpts (opts, i-1, 1);
		Error("%s: missing argument for `%s' option\n", opts->cmd, opt);
		opts->errors = true;
		return dflt;
	    }
	}
    }

    return dflt;
}


