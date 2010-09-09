/*! \file config.c
 *
 * \author John Reppy
 *
 * Code to parse a runtime-system configuration file.  This file consists of
 * name/value pairs using the syntax
 *
 *	name = value
 */

/*
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#include "manticore-config.h"
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include "manticore-rt.h"
#include "options.h"

#define DEFAULT_CONFIG_FILE	".pmlrc"

typedef struct {
    const char	*key;		//!< the key
    const char	*value;		//!< the value; may be null
} ConfigParam_t;

static int		NumConfigParams;
static int		ConfigParamsSz;
static ConfigParam_t	*ConfigParams;

typedef enum {
    END_OF_FILE,
    OK,
    BLANK,
    ERROR
} ParseStatus_t;

static ParseStatus_t ParseLine (FILE *inStrm, ConfigParam_t *param)
{
    char buf[1024];
    char *cp = fgets (buf, sizeof(buf), inStrm);
    char *key, *value;

    if (cp == 0) {
      // EOF or I/O error
	if (feof(inStrm))
	    return END_OF_FILE;
	else {
	    return ERROR;
	}
    }

  /* skip leading whitespace */
    cp += strspn (cp, " \t\n");

  /* check for comments and blank lines */
    if ((*cp == '#') || (*cp == '\0'))
	return BLANK;

  /* parse the lhs of the command */
    size_t n = strcspn (cp, " \t=");  // find end of lhs
    if (n == 0) {
      // ill formed configuration line
	return ERROR;
    }
    key = strncpy ((char *)malloc(n + 1), cp, n);
    key[n] = '\0';
    cp += n;

  /* consume the "=" */
    cp = strchr (cp, '=');
    if (cp == 0) {
      // ill formed configuration line
	return ERROR;
    }
    cp++; // skip '='
    cp += strspn (cp, " \t\n");

    if (*cp != '\0') {
      // find the end of the line
	char *cq = strchr(cp, '\0');
	if (cq[-1] = '\n') cq--;
      // copy the value
	n = cq - cp;
	value = strncpy ((char *)malloc(n+1), cp, n);
	value[n] = '\0';
    }
    else
	value = 0;

  // initialize the ConfigParam_t struct
    param->key = key;
    param->value = value;

    return OK;

}

static void ParseConfigFile (const char *fname)
{
    FILE *inStrm = fopen(fname, "r");
    if (inStrm == NULL) {
	Error ("unable to open configuration file \"%s\"\n", fname);
	exit (1);
    }

    ParseStatus_t sts = BLANK;
    int lnum = 0;
    while ((sts != END_OF_FILE) && (sts != ERROR)) {
	sts = ParseLine (inStrm, &(ConfigParams[NumConfigParams]));
	lnum++;
	if (sts == OK) {
	    if (++NumConfigParams == ConfigParamsSz) {
	      // grow the ConfigParams vector
		int newSz = (3 * ConfigParamsSz) / 2;
		ConfigParam_t *new = NEWVEC(ConfigParam_t, newSz);
		for (int i = 0;  i < ConfigParamsSz;  i++) {
		    new[i] = ConfigParams[i];
		}
		FREE (ConfigParams);
		ConfigParamsSz = newSz;
		ConfigParams = new;
	    }
	}
	else if (sts == ERROR) {
	    fprintf(stderr,
		"Error parsing configuration file \"%s\" at line %d\n",
		fname, lnum);
	    exit (1);
	}
    }

    fclose (inStrm);

}

static ConfigParam_t *FindConfigParam (const char *key)
{
    for (int i = 0;  i < NumConfigParams;  i++) {
	if (strcmp(key, ConfigParams[i].key) == 0)
	    return &(ConfigParams[i]);
    }

    return 0;
}

void InitConfiguration (Options_t *opts)
{
    NumConfigParams = 0;
    ConfigParamsSz = 16;
    ConfigParams = NEWVEC(ConfigParam_t, 16);

    const char *cfgFile = GetStringOpt (opts, "-config", 0);
    if ((cfgFile == 0) && (access (DEFAULT_CONFIG_FILE, R_OK) == 0))
	cfgFile = DEFAULT_CONFIG_FILE;

    if (cfgFile != 0) {
	if (access (cfgFile, R_OK) == 0)
	    ParseConfigFile (cfgFile);
	else {
	    fprintf(stderr,
		"Cannot open configuration file \"%s\"\n", cfgFile);
	    exit (1);
	}
    }

}

/* !\brief get an integer-valued configuration parameter.
 *  \param key the name of the parameter
 *  \param dflt the default value of the parameter
 *  \return the value associated with \arg key, or the default value
 */
int GetIntConfig (const char *key, int dflt)
{
    ConfigParam_t *cfgParam = FindConfigParam (key);
    if (cfgParam == 0)
	return dflt;
    else if (cfgParam->value == 0) {
	Error("missing value for %s\n", key);
	return dflt;
    }
    else {
	char *cp = 0;
	int n = (int)strtol (cfgParam->value, &cp, 0);
	if (cp == key)
	    return n;
	else {
	    Error("expected integer value for %s\n", key);
	    return dflt;
	}
    }
}

Addr_t GetSizeConfig (const char *key, Addr_t dfltScale, Addr_t dflt)
{
    ConfigParam_t *cfgParam = FindConfigParam (key);
    if (cfgParam == 0)
	return dflt;
    else if (cfgParam->value == 0) {
	Error("missing value for %s\n", key);
	return dflt;
    }
    else {
	int64_t sz = GetSizeValue (cfgParam->value, dfltScale);
	if (sz < 0) {
	    Error("bogus size for %s\n", key);
	    return dflt;
	}
	return ((Addr_t)sz) * dfltScale;
    }

    return dflt;
}
