/* load-log-desc.h
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#ifndef _LOG_DESC_H_
#define _LOG_DESC_H_

typedef enum {
    ADDR,
    INT,
    WORD,
    FLOAT,
    DOUBLE,
    STR0
} ArgType_t;

#define STR(n)		(STR0+(n))
#define isSTR(ty)	((ty) > STR0)
#define STRLEN(ty)	((ty) - STR0)

typedef struct {
    char	*name;
    ArgType_t	ty;
    int		loc;
    char	*desc;
} ArgDesc_t;

typedef enum {
    LOG_EVENT,		/* an independent event */
    LOG_START,		/* the start of an interval; the next event code will be the */
			/* end of the interval */
    LOG_END		/* the end of an interval; the previous event code will be the */
			/* start of the interval */
} EventKind_t;

typedef struct {
    char	*name;
    int		nArgs;
    ArgDesc_t	*args;
    EventKind_t	kind;
    char	*desc;
} EventDesc_t;

typedef struct {
    int		nEvents;
    EventDesc_t	**events;
} LogFileDesc_t;

extern LogFileDesc_t *LoadLogDesc (const char *logDescFile);

#endif /* !_LOG_DESC_H_ */
