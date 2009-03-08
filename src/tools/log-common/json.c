/* json.c
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "json.h"
#include "JSON_parser.h"

typedef enum {
    START,
    IN_OBJ,
    IN_ARR
} ParseState_t;

/* extra tag to mark the start of an array/object */
#define MARK_TAG	-1
/* extra tag to mark a field */
#define FIELD_TAG	-2

#define STACK_SZ	2048

typedef struct {
    ParseState_t	st;
    int			tsp;		/* top of stack */
    JSON_Value_t	stack[STACK_SZ];
} ParserInfo_t;

static JSON_Value_t *PushValue (ParserInfo_t *info, int tag)
{
    if (++info->tsp < STACK_SZ) {
	JSON_Value_t *v = &(info->stack[info->tsp]);
	v->tag = tag;
	return v;
    }
    else {
	fprintf (stderr, "parser stack overflow\n");
	exit (1);
    }
}

static JSON_Value_t *PopValue (ParserInfo_t *info)
{
    JSON_Value_t *v = (JSON_Value_t *)malloc(sizeof(JSON_Value_t));
    memcpy (v, &(info->stack[info->tsp]), sizeof(JSON_Value_t));
    info->tsp--;
    return v;
}

static int DoToken (void* ctx, int type, const JSON_value* value)
{
    ParserInfo_t	*info = (ParserInfo_t *)ctx;
    JSON_Value_t	*v;
    int			i, n;

    switch (type) {
      case JSON_T_ARRAY_BEGIN:
	v = PushValue (info, MARK_TAG);
	v->u.integer = info->st;
	info->st = IN_ARR;
        break;
      case JSON_T_ARRAY_END:
	assert (info->st == IN_ARR);
      /* count the number of elements */
	i = info->tsp;
	n = 0;
	while (i >= 0) {
	    if (info->stack[i].tag == MARK_TAG) break;
	    i--;
	    n++;
	}
	assert (i >= 0);
      /* allocate the array */
	JSON_Value_t **values = (JSON_Value_t **)malloc(n * sizeof(JSON_Value_t *));
      /* copy values from stack to array */
	for (i = n-1;  i >= 0;  i--)
	    values[i] = PopValue (info);
	assert (info->stack[info->tsp].tag == MARK_TAG);
      /* restore the previous state */
	info->st = info->stack[info->tsp].u.integer;
      /* put object onto stack */
	info->stack[info->tsp].tag = JSON_array;
	info->stack[info->tsp].u.array.length = n;
	info->stack[info->tsp].u.array.elems = values;
        break;
      case JSON_T_OBJECT_BEGIN:
	v = PushValue (info, MARK_TAG);
	v->u.integer = info->st;
	info->st = IN_OBJ;
        break;
      case JSON_T_OBJECT_END:
	assert (info->st == IN_OBJ);
      /* count the number of elements */
	i = info->tsp;
	n = 0;
	while (i >= 0) {
	    if (info->stack[i].tag == MARK_TAG) break;
	    i -= 2;
	    n++;
	}
	assert (i >= 0);
      /* allocate the object */
	JSON_Field_t *fields = (JSON_Field_t *)malloc(n * sizeof(JSON_Field_t));
      /* copy fields from stack to object */
	for (i = 0;  i < n;  i++) {
	    fields[n-i-1].data = PopValue (info);
	    assert (info->stack[info->tsp].tag == FIELD_TAG);
	    fields[n-i-1].label = info->stack[info->tsp--].u.string;
	}
	assert (info->stack[info->tsp].tag == MARK_TAG);
      /* restore the previous state */
	info->st = info->stack[info->tsp].u.integer;
      /* put object onto stack */
	info->stack[info->tsp].tag = JSON_object;
	info->stack[info->tsp].u.obj.length = n;
	info->stack[info->tsp].u.obj.elems = fields;
        break;
      case JSON_T_INTEGER:
	v = PushValue(info, JSON_int);
	v->u.integer = value->vu.integer_value;
        break;
      case JSON_T_FLOAT:
	v = PushValue(info, JSON_float);
	v->u.flt = value->vu.float_value;
        break;
      case JSON_T_NULL:
	v = PushValue(info, JSON_null);
        break;
      case JSON_T_TRUE:
	v = PushValue(info, JSON_bool);
	v->u.boolean = true;
        break;
      case JSON_T_FALSE:
	v = PushValue(info, JSON_bool);
	v->u.boolean = false;
        break;
      case JSON_T_KEY:
	assert (info->st == IN_OBJ);
	v = PushValue(info, FIELD_TAG);
	v->u.string = strcpy((char *)malloc(value->vu.str.length+1), value->vu.str.value);
        break;
      case JSON_T_STRING:
	v = PushValue(info, JSON_string);
	v->u.string = strcpy((char *)malloc(value->vu.str.length+1), value->vu.str.value);
        break;
      default:
        assert(0);
        break;
    }
    
    return 1;

}

JSON_Value_t *JSON_ParseFile (const char *file)
{
    JSON_config config;
    struct JSON_parser_struct* jc = 0;
    ParserInfo_t info = { .st = START, .tsp = -1 };

    init_JSON_config(&config);
    
    config.depth                  = 20;
    config.callback               = &DoToken;
    config.callback_ctx		  = &info;
    config.allow_comments         = 1;
    config.handle_floats_manually = 1;
    
    jc = new_JSON_parser(&config);
    
    FILE* input = fopen(file, "r");
    for (int count = 0; input ; ++count) {
        int next_char = fgetc(input);
        if (next_char <= 0) {
            break;
        }
        if (!JSON_parser_char(jc, next_char)) {
            delete_JSON_parser(jc);
            fprintf(stderr, "JSON_parser_char: syntax error, byte %d\n", count);
            exit (1);
        }
    }
    if (!JSON_parser_done(jc)) {
        delete_JSON_parser(jc);
        fprintf(stderr, "JSON_parser_end: syntax error\n");
	exit (1);
    }

    assert (info.tsp == 0);
    assert (info.st == START);

    return PopValue(&info);

}

void JSON_Free (JSON_Value_t *v)
{
    switch (v->tag) {
      case JSON_null:
      case JSON_bool:
      case JSON_int:
      case JSON_float:
	break;
      case JSON_string:
	free (v->u.string);
	break;
      case JSON_array:
	for (int i = 0;  i < v->u.array.length;  i++) {
	    JSON_Free (v->u.array.elems[i]);
	}
	free (v->u.array.elems);
	break;
      case JSON_object:
	for (int i = 0;  i < v->u.obj.length;  i++) {
	    free (v->u.obj.elems[i].label);
	    JSON_Free (v->u.obj.elems[i].data);
	}
	free (v->u.obj.elems);
	break;
      default:
	assert (1);
	break;
    }
    free (v);
}

JSON_Value_t *JSON_GetField (JSON_Value_t *v, const char *name)
{
    if ((v == 0) || (v->tag != JSON_object)) return 0;
    for (int i = 0;  i < v->u.obj.length;  i++) {
	if (strcasecmp(v->u.obj.elems[i].label, name) == 0)
	    return v->u.obj.elems[i].data;
    }
    return 0;
}

JSON_Value_t *JSON_GetElem (JSON_Value_t *v, int i)
{
    if ((v == 0) || (v->tag != JSON_array) || (i < 0) || (v->u.array.length <= i))
	return 0;
    else
	return v->u.array.elems[i];
}

const char *JSON_GetString (JSON_Value_t *v)
{
    if ((v == 0) || (v->tag != JSON_string))
	return 0;
    else
	return v->u.string;
}
