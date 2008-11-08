/* json.h
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#ifndef _JSON_H_
#define _JSON_H_

#include <stdint.h>
#include <stdbool.h>

typedef enum {
    JSON_null,
    JSON_bool,
    JSON_int,
    JSON_float,
    JSON_string,
    JSON_array,
    JSON_object
} JSON_Type_t;

typedef struct struct_json_array JSON_Array_t;
typedef struct struct_json_object JSON_Object_t;
typedef struct struct_json_value JSON_Value_t;

struct struct_json_array {
    int			length;
    JSON_Value_t	**elems;
};

typedef struct {
    char	*label;
    JSON_Value_t *data;
} JSON_Field_t;

struct struct_json_object {
    int			length;
    JSON_Field_t	*elems;
};

struct struct_json_value {
    JSON_Type_t		tag;
    union {
	JSON_Array_t	array;
	JSON_Object_t	obj;
	bool		boolean;
	int64_t		integer;
	double		flt;
	char		*string;
    } u;
};

JSON_Value_t *JSON_ParseFile (const char *file);
void JSON_Free (JSON_Value_t *v);

/*! \brief get the named field from a JSON object */
JSON_Value_t *JSON_GetField (JSON_Value_t *v, const char *name);

/*! \brief get the given item from  */
JSON_Value_t *JSON_GetElem (JSON_Value_t *v, int i);

const char *JSON_GetString (JSON_Value_t *v);

#endif /* !_JSON_H_ */
