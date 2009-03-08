/* json.h
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * The representation of JSON files produced by the JSON parser.
 */

#ifndef _JSON_H_
#define _JSON_H_

#include <stdint.h>
#ifndef __cplusplus
#include <stdbool.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

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

/*! \brief Parse a JSON file */
JSON_Value_t *JSON_ParseFile (const char *file);

/*! \brief Free the storage allocated for a JSON value */
void JSON_Free (JSON_Value_t *v);

/*! \brief Return the named field from a JSON object */
JSON_Value_t *JSON_GetField (JSON_Value_t *v, const char *name);

/*! \brief Return the given item from a JSON array */
JSON_Value_t *JSON_GetElem (JSON_Value_t *v, int i);

const char *JSON_GetString (JSON_Value_t *v);

#ifdef __cplusplus
}
#endif

#endif /* !_JSON_H_ */
