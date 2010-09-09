/* image.h
 *
 * COPYRIGHT (c) 2010 The Manticore Project (manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Definitions for image data.
 */

#ifndef _IMAGE_H_
#define _IMAGE_H_
typedef unsigned char Pixel_t[8];

typedef struct {
    unsigned int	wid, ht;
    Pixel_t		*pixels;
} Image_t;

typedef enum { RENDER, QUIT } Events_t;

#endif
