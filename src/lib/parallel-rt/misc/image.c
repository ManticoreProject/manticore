/* image.c
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#include "manticore-rt.h"
#include <stdio.h>

#define MAX_SZ		4096

typedef unsigned char Pixel_t[8];

typedef struct {
    unsigned int	wid, ht;
    Pixel_t		*pixels;
} Image_t;

/* M_NewImage:
 */
Image_t *M_NewImage (int wd, int ht)
{
    if ((wd <= 0) || (MAX_SZ < wd) || (ht <= 0) || (MAX_SZ < ht))
	return 0;

    Image_t *img = NEW(Image_t);
    if (img == 0) return 0;
    img->wid = wd;
    img->ht = ht;
    img->pixels = NEWVEC(Pixel_t, wd * ht);
    if (img->pixels == 0) {
	FREE(img);
	return 0;
    }

    return img;

}

/* M_FreeImage
 */
void M_FreeImage (Image_t *img)
{
    if (img != 0) {
	FREE(img->pixels);
	FREE(img);
    }
}

/* M_UpdateImage
 */
void M_UpdateImage (Image_t *img, unsigned int row, unsigned int col, double r, double g, double b)
{
    if ((img == 0) || (img->wid <= col) || (img->ht <= row))
	return;

    int i = (img->wid * row) + col;
    img->pixels[i][0] = (unsigned char)(255.0 * r);
    img->pixels[i][1] = (unsigned char)(255.0 * g);
    img->pixels[i][2] = (unsigned char)(255.0 * b);
    img->pixels[i][3] = 255;
}

/* M_OutputImage:
 */
void M_OutputImage (Image_t *img, const char *file)
{
    FILE *f = fopen(file, "wb");
    if (f == NULL)
	return;

    fprintf(f, "P6\n%d %d 255\n", img->wid, img->ht);
    for (int i = 0;  i < img->wid*img->ht;  i++) {
	fputc (img->pixels[i][0], f);
	fputc (img->pixels[i][1], f);
	fputc (img->pixels[i][2], f);
    }

    fclose (f);

}
