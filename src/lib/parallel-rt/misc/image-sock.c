/* image-sock.c
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#include "manticore-rt.h"
#include "image.h"
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>


int M_OpenSocket(char *hostname, int port) {
    int fd;
    struct sockaddr_in pin;
    struct hostent *hp;

    if ((hp = gethostbyname(hostname)) == 0) {
        printf ("ghbn failed\n");
        return -1;
    }

    memset(&pin, 0, sizeof(pin));
    pin.sin_family = AF_INET;
    pin.sin_addr.s_addr = ((struct in_addr *)(hp->h_addr))->s_addr;
    pin.sin_port = htons(port);

    if ((fd = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
        printf("socket failed\n");
        return -2;
    }

    if (connect(fd, (struct sockaddr *)&pin, sizeof(pin)) == -1) {
        printf("connect failed\n");
        return -3;
    }

    printf("win-open\n");

    return fd;
}

void M_CloseSocket(int fd) {
    close(fd);
}

Events_t M_ReadEvent(int fd) {
    int result;
    Events_t evt;

    result = recv(fd, (void**)&evt, sizeof(Events_t), MSG_DONTWAIT);

    if ((result == EAGAIN) || (result < 0)) {
        return (Events_t)-1;
    }

    return evt;
}

void M_WriteImage(int fd, Image_t *img) {
    int i,j,size;
    unsigned char *smaller;

    i = img->wid;
    send(fd, &i, sizeof(int), 0);
    i = img->ht;
    send(fd, &i, sizeof(int), 0);

    /* Store the data into 3 chars instead of the 8 used by Image_t */
    size = img->wid*img->ht*3*sizeof(unsigned char);
    smaller = malloc(size);

    for (i = 0; i < img->wid; i++) {
        for (j = 0; j < img->ht; j++) {
            smaller[i*img->wid*3+j*3] = img->pixels[i*img->wid+j][0];
            smaller[i*img->wid*3+j*3+1] = img->pixels[i*img->wid+j][1];
            smaller[i*img->wid*3+j*3+2] = img->pixels[i*img->wid+j][2];
        }
    }

    send(fd, smaller, size, 0);
    free(smaller);
}

