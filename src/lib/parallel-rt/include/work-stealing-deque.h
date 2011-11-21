/* work-stealing-deque.h
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Deque structure used by the Work Stealing scheduler.
 */

#ifndef _WORK_STEALING_DEQUE_H_
#define _WORK_STEALING_DEQUE_H_

#include "manticore-rt.h"
#include "vproc.h"
#include "value.h"

/* deque structure
 * NOTE: deques that are not claimed and contain zero elements are be freed by the memory manager
 */
struct Deque_s {
    int32_t       old;           // pointer to the oldest element in the deque
    int32_t       new;           // pointer to the address immediately to the right of the newest element
    int32_t       maxSz;         // max number of elements
    int32_t       nClaimed;      // the number of processes that hold a reference to the deque
    Value_t       elts[1];       // elements of the deque
};
typedef struct Deque_s Deque_t;

/* \brief call this function once during runtime initialization to initialize
 *     gc state */
void M_InitWorkGroupList ();

/* \brief allocate the primary deque on the given vproc to by used by the given group
 * \param self the host vproc
 * \param workGroupId the work group allocating the deque
 * \param size the max number of elements in the deque
 * \return a pointer to the freshly allocated deque
 */
Value_t M_PrimaryDequeAlloc (VProc_t *self, uint64_t workGroupId, int32_t size);

/* \brief allocate a secondary deque on the given vproc to by used by the given group
 * \param self the host vproc
 * \param workGroupId the work group allocating the deque
 * \param size the max number of elements in the deque
 * \return a pointer to the freshly allocated deque
 */
Value_t M_SecondaryDequeAlloc (VProc_t *self, uint64_t workGroupId, int32_t size);

/* \brief allocate a resume deque on the given vproc to by used by the given group
 * \param self the host vproc
 * \param workGroupId the work group allocating the deque
 * \param size the max number of elements in the deque
 * \return a pointer to the freshly allocated deque
 */
Value_t M_ResumeDequeAlloc (VProc_t *self, uint64_t workGroupId, int32_t size);

/* \brief number of roots needed for deques on the given vproc 
 * \param self the host vproc
 * \return the number of roots
 */
int M_NumDequeRoots (VProc_t *self);

/* \brief add the deque elements to the root set to be used by a minor collection
 * \param self the host vproc
 * \param rootPtr pointer to the root set
 * \return the updated root set
 */
Value_t **M_AddDequeEltsToLocalRoots (VProc_t *self, Value_t **rootPtr);

/* \brief add the deque elements to the root set to be used by a global collection
 * \param self the host vproc
 * \param rootPtr pointer to the root set
 */
void M_AddDequeEltsToGlobalRoots (VProc_t *self, Value_t **rootPtr);

/* \brief returns a pointer to the primary deque of the host vproc corresponding to the given work group
 * \param self the host vproc
 * \param the work group id
 * \return pointer to the primary deque
 */
Value_t M_PrimaryDeque (VProc_t *self, uint64_t workGroupId);

/* \brief returns a pointer to the secondary deque of the host vproc corresponding to the given work group
 * \param self the host vproc
 * \param the work group id
 * \return pointer to the secondary deque
 */
Value_t M_SecondaryDeque (VProc_t *self, uint64_t workGroupId);

/* \brief returns a list of all nonempty resume deques on the host vproc corresponding to the given work group
 * \param self the host vproc
 * \param the work group id
 * \return pointer to a linked list of all nonempty resume deques
 */
Value_t M_ResumeDeques (VProc_t *self, uint64_t workGroupId);

#endif /*! _WORK_STEALING_DEQUE_H_ */
