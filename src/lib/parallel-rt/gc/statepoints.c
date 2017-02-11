/* statepoints.c
 * 
 * Originally authored by Kavon Farvardin
 * Source: https://github.com/kavon/llvm-statepoint-utils
 */

#include <stddef.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "statepoints.h"

/***** Hash table API *****/
static statepoint_table_t* new_table (float loadFactor, uint64_t expectedElms);

static void insert_key (statepoint_table_t* table, uint64_t key, frame_info_t* value);

static size_t size_of_frame (uint16_t numSlots);

static size_t frame_size (frame_info_t* frame);
/***** End Hash table API *****/

/***** LLVM Stack map *****/
/**
 * LLVM's Documentation: http://llvm.org/docs/StackMaps.html#stack-map-format
 *
 *  "The runtime must be able to interpret the stack map record given only the ID,
 *  offset, and the order of the locations, which LLVM preserves."
 *
 *  We interpret "order of the locations" to mean that not only are callsite records
 *  cooresponding to a function grouped together and ordered from least to greatest
 *  offset, but these callsite groups are also in the same order as the array of
 *  function stack size records.
 *
 *  This appears to be the case in LLVM, and indeed, these assumptions are nessecary to
 *  figure out what groups correspond to which functions (without abusing the ID field
 *  with a post processing script) to compute the return addresses.
 */

 /******** LAYOUT ********

 stackmap_header_t;

 function_info_t[numFunctions];

 uint64_t[numConstants];

 numRecords of the following {
    callsite_header_t;

    value_location_t[numLocations];

    liveout_header_t;

    liveout_location_t[numLiveouts];

    upto 4 bytes of padding, as needed, to achieve 8 byte alignment;
}

 ******** END OF LAYOUT ********/

typedef struct __attribute__((packed)) {
    uint8_t version;
    uint8_t reserved1;
    uint16_t reserved2;
    uint32_t numFunctions;
    uint32_t numConstants;
    uint32_t numRecords;
} stackmap_header_t;

typedef struct __attribute__((packed)) {
    uint64_t address;
    uint64_t stackSize;
    uint64_t callsiteCount;   // see https://reviews.llvm.org/D23487
} function_info_t;

typedef struct __attribute__((packed)) {
    uint64_t id;
    uint32_t codeOffset;  // from the entry of the function
    uint16_t flags;
    uint16_t numLocations;
} callsite_header_t;

typedef enum {
    Register = 0x1,
    Direct = 0x2,
    Indirect = 0x3,
    Constant = 0x4,
    ConstIndex = 0x5
} location_kind_t;

typedef struct __attribute__((packed)) {
    uint8_t kind;       // possibilities come from location_kind_t, but is one byte in size.
    uint8_t flags;
    uint16_t regNum;    // Dwarf register num
    int32_t offset;     // either an offset or a "Small Constant"
} value_location_t;

typedef struct __attribute__((packed)) {
    uint16_t padding;
    uint16_t numLiveouts;
} liveout_header_t;

typedef struct __attribute__((packed)) {
    uint16_t regNum;    // Dwarf register num
    uint8_t flags;
    uint8_t size;       // in bytes
} liveout_location_t;

/***** End LLVM stack map *****/

// returns the next frame relative the current frame
frame_info_t* next_frame(frame_info_t* cur);

bool isBasePointer(value_location_t* first, value_location_t* second) {
    return first->kind == second->kind
           && first->offset == second->offset;
}

bool isIndirect(value_location_t* p) {
    return p->kind == Indirect;
}

// The assumption is that the value_location given to this function
// is known to be of the offset type, and now we need to parse the
// offset. Offsets are given relative to a register value,
// and since it might be either the frame pointer or stack pointer.
//
// This function will always return the offset relative to the stack ptr.
int32_t convert_offset(value_location_t* p, uint64_t frameSize) {
    assert(p->kind == Indirect && "not an indirect!");

    // see the x86-64 SysV ABI documentation for the table of
    // registers and their corresponding Dwarf reg numbers
    switch(p->regNum) {
        case 7: // offset is relative to stack pointer
            assert(p->offset >= 0 && "unexpected offset!");
            return p->offset;

        case 6: // offset is relative to base pointer.
                // NOTE haven't seen statepoints generate such offsets.
            assert(p->offset <= 0 && "unexpected offset!");
            return ((int32_t)frameSize) + p->offset;

        default:
            assert(false && "offset is not relative to some part of the frame!");
            // be nice if C had exceptions. let's hope an unusual offset
            // catches someone's eye.
            return 123456789;
    }
}

frame_info_t* generate_frame_info(callsite_header_t* callsite, function_info_t* fn) {
    uint64_t retAddr = fn->address + callsite->codeOffset;
    uint64_t frameSize = fn->stackSize;

    // now we parse the location array according to the specific type
    // of locations that statepoints emit:
    // http://llvm.org/docs/Statepoints.html#stack-map-format

    uint16_t numLocations = callsite->numLocations;
    value_location_t* locations = (value_location_t*)(callsite + 1);

    // the first 2 locations are constants we dont care about, but if asserts are
    // on we check that they're constants.
    for(uint16_t i = 0; i < 2; i++) {
        // printf("location kind: %u, addr: %llu\n", locations->kind, (unsigned long long)locations);
        assert(locations->kind == Constant
            && "first 2 locations must be constants in statepoint stackmaps");
        locations++;
        numLocations--;
    }

    // the 3rd constant describes the number of "deopt" parameters
    // that we should skip over.
    assert(locations->kind == Constant && "3rd location should be a constant");
    int32_t numDeopt = locations->offset;
    locations++;
    numLocations--;

    assert(numDeopt >= 0 && "unexpected negative here");
    locations += numDeopt;
    numLocations -= numDeopt;

    /*
       The remaining locations describe pointer that the GC should track, and use a special
       format:

       "Each record consists of a pair of Locations. The second element in the record
       represents the pointer (or pointers) which need updated. The first element in the
       record provides a pointer to the base of the object with which the pointer(s) being
       relocated is associated. This information is required for handling generalized
       derived pointers since a pointer may be outside the bounds of the original
       allocation, but still needs to be relocated with the allocation."

       NOTE that we are currently ignoring the following part of the documentation because
       it doesn't make sense... locations have no size field:

       "The Locations within each record may [be] a multiple of pointer size. In the later
       case, the record must be interpreted as describing a sequence of pointers and their
       corresponding base pointers. If the Location is of size N x sizeof(pointer), then
       there will be N records of one pointer each contained within the Location. Both
       Locations in a pair can be assumed to be of the same size."
    */


    assert((numLocations % 2) == 0 && "all of the pointer locations come in pairs!");
    uint16_t numSlots = numLocations / 2;

    frame_info_t* frame = malloc(size_of_frame(numSlots));
    frame->retAddr = retAddr;
    frame->frameSize = frameSize;
    frame->numSlots = numSlots;

    // now to initialize the slots, we need to make two passes in order to put
    // base pointers first, then derived pointers.
    value_location_t *start = locations;
    uint16_t numBasePtrs = 0;
    pointer_slot_t* currentSlot = frame->slots;
    for(uint16_t i = 0; i < numSlots; i++, locations += 2) {
        value_location_t* base = (value_location_t*)(locations);
        value_location_t* derived = (value_location_t*)(locations + 1);

        // we skip all locations are not indirects. 
        // TODO this was previously an assert, but if LLVM detects that the pointer was
        // a constant, say zero, it will emit a pair of constant entries, which doesn't
        // make sense.
        if (! (isIndirect(base) && isIndirect(derived))) {
            continue;
        }

        if( ! isBasePointer(base, derived)) {
            continue;
        }

        // it's a base pointer, aka base is equivalent to derived.
        // save the info.
        pointer_slot_t newSlot;
        newSlot.kind = -1;
        newSlot.offset = convert_offset(base, frameSize);
        *currentSlot = newSlot;

        // get ready for next iteration
        numBasePtrs++;
        currentSlot++;
    }

    // now we do the derived pointers. we already know all locations are indirects now.
    locations = start;
    pointer_slot_t* processedBase = frame->slots;
    for(uint16_t i = 0; i < numSlots; i++, locations += 2) {
        value_location_t* base = (value_location_t*)(locations);
        value_location_t* derived = (value_location_t*)(locations + 1);

        if (! (isIndirect(base) && isIndirect(derived))) {
            continue;
        }

        if(isBasePointer(base, derived)) {
            // already processed
            continue;
        }

        // find the index in our frame corresponding to the base pointer.
        uint16_t baseIdx;
        bool found = false;
        for(uint16_t k = 0; k < numBasePtrs; k++) {
            if(processedBase[k].offset == base->offset) {
                found = true;
                baseIdx = k;
                break;
            }
        }

        // something's gone awry, let's bail!
        assert(found && "uh oh");

        // save the derived pointer's info
        pointer_slot_t newSlot;
        newSlot.kind = baseIdx;
        newSlot.offset = convert_offset(derived, frameSize);
        *currentSlot = newSlot;

        // new iteration
        currentSlot++;
    }

    // there is no liveout information emitted for statepoints, and we place faith in
    // the input on that being the case

    return frame;
}

callsite_header_t* next_callsite(callsite_header_t* callsite) {
    uint16_t numLocations = callsite->numLocations;

    // skip over locations
    value_location_t* locations = (value_location_t*)(callsite + 1);
    locations += numLocations;

    liveout_header_t* liveout_header = (liveout_header_t*)locations;
    uint16_t numLiveouts = liveout_header->numLiveouts;

    // skip over liveouts
    liveout_location_t* liveouts = (liveout_location_t*)(liveout_header + 1);
    liveouts += numLiveouts;

    // realign pointer to 8 byte alignment.
    uint64_t ptr_val = (uint64_t)liveouts;
    ptr_val = (ptr_val + 7) & ~0x7;

    return (callsite_header_t*)ptr_val;
}

statepoint_table_t* generate_table(void* map, float load_factor) {

    uint8_t* version = (uint8_t*)map;
    if (*version != 2) {
        printf("error: only LLVM stackmap version 2 is supported.\n");
        assert(false && "see above");
        return NULL;
    }

    stackmap_header_t* header = (stackmap_header_t*)map;
    uint64_t numCallsites = header->numRecords;

    statepoint_table_t* table = new_table(load_factor, numCallsites);

    function_info_t* functions = (function_info_t*)(header + 1);

    // we skip over constants, which are uint64_t's
    callsite_header_t* callsite =
        (callsite_header_t*)(
            ((uint64_t*)(functions + header->numFunctions)) + header->numConstants
        );


    function_info_t* currentFn = functions;
    uint64_t visited = 0;
    for(uint64_t _unused = 0; _unused < numCallsites; _unused++) {
        if(visited >= currentFn->callsiteCount) {
            currentFn++;
            visited = 0;
        }

        frame_info_t* info = generate_frame_info(callsite, currentFn);

        insert_key(table, info->retAddr, info);

        // setup next iteration
        callsite = next_callsite(callsite);
        visited++;
    }

    return table;
}

// for PRIu and PRId
#define __STDC_FORMAT_MACROS 1
#include <inttypes.h>

/**
 * The hash function used to distribute keys uniformly across the table.
 * The implementation is one round of the xorshift64* algorithm.
 * Code Source: Wikipedia
 */
uint64_t hashFn(uint64_t x) {
    x ^= x >> 12; // a
        x ^= x << 25; // b
        x ^= x >> 27; // c
        return x * UINT64_C(2685821657736338717);
}

uint64_t computeBucketIndex(statepoint_table_t* table, uint64_t key) {
    // Using modulo may introduce a little bias in the table.
    // If you care, use the unbiased version that's floating around the internet.
    return hashFn(key) % table->size;
}

static size_t size_of_frame(uint16_t numSlots) {
    return sizeof(frame_info_t) + numSlots * sizeof(pointer_slot_t);
}

static size_t frame_size(frame_info_t* frame) {
    return size_of_frame(frame->numSlots);
}

// returns the next frame relative the current frame
frame_info_t* next_frame(frame_info_t* cur) {
    uint8_t* next = ((uint8_t*)cur) + frame_size(cur);
    return (frame_info_t*)next;
}


static statepoint_table_t* new_table(float loadFactor, uint64_t expectedElms) {
    assert(loadFactor > 0 && "must be positive");
    assert(expectedElms > 0 && "must be positive");

    uint64_t numBuckets = (expectedElms / loadFactor) + 1;

    table_bucket_t* buckets = calloc(numBuckets, sizeof(table_bucket_t));
    assert(buckets && "bad alloc");

    statepoint_table_t* table = malloc(sizeof(statepoint_table_t));
    assert(table && "bad alloc");

    table->size = numBuckets;
    table->buckets = buckets;

    return table;
}


void destroy_table(statepoint_table_t* table) {
    for(uint64_t i = 0; i < table->size; i++) {
        frame_info_t* entry = table->buckets[i].entries;
        if(entry != NULL) {
            free(entry);
        }
    }
    free(table->buckets);
    free(table);
}


// NOTE value must be a base pointer to a malloc operation, and the act of inserting
// the key is considered the final use of the pointer (i.e., value will be freed by the
// function).
static void insert_key(statepoint_table_t* table, uint64_t key, frame_info_t* value) {
    uint64_t idx = computeBucketIndex(table, key);
    table_bucket_t *bucket = table->buckets + idx;

    if(bucket->numEntries == 0) {
        bucket->numEntries = 1;
        bucket->sizeOfEntries = frame_size(value);
        bucket->entries = value;
    } else {
        // a collision occured!
        size_t newSize = bucket->sizeOfEntries + frame_size(value);
        frame_info_t* newEntries = realloc(bucket->entries, newSize);

        assert(newEntries && "bad alloc");

        // copy value onto the end of the possibly resized entry array
        frame_info_t* oldEnd = (frame_info_t*)(
            ((uint8_t*)newEntries) + bucket->sizeOfEntries
        );

        memmove(oldEnd, value, frame_size(value));

        free(value);

        bucket->entries = newEntries;
        bucket->sizeOfEntries = newSize;
        bucket->numEntries += 1;
    }
}


frame_info_t* lookup_return_address(statepoint_table_t *table, uint64_t retAddr) {
    uint64_t idx = computeBucketIndex(table, retAddr);
    table_bucket_t bucket = table->buckets[idx];

    uint16_t bucketLimit = bucket.numEntries;
    frame_info_t* entries = bucket.entries;

    for(uint16_t i = 0; i < bucketLimit; i++) {
        if(entries->retAddr == retAddr) {
            return entries;
        }
        entries = next_frame(entries);
    }

    return NULL;
}

void print_table(FILE *stream, statepoint_table_t* table, bool skip_empty) {
    for(uint64_t i = 0; i < table->size; i++) {
        uint16_t numEntries = table->buckets[i].numEntries;
        size_t sizeOfEntries = table->buckets[i].sizeOfEntries;
        frame_info_t* entry = table->buckets[i].entries;

        if(skip_empty && numEntries == 0) {
            continue;
        }

        fprintf(stream, "\n--- bucket #%" PRIu64 "---\n", i);
        fprintf(stream, "num entries: %" PRIu16 ", ", numEntries);
        fprintf(stream, "memory allocated (bytes): %" PRIuPTR "\n", sizeOfEntries);

        for(uint16_t i = 0; i < numEntries; i++, entry = next_frame(entry)) {
            fprintf(stream, "\t** frame #%" PRIu16 "**\n", i);
            print_frame(stream, entry);
        }
    }
    fflush(stream);
}

void print_frame(FILE *stream, frame_info_t* frame) {
    fprintf(stream, "\t\treturn address: 0x%" PRIX64 "\n", frame->retAddr);
    fprintf(stream, "\t\tframe size: %" PRIu64 "\n", frame->frameSize);

    uint16_t numSlots = frame->numSlots;
    pointer_slot_t* curSlot = frame->slots;
    fprintf(stream, "\t\tnum live ptrs: %" PRIu16 "\n", numSlots);

    for(uint16_t i = 0; i < numSlots; i++, curSlot++) {
        fprintf(stream, "\t\tptr slot #%" PRIu16 " { ", i);

        int32_t kind = curSlot->kind;
        if(kind < 0) {
            fprintf(stream, "kind: base ptr, ");
        } else {
            fprintf(stream, "kind: ptr derived from slot #%" PRId32 ", ", kind);
        }

        fprintf(stream, "frame offset: %" PRId32 " }\n", curSlot->offset);
    }
}
