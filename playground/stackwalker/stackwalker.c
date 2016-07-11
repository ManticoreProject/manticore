#include<stdio.h>
#include<stdint.h>
#include<stdlib.h>
#include <inttypes.h>

typedef struct {
	uint8_t version;
	uint8_t reserved1;
	uint16_t reserved2;
} __llvm_stackmap_header;

typedef struct {
	__llvm_stackmap_header header;
	uint32_t num_functions;
	uint32_t num_constants;
	uint32_t num_records;
} __llvm_stackmap_basic;

extern void* __LLVM_StackMaps;

extern void stackWalker(long sp){
	printf("recieved vals are %ld %ld\n", sp, (long) __LLVM_StackMaps);
	__llvm_stackmap_basic *basic = (__llvm_stackmap_basic*) &__LLVM_StackMaps;
	printf("version: %" PRIu8 "\n", basic->header.version);
	printf("reserved1: %" PRIu8 " reserved2: %" PRIu16 "\n", basic->header.reserved1, basic->header.reserved2);
	printf("num functions: %" PRIu32 "\n", basic->num_functions);
	printf("num constants: %" PRIu32 "\n", basic->num_constants);
	printf("num records: %" PRIu32 "\n", basic->num_records);
}
