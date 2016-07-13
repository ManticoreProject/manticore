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

typedef struct {
	uint64_t function_address;
	uint64_t stack_size;
} __llvm_stackmap_size_record;

typedef struct {
	uint64_t large_constant;
} __llvm_stackmap_constant;

typedef struct { 
	uint64_t patchpoint_id;
	uint32_t instruction_offset;
	uint16_t record_flags;
	uint16_t num_locations;
} __llvm_stackmap_record;

typedef struct {
	uint8_t type; // not sure what this is
	uint8_t location_flag;
	uint16_t dwarf_regnum;
	int offset;
} __llvm_stackmap_location;

typedef struct {
	uint16_t padding;
	uint16_t number_liveouts;
} __llvm_stackmap_extra_info;

typedef struct {
	uint16_t dwarf_regnum;
	uint8_t reserved;
	uint8_t size;
} __llvm_stackmap_liveout;

extern void* __LLVM_StackMaps;

extern void stackWalker(long sp){
	printf("recieved vals are %ld %ld\n", sp, (long) __LLVM_StackMaps);

	//get basic info
	__llvm_stackmap_basic *basic = (__llvm_stackmap_basic*) &__LLVM_StackMaps;
	printf("version: %" PRIu8 "\n", basic->header.version);
	printf("reserved1: %" PRIu8 " reserved2: %" PRIu16 "\n", basic->header.reserved1, basic->header.reserved2);
	
	//get fn info
	printf("num functions: %" PRIu32 "\n", basic->num_functions);
	
	__llvm_stackmap_size_record *size_record = (__llvm_stackmap_size_record*) (basic + 1);	
	for(int i = 0; i < basic->num_functions; i++){
		printf("  fn %d address: %" PRIu64 " size: %" PRIu64 "\n", i, size_record->function_address, size_record->stack_size);
		size_record += 1;
	}

	//get constant info
	__llvm_stackmap_constant *constant = (__llvm_stackmap_constant*) size_record;
	printf("num constants: %" PRIu32 "\n", basic->num_constants);
	for(int i = 0; i < basic->num_constants; i++){
		printf("  constant %d: %" PRIu64 "\n", i, constant->large_constant);	
		constant += 1;
	}
	
	//get record info
	__llvm_stackmap_record *record = (__llvm_stackmap_record*) size_record;
	
	printf("num records: %" PRIu32 "\n", basic->num_records);
	for(int i = 0; i < basic->num_records; i++){
		printf("  record %d: id: %" PRIu64 " offset: %" PRIu32 "\n", i, record->patchpoint_id, record->instruction_offset);

		__llvm_stackmap_location *location = (__llvm_stackmap_location*) (record + 1);	
		location += record->num_locations;
		__llvm_stackmap_extra_info *info = (void*) location;
		

		printf("  num of liveouts %" PRIu16 "\n", info->number_liveouts);
		
		__llvm_stackmap_liveout *liveout = (__llvm_stackmap_liveout*) (info + 1);
		for(int j = 0; j < info->number_liveouts; j++) {
			//print info about liveout here
			liveout += 1;		
		}
				
		record = (void*) liveout + sizeof(uint32_t);	
	}	
}
