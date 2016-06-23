#include<stdio.h>
#include<stdint.h>

struct StkSizeRecords {
	uint64_t functionAddress;
	uint64_t stackSize;
};

struct Constants {
	uint64_t largeConstant;
};

struct Location {
	uint8_t registerDirectIndirectConstantConstantIndex;
	uint8_t reserved;
	uint16_t dwarfRegNum;
	int offset;
};

struct LiveOuts {
	uint16_t dwarfRegNum;
	uint8_t reserved;
	uint8_t size;
};

struct StkMapRecord {
	uint64_t patchPointid;
	uint32_t instructionOffset;
	uint16_t reserved;
	uint16_t numLocations;
	struct Location* locations;
	uint16_t padding;
	uint16_t numLiveOuts;
	struct LiveOuts* liveouts;
	uint32_t otherPadding;	
};

struct Header {
	uint8_t stackMapVersion;
	uint8_t reservedVal1;
	uint8_t reservedVal2;
};

struct StackMap{
	struct Header header;
	uint32_t numFunctions;
	uint32_t numConstants;
	uint32_t numRecords;
	struct StkSizeRecords* stackSizeRecords;
	struct Constants* constants;
	struct StkMapRecord* stackMapRecord;
};

extern void stackWalker(int counter){
	printf("printing from stackwalker. counter is %d\n", counter);
}
