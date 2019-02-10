#define __is_inside_libminor_E7B72649_9D2F_4078_8EA0_5D7B61775925 1 /* Because we test internal functions we also define this preprocessor variable */

#include <stdio.h>
#include <stdlib.h>

#include "../include/minor.h"

static enum minorError sysApi_Alloc(void** lpOut, unsigned long int dwSize, void* lpFreeParam) {
	if(lpOut == NULL) { return minorE_InvalidParam; }

	if(((*lpOut) = malloc(dwSize)) == NULL) {
		return minorE_OutOfMemory;
	} else {
		return minorE_Ok;
	}
}
static enum minorError sysApi_Free(void* lpOffset, void* lpFreeParam) {
	if(lpOffset == NULL) {
		return minorE_Ok;
	}
	free(lpOffset);
	return minorE_Ok;
}
static struct minorSystemInterface sysApi = {
	&sysApi_Alloc, NULL,
	&sysApi_Free, NULL
};



static FILE* fTestOut = NULL;
static enum minorError streamSink_Write(
	struct minorStreamSink* lpSelf,
	uint8_t* lpSourceBuffer,
	unsigned long int dwBytesToWrite,
	unsigned long int* lpBytesWritten
) {
	unsigned long int i;

	printf("[SINK] Received %lu bytes\n", dwBytesToWrite);
	if(lpBytesWritten != NULL) {
		(*lpBytesWritten) = dwBytesToWrite;
	}

	for(i = 0; i < dwBytesToWrite; i=i+1) {
		printf("%c", lpSourceBuffer[i]);
	}
	if(fTestOut != NULL) {
		fwrite(lpSourceBuffer, dwBytesToWrite, 1, fTestOut);
	}

	return minorE_Ok;
}
static enum minorError streamSinkFlush(
	struct minorStreamSink* lpSelf
) {
	return minorE_Ok;
}
static struct minorStreamSink streamSink = {
	&streamSink_Write,
	&streamSinkFlush
};


int main(int argc, char* argv[]) {
	struct minorDecompressor* lpDecompressor = NULL;
	enum minorError e;
	FILE* fHandleIn;
	uint8_t bBuffer[512];
	size_t len;
	unsigned long int dwDone;

	printf("Creating decompressor for GZIP format ... ");
	e = minorDecompressorCreate(&lpDecompressor, minorAlgorithm_Gzip, &sysApi, NULL);
	if(e != minorE_Ok) {
		printf("failed (code %u)\n", e);
		return -1;
	}
	printf("ok\n");

	printf("Attaching sink to decompressor ... ");
	e = minorDecompressorAttachSink(lpDecompressor, &streamSink);
	if(e != minorE_Ok) {
		printf("failed (code %u)\n", e);
		return -1;
	}
	printf("ok\n");

	/* Open input file via standard C library */
	fHandleIn = fopen("1.txt.gz", "rb");
	fTestOut = fopen("1.txt.decompressed", "wb");
	if(fHandleIn == NULL) {
		printf("Failed to open test file 1.txt.gz\n");
		return -1;
	}

	for(;;) {
		len = fread((void*)bBuffer, 1, sizeof(bBuffer), fHandleIn);
		if(len == 0) {
			if(ferror(fHandleIn) != 0) { perror("I/O error\n"); break; }
			if(feof(fHandleIn) != 0) { perror("All read\n"); break; }
			break;
		}
		printf("\tProcessing %u bytes of compressed data\n", len);
		e = minorDecompressor_WriteMem(lpDecompressor, bBuffer, len, &dwDone);
		printf("\tDone %lu bytes, code %u\n", dwDone, e);
		if(e == minorE_Finished) {
			printf("\tDONE\n");
			break;
		}
		if(e != minorE_Ok) {
			printf("\tABORTING\n");
			break;
		}
	}
	fclose(fTestOut);
	fclose(fHandleIn);




	printf("Releasing decompressor ... ");
	e = minorDecompressorRelease(lpDecompressor);
	if(e == minorE_Ok) { printf("ok\n"); return 0; } else { printf("failed (code %u)\n", e); }
}
