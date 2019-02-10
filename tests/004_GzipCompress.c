#define __is_inside_libminor_E7B72649_9D2F_4078_8EA0_5D7B61775925 1 /* Because we test internal functions we also define this preprocessor variable */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

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


/* File stream sink */
static FILE* fTestOut = NULL;
static unsigned long int dwBytesOutTotal = 0;
static enum minorError streamSink_File_Write(
	struct minorStreamSink* lpSelf,
	uint8_t* lpSourceBuffer,
	unsigned long int dwBytesToWrite,
	unsigned long int *lpBytesWritten
) {
	dwBytesOutTotal = dwBytesOutTotal + dwBytesToWrite;
//	printf("[SINK] Received %lu bytes (total %lu)\n", dwBytesToWrite, dwBytesOutTotal);

	if(lpBytesWritten != NULL) {
		(*lpBytesWritten) = dwBytesToWrite;
	}

	/* for(i = 0; i < dwBytesToWrite; i=i+1) {
		printf("%c", lpSourceBuffer[i]);
	} */
	if(fTestOut != NULL) {
		fwrite(lpSourceBuffer, dwBytesToWrite, 1, fTestOut);
		fflush(fTestOut);
	}

	return minorE_Ok;
}
static enum minorError streamSinkFile_Flush(
	struct minorStreamSink* lpSelf
) {
	return minorE_Ok;
}
static struct minorStreamSink streamSinkFile = {
	&streamSink_File_Write,
	&streamSinkFile_Flush
};

/* File stream source */
static FILE* fTestIn = NULL;
unsigned long int dwTestInRead = 0;
static enum minorError streamSource_File_Read(
	struct minorStreamSource* lpSelf,
	uint8_t* lpDestinationBuffer,
	unsigned long int dwBytesToRead,
	unsigned long int *lpBytesRead
) {
	ssize_t readElements;
	if(lpBytesRead == NULL) {
		/* ToDo: Check if we have enough bytes remaining ... */
	}

	if(feof(fTestIn)) {
		printf("%s:%u EOF test input\n", __FILE__, __LINE__);
		return minorE_EndOfStream;
	}

	/* Read into the destination buffer */
//printf("%s:%u Read request for %lu bytes (current position %lu); ", __FILE__, __LINE__, dwBytesToRead, ftell(fTestIn));
	readElements = fread(lpDestinationBuffer, 1, dwBytesToRead, fTestIn);
//printf("read %u\n", readElements);
	if(lpBytesRead != NULL) {
		(*lpBytesRead) = readElements;
	}
	if(readElements > 0) {
		dwTestInRead = dwTestInRead + readElements;
		return minorE_Ok;
	} else {
		if(ferror(fTestIn) != 0) {
			printf("%s:%u An error occured (errno: %u)\n", __FILE__, __LINE__, errno);
			return minorE_InvalidState;
		}
		printf("%s:%u No elements read\n", __FILE__, __LINE__);
		return minorE_EndOfStream;
	}
}
static struct minorStreamSource streamSourceFile = {
	&streamSource_File_Read
};

int main(int argc, char* argv[]) {
	struct minorCompressor* lpCompressor = NULL;
	struct minorDecompressor* lpDecompressor = NULL;
	unsigned long int dwBytesDone;
	long int fileSize;
	enum minorError e;

	printf("%s:%u Creating compressor ... ", __FILE__, __LINE__);
	e = minorCompressorCreate(&lpCompressor, minorAlgorithm_Gzip, &sysApi, NULL);
	if(e != minorE_Ok) {
		printf("%s:%u failed (code %u)\n", __FILE__, __LINE__, e);
		return -1;
	}
	printf("ok\n");

	/* Attach source and sink */
	printf("%s:%u Attaching source ... ", __FILE__, __LINE__);
	e = minorCompressorAttachSource(lpCompressor, &streamSourceFile);
	if(e != minorE_Ok) { printf("%s:%u failed (code %u)\n", __FILE__, __LINE__, e); return -1; } else { printf("ok\n"); }
	printf("%s:%u Attaching sink ... ", __FILE__, __LINE__);
	e = minorCompressorAttachSink(lpCompressor, &streamSinkFile);
	if(e != minorE_Ok) { printf("%s:%u failed (code %u)\n", __FILE__, __LINE__, e); return -1; } else { printf("ok\n"); }

	/* Open the file ... */
	fTestIn = fopen("./1.txt", "rb");
	if(fTestIn == NULL) {
		printf("%s:%u Failed to open file\n", __FILE__, __LINE__);
		return -1;
	}

	/* Open output file */
	fTestOut = fopen("./1.txt.gztest", "w+b");
	if(fTestOut == NULL) {
		printf("%s:%u Failed to open file\n", __FILE__, __LINE__);
		return -1;
	}

	/* Determine file size */
	fseek(fTestIn, 0L, SEEK_END);
	fileSize = ftell(fTestIn);
	fseek(fTestIn, 0L, SEEK_SET);

	dwBytesDone = 0;
	printf("%s:%u Transferring %lu test bytes into the compressor ", __FILE__, __LINE__, fileSize);
	e = minorCompressorTransferFromInput(lpCompressor, fileSize, &dwBytesDone);
	if((e != minorE_Ok) && (e != minorE_EndOfStream) && (e != minorE_Finished)) {
		printf("%s:%u Failed. Error Code %u\n", __FILE__, __LINE__, e);
	} else {
		printf("Ok (Read %lu bytes)\n", dwBytesDone);
	}


	printf("%s:%u Releasing compressor ... ", __FILE__, __LINE__);
	e = minorCompressorRelease(lpCompressor);
	if(e != minorE_Ok) {
		printf("%s:%u failed (code %u)\n", __FILE__, __LINE__, e);
		return -1;
	}

	/* Now we will try to DECOMPRESS the outputed data again ... */
	fclose(fTestIn);
	fclose(fTestOut);


	dwTestInRead = 0;
	printf("\n\n---\n%s:%u Testing decompression\n", __FILE__, __LINE__);
	fTestIn = fopen("./1.txt.gztest", "rb");
	if(fTestIn == NULL) {
		printf("%s:%u Failed to open test GZIP file\n", __FILE__, __LINE__);
		return -1;
	}
	fTestOut = fopen("./1.txt.gztestdec", "w+b");
	if(fTestOut == NULL) {
		printf("%s:%u Failed to open file\n", __FILE__, __LINE__);
		return -1;
	}

	fseek(fTestIn, 0L, SEEK_END);
	fileSize = ftell(fTestIn);
	fseek(fTestIn, 0L, SEEK_SET);
	dwBytesDone = 0;

	e = minorDecompressorCreate(
		&lpDecompressor,
		minorAlgorithm_Gzip,
		&sysApi,
		NULL			/* No additional configuration */
	);
	if(e != minorE_Ok) {
		printf("%s:%u Failed to create decompressor, code %u\n", __FILE__, __LINE__, e);
		return -1;
	}

	printf("%s:%u Attaching source ... ", __FILE__, __LINE__);
	e = minorDecompressorAttachSource(lpDecompressor, &streamSourceFile);
	if(e != minorE_Ok) { printf("%s:%u failed (code %u)\n", __FILE__, __LINE__, e); return -1; } else { printf("ok\n"); }
	printf("%s:%u Attaching sink ... ", __FILE__, __LINE__);
	e = minorDecompressorAttachSink(lpDecompressor, &streamSinkFile);
	if(e != minorE_Ok) { printf("%s:%u failed (code %u)\n", __FILE__, __LINE__, e); return -1; } else { printf("ok\n"); }

	printf("%s:%u Transferring %lu test bytes into the decompressor ", __FILE__, __LINE__, fileSize);
	e = minorDecompressor_TransferFromInput(lpDecompressor, fileSize, &dwBytesDone);
	if((e != minorE_Ok) && (e != minorE_EndOfStream) && (e != minorE_Finished)) {
		printf("%s:%u Failed. Error Code %u\n", __FILE__, __LINE__, e);
	} else {
		printf("Ok (Read %lu bytes according to decompressor, %lu according to stream source)\n", dwBytesDone, dwTestInRead);
	}


	printf("%s:%u Releasing decompressor ... ", __FILE__, __LINE__);
	e = minorDecompressorRelease(lpDecompressor);
	if(e != minorE_Ok) {
		printf("%s:%u failed (code %u)\n", __FILE__, __LINE__, e);
		return -1;
	}

	printf("ok\n");
}
