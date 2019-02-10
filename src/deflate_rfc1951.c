 #define __is_inside_libminor_E7B72649_9D2F_4078_8EA0_5D7B61775925 1

#ifndef MINOR_DEFLATE__DEFAULT__LAZY_MATCH_DISTANCE
	#define MINOR_DEFLATE__DEFAULT__LAZY_MATCH_DISTANCE		1			/* Specifies the amount of bytes lookahead for lazy matching */
#endif
#ifndef MINOR_DEFLATE__DEFAULT__BLOCK_SIZE
	#define MINOR_DEFLATE__DEFAULT__BLOCK_SIZE				512*1024	/* Default blocksize used for output symbols. This is tuneable and determines number of deflate blocks in output stream */
#endif
#ifndef MINOR_DEFLATE__DEFAULT__LOOKBACK_DISTANCE
	#define MINOR_DEFLATE__DEFAULT__LOOKBACK_DISTANCE		32*1024		/* Maximum lookback distance. By RFC this has to be <= 32 kByte */
#endif
#ifndef MINOR_DEFLATE__DEFAULT__EARLY_MATCH_LENGTH
	#define MINOR_DEFLATE__DEFAULT__EARLY_MATCH_LENGTH		0			/* If this value is not zero every LZ77 match found that is at least this amount of bytes long is accepted immedeatly without looking for another better match */
#endif
#ifndef MINOR_DEFLATE__DEFAULT__OUTPUT_BUFFER_SIZE
	#define MINOR_DEFLATE__DEFAULT__OUTPUT_BUFFER_SIZE		32*1024		/* We cache up to 32 kByte before flushing to the application - except "flush" is explicity called */
#endif
#ifndef MINOR_DEFLATE__DEFAULT__MAX_LOOKBACK_LENGTH
	#define MINOR_DEFLATE__DEFAULT__MAX_LOOKBACK_LENGTH 	258			/* RFC says at max 258 bytes ... */
#endif

#ifndef MINOR_DEFLATE__DEFAULT__HASHTABLE_ENTRIES
	#define MINOR_DEFLATE__DEFAULT__HASHTABLE_ENTRIES		1024		/* Smaller or equal than 2^24 (16777216) */
#endif
#ifndef MINOR_DEFLATE__DEFAULT__HASHTABLE_MAXDEPTH
	#define MINOR_DEFLATE__DEFAULT__HASHTABLE_MAXDEPTH		32768		/* By default allow us to keep (in worst case) a full lookback window inside one chain */
#endif
#ifndef MINOR_DEFLATE__DEFAULT__HASHTABLE_MAXENTRIES
	#define MINOR_DEFLATE__DEFAULT__HASHTABLE_MAXENTRIES	2*32768		/* By default allow us to keep (in worst case) two times the full lookback window inside the hashtable to prevent frequent flushes */
#endif

#ifndef MINOR_DEFLATE__MAXIMUM_BITS_LITERALLENGTH
	#define MINOR_DEFLATE__MAXIMUM_BITS_LITERALLENGTH		15			/* No code longer than 15 bit can be emitted according to RFC1951 */
#endif
#ifndef MINOR_DEFLATE__MAXIMUM_BITS_DISTANCE
	#define MINOR_DEFLATE__MAXIMUM_BITS_DISTANCE			15			/* No code longer than 15 bit can be emitted according to RFC1951 */
#endif
#ifndef MINOR_DEFLATE__MAXIMUM_BITS_CODELENGHTS
	#define MINOR_DEFLATE__MAXIMUM_BITS_CODELENGHTS			7			/* No code longer than 7 bit can be emitted (length encoding of 3 bit!) according to RFC1951 */
#endif

#ifdef MINOR_DEFLATE_DEBUG
	#include <stdio.h>
#endif

#include "../include/minor.h"

#ifdef __cplusplus
	extern "C" {
#endif

/*
	Huffman table. The huffman tree is allocated as one
	big array of hufftree entries.
*/
/* Tree built as linked list (does not require node count precalculation) */
struct minorDecompressor_Deflate_HufftreeEntry {
	struct minorDecompressor_Deflate_HufftreeEntry*	lpZero;
	struct minorDecompressor_Deflate_HufftreeEntry*	lpOne;

	uint16_t										value;	/* Used if lpZero and lpOne are both NULL */
};

/*
	Define the various states of our decompressor. The
	decompressor is one large state machine that processes a
	stream of input bits and produces a stream of output
	bytes.

	These states are used by the bitwise- as well as the
	bytewise processing functions.
*/
enum minorDecompressor_Deflate_State {
	/* Initial state(s) */
	minorDecompressor_Deflate_State__ReadBlockHeader							= 0,			/* We await the block header bits (3 bits). This is also the initial state. */

	/* Final state(s) */
	minorDecompressor_Deflate_State__Done										= 1,			/* Final state after decoding the final data block (Successfully). A decoder has to be either released or reset to leave this state */
	minorDecompressor_Deflate_State__Failed										= 2,			/* Final error state in case of an (unrecoverable) encoding error. A decoder has to be either released or reset to leave this state */

	minorDecompressor_Deflate_State__ReadUncompressedHeader						= 3,			/* Read LEN and NLEN. After reading both check the ones complement of LEN is equal to NLEN */
	minorDecompressor_Deflate_State__ReadUncompressedData						= 4,			/* Simply copy uncompressed data from the source to the destination */

	minorDecompressor_Deflate_State__ReadDynamicHeader							= 5,			/* Read the "dynamic table" header. This contains number of codes encoded in the following tables */
	minorDecompressor_Deflate_State__ReadCodelengthCodelengths					= 6,			/* Read the code lengths for the huffman table used to compress the following huffman table specifications */
	minorDecompressor_Deflate_State__ReadLiteralCodelengths						= 7,			/* Read the code lengths for the literal and length codes. They may back reference the revious two length specifications */
	minorDecompressor_Deflate_State__ReadDistanceCodelengths					= 8,			/* Read the code lengths for the distance codes. They may back reference the previous two length specifications! */

	minorDecompressor_Deflate_State__ReadLiteralCodelengths__AdditionalBits 	= 13, 			/* Read additional bits appended after codelength data */
	minorDecompressor_Deflate_State__ReadDistanceCodelengths__AdditionalBits	= 14,			/* Read additional bits appended after distance code */

	minorDecompressor_Deflate_State__ReadHuff_LiteralLengthCode					= 9,			/* State during decoding of literal/length huffman symbol. Leaving this state yields either a literal symbol written into the output buffer or additional length extra bits / a transition to distance code */
	minorDecompressor_Deflate_State__ReadHuff_LengthExtra						= 10,			/* Read extra bits according to length code table from RFC1951 section 3.2.5 */
	minorDecompressor_Deflate_State__ReadHuff_DistanceCode						= 11,			/* Decode huffman symbol for distance code. Leaving yields either a data copy from the output buffer or reading extra bits */
	minorDecompressor_Deflate_State__ReadHuff_DistanceExtra						= 12,			/* Read extra bits according to distance code table from RFC1951 section 3.2.5 */



	minorDecompressor_Deflate_State__Error										= 255,
};


/*
	The following state variable describes the state of the
	bit slicer used during reading from the input stream.
*/
enum minorDecompressor_DeflateReader_State {
	minorDecompressor_DeflateReader_State__Bitwise					= 0,		/* The bitwise state copies always one byte into the internal buffer, slices it and handles bytes to the processing function(s) */
	minorDecompressor_DeflateReader_State__Bytewise					= 1,		/* In bytewise mode data is read in units of bytes and passed into the bytewise processing function. This is - for example - used for LEN and NLEN in uncompressed data blocks */
	minorDecompressor_DeflateReader_State__BlockTransfer			= 2,		/* This state is used to do the block transfer of NLEN bytes from the source to the input (in case of uncompressed data blocks) */
	minorDecompressor_DeflateReader_State__Failed					= 3,		/* Entered in case of a unrecoverable failure ... */
};

/*
	The following state controls output buffering
*/
enum minorDecompressor_Deflate_OutputBuffer_State {
	minorDecompressor_Deflate_OutputBuffer_State__Normal			= 0,		/* Output buffer does not contain any unprocessed bytes */
	minorDecompressor_Deflate_OutputBuffer_State__Unprocessed		= 1,		/* Contains unprocessed states */
	minorDecompressor_Deflate_OutputBuffer_State__LookbackRestart	= 2,		/* Restart a lookback from minorDeflate_Out_LZ77Decode_Lookback */
};

#define minorDecompressor_Deflate__StateFlags__LastBlock			0x00000001	/* The currently processed block is the last block */
struct minorDecompressor_Deflate {
	struct minorDecompressor					base;

	struct minorSystemInterface*				lpSystemAPI;

	/* State machine status */
	enum minorDecompressor_Deflate_State		state;
	enum minorDecompressor_DeflateReader_State	stateReader;
	uint32_t									stateFlags;

	/* Reader state */
	uint32_t									readerByteBuffer;				/* Used to cache one byte at a time that gets sliced into individual bits (from LSB to MSB) */
	uint8_t										readerCurrentBit;				/* Index of the current bit from 0 to 31. The mask is generated by 0x01 << readerCurrentBit */
	uint8_t										lastSymbol;						/* Used by length and distance decoder */
	unsigned long int 							extraBits;

	/* Output buffer state */
	enum minorDecompressor_Deflate_OutputBuffer_State	obState;				/* Tells us if there are any bytes that haven't been written into a sink / read from the decoder that are currently available */
	unsigned long int							dwRemainingBase;				/* If ob state is unprocessed this points to the first byte that will have to be read by the application */
	unsigned long int							dwRemainingBytes;				/* Amount of bytes that will have to be read */
	unsigned long int							obStateLookbackBase;			/* This is used during completion of an LZ77 lookback */
	unsigned long int							obStateLookbackBaseI;			/* This is used during completion of an LZ77 lookback */
	unsigned long int							obStateLookbackLength;			/* This is used during completion of an LZ77 lookback */

	/*
		Huffman table state

		-) HLIT: The number of literal/length codelength entries - 257 (257-286)
		-) HDIST: The number of distance codes - 1 (1-32)
		-) HCLEN: The number of codelength codes -4 (4-19)

		-) Codelength buffer
			The codelength buffer is separated into 3 areas:

			0-18		Maximum of 19 code length code lengths (note: Special order inside RFC! We do not reorder to allow backreferencing to work)
			19-304		Maximum of 286 literal/length codelength entries (in ascending order)
			305-336		Maximum of 32 distance codelength entries (in ascending order)

		-) Huffman table for code lengths. This is a table that
			maps from Huffman codes to 8 bit integers
		-) Huffman table for literal/length codes. This is a table that maps from
			Huffman codes to 16 bit integers.
		-) Huffman table for distance codes. This is a table that maps from
			Huffman codes to 16 bit integers.

		NOTE FOR HUFFMAN TABLES: Beware that the length and distance alphabet
			use Huffman codes only to specify prefixes to binary data embedded
			inside the compressed data!
	*/
	uint8_t										HLIT;
	uint8_t										HDIST;
	uint8_t										HCLEN;

	union {
		struct {
			unsigned long int					readCodes;
		} ReadCodelengthCodelengths;
	} s;

	#define minorDecompressor_Deflate__codelengthBuffer_Base_CodeLengthTable			0
	#define minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable			19
	#define minorDecompressor_Deflate__codelengthBuffer_Base_DistanceLengthTable		305
	uint16_t									codelengthBuffer[286+32+19];

	/*
		Root nodes for huffman trees.
	*/
	struct minorDecompressor_Deflate_HufftreeEntry 	lpHufftreeRoot_Codes;
	struct minorDecompressor_Deflate_HufftreeEntry 	lpHufftreeRoot_LiteralLength;
	struct minorDecompressor_Deflate_HufftreeEntry 	lpHufftreeRoot_Distance;

	/*
		State variables for huffman tree decoding (pointing to the LAST reached node)
		As soon as a value is decoded they get reset to their corresponding root nodes.
	*/
	struct minorDecompressor_Deflate_HufftreeEntry* lpHufftreeCurrent_Codes;
	struct minorDecompressor_Deflate_HufftreeEntry* lpHufftreeCurrent_LiteralLength;
	struct minorDecompressor_Deflate_HufftreeEntry* lpHufftreeCurrent_Distance;

	/*
		Attached stream interfaces
	*/
	struct minorStreamSource*					lpDataSource;
	struct minorStreamSink*						lpDataSink;

	/*
		State used by the LZ77 decoder
	*/
	unsigned long int							dwLastLength;
	unsigned long int							dwLastDistance;


	uint8_t										usedBufferBitmap; /* Bit 0: First buffer, Bit 1: Second buffer. Bit 7 determines if the current buffer is the first one to be processes (0) or if others have been processed previously (1) */
	unsigned long int							outputTail; /* This points to the NEXT by to be written to */
	unsigned long int							outputWindowSize; /* This is the size of BOTH buffers ... always divideable by 2 */
	#ifdef MINOR_DEFLATE_STREAMING_ENABLE
		/*
			Used if some data from the next output buffer segment has already been written (fo
			example in case of a partial block). This is required to support streaming of deflated data
			so that each finished deflate block always initiates a data push.
		*/
		unsigned long int						outputBufferWritten;
	#endif
	uint8_t										outputBuffer[]; /* 2x 32 KByte Output buffers. We cannot use smaller buffers if we want to support ALL allowed encoders according to the RFC */
};

/*
	Output buffer flushing routines
*/

static enum minorError minorDeflate_Out_LZ77Decode_Byte(	/* Note: Forward declaration - see below */
	struct minorDecompressor_Deflate* lpState,
	uint8_t byte
);
static enum minorError minorDeflate_OutputBuffer_Continue(
	struct minorDecompressor_Deflate* lpState
) {
	enum minorError e;

	while(lpState->obState != minorDecompressor_Deflate_OutputBuffer_State__Normal) {
		unsigned long int dwBytesWritten;
		unsigned long int i;

		while(lpState->obState == minorDecompressor_Deflate_OutputBuffer_State__Unprocessed) {
			/*
				Note that we do NOT modify the byte counter for anything
				that has to be flushed from the output buffer because this
				function counts READ (pulled) bytes, not WRITTEN bytes.
			*/
			dwBytesWritten = 0;
			e = lpState->lpDataSink->write(
				lpState->lpDataSink,
				&(lpState->outputBuffer[lpState->dwRemainingBase]),
				lpState->dwRemainingBytes,
				&dwBytesWritten
			);

			/*
				Update our state counter (remaining bytes in the output buffer)
			*/
			lpState->dwRemainingBytes = lpState->dwRemainingBytes - dwBytesWritten;
			lpState->dwRemainingBase = lpState->dwRemainingBase + dwBytesWritten;

			if(lpState->dwRemainingBytes == 0) {
				lpState->obState = minorDecompressor_Deflate_OutputBuffer_State__Normal;
			}

			/*
				In case of any error - we abort
			*/
			if(e != minorE_Ok) { return e; }
		}

		if(lpState->obState == minorDecompressor_Deflate_OutputBuffer_State__LookbackRestart) {
			for(i = lpState->obStateLookbackBaseI; i < lpState->obStateLookbackLength; i=i+1) {
				e = minorDeflate_Out_LZ77Decode_Byte(lpState, lpState->outputBuffer[(lpState->obStateLookbackBase+i) % lpState->outputWindowSize]);
				if(e != minorE_Ok) {
					/* Update lookback state */
					lpState->obStateLookbackBaseI = i;
					/* Return state */
					return e;
				}
			}

			if(lpState->dwRemainingBytes > 0) {
				lpState->obState = minorDecompressor_Deflate_OutputBuffer_State__Unprocessed;
			} else {
				lpState->obState = minorDecompressor_Deflate_OutputBuffer_State__Normal;
			}
		}
	}

	return (lpState->state != minorDecompressor_Deflate_State__Done) ? minorE_Ok : minorE_Finished;
}


/*
	LZ77 Functions
*/

static enum minorError minorDeflate_Out_LZ77Decode_Byte(
	struct minorDecompressor_Deflate* lpState,
	uint8_t byte
) {
	unsigned char* lpBytesWriteRequest_Off;
	unsigned long int dwBytesWritten;
	unsigned long int dwBytesWriteRequest;
	unsigned long int dwBytesWriteRequest_Off;
	enum minorError e;
	/*
		Write another byte into our output buffer. If
		we have completed a whole buffer, flush the
		OLD one and set our current marker to the empty
		one again.
	*/

	/* Store byte into buffer ... */
	lpState->outputBuffer[lpState->outputTail] = byte;
	lpState->outputTail = (lpState->outputTail + 1) % (lpState->outputWindowSize);

	/*
		Check if we just finished another block ...
		if, we pass it to the attached sink
	*/
	if(lpState->outputTail == 0) {
		/* Finished block 2 */
		#ifdef MINOR_DEFLATE_STREAMING_ENABLE
			lpBytesWriteRequest_Off = &(lpState->outputBuffer[(lpState->outputWindowSize >> 1) + lpState->outputBufferWritten]);
			dwBytesWriteRequest_Off = (lpState->outputWindowSize >> 1) + lpState->outputBufferWritten;
			dwBytesWriteRequest = (lpState->outputWindowSize >> 1) - lpState->outputBufferWritten;
		#else
			lpBytesWriteRequest_Off = &(lpState->outputBuffer[(lpState->outputWindowSize >> 1)]);
			dwBytesWriteRequest_Off = (lpState->outputWindowSize >> 1);
			dwBytesWriteRequest = lpState->outputWindowSize >> 1;
		#endif

		dwBytesWritten = 0;
		e = lpState->lpDataSink->write(
			lpState->lpDataSink,
			lpBytesWriteRequest_Off,
			dwBytesWriteRequest,
			&dwBytesWritten
		);
		lpState->usedBufferBitmap = lpState->usedBufferBitmap | 0x80;

		if(dwBytesWritten != dwBytesWriteRequest) {
			lpState->obState = minorDecompressor_Deflate_OutputBuffer_State__Unprocessed;

			lpState->dwRemainingBase = dwBytesWriteRequest_Off + dwBytesWritten;
			lpState->dwRemainingBytes = dwBytesWriteRequest - dwBytesWritten;

			if(e == minorE_Ok) {
				/* We have to try to write again */
				if((e = minorDeflate_OutputBuffer_Continue(lpState))) {
					return e;
				}
			}
		}

		#ifdef MINOR_DEFLATE_STREAMING_ENABLE
			lpState->outputBufferWritten = 0;
		#endif
	} else if(lpState->outputTail == (lpState->outputWindowSize >> 1)) {
		/* Finished block 1 */
		#ifdef MINOR_DEFLATE_STREAMING_ENABLE
			lpBytesWriteRequest_Off = &(lpState->outputBuffer[lpState->outputBufferWritten]);
			dwBytesWriteRequest_Off = lpState->outputBufferWritten;
			dwBytesWriteRequest = (lpState->outputWindowSize >> 1) - lpState->outputBufferWritten;
		#else
			lpBytesWriteRequest_Off = &(lpState->outputBuffer[0]);
			dwBytesWriteRequest_Off = 0;
			dwBytesWriteRequest = lpState->outputWindowSize >> 1;
		#endif

		dwBytesWritten = 0;
		e = lpState->lpDataSink->write(
			lpState->lpDataSink,
			lpBytesWriteRequest_Off,
			dwBytesWriteRequest,
			&dwBytesWritten
		);
		lpState->usedBufferBitmap = lpState->usedBufferBitmap | 0x80;

		#ifdef MINOR_DEFLATE_STREAMING_ENABLE
			lpState->outputBufferWritten = 0;
		#endif

		if(dwBytesWritten != dwBytesWriteRequest) {
			lpState->obState = minorDecompressor_Deflate_OutputBuffer_State__Unprocessed;
			lpState->dwRemainingBase = dwBytesWriteRequest_Off + dwBytesWritten;
			lpState->dwRemainingBytes = dwBytesWriteRequest - dwBytesWritten;

			if(e == minorE_Ok) {
				/* We have to try to write again */
				if((e = minorDeflate_OutputBuffer_Continue(lpState))) {
					return e;
				}
			}
		}
	} else {
		e = minorE_Ok;
	}

	return e;
}

static enum minorError minorDeflate_Out_LZ77Decode_Lookback(
	struct minorDecompressor_Deflate* lpState,
	unsigned long int dwLength,
	unsigned long int dwDistance
) {
	enum minorError e;
	unsigned long int dwLookbackBase;
	unsigned long int i;
	/*
		Look back into our output stream and copy
		corresponding bytes (notice: input and output
		may overlap!)

		First check if distance lies inside the reachable area
	*/
	if((lpState->usedBufferBitmap & 0x80) == 0) {
		if(dwDistance >= lpState->outputTail) {
			return minorE_InvalidParam;
		}
	} else {
		if(dwDistance >= (lpState->outputTail + (lpState->outputWindowSize >> 1))) {
			return minorE_InvalidParam;
		}
	}

	if(dwDistance <= lpState->outputTail) {
		dwLookbackBase = lpState->outputTail - dwDistance;
	} else {
		dwLookbackBase = (lpState->outputWindowSize + lpState->outputTail - dwDistance) % (lpState->outputWindowSize);
	}

	for(i = 0; i < dwLength; i=i+1) {
		e = minorDeflate_Out_LZ77Decode_Byte(lpState, lpState->outputBuffer[(dwLookbackBase+i) % lpState->outputWindowSize]);
		if(e != minorE_Ok) {
			lpState->obState = minorDecompressor_Deflate_OutputBuffer_State__LookbackRestart;
			lpState->obStateLookbackBaseI = i;
			lpState->obStateLookbackBase = dwLookbackBase;
			lpState->obStateLookbackLength = dwLength;
			return e;
		}
	}
	return minorE_Ok;
}






/*
	Huffman Tree Utility Functions.

	1) Generation of a huffman tree for a continuous alphabet:
		This requires a buffer of 16 bit entries that contain the length
		of each code for each values as well as the base value.

		If one passes for example base value = 1 and lengths [ 2, 3, 3, 1 ]
		one gets:
			value	length		code
			1		2			10
			2		3			110
			3		3			111
			4		1			0

		The insertNode function is used during tree construction.
*/


static enum minorError minorDeflate_Hufftree_InsertNode(
	struct minorDecompressor_Deflate_HufftreeEntry* lpRoot,

	unsigned long int dwCodeLength,
	uint32_t code,
	uint16_t value,

	struct minorSystemInterface* lpSystem
) {
	struct minorDecompressor_Deflate_HufftreeEntry* lpCur = lpRoot;
	struct minorDecompressor_Deflate_HufftreeEntry* lpNew;
	unsigned long int i;
	uint32_t codeTemp = code;
	enum minorError e;

	/* Parameter validation */
	if(lpRoot == NULL) { return minorE_InvalidParam; }
	if(dwCodeLength > 18) { return minorE_InvalidParam; } /* See RFC ... */
	if(dwCodeLength == 0) { return minorE_InvalidParam; } /* There are no codes with length 0 ... */

	codeTemp = codeTemp << (32-dwCodeLength);

	/*
		Traverse tree from root downwards and
		insert value at given position, allocating new branches
		as necassary
	*/
	for(i = 0; i < dwCodeLength; i=i+1) {
		if((codeTemp & 0x80000000) != 0) {
			/* We encountered a one */
			if(lpCur->lpOne != NULL) {
				lpCur = lpCur->lpOne;
			} else {
				if((e = lpSystem->alloc((void**)(&lpNew), sizeof(struct minorDecompressor_Deflate_HufftreeEntry), lpSystem->lpFreeParam_Alloc)) != minorE_Ok) {
					return e;
				}
				lpNew->lpZero = NULL;
				lpNew->lpOne = NULL;
				lpCur->lpOne = lpNew;
				lpCur = lpNew;
			}
		} else {
			/* We encountered a zero */
			if(lpCur->lpZero) {
				lpCur = lpCur->lpZero;
			} else {
				if((e = lpSystem->alloc((void**)(&lpNew), sizeof(struct minorDecompressor_Deflate_HufftreeEntry), lpSystem->lpFreeParam_Alloc)) != minorE_Ok) {
					return e;
				}
				lpNew->lpZero = NULL;
				lpNew->lpOne = NULL;
				lpCur->lpZero = lpNew;
				lpCur = lpNew;
			}
		}

		codeTemp = codeTemp << 1;
	}
	lpCur->value = value;
	return minorE_Ok;
}

static void minorDeflate_HufftreeRelease_Recursive(
	struct minorDecompressor_Deflate_HufftreeEntry* lpNode,
	struct minorSystemInterface* lpSystem
) {
	if(lpNode->lpZero != NULL) { minorDeflate_HufftreeRelease_Recursive(lpNode->lpZero, lpSystem); lpNode->lpZero = NULL; }
	if(lpNode->lpOne != NULL) { minorDeflate_HufftreeRelease_Recursive(lpNode->lpOne, lpSystem); lpNode->lpOne = NULL; }

	lpSystem->free((void*)lpNode, lpSystem->lpFreeParam_Free);
	return;
}

static enum minorError minorDeflate_HufftreeRelease(
	struct minorDecompressor_Deflate_HufftreeEntry* lpRoot,
	struct minorSystemInterface* lpSystem
) {
	if((lpRoot == NULL) || (lpSystem == NULL)) { return minorE_InvalidParam; }

	/* Release all nodes EXCEPT the root ... */
	if(lpRoot->lpZero != NULL) { minorDeflate_HufftreeRelease_Recursive(lpRoot->lpZero, lpSystem); lpRoot->lpZero = NULL; }
	if(lpRoot->lpOne != NULL) { minorDeflate_HufftreeRelease_Recursive(lpRoot->lpOne, lpSystem); lpRoot->lpOne = NULL; }
	lpRoot->value = 0;

	return minorE_Ok;
}

static enum minorError minorDeflate_CreateHufftreeFromLengths(
	struct minorDecompressor_Deflate_HufftreeEntry* lpRoot,

	uint16_t baseValue,
	uint16_t* lpLengthArray,
	unsigned long int dwLengthCount,

	struct minorSystemInterface* lpSystem
) {
	uint16_t dwLongestLength = 0;
	uint32_t code = 0;
	unsigned long int* lpCountPerLength = NULL;
	unsigned long int i, j;
	enum minorError e;

	/* Input parameter validation */
	if((lpSystem == NULL) || (dwLengthCount == 0) || (lpLengthArray == NULL)) { return minorE_InvalidParam; }

	/* Determine longest length and create our length index */
	dwLongestLength = 0;
	for(i = 0; i < dwLengthCount; i=i+1) {
		#ifdef MINOR_DEFLATE_DEBUG
			printf("%s:%u Length for symbol %3lu is %u\n", __FILE__, __LINE__, i, lpLengthArray[i]);
		#endif
		if(dwLongestLength < lpLengthArray[i]) { dwLongestLength = lpLengthArray[i]; }
	}

	/*
		Note: This situation is possible in case we don't have any lookback (HDIST = 0)
	*/
	if(dwLongestLength == 0) {
		return minorE_Ok;
	}

	/* Now create a table with number of items per length ... */
	if((e = lpSystem->alloc((void**)(&lpCountPerLength), sizeof(unsigned long int)*dwLongestLength, lpSystem->lpFreeParam_Alloc)) != minorE_Ok) { goto cleanup; }

	/* Initialize counts to 0 */
	for(i = 0; i < dwLongestLength-1; i=i+1) { lpCountPerLength[i] = 0; }

	/* Count elements per length */
	for(i = 0; i < dwLengthCount; i=i+1) {
		if(lpLengthArray[i] != 0) {
			lpCountPerLength[lpLengthArray[i]-1] = lpCountPerLength[lpLengthArray[i]-1] + 1;
		}
	}

	/*
		Now we know the counts per length we can calculate
		the base codes and insert into symbol table trees.
	*/
	#ifdef MINOR_DEFLATE_DEBUG
		printf("%s:%u Generating codetable:\n", __FILE__, __LINE__);
	#endif
	code = 0;
	for(i = 0; i < dwLongestLength; i=i+1) {
		/* Use our last code as prefix ... */
		code = code << 1;

		/* Now locate all symbols with the given length and assign their codes ... */
		for(j = 0; j < dwLengthCount; j=j+1) {
			if(lpLengthArray[j] == i+1) {
				/* The symbol "j" should be inserted into the table with the next code */
				#ifdef MINOR_DEFLATE_DEBUG
					printf("%s:%u Symbol %3lu code: ", __FILE__, __LINE__, j+baseValue);
					for(int dbgI = 0; dbgI < i+1; dbgI=dbgI+1) {
						printf("%c", ((code >> dbgI) & 0x01) == 0 ? 0x30 : 0x31);
					}
					printf("\n");
				#endif
				e = minorDeflate_Hufftree_InsertNode(lpRoot, i+1, code, j+baseValue, lpSystem);
				if(e != minorE_Ok) { goto cleanup; }

				code = code + 1;
			}
		}
	}

	e = minorE_Ok;

cleanup:
	if(lpCountPerLength != NULL) { lpSystem->free((void*)lpCountPerLength, lpSystem->lpFreeParam_Free); lpCountPerLength = NULL; }
	return e;
}



/*
	Lookup tables for length and distance codes. They are indexed by the
	code - basecode and contain the additional bit count and the base
	for the additional length.
*/
#define minorDeflate__LUT__LengthCodes__CODECOUNT 29
static uint16_t minorDeflate__LUT__LengthCodes[minorDeflate__LUT__LengthCodes__CODECOUNT][2] = {
	{ 0,   3 }, /* 257 */
	{ 0,   4 }, /* 258 */
	{ 0,   5 }, /* 259 */
	{ 0,   6 }, /* 260 */
	{ 0,   7 }, /* 261 */
	{ 0,   8 }, /* 262 */
	{ 0,   9 }, /* 263 */
	{ 0,  10 }, /* 264 */
	{ 1,  11 }, /* 265 */
	{ 1,  13 }, /* 266 */
	{ 1,  15 }, /* 267 */
	{ 1,  17 }, /* 268 */
	{ 2,  19 }, /* 269 */
	{ 2,  23 }, /* 270 */
	{ 2,  27 }, /* 271 */
	{ 2,  31 }, /* 272 */
	{ 3,  35 }, /* 273 */
	{ 3,  43 }, /* 274 */
	{ 3,  51 }, /* 275 */
	{ 3,  59 }, /* 276 */
	{ 4,  67 }, /* 277 */
	{ 4,  83 }, /* 278 */
	{ 4,  99 }, /* 279 */
	{ 4, 115 }, /* 280 */
	{ 5, 131 }, /* 281 */
	{ 5, 163 }, /* 282 */
	{ 5, 195 }, /* 283 */
	{ 5, 227 }, /* 284 */
	{ 0, 258 }  /* 285 */
};

#define minorDeflate__LUT__DistanceCodes__CODECOUNT 30
static uint16_t minorDeflate__LUT__DistanceCodes[minorDeflate__LUT__DistanceCodes__CODECOUNT][2] = {
	{  0,     1},
	{  0,     2},
	{  0,     3},
	{  0,     4},
	{  1,     5},
	{  1,     7},
	{  2,     9},
	{  2,    13},
	{  3,    17},
	{  3,    25},
	{  4,    33},
	{  4,    49},
	{  5,    65},
	{  5,    97},
	{  6,   129},
	{  6,   193},
	{  7,   257},
	{  7,   385},
	{  8,   513},
	{  8,   769},
	{  9,  1025},
	{  9,  1537},
	{ 10,  2049},
	{ 10,  3073},
	{ 11,  4097},
	{ 11,  6145},
	{ 12,  8193},
	{ 12, 12289},
	{ 13, 16385},
	{ 13, 24577}
};

/*
	Bitreader state machine
*/
static enum minorError minorDeflate_ProcessBit(
	struct minorDecompressor_Deflate* lpState,
	uint8_t bit
) {
	enum minorError e;
	unsigned long int i;
	struct minorDecompressor_Deflate_HufftreeEntry* lpNextHuff;
	uint16_t decodedValue;
	unsigned long int dwRemaining;
	unsigned long int dwWritten;

	if(lpState == NULL) { return minorE_InvalidParam; }

	switch(lpState->state) {
		case minorDecompressor_Deflate_State__ReadBlockHeader:
			lpState->readerByteBuffer = (lpState->readerByteBuffer >> 1) | ((bit == 0) ? 0 : 0x80);
			if((lpState->readerCurrentBit = lpState->readerCurrentBit + 1) == 3) {
				/* We have a complete block header. Switch to appropriate state */
				lpState->readerByteBuffer = lpState->readerByteBuffer >> 5;
				if((lpState->readerByteBuffer & 0x01) == 0) {
					/* This will not be the last block */
					lpState->stateFlags = lpState->stateFlags & (~minorDecompressor_Deflate__StateFlags__LastBlock);
				} else {
					/* This will be the last block */
					lpState->stateFlags = lpState->stateFlags | minorDecompressor_Deflate__StateFlags__LastBlock;
				}

				if(((lpState->readerByteBuffer >> 1) & 0x03) == 0x00) {
					/*
						Uncompressed block, read bytewise after bytewise header.
						We swap states and force byte alignment of the reader.
					*/
					lpState->state = minorDecompressor_Deflate_State__ReadUncompressedHeader;
					lpState->stateReader = minorDecompressor_DeflateReader_State__Bytewise; /* We have to switch to bytewise ... */
					lpState->readerCurrentBit = 0;
					lpState->readerByteBuffer = 0;

					#ifdef MINOR_DEFLATE_DEBUG
						printf("%s:%u [DEFLATE] Read raw block header\n", __FILE__, __LINE__);
					#endif

					return minorE_OkAlignByte; /* ... and synchronize with byte boundary */
				} else if(((lpState->readerByteBuffer >> 1) & 0x03) == 0x01) {
					/*
						Read with fixed huffman codes
						Generate predefined tables
					*/
					#ifdef MINOR_DEFLATE_DEBUG
						printf("%s:%u [DEFLATE] Using fixed huffman trees\n", __FILE__, __LINE__);
					#endif

					for(i =   0; i < 144; i=i+1) 	{ lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable+i] = 8; }
					for(i = 144; i < 256; i=i+1)	{ lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable+i] = 9; }
					for(i = 256; i < 280; i=i+1) 	{ lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable+i] = 7; }
					for(i = 280; i < 288; i=i+1) 	{ lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable+i] = 8; }

					e = minorDeflate_CreateHufftreeFromLengths(
						&(lpState->lpHufftreeRoot_LiteralLength),
						0,
						&(lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable]),
						288,
						lpState->lpSystemAPI
					);
					if(e != minorE_Ok) {
						lpState->state = minorDecompressor_Deflate_State__Error;
						return e;
					}

					for(i = 0; i < 32; i=i+1) 		{ lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_DistanceLengthTable+i] = 5; }

					e = minorDeflate_CreateHufftreeFromLengths(
						&(lpState->lpHufftreeRoot_Distance),
						0,
						&(lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_DistanceLengthTable]),
						32,
						lpState->lpSystemAPI
					);
					if(e != minorE_Ok) {
						lpState->state = minorDecompressor_Deflate_State__Error;
						return e;
					}
					#ifdef MINOR_DEFLATE_DEBUG
						printf("%s:%u [DEFLATE] Build trees\n", __FILE__, __LINE__);
					#endif

					/* switch to huffman coded data state ... */
					lpState->lpHufftreeCurrent_LiteralLength = &(lpState->lpHufftreeRoot_LiteralLength);
					lpState->lpHufftreeCurrent_Distance = &(lpState->lpHufftreeRoot_Distance);
					lpState->s.ReadCodelengthCodelengths.readCodes = 0;
					lpState->state = minorDecompressor_Deflate_State__ReadHuff_LiteralLengthCode;

					return minorE_Ok;
				} else if(((lpState->readerByteBuffer >> 1) & 0x03) == 0x02) {
					/* Read with dynamic huffman codes */
					#ifdef MINOR_DEFLATE_DEBUG
						printf("%s:%u [DEFLATE] Using dynamic huffman trees\n", __FILE__, __LINE__);
					#endif
					lpState->state = minorDecompressor_Deflate_State__ReadDynamicHeader;
					lpState->readerCurrentBit = 0;
					lpState->readerByteBuffer = 0;
					return minorE_Ok;
				} else {
					lpState->state = minorDecompressor_Deflate_State__Error;
					#ifdef MINOR_DEFLATE_DEBUG
						printf("%s:%u [DEFLATE] Encoding error\n", __FILE__, __LINE__);
					#endif
					#ifdef MINOR_DEFLATE_DEBUG
						printf("%s:%u [DEFLATE] Encoding error\n", __FILE__, __LINE__);
					#endif
					return minorE_EncodingError;
				}
			}
			return minorE_Ok;

/* == Reading code tables == */
		case minorDecompressor_Deflate_State__ReadDynamicHeader:
			/* In this state we will read HLIST, HDIST and HCLEN */
			lpState->readerByteBuffer = (lpState->readerByteBuffer >> 1) | ((bit == 0) ? 0 : 0x8000);
			if((lpState->readerCurrentBit = lpState->readerCurrentBit + 1) == 14) {
				/* We've read all 14 bits */
				lpState->readerByteBuffer = lpState->readerByteBuffer >> 2;
				lpState->HLIT = (uint8_t)(lpState->readerByteBuffer & 0x1F);
				lpState->HDIST = (uint8_t)((lpState->readerByteBuffer >> 5) & 0x1F);
				lpState->HCLEN = (uint8_t)((lpState->readerByteBuffer >> 10) & 0x0F);

				lpState->readerByteBuffer = 0;
				lpState->readerCurrentBit = 0;

				lpState->state = minorDecompressor_Deflate_State__ReadCodelengthCodelengths;
				lpState->s.ReadCodelengthCodelengths.readCodes = 0;

				for(i = 0; i < minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable; i=i+1) { lpState->codelengthBuffer[i] = 0x00; } /* Zero lengths for codelengths */
			}
			return minorE_Ok;

		case minorDecompressor_Deflate_State__ReadCodelengthCodelengths:
			/* decode (HCLEN+4) * 3 bit codes ... store every 3 bits */
			lpState->readerByteBuffer = (lpState->readerByteBuffer >> 1) | ((bit == 0) ? 0 : 0x80);
			if((lpState->readerCurrentBit = lpState->readerCurrentBit + 1) == 3) {
				/* We've read the next code. Store */

				lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_CodeLengthTable+lpState->s.ReadCodelengthCodelengths.readCodes] = (lpState->readerByteBuffer >> 5) & 0x07;
				lpState->s.ReadCodelengthCodelengths.readCodes = lpState->s.ReadCodelengthCodelengths.readCodes + 1;
				lpState->readerCurrentBit = 0;
				lpState->readerByteBuffer = 0;

				if(lpState->s.ReadCodelengthCodelengths.readCodes == (lpState->HCLEN + 4)) {
					/*
						We've read all codes. Build huffman tree for these codelengtsh.
						We have to TEMPORARILY reorder them (and use the Literal Length Table area to do so ...)
					*/
					lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable+ 0] = lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_CodeLengthTable+ 3];
					lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable+ 1] = lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_CodeLengthTable+17];
					lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable+ 2] = lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_CodeLengthTable+15];
					lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable+ 3] = lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_CodeLengthTable+13];
					lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable+ 4] = lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_CodeLengthTable+11];
					lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable+ 5] = lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_CodeLengthTable+ 9];
					lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable+ 6] = lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_CodeLengthTable+ 7];
					lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable+ 7] = lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_CodeLengthTable+ 5];
					lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable+ 8] = lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_CodeLengthTable+ 4];
					lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable+ 9] = lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_CodeLengthTable+ 6];
					lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable+10] = lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_CodeLengthTable+ 8];
					lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable+11] = lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_CodeLengthTable+10];
					lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable+12] = lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_CodeLengthTable+12];
					lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable+13] = lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_CodeLengthTable+14];
					lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable+14] = lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_CodeLengthTable+16];
					lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable+15] = lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_CodeLengthTable+18];
					lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable+16] = lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_CodeLengthTable+ 0];
					lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable+17] = lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_CodeLengthTable+ 1];
					lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable+18] = lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_CodeLengthTable+ 2];

					e = minorDeflate_CreateHufftreeFromLengths(
						&(lpState->lpHufftreeRoot_Codes),
						0,
						&(lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable+0]),
						19,
						lpState->lpSystemAPI
					);

					if(e != minorE_Ok) {
						lpState->state = minorDecompressor_Deflate_State__Error;
						return e;
					}

					/* Zero literal and distance codes */
					for(i = 0; i < ((sizeof(lpState->codelengthBuffer)/sizeof(uint16_t))-19); i=i+1) { lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable+i] = 0x0000; }

					/* We will read the HLIT and HDIST codes next ... */
					lpState->s.ReadCodelengthCodelengths.readCodes = 0;
					lpState->state = minorDecompressor_Deflate_State__ReadLiteralCodelengths;
					lpState->lpHufftreeCurrent_Codes = &(lpState->lpHufftreeRoot_Codes); /* And we start traversing the hufftree from it's root */
				}
			}
			return minorE_Ok;

		case minorDecompressor_Deflate_State__ReadLiteralCodelengths:
			/*
				Reading huffman coded codelengths for literal and length codelengths.
				Every bit that we encounter traverses the huffman tree for the codelength alphabet.
			*/
			if(bit == 0) {
				lpNextHuff = lpState->lpHufftreeCurrent_Codes->lpZero;
			} else {
				lpNextHuff = lpState->lpHufftreeCurrent_Codes->lpOne;
			}
			if(lpNextHuff != NULL) {
				lpState->lpHufftreeCurrent_Codes = lpNextHuff;
				if((lpNextHuff->lpZero != NULL) || (lpNextHuff->lpOne != NULL)) {
					return minorE_Ok;
				}
			}
			/* We have decoded an entire value ... */
			decodedValue = lpState->lpHufftreeCurrent_Codes->value;
			lpState->lpHufftreeCurrent_Codes = &(lpState->lpHufftreeRoot_Codes); /* Reset huffman tree traversal for next run */

			if(decodedValue < 16) {
				/* We have received a literal codelength ... store and continue */
				lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable+lpState->s.ReadCodelengthCodelengths.readCodes] = decodedValue;
				lpState->s.ReadCodelengthCodelengths.readCodes = lpState->s.ReadCodelengthCodelengths.readCodes + 1;
				if(lpState->s.ReadCodelengthCodelengths.readCodes == (lpState->HLIT + 257)) {
					/* We are done ... next state will decode distance lengths */
					lpState->s.ReadCodelengthCodelengths.readCodes = 0;
					lpState->state = minorDecompressor_Deflate_State__ReadDistanceCodelengths;
				}
				return minorE_Ok;
			}

			/*
				These codes have a different meaning:
					16:	Copy previous code length 3-6 times. Two additional bits encode that length (0->3, 1->4, 2->5, 3->6) ...
					17: Repeat a code length of ZERO for 3-10 times (3 bits of length)
					18: Repeat a code length of ZERO for 11-138 times (7 bits of length)
			*/
			lpState->state = minorDecompressor_Deflate_State__ReadLiteralCodelengths__AdditionalBits;
			lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable+lpState->s.ReadCodelengthCodelengths.readCodes] = decodedValue; /* CACHE our value there. Its NOT the length ... */
			lpState->readerByteBuffer = 0;
			if(decodedValue == 16) {
				lpState->readerCurrentBit = 2; /* We read two additional bits */
				return minorE_Ok;
			} else if(decodedValue == 17) {
				lpState->readerCurrentBit = 3; /* We read three additional bits */
				return minorE_Ok;
			} else if(decodedValue == 18) {
				lpState->readerCurrentBit = 7; /* We read seven additional bits */
				return minorE_Ok;
			} else {
				lpState->state = minorDecompressor_Deflate_State__Error;
				#ifdef MINOR_DEFLATE_DEBUG
					printf("%s:%u [DEFLATE] Encoding error\n", __FILE__, __LINE__);
				#endif
				return minorE_EncodingError;
			}
			/* DONE */
		case minorDecompressor_Deflate_State__ReadLiteralCodelengths__AdditionalBits:
			/* Fetch additional bits to decode a literal or length code */
			lpState->readerByteBuffer = (lpState->readerByteBuffer >> 1) | ((bit == 0) ? 0 : 0x80);
			if((lpState->readerCurrentBit = lpState->readerCurrentBit - 1) == 0) {
				/* We fetched all bits ... check what we should do with them ... */
				if(lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable+lpState->s.ReadCodelengthCodelengths.readCodes] == 16) {
					/* Repeat the previous code this number of times */
					if((((lpState->readerByteBuffer >> 6)+3) + lpState->s.ReadCodelengthCodelengths.readCodes) > (lpState->HLIT+257+lpState->HDIST+1)) {
						lpState->state = minorDecompressor_Deflate_State__Error;
						#ifdef MINOR_DEFLATE_DEBUG
							printf("%s:%u [DEFLATE] Encoding error\n", __FILE__, __LINE__);
						#endif
						return minorE_EncodingError;
					}

					/* Fetch the code to repeat ... */
					if(lpState->s.ReadCodelengthCodelengths.readCodes == 0) {
						/* Lookback into code length alphabet codes */
						decodedValue = lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_CodeLengthTable+lpState->HCLEN+4-1];
					} else {
						/* Use last decoded value */
						decodedValue = lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable+lpState->s.ReadCodelengthCodelengths.readCodes-1];
					}

					for(i = 0; i < ((lpState->readerByteBuffer >> 6)+3); i=i+1) {
						if((lpState->HLIT+257) > lpState->s.ReadCodelengthCodelengths.readCodes) {
							lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable+lpState->s.ReadCodelengthCodelengths.readCodes] = decodedValue;
						} else {
							/* Handle overlapping arrays */
							lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_DistanceLengthTable+(lpState->s.ReadCodelengthCodelengths.readCodes - (lpState->HLIT+257))] = decodedValue;
						}
						lpState->s.ReadCodelengthCodelengths.readCodes = lpState->s.ReadCodelengthCodelengths.readCodes + 1;
					}
				} else if(lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable+lpState->s.ReadCodelengthCodelengths.readCodes] == 17) {
					if((((lpState->readerByteBuffer >> 5)+3) + lpState->s.ReadCodelengthCodelengths.readCodes) > (lpState->HLIT+257+lpState->HDIST+1)) {
						lpState->state = minorDecompressor_Deflate_State__Error;
						#ifdef MINOR_DEFLATE_DEBUG
							printf("%s:%u [DEFLATE] Encoding error\n", __FILE__, __LINE__);
						#endif
						return minorE_EncodingError;
					}
					for(i = 0; i < ((lpState->readerByteBuffer >> 5)+3); i=i+1) {
						if((lpState->HLIT+257) > lpState->s.ReadCodelengthCodelengths.readCodes) {
							lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable+lpState->s.ReadCodelengthCodelengths.readCodes] = 0x00;
						} else {
							/* Handle overlapping arrays */
							lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_DistanceLengthTable+(lpState->s.ReadCodelengthCodelengths.readCodes -  - (lpState->HLIT+257))] = 0x00;
						}
						lpState->s.ReadCodelengthCodelengths.readCodes = lpState->s.ReadCodelengthCodelengths.readCodes + 1;
					}
				} else if(lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable+lpState->s.ReadCodelengthCodelengths.readCodes] == 18) {
					if((((lpState->readerByteBuffer >> 1)+11) + lpState->s.ReadCodelengthCodelengths.readCodes) > (lpState->HLIT+257+lpState->HDIST+1)) {
						lpState->state = minorDecompressor_Deflate_State__Error;
						#ifdef MINOR_DEFLATE_DEBUG
							printf("%s:%u [DEFLATE] Encoding error\n", __FILE__, __LINE__);
						#endif
						return minorE_EncodingError;
					}

					for(i = 0; i < ((lpState->readerByteBuffer >> 1)+11); i=i+1) {
						if((lpState->HLIT+257) > lpState->s.ReadCodelengthCodelengths.readCodes) {
							lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable+lpState->s.ReadCodelengthCodelengths.readCodes] = 0x00;
						} else {
							/* Handle overlapping arrays */
							lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_DistanceLengthTable+(lpState->s.ReadCodelengthCodelengths.readCodes - (lpState->HLIT+257))] = 0x00;
						}
						lpState->s.ReadCodelengthCodelengths.readCodes = lpState->s.ReadCodelengthCodelengths.readCodes + 1;
					}
				}

				if((lpState->HLIT+257) <= lpState->s.ReadCodelengthCodelengths.readCodes) {
					/* We are done reading this codes ... */
					lpState->s.ReadCodelengthCodelengths.readCodes = lpState->s.ReadCodelengthCodelengths.readCodes - (lpState->HLIT+257);
					lpState->state = minorDecompressor_Deflate_State__ReadDistanceCodelengths;
				} else {
					/* Transition back into read literal codelengths state */
					lpState->state = minorDecompressor_Deflate_State__ReadLiteralCodelengths;
				}
			}
			return minorE_Ok;

		case minorDecompressor_Deflate_State__ReadDistanceCodelengths:
			/*
				Reading huffman coded codelengths for literal and length codelengths.
				Every bit that we encounter traverses the huffman tree for the codelength alphabet.
			*/
			if(bit == 0) {
				lpNextHuff = lpState->lpHufftreeCurrent_Codes->lpZero;
			} else {
				lpNextHuff = lpState->lpHufftreeCurrent_Codes->lpOne;
			}
			if(lpNextHuff != NULL) {
				lpState->lpHufftreeCurrent_Codes = lpNextHuff;
				if((lpNextHuff->lpZero != NULL) || (lpNextHuff->lpOne != NULL)) {
					return minorE_Ok;
				}
			}
			/* We have decoded an entire value ... */
			decodedValue = lpState->lpHufftreeCurrent_Codes->value;
			lpState->lpHufftreeCurrent_Codes = &(lpState->lpHufftreeRoot_Codes); /* Reset huffman tree traversal for next run */

			if(decodedValue < 16) {
				/* We have received a literal codelength ... store and continue */
				lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_DistanceLengthTable+lpState->s.ReadCodelengthCodelengths.readCodes] = decodedValue;
				lpState->s.ReadCodelengthCodelengths.readCodes = lpState->s.ReadCodelengthCodelengths.readCodes + 1;

				if(lpState->s.ReadCodelengthCodelengths.readCodes == (lpState->HDIST + 1)) {
					/* We are done ... next state will already contain compressed data ... */

					e = minorDeflate_CreateHufftreeFromLengths(
						&(lpState->lpHufftreeRoot_LiteralLength),
						0,
						&(lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable]),
						lpState->HLIT + 257,
						lpState->lpSystemAPI
					);
					if(e != minorE_Ok) {
						lpState->state = minorDecompressor_Deflate_State__Error;
						return e;
					}

					e = minorDeflate_CreateHufftreeFromLengths(
						&(lpState->lpHufftreeRoot_Distance),
						0,
						&(lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_DistanceLengthTable]),
						lpState->HDIST + 1,
						lpState->lpSystemAPI
					);
					if(e != minorE_Ok) {
						lpState->state = minorDecompressor_Deflate_State__Error;
						return e;
					}

					lpState->lpHufftreeCurrent_LiteralLength = &(lpState->lpHufftreeRoot_LiteralLength);
					lpState->lpHufftreeCurrent_Distance = &(lpState->lpHufftreeRoot_Distance);

					lpState->s.ReadCodelengthCodelengths.readCodes = 0;
					lpState->state = minorDecompressor_Deflate_State__ReadHuff_LiteralLengthCode;
				}
				return minorE_Ok;
			}

			/*
				These codes have a different meaning:
					16:	Copy previous code length 3-6 times. Two additional bits encode that length (0->3, 1->4, 2->5, 3->6) ...
					17: Repeat a code length of ZERO for 3-10 times (3 bits of length)
					18: Repeat a code length of ZERO for 11-138 times (7 bits of length)
			*/
			lpState->state = minorDecompressor_Deflate_State__ReadDistanceCodelengths__AdditionalBits;
			lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_DistanceLengthTable+lpState->s.ReadCodelengthCodelengths.readCodes] = decodedValue; /* CACHE our value there. Its NOT the length ... */
			lpState->readerByteBuffer = 0;
			if(decodedValue == 16) {
				lpState->readerCurrentBit = 2; /* We read two additional bits */
				return minorE_Ok;
			} else if(decodedValue == 17) {
				lpState->readerCurrentBit = 3; /* We read two additional bits */
				return minorE_Ok;
			} else if(decodedValue == 18) {
				lpState->readerCurrentBit = 7; /* We read two additional bits */
				return minorE_Ok;
			} else {
				lpState->state = minorDecompressor_Deflate_State__Error;
				#ifdef MINOR_DEFLATE_DEBUG
					printf("%s:%u [DEFLATE] Encoding error\n", __FILE__, __LINE__);
				#endif
				return minorE_EncodingError;
			}

		case minorDecompressor_Deflate_State__ReadDistanceCodelengths__AdditionalBits:
			/* Fetch additional bits to decode a literal or length code */
			lpState->readerByteBuffer = (lpState->readerByteBuffer >> 1) | ((bit == 0) ? 0 : 0x80);
			if((lpState->readerCurrentBit = lpState->readerCurrentBit - 1) == 0) {
				/* We fetched all bits ... check what we should do with them ... */
				if(lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_DistanceLengthTable+lpState->s.ReadCodelengthCodelengths.readCodes] == 16) {
					/* Repeat the previous code this number of times */
					if((((lpState->readerByteBuffer >> 6)+3) + lpState->s.ReadCodelengthCodelengths.readCodes) > (lpState->HDIST+1)) {
						lpState->state = minorDecompressor_Deflate_State__Error;
						#ifdef MINOR_DEFLATE_DEBUG
							printf("%s:%u [DEFLATE] Encoding error\n", __FILE__, __LINE__);
						#endif
						return minorE_EncodingError;
					}

					/* Fetch the code to repeat ... */
					if(lpState->s.ReadCodelengthCodelengths.readCodes == 0) {
						/* Lookback into code length alphabet codes */
						decodedValue = lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable+lpState->HLIT+257-1];
					} else {
						/* Use last decoded value */
						decodedValue = lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_DistanceLengthTable+lpState->s.ReadCodelengthCodelengths.readCodes-1];
					}
					for(i = 0; i < ((lpState->readerByteBuffer >> 6)+3); i=i+1) {
						lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_DistanceLengthTable+lpState->s.ReadCodelengthCodelengths.readCodes] = decodedValue;
						lpState->s.ReadCodelengthCodelengths.readCodes = lpState->s.ReadCodelengthCodelengths.readCodes + 1;
					}
				} else if(lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_DistanceLengthTable+lpState->s.ReadCodelengthCodelengths.readCodes] == 17) {
					if((((lpState->readerByteBuffer >> 5)+3) + lpState->s.ReadCodelengthCodelengths.readCodes) > (lpState->HDIST+1)) {
						lpState->state = minorDecompressor_Deflate_State__Error;
						#ifdef MINOR_DEFLATE_DEBUG
							printf("%s:%u [DEFLATE] Encoding error\n", __FILE__, __LINE__);
						#endif
						return minorE_EncodingError;
					}

					for(i = 0; i < ((lpState->readerByteBuffer >> 5)+3); i=i+1) {
						lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_DistanceLengthTable+lpState->s.ReadCodelengthCodelengths.readCodes] = 0x00;
						lpState->s.ReadCodelengthCodelengths.readCodes = lpState->s.ReadCodelengthCodelengths.readCodes + 1;
					}
				} else if(lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_DistanceLengthTable+lpState->s.ReadCodelengthCodelengths.readCodes] == 18) {
					if((((lpState->readerByteBuffer >> 1)+11) + lpState->s.ReadCodelengthCodelengths.readCodes) > (lpState->HDIST+1)) {
						lpState->state = minorDecompressor_Deflate_State__Error;
						#ifdef MINOR_DEFLATE_DEBUG
							printf("%s:%u [DEFLATE] Encoding error\n", __FILE__, __LINE__);
						#endif
						return minorE_EncodingError;
					}

					for(i = 0; i < ((lpState->readerByteBuffer >> 1)+11); i=i+1) {
						lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_DistanceLengthTable+lpState->s.ReadCodelengthCodelengths.readCodes] = 0x00;
						lpState->s.ReadCodelengthCodelengths.readCodes = lpState->s.ReadCodelengthCodelengths.readCodes + 1;
					}
				} else {
					lpState->state = minorDecompressor_Deflate_State__Error;
					#ifdef MINOR_DEFLATE_DEBUG
						printf("%s:%u [DEFLATE] Encoding error\n", __FILE__, __LINE__);
					#endif
					return minorE_EncodingError;
				}

				if(lpState->HDIST+1 == lpState->s.ReadCodelengthCodelengths.readCodes) {
					e = minorDeflate_CreateHufftreeFromLengths(
						&(lpState->lpHufftreeRoot_LiteralLength),
						0,
						&(lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_LiteralLengthTable]),
						lpState->HLIT + 257,
						lpState->lpSystemAPI
					);
					if(e != minorE_Ok) {
						lpState->state = minorDecompressor_Deflate_State__Error;
						return e;
					}

					e = minorDeflate_CreateHufftreeFromLengths(
						&(lpState->lpHufftreeRoot_Distance),
						0,
						&(lpState->codelengthBuffer[minorDecompressor_Deflate__codelengthBuffer_Base_DistanceLengthTable]),
						lpState->HDIST + 1,
						lpState->lpSystemAPI
					);
					if(e != minorE_Ok) {
						lpState->state = minorDecompressor_Deflate_State__Error;
						return e;
					}

					lpState->lpHufftreeCurrent_LiteralLength = &(lpState->lpHufftreeRoot_LiteralLength);
					lpState->lpHufftreeCurrent_Distance = &(lpState->lpHufftreeRoot_Distance);

					lpState->s.ReadCodelengthCodelengths.readCodes = 0;
					lpState->state = minorDecompressor_Deflate_State__ReadHuff_LiteralLengthCode;
				} else {
					/* Transition back into read distance codelengths state */
					lpState->state = minorDecompressor_Deflate_State__ReadDistanceCodelengths;
				}
			}
			return minorE_Ok;

/* == Reading compressed data */
		case minorDecompressor_Deflate_State__ReadHuff_LiteralLengthCode:
			/* We are currently decoding an literal or length value */
			if(bit == 0) {
				lpNextHuff = lpState->lpHufftreeCurrent_LiteralLength->lpZero;
			} else {
				lpNextHuff = lpState->lpHufftreeCurrent_LiteralLength->lpOne;
			}

			if(lpNextHuff != NULL) {
				lpState->lpHufftreeCurrent_LiteralLength = lpNextHuff;
				if((lpNextHuff->lpZero != NULL) || (lpNextHuff->lpOne != NULL)) {
					return minorE_Ok; /* Continue with next bit from huffman symbol */
				}
			}

			/* We have decoded a length or literal symbol */
			decodedValue = lpState->lpHufftreeCurrent_LiteralLength->value;
			lpState->lpHufftreeCurrent_LiteralLength = &(lpState->lpHufftreeRoot_LiteralLength); /* Restart our search at the next iteration */

			if(decodedValue < 256) {
				/*
					Copy to output buffer; Notice that the output buffer gets cached in 2*Blocksize
					blocks (normally 32K, for deflate64 also 64K per block) to allow backreferences
				*/
				e = minorDeflate_Out_LZ77Decode_Byte(lpState, decodedValue);
				return e; /* And continue in the same state for the next symbol ... */
			}

			/*
				We have decoded one of the "special" symbols:
					256: End of block
					257-285: Lengths followed by an dependent number of bits
			*/
			if(decodedValue == 256) {
				#ifdef MINOR_DEFLATE_DEBUG
					printf("%s:%u [DEFLATE] End of block\n", __FILE__, __LINE__);
				#endif
				/*
					Handle end of block. If we have processed the LAST block we have finished
					the whole processing for the current data stream and switch to finished
					state. If we are processing multiple blocks we DO NOT sync back to the
					next byte boundary but start again with reading the block headers ...
				*/

				/*
					If we have remaining bytes AND are inside the last block, write them into
					the attached data sink.
				*/
				#ifndef MINOR_DEFLATE_STREAMING_ENABLE
					if((lpState->stateFlags & minorDecompressor_Deflate__StateFlags__LastBlock) != 0) {
						if(lpState->outputTail < (lpState->outputWindowSize >> 1)) {
							dwRemaining = lpState->outputTail;
						} else {
							dwRemaining = lpState->outputTail - (lpState->outputWindowSize >> 1);
						}
						if(dwRemaining > 0) {
							dwWritten = 0;
							e = lpState->lpDataSink->write(
								lpState->lpDataSink,
								&(lpState->outputBuffer[lpState->outputTail < (lpState->outputWindowSize >> 1) ? 0 : (lpState->outputWindowSize >> 1)]),
								dwRemaining,
								&dwWritten
							);

							if(dwWritten < dwRemaining) {
								/* Enter output buffering mode */
								lpState->obState = minorDecompressor_Deflate_OutputBuffer_State__Unprocessed;
								lpState->dwRemainingBase = (lpState->outputTail < (lpState->outputWindowSize >> 1) ? 0 : (lpState->outputWindowSize >> 1)) + dwWritten;
								lpState->dwRemainingBytes = dwRemaining - dwWritten;
							}
						}
					}
				#else
					if(lpState->outputTail < (lpState->outputWindowSize >> 1)) {
						dwRemaining = lpState->outputTail - lpState->outputBufferWritten;
					} else {
						dwRemaining = lpState->outputTail - (lpState->outputWindowSize >> 1) - lpState->outputBufferWritten;
					}
					if(dwRemaining > 0) {
						dwWritten = 0;
						e = lpState->lpDataSink->write(
							lpState->lpDataSink,
							&(lpState->outputBuffer[(lpState->outputTail < (lpState->outputWindowSize >> 1) ? 0 : (lpState->outputWindowSize >> 1)) + lpState->outputBufferWritten]),
							dwRemaining,
							&dwWritten
						);
						lpState->outputBufferWritten = lpState->outputBufferWritten + dwRemaining;

						if(dwWritten < dwRemaining) {
							lpState->obState = minorDecompressor_Deflate_OutputBuffer_State__Unprocessed;
							lpState->dwRemainingBase = ((lpState->outputTail < (lpState->outputWindowSize >> 1) ? 0 : (lpState->outputWindowSize >> 1)) + lpState->outputBufferWritten) + dwWritten;
							lpState->dwRemainingBytes = dwRemaining - dwWritten;
						}
					}
				#endif

				/* Cleanup our huffman trees if any */
				minorDeflate_HufftreeRelease(&(lpState->lpHufftreeRoot_Codes), lpState->lpSystemAPI);
				minorDeflate_HufftreeRelease(&(lpState->lpHufftreeRoot_LiteralLength), lpState->lpSystemAPI);
				minorDeflate_HufftreeRelease(&(lpState->lpHufftreeRoot_Distance), lpState->lpSystemAPI);

				if((lpState->stateFlags & minorDecompressor_Deflate__StateFlags__LastBlock) != 0) {
					lpState->state = minorDecompressor_Deflate_State__Done;
					return (lpState->obState == minorDecompressor_Deflate_OutputBuffer_State__Normal) ? minorE_Finished : e;
				}
				lpState->state = minorDecompressor_Deflate_State__ReadBlockHeader; /* We will read the next block header, starting from the next bit */
				lpState->readerByteBuffer = 0; /* We havent read anything till now ... */
				lpState->readerCurrentBit = 0; /* He starts again with bit 0 of the header */
				return (lpState->obState == minorDecompressor_Deflate_OutputBuffer_State__Normal) ? minorE_Ok : e;
			}

			/*
				We have decoded one of the <length,distance> pairs where we've now
				read the potential partial length specification. If we have additional bits
				we read them via the minorDecompressor_Deflate_State__ReadHuff_LengthExtra state.
				If we don't have them we transition to the minorDecompressor_Deflate_State__ReadHuff_DistanceCode
				state to decode the next huffman code containing the relative distance for the backreference
			*/
			if((lpState->readerCurrentBit = minorDeflate__LUT__LengthCodes[decodedValue-257][0]) == 0) {
				/*
					We dont have additional bits, so we don't have to go through the extra
					bits state. We can already transition to the distance decoder after
					storing the length (used later on by the LZ77 module)
				*/
				lpState->dwLastLength = minorDeflate__LUT__LengthCodes[decodedValue-257][1]; /* Store decoded length ... */
				lpState->readerByteBuffer = 0;
				lpState->state = minorDecompressor_Deflate_State__ReadHuff_DistanceCode;
				return minorE_Ok;
			} else {
				/*
					Transition to processing of next bits.
				*/
				lpState->extraBits =  lpState->readerCurrentBit;
				lpState->dwLastLength = minorDeflate__LUT__LengthCodes[decodedValue-257][1]; /* Store base offset ... */
				lpState->readerByteBuffer = 0;
				lpState->state = minorDecompressor_Deflate_State__ReadHuff_LengthExtra;
				return minorE_Ok;
			}

		case minorDecompressor_Deflate_State__ReadHuff_LengthExtra:
			/* Reading and potentially decoding extra bits from length code */
			lpState->readerByteBuffer = (lpState->readerByteBuffer >> 1) | ((bit == 0) ? 0 : 0x8000);
			if((lpState->readerCurrentBit = lpState->readerCurrentBit - 1) != 0) {
				return minorE_Ok; /* We continue reading the remaining bytes */
			}

			/* Decode bits ... */
			lpState->readerByteBuffer = lpState->readerByteBuffer >> (16 - lpState->extraBits);
			lpState->dwLastLength = lpState->dwLastLength + lpState->readerByteBuffer; /* We simply add our offset to the base offset */
			lpState->readerByteBuffer = 0;
			lpState->state = minorDecompressor_Deflate_State__ReadHuff_DistanceCode;
			return minorE_Ok;

		case minorDecompressor_Deflate_State__ReadHuff_DistanceCode:
			/* We are currently decoding an literal or length value */
			if(bit == 0) {
				lpNextHuff = lpState->lpHufftreeCurrent_Distance->lpZero;
			} else {
				lpNextHuff = lpState->lpHufftreeCurrent_Distance->lpOne;
			}

			if(lpNextHuff != NULL) {
				lpState->lpHufftreeCurrent_Distance = lpNextHuff;
				if((lpNextHuff->lpZero != NULL) || (lpNextHuff->lpOne != NULL)) {
					return minorE_Ok; /* Continue with next bit from huffman symbol */
				}
			}

			/* Decoded one symbol of prefix */
			decodedValue = lpState->lpHufftreeCurrent_Distance->value;
			lpState->lpHufftreeCurrent_Distance = &(lpState->lpHufftreeRoot_Distance); /* Restart our search at the next iteration */

			/* Check if we have to fetch extra bits ... */
			if((lpState->readerCurrentBit = minorDeflate__LUT__DistanceCodes[decodedValue][0]) == 0) {
				/*
					We dont have additional bits, so we don't have to go through the extra
					bits state. We can now execute the LZ77 lookback with
						length = lpState->dwLastLength
						distance = decodedValue
				*/
				e = minorDeflate_Out_LZ77Decode_Lookback(lpState, lpState->dwLastLength, minorDeflate__LUT__DistanceCodes[decodedValue][1]);

				/* And now return to length/literal code */
				lpState->readerByteBuffer = 0;
				lpState->readerCurrentBit = 0;
				lpState->state = minorDecompressor_Deflate_State__ReadHuff_LiteralLengthCode;

				return e;
			} else {
				/*
					Transition to processing of next bits.
				*/
				lpState->extraBits = lpState->readerCurrentBit;
				lpState->dwLastDistance = minorDeflate__LUT__DistanceCodes[decodedValue][1]; /* Store base offset ... */
				lpState->readerByteBuffer = 0;
				lpState->state = minorDecompressor_Deflate_State__ReadHuff_DistanceExtra;
			}
			return minorE_Ok;

		case minorDecompressor_Deflate_State__ReadHuff_DistanceExtra:
			/* Reading and potentially decoding extra bits from distance code */
			lpState->readerByteBuffer = (lpState->readerByteBuffer >> 1) | ((bit == 0) ? 0 : 0x8000);
			if((lpState->readerCurrentBit = lpState->readerCurrentBit - 1) != 0) {
				return minorE_Ok; /* We continue reading the remaining bytes */
			}

			/* Decode bits ... */
			lpState->readerByteBuffer = lpState->readerByteBuffer >> (16 - lpState->extraBits);
			lpState->dwLastDistance = lpState->dwLastDistance + lpState->readerByteBuffer; /* We simply add our offset to the base offset */

			e = minorDeflate_Out_LZ77Decode_Lookback(lpState, lpState->dwLastLength, lpState->dwLastDistance);

			/* Transition back to literal / length code decoding */
			lpState->readerByteBuffer = 0;
			lpState->readerCurrentBit = 0;
			lpState->state = minorDecompressor_Deflate_State__ReadHuff_LiteralLengthCode;
			return e;

		case minorDecompressor_Deflate_State__Failed:
			return minorE_AlreadyFailed;

		default:
			/* If we ever reach this point we have done something horrible wrong ... */
			return minorE_ImplementationError;
	}
}


static enum minorError minorDeflate_ProcessByte(
	struct minorDecompressor_Deflate* lpState,
	uint8_t byteData
) {
	uint8_t b;
	enum minorError e;
	unsigned long int i;
	unsigned long int len;
	unsigned long int dwRemaining;
	unsigned long int dwWritten;
	uint16_t bLen, bNLen;

	if(lpState == NULL) { return minorE_InvalidParam; }

	if(lpState->stateReader == minorDecompressor_DeflateReader_State__Bitwise) {
		b = byteData;
		for(i = 0; i < 8; i=i+1) {
			e = minorDeflate_ProcessBit(lpState, b & 0x01);
			b = b >> 1;

			if(e == minorE_OkAlignByte) {
				return minorE_Ok; /* Discard remaining bits */
			} else if(e != minorE_Ok) {
				return e; /* Finished is also handled this way ... */
			}
		}
		return minorE_Ok;
	} else if(lpState->stateReader == minorDecompressor_DeflateReader_State__Bytewise) {
		/* This means we are reading the uncompressed block header ... */
		lpState->readerByteBuffer = (lpState->readerByteBuffer << 8) | (((uint32_t)byteData) & 0xFF);
		if((lpState->readerCurrentBit = lpState->readerCurrentBit + 1) < 4) {
			return minorE_Ok;
		}
		#ifdef MINOR_DEFLATE_DEBUG
			printf("%s:%u [DEFLATE] Read raw header bytes (len and nlen): 0x%08x\n", __FILE__, __LINE__, lpState->readerByteBuffer);
		#endif

		bLen = (uint16_t)((uint8_t)((lpState->readerByteBuffer >> 24) & 0xFF)) |
			(((uint16_t)((uint8_t)((lpState->readerByteBuffer >> 16) & 0xFF))) << 8);
		bNLen = ~((uint16_t)((uint8_t)((lpState->readerByteBuffer >> 8) & 0xFF)) |
			(((uint16_t)((uint8_t)((lpState->readerByteBuffer >> 0) & 0xFF))) << 8));

		#ifdef MINOR_DEFLATE_DEBUG
			printf("%s:%u LEN: 0x%04x, NLEN: 0x%04x\n", __FILE__, __LINE__, bLen, bNLen);
		#endif

		len = (unsigned long int)bLen;

		if(bLen != bNLen) {
			/* Ones complement mismatches ... */
			lpState->state = minorDecompressor_Deflate_State__Failed;
			lpState->stateReader = minorDecompressor_DeflateReader_State__Failed;
			#ifdef MINOR_DEFLATE_DEBUG
				printf("%s:%u [DEFLATE] Encoding error. Read len %lu\n", __FILE__, __LINE__, len);
			#endif
			return minorE_EncodingError;
		}

		/* Go to blockwise copy mode for uncompressed data ... */
		lpState->dwLastLength = len;
		lpState->stateReader = minorDecompressor_DeflateReader_State__BlockTransfer;
		#ifdef MINOR_DEFLATE_DEBUG
			printf("%s:%u [DEFLATE] Going to read raw data block containing %lu bytes\n", __FILE__, __LINE__, lpState->dwLastLength);
		#endif
		return minorE_Ok;
	} else if(lpState->stateReader == minorDecompressor_DeflateReader_State__BlockTransfer) {
		/*
			Copy byte by byte ... notice that we have to fill at least the LAST output buffer
			because of possible backreferences from the next block
		*/

		e = minorDeflate_Out_LZ77Decode_Byte(lpState, byteData);

		lpState->dwLastLength = lpState->dwLastLength - 1;
		if(lpState->dwLastLength > 0) { return minorE_Ok; }

		/* Check if this is the last block - in this case flush the output buffer */
		#ifndef MINOR_DEFLATE_STREAMING_ENABLE
			if((lpState->stateFlags & minorDecompressor_Deflate__StateFlags__LastBlock) != 0) {
				if(lpState->outputTail < (lpState->outputWindowSize >> 1)) {
					dwRemaining = lpState->outputTail;
				} else {
					dwRemaining = lpState->outputTail - (lpState->outputWindowSize >> 1);
				}
				if(dwRemaining > 0) {
					dwWritten = 0;
					e = lpState->lpDataSink->write(
						lpState->lpDataSink,
						&(lpState->outputBuffer[lpState->outputTail < (lpState->outputWindowSize >> 1) ? 0 : (lpState->outputWindowSize >> 1)]),
						dwRemaining,
						&dwWritten
					);
					if(dwWritten < dwRemaining) {
						lpState->obState = minorDecompressor_Deflate_OutputBuffer_State__Unprocessed;
						lpState->dwRemainingBase = (lpState->outputTail < (lpState->outputWindowSize >> 1) ? 0 : (lpState->outputWindowSize >> 1)) + dwWritten;
						lpState->dwRemainingBytes = dwRemaining - dwWritten;
					}
				}
			}
		#else
			if(lpState->outputTail < (lpState->outputWindowSize >> 1)) {
				dwRemaining = lpState->outputTail - lpState->outputBufferWritten;
			} else {
				dwRemaining = lpState->outputTail - (lpState->outputWindowSize >> 1) - lpState->outputBufferWritten;
			}
			if(dwRemaining > 0) {
				dwWritten = 0;
				e = lpState->lpDataSink->write(
					lpState->lpDataSink,
					&(lpState->outputBuffer[(lpState->outputTail < (lpState->outputWindowSize >> 1) ? 0 : (lpState->outputWindowSize >> 1)) + lpState->outputBufferWritten]),
					dwRemaining,
					&dwWritten
				);

				if(dwWritten < dwRemaining) {
					lpState->obState = minorDecompressor_Deflate_OutputBuffer_State__Unprocessed;
					lpState->dwRemainingBase = ((lpState->outputTail < (lpState->outputWindowSize >> 1) ? 0 : (lpState->outputWindowSize >> 1)) + lpState->outputBufferWritten)+dwWritten;
					lpState->dwRemainingBytes = dwRemaining - dwWritten;
				}

				lpState->outputBufferWritten = lpState->outputBufferWritten + dwRemaining;
			}
		#endif

		/* Cleanup our huffman trees if any */
		minorDeflate_HufftreeRelease(&(lpState->lpHufftreeRoot_Codes), lpState->lpSystemAPI);
		minorDeflate_HufftreeRelease(&(lpState->lpHufftreeRoot_LiteralLength), lpState->lpSystemAPI);
		minorDeflate_HufftreeRelease(&(lpState->lpHufftreeRoot_Distance), lpState->lpSystemAPI);

		if((lpState->stateFlags & minorDecompressor_Deflate__StateFlags__LastBlock) != 0) {
			lpState->state = minorDecompressor_Deflate_State__Done;
			return (lpState->obState == minorDecompressor_Deflate_OutputBuffer_State__Normal) ? minorE_Finished : e;
		}

		/* Switch back to next block mode ... */
		lpState->state = minorDecompressor_Deflate_State__ReadBlockHeader;
		lpState->readerByteBuffer = 0;
		lpState->readerCurrentBit = 0;
		lpState->stateReader = minorDecompressor_DeflateReader_State__Bitwise;

		return (lpState->obState == minorDecompressor_Deflate_OutputBuffer_State__Normal) ? minorE_Ok : e;
	} else if(lpState->stateReader == minorDecompressor_DeflateReader_State__Failed) {
		return minorE_AlreadyFailed;
	} else {
		return minorE_ImplementationError;
	}
}

/*
	"Public" API called from the outside via our multiplexer
*/
enum minorError minorDecompressorCreate_Deflate(
	struct minorDecompressor** 			lpOut,			/* Output location for the compressor object. There will be NO REUSE. Any previous object pointed to will be orphaned if not released before! */

	enum minorAlgorithm					algorithm,		/*  Allows to select the algorithm to be used during data compression */
	struct minorSystemInterface*		lpSystem,		/* Required system interface */
	struct minorConfigurationElement* 	lpConfiguration	/* Pointer to the first configuration element inside the configuration chain or NULL */
) {
	enum minorError e;
	struct minorConfigurationElement* lpCurrentConf;
	unsigned long int i;
	unsigned long int confTableSize = LIBMINOR_DEFLATE__BLOCKSIZE_K;
	uint8_t* lpDictionary = NULL;
	unsigned long int dwDictionaryLength = 0;

	if(lpOut == NULL) { return minorE_InvalidParam; }
	(*lpOut) = NULL;

	if(lpSystem == NULL) { return minorE_InvalidParam; }
	if(algorithm != minorAlgorithm_Deflate) { return minorE_InvalidParam; }

	/* Validate configuration */
	if((e = minorDecompressor_CheckConfiguration_Deflate(lpConfiguration)) != minorE_Ok) { return e; }

	/*
		Check for configuration options ...
	*/
	lpCurrentConf = lpConfiguration;
	while(lpCurrentConf != NULL) {
		if(lpCurrentConf->typeMajor == minorConfiguration_MajorId_Deflate) {
			if(((struct minorConfigurationElement_Deflate*)lpCurrentConf)->typeMinor == minorConfiguration_MinorID_Deflate_BackreferenceTableSize) {
				confTableSize = ((struct minorConfigurationElement_Deflate_BackreferenceTableSize*)lpCurrentConf)->slidingWindowSize;
			}
			#ifdef MINOR_DEFLATE_STREAMING_ENABLE
				else if(((struct minorConfigurationElement_Deflate*)lpCurrentConf)->typeMinor == minorConfiguration_MinorID_Deflate_PreloadDictionary) {
					/* We have to preload a dictionary into our decompression buffers. See description below ... */
					lpDictionary = ((struct minorConfigurationElement_Deflate_PreloadDictionary*)lpCurrentConf)->lpDictionary;
					dwDictionaryLength = ((struct minorConfigurationElement_Deflate_PreloadDictionary*)lpCurrentConf)->dwByteLength;
				}
			#endif
		}
		lpCurrentConf = lpCurrentConf->lpNext;
	}

	/*
		Check table size IF specified. Because we have to
		be capable of DEcoding any standard conformant deflate
		stream we have to have at least LIBMINOR_DEFLATE__BLOCKSIZE_K
		of bufferspace (32KByte)
	*/
	if(confTableSize < LIBMINOR_DEFLATE__BLOCKSIZE_K) {
		return minorE_InvalidParam;
	}

	/*
		If a dictionary has been specified check its size. A dictionary
		can only be one block size long. The dictionary is preloaded
		into the output buffer to allow LZ77 lookback to backreference into
		the dictionary. No output data is generated from this output
		dictionary. This allows applications to preload common strings
		that are used periodically inside a data stream (for example headers,
		etc.). The same dictionary has to be loaded during compression and
		decompression for this mechanism to work.
	*/
	#ifdef MINOR_DEFLATE_STREAMING_ENABLE
		if(((lpDictionary != NULL) && (dwDictionaryLength == 0)) || ((lpDictionary == NULL) && (dwDictionaryLength != 0))) {
			return minorE_InvalidParam;
		}
		if(lpDictionary != NULL) {
			if(dwDictionaryLength > confTableSize) {
				return minorE_InvalidParam;
			}
		}
	#endif

	/* Check function pointers in system API are set */
	if((lpSystem->alloc == NULL) || (lpSystem->free == NULL)) {
		return minorE_InvalidParam;
	}
	/* Allocate statemachine and initialize ... */
	e = lpSystem->alloc((void**)lpOut, sizeof(struct minorDecompressor_Deflate)+(confTableSize * 2 * 1024), lpSystem->lpFreeParam_Alloc);
	if(e != minorE_Ok) { return e; }

	/* Initialize structure */
	(*((struct minorDecompressor_Deflate**)(lpOut)))->base.algorithm = minorAlgorithm_Deflate;
	(*((struct minorDecompressor_Deflate**)(lpOut)))->lpSystemAPI = lpSystem;
	(*((struct minorDecompressor_Deflate**)(lpOut)))->state = minorDecompressor_Deflate_State__ReadBlockHeader;
	(*((struct minorDecompressor_Deflate**)(lpOut)))->stateReader = minorDecompressor_DeflateReader_State__Bitwise;
	(*((struct minorDecompressor_Deflate**)(lpOut)))->stateFlags = 0;
	(*((struct minorDecompressor_Deflate**)(lpOut)))->readerByteBuffer = 0;
	(*((struct minorDecompressor_Deflate**)(lpOut)))->readerCurrentBit = 0;
	(*((struct minorDecompressor_Deflate**)(lpOut)))->lastSymbol = 0;

	/* Initialize output buffer in no-pending-data state */
	(*((struct minorDecompressor_Deflate**)(lpOut)))->obState = minorDecompressor_Deflate_OutputBuffer_State__Normal;
	(*((struct minorDecompressor_Deflate**)(lpOut)))->dwRemainingBase = 0;
	(*((struct minorDecompressor_Deflate**)(lpOut)))->dwRemainingBytes = 0;

	(*((struct minorDecompressor_Deflate**)(lpOut)))->HLIT = 0;
	(*((struct minorDecompressor_Deflate**)(lpOut)))->HDIST = 0;
	(*((struct minorDecompressor_Deflate**)(lpOut)))->HCLEN = 0;

	(*((struct minorDecompressor_Deflate**)(lpOut)))->lpHufftreeRoot_Codes.lpZero = NULL; 			(*((struct minorDecompressor_Deflate**)(lpOut)))->lpHufftreeRoot_Codes.lpOne = NULL;
	(*((struct minorDecompressor_Deflate**)(lpOut)))->lpHufftreeRoot_Distance.lpZero = NULL; 		(*((struct minorDecompressor_Deflate**)(lpOut)))->lpHufftreeRoot_Distance.lpOne = NULL;
	(*((struct minorDecompressor_Deflate**)(lpOut)))->lpHufftreeRoot_LiteralLength.lpZero = NULL; 	(*((struct minorDecompressor_Deflate**)(lpOut)))->lpHufftreeRoot_LiteralLength.lpOne = NULL;

	(*((struct minorDecompressor_Deflate**)(lpOut)))->lpHufftreeCurrent_Codes = NULL;
	(*((struct minorDecompressor_Deflate**)(lpOut)))->lpHufftreeCurrent_Distance = NULL;
	(*((struct minorDecompressor_Deflate**)(lpOut)))->lpHufftreeCurrent_LiteralLength = NULL;

	(*((struct minorDecompressor_Deflate**)(lpOut)))->usedBufferBitmap = 0;
	(*((struct minorDecompressor_Deflate**)(lpOut)))->outputTail = 0;
	#ifdef MINOR_DEFLATE_STREAMING_ENABLE
		(*((struct minorDecompressor_Deflate**)(lpOut)))->outputBufferWritten = 0;
	#endif

	(*((struct minorDecompressor_Deflate**)(lpOut)))->lpDataSource = NULL;
	(*((struct minorDecompressor_Deflate**)(lpOut)))->lpDataSink = NULL;

	(*((struct minorDecompressor_Deflate**)(lpOut)))->outputWindowSize = 2*1024*confTableSize;

	/*
		In case of a preloaded dictionary: Load data into output buffer and set
		outputBufferWritten to dictionary size.
	*/
	#ifdef MINOR_DEFLATE_STREAMING_ENABLE
		for(i = 0; i < dwDictionaryLength; i=i+1) { (*((struct minorDecompressor_Deflate**)(lpOut)))->outputBuffer[i] = lpDictionary[i]; }
		if(dwDictionaryLength != confTableSize) {
			(*((struct minorDecompressor_Deflate**)(lpOut)))->outputBufferWritten = dwDictionaryLength;
		}
		(*((struct minorDecompressor_Deflate**)(lpOut)))->outputTail = dwDictionaryLength;
	#endif

	return minorE_Ok;
}

enum minorError minorDecompressorRelease_Deflate(
	struct minorDecompressor*				lpObject		/* Allows releasing the minor object */
) {
	struct minorSystemInterface* lpSys;

	if(lpObject == NULL) { return minorE_Ok; }

	minorDeflate_HufftreeRelease(&(((struct minorDecompressor_Deflate*)lpObject)->lpHufftreeRoot_Codes), ((struct minorDecompressor_Deflate*)lpObject)->lpSystemAPI);
	minorDeflate_HufftreeRelease(&(((struct minorDecompressor_Deflate*)lpObject)->lpHufftreeRoot_Distance), ((struct minorDecompressor_Deflate*)lpObject)->lpSystemAPI);
	minorDeflate_HufftreeRelease(&(((struct minorDecompressor_Deflate*)lpObject)->lpHufftreeRoot_LiteralLength), ((struct minorDecompressor_Deflate*)lpObject)->lpSystemAPI);

	lpSys = ((struct minorDecompressor_Deflate*)lpObject)->lpSystemAPI;

	lpSys->free((void*)lpObject, lpSys->lpFreeParam_Free);
	return minorE_Ok;
}

enum minorError minorDecompressorAttachSource_Deflate(
	struct minorDecompressor*			lpObject,
	struct minorStreamSource*			lpSource
) {
	if(lpObject == NULL) { return minorE_InvalidParam; }

	((struct minorDecompressor_Deflate*)lpObject)->lpDataSource = lpSource;
	return minorE_Ok;
}

enum minorError minorDecompressorAttachSink_Deflate(
	struct minorDecompressor*			lpObject,
	struct minorStreamSink*				lpSink
) {
	if(lpObject == NULL) { return minorE_InvalidParam; }

	((struct minorDecompressor_Deflate*)lpObject)->lpDataSink = lpSink;
	return minorE_Ok;
}




enum minorError minorDecompressor_TransferFromInput_Deflate(
	struct minorDecompressor*			lpObject,
	unsigned long int 					dwBytesToRead,
	unsigned long int					*lpBytesDone
) {
	enum minorError e;
	struct minorDecompressor_Deflate* lpSelf;
	unsigned long int dwBytesToGo;
	unsigned long int dwBytesRead;
	uint8_t nextByte;

	if(lpObject == NULL) { return minorE_InvalidParam; }

	lpSelf = (struct minorDecompressor_Deflate*)lpObject;

	/*
		Check if we have attached input and output streams
	*/
	if(lpSelf->lpDataSource == NULL) { return minorE_MissingDataSource; }
	if(lpSelf->lpDataSink == NULL) { return minorE_MissingDataSink; }

	if(lpBytesDone != NULL) { (*lpBytesDone) = 0; }

	/*
		Now we push one byte after each other into our input
		buffer. The data will be pushed into our output buffer
		by the state machine
	*/
	dwBytesToGo = dwBytesToRead;

	/*
		As long as there is any pending data in the output buffer
		flush that data (we cannot continue decoding with a full
		output buffer)
	*/
	if((e = minorDeflate_OutputBuffer_Continue(lpSelf)) != minorE_Ok) { return e; }

	while(dwBytesToGo > 0) {
		e = lpSelf->lpDataSource->read(
			lpSelf->lpDataSource,
			&nextByte,
			1,
			&dwBytesRead
		);
		if(e == minorE_Ok) {
			/* We have received the next byte, push into our state machine ... */
			e = minorDeflate_ProcessByte(lpSelf, nextByte);

			if((e != minorE_Ok) && (e != minorE_Finished)) {
				return e;
			}

			if(lpBytesDone != NULL) { (*lpBytesDone) = (*lpBytesDone) + 1; }
			if(e == minorE_Finished) {
				return e;
			}
			dwBytesToGo = dwBytesToGo - 1;
		} else if(e == minorE_Suspend) {
			return minorE_Suspend;
		} else {
			/* An error occured */
			return e;
		}
	}

	return minorE_Ok;
}

enum minorError minorDecompressor_Execute_Deflate(
	struct minorDecompressor*			lpObject
) {
	enum minorError e;
	struct minorDecompressor_Deflate* lpSelf;
	unsigned long int dwBytesRead;
	uint8_t nextByte;

	if(lpObject == NULL) { return minorE_InvalidParam; }

	lpSelf = (struct minorDecompressor_Deflate*)lpObject;

	/*
		Check if we have attached input and output streams
	*/
	if(lpSelf->lpDataSource == NULL) { return minorE_MissingDataSource; }
	if(lpSelf->lpDataSink == NULL) { return minorE_MissingDataSink; }

	/*
		As long as there is any pending data in the output buffer
		flush that data (we cannot continue decoding with a full
		output buffer)
	*/
	if((e = minorDeflate_OutputBuffer_Continue(lpSelf)) != minorE_Ok) { return e; }

	for(;;) {
		e = lpSelf->lpDataSource->read(
			lpSelf->lpDataSource,
			&nextByte,
			1,
			&dwBytesRead
		);
		if(e == minorE_Ok) {
			/* We have received the next byte, push into our state machine ... */
			e = minorDeflate_ProcessByte(lpSelf, nextByte);
			if((e != minorE_Ok) && (e != minorE_Finished)) { return e; }

			if(e == minorE_Finished) { return e; }
		} else if(e == minorE_Suspend) {
			return minorE_Suspend;
		} else {
			/* An error occured */
			return e;
		}
	}
}



struct minorDecompressor_WriteMem__LocalSource {
	struct minorStreamSource			base;

	uint8_t*							lpBase;
	unsigned long int					dwBytesToGo;
	unsigned long int					dwCurrentOffset;
};

static enum minorError minorDecompressor_WriteMem__LocalSource__Read(
	struct minorStreamSource* 		lpSelf,
	uint8_t* 						lpDestinationBuffer,
	unsigned long int				dwBytesToRead,
	unsigned long int				*lpBytesRead
) {
	struct minorDecompressor_WriteMem__LocalSource* lpInfo;
	unsigned long int dwPassBytes;
	unsigned long int i;

	if(lpSelf == NULL) { return minorE_InvalidParam; }
	lpInfo = (struct minorDecompressor_WriteMem__LocalSource*)lpSelf;

	if(dwBytesToRead < lpInfo->dwBytesToGo) { dwPassBytes = dwBytesToRead; } else { dwPassBytes = lpInfo->dwBytesToGo; }
	if(dwPassBytes == 0) { return minorE_EndOfStream; }

	for(i = 0; i < dwPassBytes; i=i+1) {
		lpDestinationBuffer[i] = lpInfo->lpBase[lpInfo->dwCurrentOffset];
		lpInfo->dwCurrentOffset = lpInfo->dwCurrentOffset + 1;
		lpInfo->dwBytesToGo = lpInfo->dwBytesToGo - 1;
	}

	return minorE_Ok;
}

enum minorError minorDecompressor_WriteMem_Deflate(
	struct minorDecompressor*			lpObjectParam,

	uint8_t* 							lpDataIn,
	unsigned long int 					dwBytesToWrite,
	unsigned long int					*dwBytesDone
) {
	enum minorError e;
	struct minorStreamSource* oldSource;

	struct minorDecompressor_WriteMem__LocalSource localSource;
	struct minorDecompressor_Deflate* lpObject;

	lpObject = (struct minorDecompressor_Deflate*)lpObjectParam;

	if(lpObject == NULL) { return minorE_InvalidParam; }

	if(lpObject->lpDataSink == NULL) { return minorE_MissingDataSink; }
	oldSource = lpObject->lpDataSource; /* Backup if we MAY require this again later on ... */

	/* We setup our own "temporary" source ... */
	localSource.base.read = &minorDecompressor_WriteMem__LocalSource__Read;
	localSource.lpBase = lpDataIn;
	localSource.dwBytesToGo = dwBytesToWrite;
	localSource.dwCurrentOffset = 0;
	lpObject->lpDataSource = (struct minorStreamSource*)&localSource;

	e = minorDecompressor_TransferFromInput_Deflate(
		lpObjectParam,
		dwBytesToWrite,
		dwBytesDone
	);

	lpObject->lpDataSource = oldSource;
	return e;
}

/*
	Configuration validation function. This is used in case of deflate. We do not support any
	child objects so we do not have to pass through to lower layers.
*/
enum minorError minorDecompressor_CheckConfiguration_Deflate(
	struct minorConfigurationElement*	lpConfiguration
) {
	struct minorConfigurationElement* lpCurrentElement = lpConfiguration;
	struct minorConfigurationElement_Deflate* lpCurrentElement_Deflate;

	while(lpCurrentElement != NULL) {
		/*
			Check if it's our major ID. We can only validate settings that are specific to our
			module and that have not been processed until now
		*/
		if((lpCurrentElement->typeMajor == minorConfiguration_MajorId_Deflate) && ((lpCurrentElement->dwFlags & minorConfiguration__Flag__Processed) == 0)) {
			lpCurrentElement_Deflate = (struct minorConfigurationElement_Deflate*)lpCurrentElement;
			switch(lpCurrentElement_Deflate->typeMinor) {
				case minorConfiguration_MinorID_Deflate_BackreferenceTableSize:
					/*
						Because we are a DEcompressor we require at least the maximum
						blocksize that the deflate specification requires
					*/
					if(((struct minorConfigurationElement_Deflate_BackreferenceTableSize*)lpCurrentElement)->slidingWindowSize < LIBMINOR_DEFLATE__BLOCKSIZE_K) {
						return minorE_InvalidParam;
					}

					lpCurrentElement->dwFlags = lpCurrentElement->dwFlags | minorConfiguration__Flag__Processed; /* Mark supported */
					break;
				case minorConfiguration_MinorID_Deflate_PreloadDictionary:
					/*
						The maximum size for a preload dictionary is one backreference block from
						the RFC (i.e. 32 K)
					*/
					#ifdef MINOR_DEFLATE_STREAMING_ENABLE
						if((((struct minorConfigurationElement_Deflate_PreloadDictionary*)lpCurrentElement)->lpDictionary != NULL) || (((struct minorConfigurationElement_Deflate_PreloadDictionary*)lpCurrentElement)->dwByteLength != 0)) {
							if(((struct minorConfigurationElement_Deflate_PreloadDictionary*)lpCurrentElement)->lpDictionary == NULL) { return minorE_InvalidParam; }
							if(((struct minorConfigurationElement_Deflate_PreloadDictionary*)lpCurrentElement)->dwByteLength == 0) { return minorE_InvalidParam; }

							if(((struct minorConfigurationElement_Deflate_PreloadDictionary*)lpCurrentElement)->dwByteLength > (LIBMINOR_DEFLATE__BLOCKSIZE_K*1024)) { return minorE_InvalidParam; }
						}
					#else
						/* This configuration is only supported if streaming mode is supported */
						if((lpCurrentElement->dwFlags & minorConfiguration__Flag__Critical) != 0) { return minorE_UnsupportedConfiguration; }
					#endif
					lpCurrentElement->dwFlags = lpCurrentElement->dwFlags | minorConfiguration__Flag__Processed; /* Mark supported */
					break;
				default:
					/* Unknown configuration option */
					if((lpCurrentElement->dwFlags & minorConfiguration__Flag__Critical) != 0) { return minorE_UnsupportedConfiguration; }
					lpCurrentElement->dwFlags = lpCurrentElement->dwFlags | minorConfiguration__Flag__Processed; /* Mark supported */
					break;
			}
		}

		/* Iterate to next element */
		lpCurrentElement = lpCurrentElement->lpNext;
	}
	return minorE_Ok;
}

/*
	Compressor code
	===============
*/

enum minorCompressor_Deflate_State {
	minorCompressor_Deflate_State__AcceptingData					= 1,	/* The default state after creation and if not blocking. The compressor accepts new data view write into it's sink */
	minorCompressor_Deflate_State__FlushLastData					= 2,	/* Entered on EndOfStream from accepting data. Transitional state except when blocking / deferring writes */
	minorCompressor_Deflate_State__FlushData						= 3,	/* Entered whenever flush is requested. Same as FlushLastData except it will return to Accepting data */

	minorCompressor_Deflate_State__ProcessByte_Normal				= 4,	/* After accepting data bytes are processed by the LZ77 encoder */
	minorCompressor_Deflate_State__ProcessByte_FlushBlock			= 5,	/* After flushing last data accepting data bytes are processed by the LZ77 encoder */
	minorCompressor_Deflate_State__ProcessByte_FlushLastBlock		= 6,	/* After flushing data accepting data bytes are processed by the LZ77 encoder */

	minorCompressor_Deflate_State__FlushBlock_1						= 7,	/* After accepting data block data is written into the sink (after it has been encoded) */
	minorCompressor_Deflate_State__FlushBlock_2						= 8,	/* After flushing last data block data is written into the sink (after it has been encoded) */
	minorCompressor_Deflate_State__FlushBlock_3						= 9,	/* After flushing block data is written into the sink (after it has been encoded) */

	minorCompressor_Deflate_State__Error							= 10,	/* Permanent error state */
	minorCompressor_Deflate_State__Finished							= 11,	/* Finished (not accepting anymore) */
};

enum minorCompressor_Deflate_FlushBlock_State {
	minorCompressor_Deflate_FlushBlock_State__FlushRawData			= 0,	/* We always start by flushing the buffered raw data */
	minorCompressor_Deflate_FlushBlock_State__CandidateSelection	= 1,	/* We are going to built alphabets and decide which encoding (raw, dynamic tree, fixed tree) we'll use */

	minorCompressor_Deflate_FlushBlock_State__RawSelectedHeader		= 2,	/* Entered the write raw state, currently writing 3 bit blockheader */
	minorCompressor_Deflate_FlushBlock_State__RawLENNLEN			= 3,	/* Currently flushing bytes from LEN/NLEN fields */
	minorCompressor_Deflate_FlushBlock_State__RawBytes				= 4,	/* Flushing raw bytes ... */

	minorCompressor_Deflate_FlushBlock_State__DynamicSelectedHeader	= 5,	/* Selected dynamic trees, writing header (block header HLIT, HDIST, HCLEN) */
	minorCompressor_Deflate_FlushBlock_State__DynamicCodeLengthLens	= 6,	/* Emitting code length lengths codes */
	minorCompressor_Deflate_FlushBlock_State__DynamicCodeLengths	= 7,	/* Emitting symbols for codelengths */
	minorCompressor_Deflate_FlushBlock_State__DynamicData			= 8,	/* Emitting data */

	minorCompressor_Deflate_FlushBlock_State__FixedSelectedHeader	= 9,	/* Fixed trees selected, writing header (block header) */
};


struct minorCompressor_Deflate_HashTableEntry {
	unsigned long int 	dwAbsolutePosition; 					/* Absolute position inside the data stream (including any preload dictionary size) */
	unsigned long int	dwBufferOffset; 						/* Position inside the raw ringbuffer as long as we are in rnage */
	struct minorCompressor_Deflate_HashTableEntry* lpNext; 		/* We build a linked list inside each bucket */
};
struct minorCompressor_Deflate_HashTable {
	unsigned long int 	dwBuckets;								/* Number of buckets inside the hashtable; each bucket contains a chained list of elements in inverse order of their occurance (this favours short distance codes) */
	unsigned long int 	dwMaxDepth;								/* Maximum depth of each of the linked lists (if set to 0 we only clear entries that are out of range that we reach during scanning) */

	/*
		Hash function configuration

		Our hash function is a simple xor-based method that reduces a
		sequence of 3 bytes (24 bits) into an shorter sequence. To acomplish
		that the 24 bit sequence is shifted multiple times, masked via logical
		or and then xor'ed together. In case of a 24 -> 8 bit reduction the sequence
		would lead to shiftCount = 3 and shiftDistance = 8 with a shiftMask = 0xFF
		which leads to the following calculation:

		index = ((bytes >> 0*8) & 0xFF) ^ ((bytes >> 1*8) & 0xFF) ^ ((bytes >> 2*8) & 0xFF)
	*/
	unsigned long int	dwHashShiftCount;
	unsigned long int	dwHashShiftDistance;
	uint32_t			dwHashShiftMask;

	struct {
		struct minorCompressor_Deflate_HashTableEntry* 	lpFirst;
		unsigned long int 								dwCurrentDepth;
	} entries[];
};

struct minorCompressor_Deflate__CodeTable {
	uint8_t													symbols[2*(minorCompressor_Deflate__ALPHABETSIZE_LITERAL+minorCompressor_Deflate__ALPHABETSIZE_DISTANCE)]; /* Always symbol + optional additional data */
	unsigned long int										dwUsedSymbols;
};

#define MINOR_COMPRESSOR_DEFLATE_FLAG__CANDIDATE_RAW		0x00000001			/* Uncompressed data blocks can be emitted in case they are smaller than alternatives */
#define MINOR_COMPRESSOR_DEFLATE_FLAG__CANDIDATE_FIXED		0x00000002			/* Fixed huffman tables may be used */
#define MINOR_COMPRESSOR_DEFLATE_FLAG__CANDIDATE_DYNAMIC	0x00000004			/* Dynamic huffman tables may be used */
struct minorCompressor_Deflate {
	struct minorCompressor						base; /* Base structure that encodes the type of the compressor. Everything else is opaque to the application using our compressor */

	struct minorSystemInterface*				lpSystemAPI;
	uint32_t									dwFlags;

	/*
		Lazy matching distance: This describes how many bytes lookahead
		should be used to try to find a better match for a common substring.
		The RFC suggests a distance of 1. 0 may be choosen for higher performance,
		higher values make the encoder slower (linear)
	*/
	unsigned long int							dwLazyMatchDistance;

	/*
		The size of a block. If not flushed the LZ77 substring detector
		creates literal symbols and <distance,length> pairs until it
		has filled the block. Then the huffman coder uses the collected
		statistics to create the huffman trees, encoded every cached
		symbol and flushed them to the attached sink
	*/
	unsigned long int							dwBlockSize;

	/*
		Huffman tree statistics

		The alphabets for which statistics are gathered are:
			- Literal length (0-285; 286 values)
			- The 30 codes for distance (note that each code MAY correspond to a whole range!)
	*/
	struct {
		unsigned long int							dwAlphabetSize;
		struct minorHuffutilAlphabetEntry_U32		entry[minorCompressor_Deflate__ALPHABETSIZE_LITERAL];
	} statLiteralLength;
	struct {
		unsigned long int							dwAlphabetSize;
		struct minorHuffutilAlphabetEntry_U32		entry[minorCompressor_Deflate__ALPHABETSIZE_DISTANCE];
	} statDistance;
	struct {
		unsigned long int							dwAlphabetSize;
		struct minorHuffutilAlphabetEntry_U32		entry[minorCompressor_Deflate__ALPHABETSIZE_CODELENGTH];
	} alphabetCodelengthCodes;
	struct minorCompressor_Deflate__CodeTable		codelengthcodeTable;

	/*
		LZ77 lookback distance
		This allows configuration of maximum lookback DISTANCE. The
		specification limits this distance to 32768 bytes (32 kByte) but
		it may be enhanced to improve performance.
		Larger values MAY be used in non-conforming mode (if the encoder is
		extended to allow for additional symbols like in deflate64)

		The LZ77 Maximum Match Length describes the maximum LENGTH of a match.
		The RFC limits the length to a maximum of 258 bytes.
	*/
	unsigned long int							dwLZ77LookbackDistance;
	unsigned long int							dwLZ77MaximumMatchLength;

	/*
		"Early match" length.
		If an lookback candidte reached at least this length
		it is immediately accepted (no further matching is done).
		If set to 0 everytime a whole scan is done during no
		further candidates are found (i.e. the best match has been
		found).
	*/
	unsigned long int							dwLZ77EarlyMatchLength;

	/*
		Dictionary information

		The preload dictionary is an enhanced feature not allowed by deflate
		itself but supported by the zlib implementation.
	*/
	uint8_t*									lpRawDictionary;
	unsigned long int							dwDictionaryLength;
	unsigned long int							dwUnprocessedDictionaryBytes; /* Normally 0,1,2 after the initialization */

	/* Attached stream interfaces */
	struct minorStreamSource*					lpDataSource;
	struct minorStreamSink*						lpDataSink;

	/* Current hash table for LZ77 */
	struct minorCompressor_Deflate_HashTable*	lpLZ77HashTable;

	/* Bytes pushed into the block */
	unsigned long int							dwBlockBytesDone;

	/* Shift register ... the first bit is bit 0 */
	uint8_t										bitShiftreg;
	uint8_t										shiftPosition;

	/*
		Buffers:

		Raw Ring Buffer.
		This buffer buffers the raw data bytes. If an uncompressed raw data block is a candidate
		for emission the buffer has to be capable of holding a full raw data block as well as
		dwLZ77LookbackDistance bits. Else it has to hold at least
		(dwLZ77LookbackDistance+dwLZ77MaximumMatchLength+dwLazyMatchDistance+1) bytes. The last
		byte that's used is used to prevent head and tail crashing into each other.

		LZ77 symbol buffer.
		This buffer holds at least dwBlockSize unencoded SYMBOLS. Each symbol either represents
		an literal (0-255) or an <distance, length> code. Because each distance,length code
		replaces three literal bytes the maximum number of symbols is dwBlockSize + 1 (end of block).
		Each symbol is representable as a 16 bit value -> (dwBlockSize+1) * sizeof(uint16_t) bytes

		Output buffer
		The output buffer can have a variable size (or be totally omitted). After a block has been
		cached inside the symbol buffer and the huffman tree has been built (if required and not
		a raw data block is used) the compressor starts to output bytes - either (in case of an omitted
		output buffer) one byte at a time into the attached sink or one output buffer block (or less for
		the last block).
	*/
	uint8_t*									lpRawRingBuffer;			/* Pointer into our data array to the base offset of our ringbuffer */
	unsigned long int							dwRawRingSize;				/* The size of the ringbuffer (see comment above) */
	unsigned long int							dwRawRingUsed;				/* Used to track if we have to wraparound for lookback / look forward */
	unsigned long int							dwRawRingHead;				/* "Head" of our buffer. This points to the next byte that we write to. This cannot be incremented to match the tail again so one byte is always unoccupied */
	unsigned long int							dwRawRingTail;				/* "Tail" of our buffer. This points to the next byte that will be consumed by LZ77 and Huffman */
	unsigned long int							dwRawRingHeadAbsolute;		/* Absolute position of raw ring head */

	uint16_t*									lpSymbolBuffer;				/* Buffer for the symbols emitted by our LZ77 encoder */
	/* unsigned long int						dwSymbolBufferSize;			CAN BE CALCULATED EASILY FROM (dwBlockSize + 1) * sizeof(uint16_t) */
	unsigned long int							dwSymbolBufferUsed;			/* This acts as the pointer to the next usable symbol position as well as fill counter (this is NOT a ringbuffer!) */

	uint8_t*									lpOutputBuffer;				/* Buffer for our generated output bytes. Optional. Can be NULL */
	unsigned long int							dwOutputBufferSize;			/* Bytesize of our output buffer */
	unsigned long int							dwOutputBufferUsed;			/* This acts as the pointer to the next usable byte position as well as fill counter (this is NOT a ringbuffer!) */

	enum minorCompressor_Deflate_State			currentState;				/* Current state of the decoder (used for restart) */
	struct {
		uint8_t											lastByte;
		unsigned long int								dwCurrentBit;
		unsigned long int								dwCurrentByte;
		enum minorCompressor_Deflate_FlushBlock_State	flushState;

		uint32_t 										nextSymbolBits;
		uint32_t 										dwSymbolBitCount;
		uint32_t 										dwAdditionalBitCount;
		uint32_t										nextAdditionalBits;

		uint32_t										nextDistanceSymbolBits;
		uint32_t										dwDistanceSymbolBitCount;
		uint32_t										dwAdditionalDistanceBitCount;
		uint32_t										nextAdditionalDistanceBits;
	} stateInfo;

	/*
		Some useful relations for the raw ringbuffer:

		dwBufferedData			Amount of data between head and tail
		dwLookbackAvailable		Amount of data behind the tail (outside head and tail) possibly with wraparound

		if(lpCompressor->dwRawRingUsed == lpCompressor->dwRawRingSize) {
			if(lpCompressor->dwRawRingHead >= lpCompressor->dwRawRingTail) {
				dwBufferedData = lpCompressor->dwRawRingHead - lpCompressor->dwRawRingTail;
				dwLookbackAvailable = lpCompressor->dwRawRingSize - dwBufferedData;
			} else {
				dwBufferedData = lpCompressor->dwRawRingSize - (lpCompressor->dwRawRingHead - lpCompressor->dwRawRingTail);
				dwLookbackAvailable = lpCompressor->dwRawRingSize - dwBufferedData;
			}
		} else {
			dwBufferedData = lpCompressor->dwRawRingHead - lpCompressor->dwRawRingTail;
			dwLookbackAvailable = lpCompressor->dwRawRingTail;
		}

		Or in short notation:

		dwBufferedData = (lpCompressor->dwRawRingHead >= lpCompressor->dwRawRingTail) ? (lpCompressor->dwRawRingHead - lpCompressor->dwRawRingTail) : (lpCompressor->dwRawRingSize - (lpCompressor->dwRawRingHead - lpCompressor->dwRawRingTail));
		dwLookbackAvailable = (lpCompressor->dwRawRingUsed == lpCompressor->dwRawRingSize) ? (lpCompressor->dwRawRingSize - dwBufferedData) : lpCompressor->dwRawRingTail;
	*/

	/* Data buffer (raw ring buffer, symbol buffer, output buffer in exactly that order) */
	uint8_t										bData[];
};


static inline unsigned long int minorCompressor_Deflate_Hash(
	uint32_t bThreeBytes,
	const struct minorCompressor_Deflate_HashTable* lpTable
) {
	uint32_t result;
	unsigned long int i;

	result = 0;

	for(i = 0; i < lpTable->dwHashShiftCount; i=i+1) {
		result = result ^ ((bThreeBytes >> i * lpTable->dwHashShiftDistance) & lpTable->dwHashShiftMask);
	}

	return (unsigned long int)result;
}

static enum minorError minorCompressor_Deflate_Hashtable_Create(
	struct minorCompressor_Deflate_HashTable** lpTable,
	struct minorSystemInterface* lpSystem,

	unsigned long int dwBucketCount,
	unsigned long int dwMaxDepth
) {
	enum minorError e;
	uint32_t shiftMask;
	unsigned long int shiftDistance;
	unsigned long int shiftCount;
	unsigned long int i;

	unsigned long int dwIndexBits;

	if((lpTable == NULL) || (lpSystem == NULL)) { return minorE_InvalidParam; }
	if(dwBucketCount > 16777216) { return minorE_InvalidParam; }
	(*lpTable) = NULL;

	e = lpSystem->alloc((void**)(lpTable), sizeof(struct minorCompressor_Deflate_HashTable) + dwBucketCount*sizeof(((struct minorCompressor_Deflate_HashTable*)0)->entries[0]), lpSystem->lpFreeParam_Alloc);
	if(e != minorE_Ok) { return e; }

	(*lpTable)->dwBuckets = dwBucketCount;
	(*lpTable)->dwMaxDepth = dwMaxDepth;

	dwIndexBits = 0;
	shiftMask = 0;
	i = dwBucketCount-1;
	while(i != 0) {
		dwIndexBits 	= dwIndexBits + 1;
		shiftMask 		= (shiftMask << 1) | 0x00000001;

		i = i >> 1;
	}

	shiftCount = (24 / dwIndexBits) + ((24 % dwIndexBits == 0) ? 0 : 1);
	shiftDistance = dwIndexBits;

	(*lpTable)->dwHashShiftCount = shiftCount;
	(*lpTable)->dwHashShiftDistance = shiftDistance;
	(*lpTable)->dwHashShiftMask = shiftMask;

	for(i = 0; i < dwBucketCount; i=i+1) {
		(*lpTable)->entries[i].lpFirst 			= NULL;
		(*lpTable)->entries[i].dwCurrentDepth 	= 0;
	}

	return minorE_Ok;
}

static enum minorError minorCompressor_Deflate_Hashtable_Release(
	struct minorCompressor_Deflate_HashTable** lpTable,
	struct minorSystemInterface* lpSystem
) {
	unsigned long int i;
	struct minorCompressor_Deflate_HashTableEntry* lpEntry;
	struct minorCompressor_Deflate_HashTableEntry* lpEntryNext;

	if((lpTable == NULL) || (lpSystem == NULL)) 	{ return minorE_InvalidParam; }
	if((*lpTable) == NULL) 							{ return minorE_Ok; }

	/* Release all chains inside the buckets */
	for(i = 0; i < (*lpTable)->dwBuckets; i=i+1) {
		lpEntry = (*lpTable)->entries[i].lpFirst;
		while(lpEntry != NULL) {
			lpEntryNext = lpEntry->lpNext;
			lpSystem->free((void*)lpEntry, lpSystem->lpFreeParam_Free);
			lpEntry = lpEntryNext;
		}
		(*lpTable)->entries[i].lpFirst = NULL;
		(*lpTable)->entries[i].dwCurrentDepth = 0;
	}

	lpSystem->free((void*)(*lpTable), lpSystem->lpFreeParam_Free);
	(*lpTable) = NULL;
	return minorE_Ok;
}

#if 0
	/* This is currently not used */
	static enum minorError minorCompressor_Deflate_Hashtable_Clear(
		struct minorCompressor_Deflate_HashTable* lpTable,
		struct minorSystemInterface* lpSystem
	) {
		unsigned long int i;
		struct minorCompressor_Deflate_HashTableEntry* lpEntry;
		struct minorCompressor_Deflate_HashTableEntry* lpEntryNext;

		if((lpTable == NULL) || (lpSystem == NULL)) 	{ return minorE_InvalidParam; }

		/* Release all chains inside the buckets */
		for(i = 0; i < lpTable->dwBuckets; i=i+1) {
			lpEntry = lpTable->entries[i].lpFirst;
			while(lpEntry != NULL) {
				lpEntryNext = lpEntry->lpNext;
				lpSystem->free((void*)lpEntry, lpSystem->lpFreeParam_Free);
				lpEntry = lpEntryNext;
			}
			lpTable->entries[i].lpFirst = NULL;
			lpTable->entries[i].dwCurrentDepth = 0;
		}
		return minorE_Ok;
	}
#endif

static enum minorError minorCompressor_Deflate_Hashtable_Insert(
	uint32_t bThreeBytes,
	unsigned long int dwAbsolutePosition,
	unsigned long int dwBufferOffset,
	unsigned long int dwKeepableDistance,

	struct minorCompressor_Deflate_HashTable* lpTable,
	struct minorSystemInterface* lpSystem
) {
	enum minorError e;
	struct minorCompressor_Deflate_HashTableEntry* lpEntry = NULL;
	struct minorCompressor_Deflate_HashTableEntry* lpEntryPrev;
	unsigned long int dwBucket;
	unsigned long int dwKeepable;

	if((lpTable == NULL) || (lpSystem == NULL)) { return minorE_InvalidParam; }
	dwBucket = minorCompressor_Deflate_Hash(bThreeBytes, lpTable) % lpTable->dwBuckets;

	/*
		Check that we are not trying to do a double insert. This MAY sometimes happen
		and we should blindly accept without doing another push
	*/

	if(lpTable->entries[dwBucket].lpFirst != NULL) {
		if(lpTable->entries[dwBucket].lpFirst->dwAbsolutePosition == dwAbsolutePosition) { return minorE_Ok; }
	}

	if((e = lpSystem->alloc((void**)(&lpEntry),sizeof(struct minorCompressor_Deflate_HashTableEntry), lpSystem->lpFreeParam_Alloc)) != minorE_Ok) { return e; }
	lpEntry->dwAbsolutePosition = dwAbsolutePosition;
	lpEntry->dwBufferOffset = dwBufferOffset;
	lpEntry->lpNext = lpTable->entries[dwBucket].lpFirst;

	lpTable->entries[dwBucket].lpFirst = lpEntry;
	lpTable->entries[dwBucket].dwCurrentDepth = lpTable->entries[dwBucket].dwCurrentDepth + 1;

	if((lpTable->dwMaxDepth > 0) && (lpTable->entries[dwBucket].dwCurrentDepth > lpTable->dwMaxDepth)) {
		/* We have to remove at least the last element, possibly clean more of them ... */
		dwKeepable = lpTable->dwMaxDepth;
		lpEntryPrev = NULL;
		lpEntry = lpTable->entries[dwBucket].lpFirst;
		while((lpEntry != NULL) && ((dwAbsolutePosition - lpEntry->dwAbsolutePosition) <= dwKeepableDistance) && (dwKeepable > 0)) {
			lpEntryPrev = lpEntry;
			lpEntry = lpEntry->lpNext;
			dwKeepable = dwKeepable - 1;
		}

		if(lpEntry != NULL) {
			/* Unlink the chain from the predecessor ... */
			if(lpEntryPrev != NULL) { lpEntryPrev->lpNext = NULL; } else { lpTable->entries[dwBucket].lpFirst = NULL; lpTable->entries[dwBucket].dwCurrentDepth = 0; }

			/* If we haven't reached the end of the list until now we have to release something ... */
			while(lpEntry != NULL) {
				lpEntryPrev = lpEntry;
				lpEntry = lpEntry->lpNext;

				lpSystem->free((void*)lpEntryPrev, lpSystem->lpFreeParam_Free);
				lpTable->entries[dwBucket].dwCurrentDepth = lpTable->entries[dwBucket].dwCurrentDepth - 1;
			}
		}
	}

	return minorE_Ok;
}

static enum minorError minorCompressor_Deflate_Hashtable_Search(
	struct minorCompressor_Deflate* lpCompressor,

	unsigned long int dwStartAbsolutePosition,
	unsigned long int dwStartBufferIndex,

	unsigned long int *lpBestMatch_PositionAbsolute,
	unsigned long int *lpBestMatch_Distance,
	unsigned long int *lpBestMatch_Length,
	unsigned long int *lpBestMatch_LazyDistance
) {
	unsigned long int dwBufferedData;

	unsigned long int dwLazyIndex;
	unsigned long int dwHashBucket;
	uint32_t bThreeBytes;
	struct minorCompressor_Deflate_HashTableEntry* lpCandidateMatch;
	struct minorCompressor_Deflate_HashTableEntry* lpCandidateMatchPrev;

	unsigned long int dwCurrent_BufferIndex;
	unsigned long int dwCurrent_AbsolutePosition;

	unsigned long int dwMatch_Max;
	unsigned long int dwMatch_Cur;

	/* Input parameter validation */
	if(lpCompressor == NULL) { return minorE_InvalidParam; }
	if((lpBestMatch_Length == NULL) || (lpBestMatch_Distance == NULL)) { return minorE_InvalidParam; }

	/* Initialize output into a no-match status */
	if(lpBestMatch_PositionAbsolute != NULL) { (*lpBestMatch_PositionAbsolute) = 0; }
	(*lpBestMatch_Distance) 	= 0;
	(*lpBestMatch_Length) 		= 0;
	(*lpBestMatch_LazyDistance) = 0;

	/* dwBufferedData is lookahead size (!), NOT the lookback distance */
	dwBufferedData = (lpCompressor->dwRawRingHead >= lpCompressor->dwRawRingTail) ? (lpCompressor->dwRawRingHead - lpCompressor->dwRawRingTail) : (lpCompressor->dwRawRingSize - lpCompressor->dwRawRingTail + lpCompressor->dwRawRingHead);

	/* We never perform a scan for less than 3 bytes, we'll signal ok in a no-match status */
	if(dwBufferedData < 3) { return minorE_Ok; }

	/*
		In many cases we perform more than one scan; one at the current location
		and (for example in RFC conforming mode) a second at the next location.
		This allows us to possibly find a better match. Configuration options allow
		to run deflate in non RFC mode with a much longer lazy matching distance
		but beware that this increases the complexity of the scan linearly with
		the lazy matching length (Setting to 1 instead of 0 may double the time in
		worst case, setting it to 2 tripple it, etc.)
	*/
	dwCurrent_BufferIndex 		= dwStartBufferIndex;
	dwCurrent_AbsolutePosition 	= dwStartAbsolutePosition;

	for(dwLazyIndex = 0; dwLazyIndex <= lpCompressor->dwLazyMatchDistance; dwLazyIndex = dwLazyIndex + 1) {
		/* Check if we have any entry in our hashmap to start our match against ... */
		if((dwBufferedData - dwLazyIndex) < 3) { break; } /* We cannot perform lazy matching at the end ... */
		bThreeBytes = ((uint32_t)(lpCompressor->lpRawRingBuffer[(dwCurrent_BufferIndex + dwLazyIndex) % lpCompressor->dwRawRingSize])) |
			(((uint32_t)(lpCompressor->lpRawRingBuffer[(dwCurrent_BufferIndex + dwLazyIndex + 1) % lpCompressor->dwRawRingSize])) << 8) |
			(((uint32_t)(lpCompressor->lpRawRingBuffer[(dwCurrent_BufferIndex + dwLazyIndex + 2) % lpCompressor->dwRawRingSize])) << 16);
		dwHashBucket = minorCompressor_Deflate_Hash(bThreeBytes, lpCompressor->lpLZ77HashTable);

		lpCandidateMatch = lpCompressor->lpLZ77HashTable->entries[dwHashBucket].lpFirst;
		lpCandidateMatchPrev = NULL;

		while(lpCandidateMatch != NULL) {
			/*
				Scan through all candidates
				Note that if one candidate is out of range, all following are too so we can discard the
				chain in that case.
			*/
			if(((dwCurrent_AbsolutePosition+dwLazyIndex) - lpCandidateMatch->dwAbsolutePosition) > lpCompressor->dwLZ77LookbackDistance) {
				/* Out of range. Release this and all following entries */
				if(lpCandidateMatchPrev != NULL) {
					lpCandidateMatchPrev->lpNext = NULL;
				} else {
					/* Unlink from the bucket itself */
					lpCompressor->lpLZ77HashTable->entries[dwHashBucket].lpFirst = NULL;
				}

				/* Now release the chain */
				while(lpCandidateMatch != NULL) {
					lpCandidateMatchPrev = lpCandidateMatch;
					lpCandidateMatch = lpCandidateMatchPrev->lpNext;
					lpCompressor->lpSystemAPI->free((void*)lpCandidateMatchPrev, lpCompressor->lpSystemAPI->lpFreeParam_Free);
					lpCompressor->lpLZ77HashTable->entries[dwHashBucket].dwCurrentDepth = lpCompressor->lpLZ77HashTable->entries[dwHashBucket].dwCurrentDepth - 1;
				}
				lpCandidateMatch = NULL;
			} else {
				/*

					We have a possible match

					We can match up to:
						* no more than (dwLZ77MaximumMatchLength) bytes
						* no more than dwBufferedData-dwLazyIndex
				*/
				dwMatch_Max = (lpCompressor->dwLZ77MaximumMatchLength > (dwBufferedData - dwLazyIndex)) ? (dwBufferedData - dwLazyIndex) : lpCompressor->dwLZ77MaximumMatchLength;

				for(dwMatch_Cur = 0; dwMatch_Cur < dwMatch_Max; dwMatch_Cur = dwMatch_Cur + 1) {
					/*
						Lookback: lpCompressor->lpRawRingBuffer[(dwCurrent_BufferIndex + dwLazyIndex + dwMatch_Cur) % lpCompressor->dwRawRingSize]
						Current: lpCompressor->lpRawRingBuffer[(lpCandidateMatch->dwBufferOffset + dwMatch_Cur) % lpCompressor->dwRawRingSize]
					*/
					if(lpCompressor->lpRawRingBuffer[(dwCurrent_BufferIndex + dwLazyIndex + dwMatch_Cur) % lpCompressor->dwRawRingSize] != lpCompressor->lpRawRingBuffer[(lpCandidateMatch->dwBufferOffset + dwMatch_Cur) % lpCompressor->dwRawRingSize]) {
						break;
					} else {
					}
				}

				/* Keep track of the "best" match */
				if(dwMatch_Cur > (*lpBestMatch_Length)) {
					(*lpBestMatch_Length) = dwMatch_Cur;
					(*lpBestMatch_Distance) = (((dwCurrent_BufferIndex + dwLazyIndex + dwMatch_Cur) % lpCompressor->dwRawRingSize) > ((lpCandidateMatch->dwBufferOffset + dwMatch_Cur) % lpCompressor->dwRawRingSize) ) ?
							((dwCurrent_BufferIndex + dwLazyIndex + dwMatch_Cur) % lpCompressor->dwRawRingSize) - ((lpCandidateMatch->dwBufferOffset + dwMatch_Cur) % lpCompressor->dwRawRingSize) :
							(lpCompressor->dwRawRingSize) - ((lpCandidateMatch->dwBufferOffset + dwMatch_Cur) % lpCompressor->dwRawRingSize) + ((dwCurrent_BufferIndex + dwLazyIndex + dwMatch_Cur) % lpCompressor->dwRawRingSize);
					if(lpBestMatch_PositionAbsolute != NULL) { (*lpBestMatch_PositionAbsolute) = lpCandidateMatch->dwAbsolutePosition; }
					(*lpBestMatch_LazyDistance) = dwLazyIndex;

					if((((*lpBestMatch_Length) >= lpCompressor->dwLZ77EarlyMatchLength) && (lpCompressor->dwLZ77EarlyMatchLength != 0)) || ((*lpBestMatch_Length) == lpCompressor->dwLZ77MaximumMatchLength)) {
						/* Immediately accept this match ... */
						return minorE_Ok;
					}
				}

				/* Select the next match */
				lpCandidateMatchPrev = lpCandidateMatch;
				lpCandidateMatch = lpCandidateMatch->lpNext;
			}
		}
		/* In the next loop iteration we scan again with the next lazy index (if this feature is enabled) */
		if((dwLazyIndex == 0) && ((*lpBestMatch_Length) == 0)) { break; } /* We do not have to check lazyly at the next position because we haven't found a match ... */
	}

	return minorE_Ok;
}

static enum minorError minorCompressor_Deflate__PackageMerge_GenerateDictionary(
	struct minorCompressor_Deflate* lpThis
) {
	enum minorError e;
	unsigned long int i;
	#ifdef MINOR_DEFLATE_DEBUG
		unsigned long int j;
		uint32_t code;
		double dSum;
		double dAvgLen;
		double dAvgLen2;
	#endif

	unsigned long int dwUsedCodes_Literal;
	unsigned long int dwUsedCodes_Distance;

	uint8_t lastCode;
	uint8_t lastCodeCount;
	uint8_t currentCode;

	if(lpThis == NULL) { return minorE_InvalidParam; }

	/*
		We run package-merge on literals as well as on distances.
		All data structures will be allocated on the heap (because of their size)
	*/
	e = minorHuffutilCreateCodes((struct minorHuffutilAlphabet*)(&lpThis->statLiteralLength), lpThis->lpSystemAPI, MINOR_DEFLATE__MAXIMUM_BITS_LITERALLENGTH);
	if(e != minorE_Ok) { return e; }
	e = minorHuffutilCreateCodes((struct minorHuffutilAlphabet*)(&lpThis->statDistance), lpThis->lpSystemAPI, MINOR_DEFLATE__MAXIMUM_BITS_DISTANCE);
	if(e != minorE_Ok) { return e; }

	#ifdef MINOR_DEFLATE_DEBUG
		/* DEBUG: Output */
		printf("%s:%u\n", __FILE__, __LINE__);
		printf("Dumping generated literal/length codetables:\n");
		printf("Symbol\tLength\tCode\n");
		for(j = 0; j < (sizeof(lpThis->statLiteralLength.entry)/sizeof(struct minorHuffutilAlphabetEntry_U32)); j=j+1) {
			printf("%3u\t%u\t", lpThis->statLiteralLength.entry[j].dwSymbol, lpThis->statLiteralLength.entry[j].huffLength);
			if(lpThis->statLiteralLength.entry[j].huffLength > 0) {
				code = lpThis->statLiteralLength.entry[j].huffCode << (32 - lpThis->statLiteralLength.entry[j].huffLength);
				for(i = 0; i < lpThis->statLiteralLength.entry[j].huffLength; i=i+1) {
					printf("%c", ((code & 0x80000000) != 0) ? '1' : '0');
					code = code << 1;
				}
			}
			printf("\n");
		}
		printf("\n\n");

		printf("%s:%u\n", __FILE__, __LINE__);
		printf("Dumping generated distance codetables:\n");
		printf("Symbol\tLength\tCode\n");
		for(j = 0; j < (sizeof(lpThis->statDistance.entry)/sizeof(struct minorHuffutilAlphabetEntry_U32)); j=j+1) {
			printf("%3u\t%u\t", lpThis->statDistance.entry[j].dwSymbol, lpThis->statDistance.entry[j].huffLength);
			if(lpThis->statDistance.entry[j].huffLength > 0) {
				code = lpThis->statDistance.entry[j].huffCode << (32 - lpThis->statDistance.entry[j].huffLength);
				for(i = 0; i < lpThis->statDistance.entry[j].huffLength; i=i+1) {
					printf("%c", ((code & 0x80000000) != 0) ? '1' : '0');
					code = code << 1;
				}
			}
			printf("\n");
		}
		printf("\n\n");
	#endif

	/* Do RLE of codelengths ... */

	lpThis->codelengthcodeTable.dwUsedSymbols = 0;

	/*
		First calculate HLIT and HDIST

		Minima: 257 Literal codes
		1 Distance code
	*/
	dwUsedCodes_Literal = minorCompressor_Deflate__ALPHABETSIZE_LITERAL;
	while((lpThis->statLiteralLength.entry[dwUsedCodes_Literal-1].huffLength == 0) && (dwUsedCodes_Literal > 257)) { dwUsedCodes_Literal = dwUsedCodes_Literal - 1; }
	dwUsedCodes_Distance = minorCompressor_Deflate__ALPHABETSIZE_DISTANCE;
	while((lpThis->statDistance.entry[dwUsedCodes_Distance-1].huffLength == 0) && (dwUsedCodes_Distance > 1)) { dwUsedCodes_Distance = dwUsedCodes_Distance - 1; }

	/* Now do RLE keeping codes in place ... */
	for(i = 0; i < (sizeof(lpThis->alphabetCodelengthCodes.entry)/sizeof(struct minorHuffutilAlphabetEntry_U32)); i=i+1) {
		lpThis->alphabetCodelengthCodes.entry[i].dwSymbol = i;
		lpThis->alphabetCodelengthCodes.entry[i].dProbability = 0;
		lpThis->alphabetCodelengthCodes.entry[i].bAdditionalBits = (i < 16) ? 0 : ((i == 16) ? 2 : ((i == 17) ? 3 : 7));
		lpThis->alphabetCodelengthCodes.entry[i].huffCode = 0;
		lpThis->alphabetCodelengthCodes.entry[i].huffMask = 0;
		lpThis->alphabetCodelengthCodes.entry[i].huffLength = 0;
	}
	lpThis->alphabetCodelengthCodes.dwAlphabetSize = sizeof(lpThis->alphabetCodelengthCodes.entry)/sizeof(struct minorHuffutilAlphabetEntry_U32);

	lastCode = lpThis->statLiteralLength.entry[0].huffLength;
	lastCodeCount = 1;
	lpThis->codelengthcodeTable.dwUsedSymbols = 0;
	for(i = 1; i < (dwUsedCodes_Literal+dwUsedCodes_Distance); i=i+1) {
		currentCode = (uint8_t)((i < dwUsedCodes_Literal) ? lpThis->statLiteralLength.entry[i].huffLength : lpThis->statDistance.entry[i - dwUsedCodes_Literal].huffLength);

		if(currentCode != lastCode) {
			if(lastCode == 0) {
				while(lastCodeCount >= 3) {
					if((lastCodeCount >= 11) && (lastCodeCount < 139)) {
						lpThis->codelengthcodeTable.symbols[lpThis->codelengthcodeTable.dwUsedSymbols] = 18;
						lpThis->codelengthcodeTable.symbols[lpThis->codelengthcodeTable.dwUsedSymbols+1] = lastCodeCount-11;
						lastCodeCount = 0;
						lpThis->codelengthcodeTable.dwUsedSymbols = lpThis->codelengthcodeTable.dwUsedSymbols + 2;
						lpThis->alphabetCodelengthCodes.entry[18].dProbability = lpThis->alphabetCodelengthCodes.entry[18].dProbability + 1;
					} else if(lastCodeCount > 138) {
						lpThis->codelengthcodeTable.symbols[lpThis->codelengthcodeTable.dwUsedSymbols] = 18;
						lpThis->codelengthcodeTable.symbols[lpThis->codelengthcodeTable.dwUsedSymbols+1] = 138-11;
						lastCodeCount = lastCodeCount - 138;
						lpThis->codelengthcodeTable.dwUsedSymbols = lpThis->codelengthcodeTable.dwUsedSymbols + 2;
						lpThis->alphabetCodelengthCodes.entry[18].dProbability = lpThis->alphabetCodelengthCodes.entry[18].dProbability + 1;
					} else {
						lpThis->codelengthcodeTable.symbols[lpThis->codelengthcodeTable.dwUsedSymbols] = 17;
						lpThis->codelengthcodeTable.symbols[lpThis->codelengthcodeTable.dwUsedSymbols+1] = lastCodeCount-3;
						lastCodeCount = 0;
						lpThis->codelengthcodeTable.dwUsedSymbols = lpThis->codelengthcodeTable.dwUsedSymbols + 2;
						lpThis->alphabetCodelengthCodes.entry[17].dProbability = lpThis->alphabetCodelengthCodes.entry[17].dProbability + 1;
					}
				}
				if(lastCodeCount != 0) { lpThis->codelengthcodeTable.symbols[lpThis->codelengthcodeTable.dwUsedSymbols] = lastCode; lastCodeCount = lastCodeCount - 1; lpThis->codelengthcodeTable.dwUsedSymbols = lpThis->codelengthcodeTable.dwUsedSymbols + 1; lpThis->alphabetCodelengthCodes.entry[lastCode].dProbability = lpThis->alphabetCodelengthCodes.entry[lastCode].dProbability + 1; }
				if(lastCodeCount != 0) { lpThis->codelengthcodeTable.symbols[lpThis->codelengthcodeTable.dwUsedSymbols] = lastCode; lastCodeCount = lastCodeCount - 1; lpThis->codelengthcodeTable.dwUsedSymbols = lpThis->codelengthcodeTable.dwUsedSymbols + 1; lpThis->alphabetCodelengthCodes.entry[lastCode].dProbability = lpThis->alphabetCodelengthCodes.entry[lastCode].dProbability + 1; }
			} else {
				/* We have to emit the code at least once to be capable of issuing repeats */
				if(lastCodeCount != 0) { lpThis->codelengthcodeTable.symbols[lpThis->codelengthcodeTable.dwUsedSymbols] = lastCode; lastCodeCount = lastCodeCount - 1; lpThis->codelengthcodeTable.dwUsedSymbols = lpThis->codelengthcodeTable.dwUsedSymbols + 1; lpThis->alphabetCodelengthCodes.entry[lastCode].dProbability = lpThis->alphabetCodelengthCodes.entry[lastCode].dProbability + 1; }
				while(lastCodeCount >= 3) {
					if(lastCodeCount >= 6) {
						lpThis->codelengthcodeTable.symbols[lpThis->codelengthcodeTable.dwUsedSymbols] = 16;
						lpThis->codelengthcodeTable.symbols[lpThis->codelengthcodeTable.dwUsedSymbols+1] = 3;
						lastCodeCount = lastCodeCount - 6;
						lpThis->codelengthcodeTable.dwUsedSymbols = lpThis->codelengthcodeTable.dwUsedSymbols + 2;
						lpThis->alphabetCodelengthCodes.entry[16].dProbability = lpThis->alphabetCodelengthCodes.entry[16].dProbability + 1;
					} else {
						lpThis->codelengthcodeTable.symbols[lpThis->codelengthcodeTable.dwUsedSymbols] = 16;
						lpThis->codelengthcodeTable.symbols[lpThis->codelengthcodeTable.dwUsedSymbols+1] = lastCodeCount - 3;
						lastCodeCount = 0;
						lpThis->codelengthcodeTable.dwUsedSymbols = lpThis->codelengthcodeTable.dwUsedSymbols + 2;
						lpThis->alphabetCodelengthCodes.entry[16].dProbability = lpThis->alphabetCodelengthCodes.entry[16].dProbability + 1;
					}
				}

				/* Emit remaining codes */
				if(lastCodeCount != 0) { lpThis->codelengthcodeTable.symbols[lpThis->codelengthcodeTable.dwUsedSymbols] = lastCode; lastCodeCount = lastCodeCount - 1; lpThis->codelengthcodeTable.dwUsedSymbols = lpThis->codelengthcodeTable.dwUsedSymbols + 1; lpThis->alphabetCodelengthCodes.entry[lastCode].dProbability = lpThis->alphabetCodelengthCodes.entry[lastCode].dProbability + 1; }
				if(lastCodeCount != 0) { lpThis->codelengthcodeTable.symbols[lpThis->codelengthcodeTable.dwUsedSymbols] = lastCode; lastCodeCount = lastCodeCount - 1; lpThis->codelengthcodeTable.dwUsedSymbols = lpThis->codelengthcodeTable.dwUsedSymbols + 1; lpThis->alphabetCodelengthCodes.entry[lastCode].dProbability = lpThis->alphabetCodelengthCodes.entry[lastCode].dProbability + 1; }
			}
			lastCode = currentCode;
			lastCodeCount = 1;
		} else {
			lastCodeCount = lastCodeCount + 1;
		}
	}
	/* Now we emit the last symbol ... */
	if(lastCodeCount != 0) {
		if(lastCode == 0) {
			while(lastCodeCount >= 3) {
				if((lastCodeCount >= 11) && (lastCodeCount < 139)) {
					lpThis->codelengthcodeTable.symbols[lpThis->codelengthcodeTable.dwUsedSymbols] = 18;
					lpThis->codelengthcodeTable.symbols[lpThis->codelengthcodeTable.dwUsedSymbols+1] = lastCodeCount-11;
					lastCodeCount = 0;
					lpThis->codelengthcodeTable.dwUsedSymbols = lpThis->codelengthcodeTable.dwUsedSymbols + 2;
					lpThis->alphabetCodelengthCodes.entry[18].dProbability = lpThis->alphabetCodelengthCodes.entry[18].dProbability + 1;
				} else if(lastCodeCount > 138) {
					lpThis->codelengthcodeTable.symbols[lpThis->codelengthcodeTable.dwUsedSymbols] = 18;
					lpThis->codelengthcodeTable.symbols[lpThis->codelengthcodeTable.dwUsedSymbols+1] = 138-11;
					lastCodeCount = lastCodeCount - 138;
					lpThis->codelengthcodeTable.dwUsedSymbols = lpThis->codelengthcodeTable.dwUsedSymbols + 2;
					lpThis->alphabetCodelengthCodes.entry[18].dProbability = lpThis->alphabetCodelengthCodes.entry[18].dProbability + 1;
				} else {
					lpThis->codelengthcodeTable.symbols[lpThis->codelengthcodeTable.dwUsedSymbols] = 17;
					lpThis->codelengthcodeTable.symbols[lpThis->codelengthcodeTable.dwUsedSymbols+1] = lastCodeCount-3;
					lastCodeCount = 0;
					lpThis->codelengthcodeTable.dwUsedSymbols = lpThis->codelengthcodeTable.dwUsedSymbols + 2;
					lpThis->alphabetCodelengthCodes.entry[17].dProbability = lpThis->alphabetCodelengthCodes.entry[17].dProbability + 1;
				}
			}
			if(lastCodeCount != 0) { lpThis->codelengthcodeTable.symbols[lpThis->codelengthcodeTable.dwUsedSymbols] = lastCode; lastCodeCount = lastCodeCount - 1; lpThis->codelengthcodeTable.dwUsedSymbols = lpThis->codelengthcodeTable.dwUsedSymbols + 1; lpThis->alphabetCodelengthCodes.entry[lastCode].dProbability = lpThis->alphabetCodelengthCodes.entry[lastCode].dProbability + 1; }
			if(lastCodeCount != 0) { lpThis->codelengthcodeTable.symbols[lpThis->codelengthcodeTable.dwUsedSymbols] = lastCode; lastCodeCount = lastCodeCount - 1; lpThis->codelengthcodeTable.dwUsedSymbols = lpThis->codelengthcodeTable.dwUsedSymbols + 1; lpThis->alphabetCodelengthCodes.entry[lastCode].dProbability = lpThis->alphabetCodelengthCodes.entry[lastCode].dProbability + 1; }
		} else {
			/* We have to emit the code at least once to be capable of issuing repeats */
			if(lastCodeCount != 0) { lpThis->codelengthcodeTable.symbols[lpThis->codelengthcodeTable.dwUsedSymbols] = lastCode; lastCodeCount = lastCodeCount - 1; lpThis->codelengthcodeTable.dwUsedSymbols = lpThis->codelengthcodeTable.dwUsedSymbols + 1; lpThis->alphabetCodelengthCodes.entry[lastCode].dProbability = lpThis->alphabetCodelengthCodes.entry[lastCode].dProbability + 1; }

			while(lastCodeCount >= 3) {
				if(lastCodeCount >= 6) {
					lpThis->codelengthcodeTable.symbols[lpThis->codelengthcodeTable.dwUsedSymbols] = 16;
					lpThis->codelengthcodeTable.symbols[lpThis->codelengthcodeTable.dwUsedSymbols+1] = 3;
					lastCodeCount = lastCodeCount - 6;
					lpThis->codelengthcodeTable.dwUsedSymbols = lpThis->codelengthcodeTable.dwUsedSymbols + 2;
					lpThis->alphabetCodelengthCodes.entry[16].dProbability = lpThis->alphabetCodelengthCodes.entry[16].dProbability + 1;
				} else {
					lpThis->codelengthcodeTable.symbols[lpThis->codelengthcodeTable.dwUsedSymbols] = 16;
					lpThis->codelengthcodeTable.symbols[lpThis->codelengthcodeTable.dwUsedSymbols+1] = lastCodeCount - 3;
					lastCodeCount = 0;
					lpThis->codelengthcodeTable.dwUsedSymbols = lpThis->codelengthcodeTable.dwUsedSymbols + 2;
					lpThis->alphabetCodelengthCodes.entry[16].dProbability = lpThis->alphabetCodelengthCodes.entry[16].dProbability + 1;
				}
			}

			/* Emit remaining codes */
			if(lastCodeCount != 0) { lpThis->codelengthcodeTable.symbols[lpThis->codelengthcodeTable.dwUsedSymbols] = lastCode; lastCodeCount = lastCodeCount - 1; lpThis->codelengthcodeTable.dwUsedSymbols = lpThis->codelengthcodeTable.dwUsedSymbols + 1; lpThis->alphabetCodelengthCodes.entry[lastCode].dProbability = lpThis->alphabetCodelengthCodes.entry[lastCode].dProbability + 1; }
			if(lastCodeCount != 0) { lpThis->codelengthcodeTable.symbols[lpThis->codelengthcodeTable.dwUsedSymbols] = lastCode; lastCodeCount = lastCodeCount - 1; lpThis->codelengthcodeTable.dwUsedSymbols = lpThis->codelengthcodeTable.dwUsedSymbols + 1; lpThis->alphabetCodelengthCodes.entry[lastCode].dProbability = lpThis->alphabetCodelengthCodes.entry[lastCode].dProbability + 1; }
		}
	}

	/* Now we've got the RLE compressed codelength table. Generate huffman codes again */
	e = minorHuffutilCreateCodes((struct minorHuffutilAlphabet*)(&lpThis->alphabetCodelengthCodes), lpThis->lpSystemAPI, MINOR_DEFLATE__MAXIMUM_BITS_CODELENGHTS);
	if(e != minorE_Ok) { return e; }

	#ifdef MINOR_DEFLATE_DEBUG
		/* DEBUG: Output */
		printf("%s:%u\n", __FILE__, __LINE__);
		printf("Dumping generated codelengths codetables:\n");
		printf("Symbol\tLength\tCode\n");
		for(j = 0; j < (sizeof(lpThis->alphabetCodelengthCodes.entry)/sizeof(struct minorHuffutilAlphabetEntry_U32)); j=j+1) {
			printf("%3u\t%u\t", lpThis->alphabetCodelengthCodes.entry[j].dwSymbol, lpThis->alphabetCodelengthCodes.entry[j].huffLength);
			if(lpThis->alphabetCodelengthCodes.entry[j].huffLength > 0) {
				code = lpThis->alphabetCodelengthCodes.entry[j].huffCode << (32 - lpThis->alphabetCodelengthCodes.entry[j].huffLength);
				for(i = 0; i < lpThis->alphabetCodelengthCodes.entry[j].huffLength; i=i+1) {
					printf("%c", ((code & 0x80000000) != 0) ? '1' : '0');
					code = code << 1;
				}
			}
			printf("\n");
		}
		printf("\n\n");
	#endif

	#ifdef MINOR_DEFLATE_DEBUG
		/* Some block statistics */
		dSum = 0; dAvgLen = 0; dAvgLen2 = 0;
		for(j = 0; j < lpThis->statLiteralLength.dwAlphabetSize; j=j+1) { dSum = dSum + lpThis->statLiteralLength.entry[j].dProbability; }
		for(j = 0; j < lpThis->statLiteralLength.dwAlphabetSize; j=j+1) {
			dAvgLen = dAvgLen + (lpThis->statLiteralLength.entry[j].dProbability / dSum) * (lpThis->statLiteralLength.entry[j].huffLength + lpThis->statLiteralLength.entry[j].bAdditionalBits);
			dAvgLen2 = dAvgLen2 + (lpThis->statLiteralLength.entry[j].dProbability / dSum) * (lpThis->statLiteralLength.entry[j].huffLength);
		}
		printf("%s:%u Debug stats - Literal/Length average codeword length excluding additional bits: %lf\n", __FILE__, __LINE__, dAvgLen2);
		printf("%s:%u Debug stats - Literal/Length average codeword length including additional bits: %lf (Reduction by %3.3lf percent)\n", __FILE__, __LINE__, dAvgLen, (1.0-dAvgLen/9.0)*100.0);

		dSum = 0; dAvgLen = 0; dAvgLen2 = 0;
		for(j = 0; j < lpThis->statDistance.dwAlphabetSize; j=j+1) { dSum = dSum + lpThis->statDistance.entry[j].dProbability; }
		for(j = 0; j < lpThis->statDistance.dwAlphabetSize; j=j+1) {
			dAvgLen = dAvgLen + (lpThis->statDistance.entry[j].dProbability / dSum) * (lpThis->statDistance.entry[j].huffLength + lpThis->statDistance.entry[j].bAdditionalBits);
			dAvgLen2 = dAvgLen2 + (lpThis->statDistance.entry[j].dProbability / dSum) * (lpThis->statDistance.entry[j].huffLength);
		}
		printf("%s:%u Debug stats - Distance average codeword length excluding additional bits: %lf\n", __FILE__, __LINE__, dAvgLen2);
		printf("%s:%u Debug stats - Distance average codeword length including additional bits: %lf (Reduction by %3.3lf percent)\n", __FILE__, __LINE__, dAvgLen, (1.0-dAvgLen/16.0)*100.0);

		dSum = 0; dAvgLen = 0; dAvgLen2 = 0;
		for(j = 0; j < lpThis->alphabetCodelengthCodes.dwAlphabetSize; j=j+1) { dSum = dSum + lpThis->alphabetCodelengthCodes.entry[j].dProbability; }
		for(j = 0; j < lpThis->alphabetCodelengthCodes.dwAlphabetSize; j=j+1) {
			dAvgLen = dAvgLen + (lpThis->alphabetCodelengthCodes.entry[j].dProbability / dSum) * (lpThis->alphabetCodelengthCodes.entry[j].huffLength + lpThis->alphabetCodelengthCodes.entry[j].bAdditionalBits);
			dAvgLen2 = dAvgLen2 + (lpThis->alphabetCodelengthCodes.entry[j].dProbability / dSum) * (lpThis->alphabetCodelengthCodes.entry[j].huffLength);
		}
		printf("%s:%u Debug stats - Codelength code average codeword length excluding additional bits: %lf\n", __FILE__, __LINE__, dAvgLen2);
		printf("%s:%u Debug stats - Codelength code average codeword length including additional bits: %lf (Reduction by %3.3lf percent)\n", __FILE__, __LINE__, dAvgLen, (1.0-dAvgLen/5.0)*100.0);
	#endif

	/*
		Note that codelengths huffLength are NOT WRITTEN IN ORDER! They are emitted by the block encoder from
		our generated statistics in order 16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15
		at max. (HCLEN+4) bits as 3 bit sequences. This allows to emit a reduced number of codelengths.
	*/


	e = minorE_Ok;

	return e;
}

static enum minorError minorCompressor_Deflate__ProcessLZ77Symbol_Symbol(
	struct minorCompressor_Deflate* lpThis,
	uint16_t distance,
	uint16_t length
) {
	/*
		Symbol encoding inside the codebuffer:
			length/literal:		0-255		literally stored as value
					length:		> 255		9 Bits (0x1FF) used as "code" part as will be encoded later on,
											the upper 7 bits may be used as additional bits (such that >> 9 will only result in additional bits)
			distance:			ENCODED AS VALUE NOT WITH CODE + ADDITIONAL BITS. THIS WILL BE DONE AT A LATER STAGE!
								THE CODE IS ONLY CALUCLATED HERE TO PERFORM HUFFMAN TREE STATISTICS!
	*/
	unsigned long int i;
	uint16_t codewordLength;
	/* uint8_t additionalBitsLength; */
	uint16_t bitsLength;

	uint16_t codewordDistance;
	/* uint8_t additionalBitsDistance;
	uint16_t bitsDistance; */

	uint16_t encodedTempLength;
	uint16_t encodedTempDistance;

	/* Lookup the codeword for length code (inside the same alphabet as our literals) */
	for(i = 0; i < minorDeflate__LUT__LengthCodes__CODECOUNT-1; i=i+1) {
		if(length < minorDeflate__LUT__LengthCodes[i+1][1]) {
			/* Found entry i ... */
			codewordLength = i + 257;
			/* additionalBitsLength = minorDeflate__LUT__LengthCodes[i][0]; */
			bitsLength = length - minorDeflate__LUT__LengthCodes[i][1];
			break;
		}
	}
	if(i == minorDeflate__LUT__LengthCodes__CODECOUNT-1) {
		/* We use the last entry ... */
		codewordLength = i + 257;
		/* additionalBitsLength = minorDeflate__LUT__LengthCodes[i][0]; */
		bitsLength = length - minorDeflate__LUT__LengthCodes[i][1];
	}

	encodedTempLength = codewordLength | (bitsLength << 9); /* See comment above! */


	/* Lookup the codeword for the distance code */
	for(i = 0; i < minorDeflate__LUT__DistanceCodes__CODECOUNT-1; i=i+1) {
		if(distance < minorDeflate__LUT__DistanceCodes[i+1][1]) {
			/* Found entry i */
			codewordDistance = i;
			/* additionalBitsDistance = minorDeflate__LUT__DistanceCodes[i][0]; */
			/* bitsDistance = distance - minorDeflate__LUT__DistanceCodes[i][1]; */
			break;
		}
	}
	if(i == minorDeflate__LUT__DistanceCodes__CODECOUNT-1) {
		/* We use the last entry ... */
		codewordDistance = i;
		/* additionalBitsDistance = minorDeflate__LUT__DistanceCodes[i][0]; */
		/* bitsDistance = distance - minorDeflate__LUT__DistanceCodes[i][1]; */
	}

	encodedTempDistance = distance;

	/*
		Assertion that we stay inside our buffer. This will always be true
		as long as we don't cross our block buffer ...
	*/
	if(lpThis->dwSymbolBufferUsed > (lpThis->dwBlockSize-2)) {
		return minorE_ImplementationError;
	}
	/* Insert into the symbol buffer */
	lpThis->lpSymbolBuffer[lpThis->dwSymbolBufferUsed] = encodedTempLength;
	lpThis->lpSymbolBuffer[lpThis->dwSymbolBufferUsed+1] = encodedTempDistance;
	lpThis->dwSymbolBufferUsed = lpThis->dwSymbolBufferUsed + 2;
	/* And update block statistics */
	lpThis->statLiteralLength.entry[codewordLength].dProbability = lpThis->statLiteralLength.entry[codewordLength].dProbability + 1;
	lpThis->statDistance.entry[codewordDistance].dProbability = lpThis->statDistance.entry[codewordDistance].dProbability + 1;

	return minorE_Ok;
}
static enum minorError minorCompressor_Deflate__ProcessLZ77Symbol_Literal(
	struct minorCompressor_Deflate* lpThis,
	uint16_t literal
) {
	/*
		Assertion that we stay inside our buffer. This will always be true
		as long as we don't cross our block buffer ...
	*/
	if(lpThis->dwSymbolBufferUsed > (lpThis->dwBlockSize-1)) {
		return minorE_ImplementationError;
	}

	/* A literal simply get's attached to our symbol buffer ... */
	lpThis->lpSymbolBuffer[lpThis->dwSymbolBufferUsed] = literal;
	lpThis->dwSymbolBufferUsed = lpThis->dwSymbolBufferUsed + 1;
	/* Update block statistics for this literal */
	lpThis->statLiteralLength.entry[literal].dProbability = lpThis->statLiteralLength.entry[literal].dProbability + 1;

	return minorE_Ok;
}

static enum minorError minorCompressor_Deflate__FlushBlock_EmitBit(
	struct minorCompressor_Deflate* lpThis,
	uint8_t bitValue
) {
	enum minorError e;

	if(lpThis == NULL) { return minorE_InvalidParam; }
	e = minorE_Ok;
	lpThis->bitShiftreg = (lpThis->bitShiftreg >> 1) | (((bitValue == 0) ? 0 : 0x80));
	if((lpThis->shiftPosition = lpThis->shiftPosition + 1) == 8) {
		/* Emit byte into sink OR our output buffer ... */
		/* TODO: Output buffer support */
		e = lpThis->lpDataSink->write(
			lpThis->lpDataSink,
			&(lpThis->bitShiftreg),
			1,
			NULL
		);
		lpThis->shiftPosition = 0;
	}
	return e;
}

static enum minorError minorCompressor_Deflate__FlushBlock_EmitBitFlush(
	struct minorCompressor_Deflate* lpThis
) {
	enum minorError e;

	if(lpThis == NULL) { return minorE_InvalidParam; }
	if(lpThis->shiftPosition == 0) { return minorE_Ok; }
	while(lpThis->shiftPosition != 8) {
		lpThis->shiftPosition = lpThis->shiftPosition + 1;
		lpThis->bitShiftreg = lpThis->bitShiftreg >> 1;
	}
	/* Emit byte into sink OR our output buffer ... */
	e = lpThis->lpDataSink->write(
		lpThis->lpDataSink,
		&(lpThis->bitShiftreg),
		1,
		NULL
	);
	if(e == minorE_Ok) {
		lpThis->bitShiftreg = 0; /* This is required for restartability ... */
	}
	return e;
}


/* See RFC1951 Section 3.2.7 */
static unsigned long int minorCompressor_Deflate__FlushBlock__CodelengthAlphabetOrder[minorCompressor_Deflate__ALPHABETSIZE_CODELENGTH] = { 16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15 };

static enum minorError minorCompressor_Deflate__FlushBlock(
	struct minorCompressor_Deflate* lpThis
) {
	unsigned long int dwCandidateSize_Raw 		= ~0;
	unsigned long int dwCandidateSize_Dynamic 	= ~0;
	unsigned long int dwCandidateSize_Fixed		= ~0;

	unsigned long int dwBytesDone;

	unsigned long int dwCachedDataLookahead;
	unsigned long int dwCachedDataLookback;

	unsigned long int dwBestMatch_Absolute;
	unsigned long int dwBestMatch_Distance;
	unsigned long int dwBestMatch_Length;
	unsigned long int dwBestMatch_LazyIndex;
	unsigned long int dwSkipIndex;

	unsigned long int i;

	unsigned char bTemp;

	uint32_t bThreeBytes;

	uint32_t hclen = 0;
	uint32_t hdist = 0;
	uint32_t hlit  = 0;

	uint32_t dwTempDistance;
	uint32_t codeCurrent;

	enum minorError e;

	if(lpThis == NULL) { return minorE_InvalidParam; }

	/* Check current state */
	switch(lpThis->currentState) {
		/*
			We've been called by transfer function or similar (not continues).
			We enter the appropriate flushblock state (different states depending
			on which state we enter on leave ...)
		*/
		case minorCompressor_Deflate_State__AcceptingData:		lpThis->currentState = minorCompressor_Deflate_State__FlushBlock_1;
																lpThis->stateInfo.flushState = minorCompressor_Deflate_FlushBlock_State__FlushRawData;
																break;
		case minorCompressor_Deflate_State__FlushLastData:		lpThis->currentState = minorCompressor_Deflate_State__FlushBlock_2;
																lpThis->stateInfo.flushState = minorCompressor_Deflate_FlushBlock_State__FlushRawData;
																break;
		case minorCompressor_Deflate_State__FlushData:			lpThis->currentState = minorCompressor_Deflate_State__FlushBlock_3;
																lpThis->stateInfo.flushState = minorCompressor_Deflate_FlushBlock_State__FlushRawData;
																break;

		/*
			Re-entry after a continues call. We decide with our
			own internal (linear) state machine what to do next
		*/
		case minorCompressor_Deflate_State__FlushBlock_1:		break;
		case minorCompressor_Deflate_State__FlushBlock_2:		break;
		case minorCompressor_Deflate_State__FlushBlock_3:		break;

		/*
			We cannot enter from any other state
		*/
		default:												return minorE_InvalidState; /* Should never happen */
	}

	if(lpThis->stateInfo.flushState == minorCompressor_Deflate_FlushBlock_State__FlushRawData) {
		/* Calculate lookahead and lookback distances */
		if(lpThis->dwRawRingHead > lpThis->dwRawRingTail) {
			dwCachedDataLookahead = lpThis->dwRawRingHead - lpThis->dwRawRingTail;
		} else {
			dwCachedDataLookahead = (lpThis->dwRawRingSize - lpThis->dwRawRingTail) + lpThis->dwRawRingHead;
		}
		dwCachedDataLookback = lpThis->dwRawRingUsed - dwCachedDataLookahead;


		/*
			If there is any data left inside the ringbuffer that has not been
			processed until now (registering the sequences inside the hashtable,
			scanning for repetitions, etc.)
		*/

		while(dwCachedDataLookahead > 2) {
			/*
				Even if we're at the stage of emitting the last bytes inside this block we've
				to register the sequences for further looking forward.
			*/
			if(dwCachedDataLookback >= 2) {
				bThreeBytes = ((uint32_t)(lpThis->lpRawRingBuffer[(lpThis->dwRawRingTail-2+lpThis->dwRawRingSize) % lpThis->dwRawRingSize])) |
					((uint32_t)(lpThis->lpRawRingBuffer[(lpThis->dwRawRingTail-1+lpThis->dwRawRingSize) % lpThis->dwRawRingSize]) << 8) |
					((uint32_t)(lpThis->lpRawRingBuffer[(lpThis->dwRawRingTail+lpThis->dwRawRingSize) % lpThis->dwRawRingSize]) << 16);
				e = minorCompressor_Deflate_Hashtable_Insert(
					bThreeBytes,
					lpThis->dwRawRingHeadAbsolute - dwCachedDataLookahead - 2,
					(lpThis->dwRawRingTail - 2 + lpThis->dwRawRingSize) % lpThis->dwRawRingSize,
					lpThis->dwLZ77LookbackDistance,
					lpThis->lpLZ77HashTable,
					lpThis->lpSystemAPI
				);
				if(e != minorE_Ok) { return e; }
			}

			if((dwCachedDataLookback >= 1) && (dwCachedDataLookahead >= 1)) {
				bThreeBytes = ((uint32_t)(lpThis->lpRawRingBuffer[(lpThis->dwRawRingTail-1+lpThis->dwRawRingSize) % lpThis->dwRawRingSize])) |
					((uint32_t)(lpThis->lpRawRingBuffer[(lpThis->dwRawRingTail-0+lpThis->dwRawRingSize) % lpThis->dwRawRingSize]) << 8) |
					((uint32_t)(lpThis->lpRawRingBuffer[(lpThis->dwRawRingTail+1+lpThis->dwRawRingSize) % lpThis->dwRawRingSize]) << 16);

				e = minorCompressor_Deflate_Hashtable_Insert(
					bThreeBytes,
					lpThis->dwRawRingHeadAbsolute - dwCachedDataLookahead - 1,
					(lpThis->dwRawRingTail - 1 + lpThis->dwRawRingSize) % lpThis->dwRawRingSize,
					lpThis->dwLZ77LookbackDistance,
					lpThis->lpLZ77HashTable,
					lpThis->lpSystemAPI
				);
				if(e != minorE_Ok) { return e; }
			}

			/*
				Perform a search for any repeating sequence ...
			*/
			e = minorCompressor_Deflate_Hashtable_Search(
				lpThis,
				lpThis->dwRawRingHeadAbsolute - dwCachedDataLookahead,
				lpThis->dwRawRingTail,
				&dwBestMatch_Absolute,
				&dwBestMatch_Distance,
				&dwBestMatch_Length,
				&dwBestMatch_LazyIndex
			);
			if(e != minorE_Ok) { return e; }

			if(dwBestMatch_Length > 2) {
				/*
					Our currently pointed at symbol has been detected as the start
					of a repeated sequence.
				*/
				while(dwBestMatch_LazyIndex > 0) {
					/* We first emit literals for the skipped elements and register them (correctly) */
					e = minorCompressor_Deflate__ProcessLZ77Symbol_Literal(lpThis, (uint16_t)(lpThis->lpRawRingBuffer[lpThis->dwRawRingTail] & 0xFF));
					if(e != minorE_Ok) { return e; }

					/* Register sequence with us as base if possible ... */
					if(dwCachedDataLookahead >= 2) {
						bThreeBytes = ((uint32_t)(lpThis->lpRawRingBuffer[(lpThis->dwRawRingTail+0+lpThis->dwRawRingSize) % lpThis->dwRawRingSize])) |
							((uint32_t)(lpThis->lpRawRingBuffer[(lpThis->dwRawRingTail+1+lpThis->dwRawRingSize) % lpThis->dwRawRingSize]) << 8) |
							((uint32_t)(lpThis->lpRawRingBuffer[(lpThis->dwRawRingTail+2+lpThis->dwRawRingSize) % lpThis->dwRawRingSize]) << 16);

						e = minorCompressor_Deflate_Hashtable_Insert(
							bThreeBytes,
							lpThis->dwRawRingHeadAbsolute - dwCachedDataLookahead,
							lpThis->dwRawRingTail,
							lpThis->dwLZ77LookbackDistance,
							lpThis->lpLZ77HashTable,
							lpThis->lpSystemAPI
						);
						if(e != minorE_Ok) { return e; }
					}
					lpThis->dwRawRingTail = (lpThis->dwRawRingTail + 1) % lpThis->dwRawRingSize;
					dwCachedDataLookback = dwCachedDataLookback + 1;
					dwCachedDataLookahead = dwCachedDataLookahead - 1;
					dwBestMatch_LazyIndex = dwBestMatch_LazyIndex - 1;
				}
				/* Emit Symbol for this match ... */
				e = minorCompressor_Deflate__ProcessLZ77Symbol_Symbol(lpThis, (uint16_t)dwBestMatch_Distance, (uint16_t)dwBestMatch_Length);
				if(e != minorE_Ok) { return e; }

				/* Skip the following repeated sequences and REGISTER the entries (if possible) into the hashtable */
				for(dwSkipIndex = 0; dwSkipIndex < dwBestMatch_Length; dwSkipIndex = dwSkipIndex + 1) {
					if((dwCachedDataLookahead - dwSkipIndex) > 2) {
						bThreeBytes = ((uint32_t)(lpThis->lpRawRingBuffer[(lpThis->dwRawRingTail+0+dwSkipIndex+lpThis->dwRawRingSize) % lpThis->dwRawRingSize])) |
							((uint32_t)(lpThis->lpRawRingBuffer[(lpThis->dwRawRingTail+1+dwSkipIndex+lpThis->dwRawRingSize) % lpThis->dwRawRingSize]) << 8) |
							((uint32_t)(lpThis->lpRawRingBuffer[(lpThis->dwRawRingTail+2+dwSkipIndex+lpThis->dwRawRingSize) % lpThis->dwRawRingSize]) << 16);

						e = minorCompressor_Deflate_Hashtable_Insert(
							bThreeBytes,
							lpThis->dwRawRingHeadAbsolute - dwCachedDataLookahead + dwSkipIndex,
							(lpThis->dwRawRingTail + dwSkipIndex) % lpThis->dwRawRingSize,
							lpThis->dwLZ77LookbackDistance,
							lpThis->lpLZ77HashTable,
							lpThis->lpSystemAPI
						);
						if(e != minorE_Ok) { return e; }
					}
				}
				lpThis->dwRawRingTail = (lpThis->dwRawRingTail + dwBestMatch_Length) % lpThis->dwRawRingSize,
				dwCachedDataLookback = dwCachedDataLookback + dwBestMatch_Length;
				dwCachedDataLookahead = dwCachedDataLookahead - dwBestMatch_Length;
			} else {
				/*
					We have to emit a literal symbol for this tail element
				*/
				#ifdef MINOR_DEFLATE_DEBUG
					printf("%s:%u No match\n", __FILE__, __LINE__);
				#endif
				e = minorCompressor_Deflate__ProcessLZ77Symbol_Literal(lpThis, (uint16_t)(lpThis->lpRawRingBuffer[lpThis->dwRawRingTail]));
				if(e != minorE_Ok) { return e; }

				/* Register sequence with us as base if possible ... */
				if(dwCachedDataLookahead >= 2) {
					bThreeBytes = ((uint32_t)(lpThis->lpRawRingBuffer[(lpThis->dwRawRingTail+0+lpThis->dwRawRingSize) % lpThis->dwRawRingSize])) |
						((uint32_t)(lpThis->lpRawRingBuffer[(lpThis->dwRawRingTail+1+lpThis->dwRawRingSize) % lpThis->dwRawRingSize]) << 8) |
						((uint32_t)(lpThis->lpRawRingBuffer[(lpThis->dwRawRingTail+2+lpThis->dwRawRingSize) % lpThis->dwRawRingSize]) << 16);

					e = minorCompressor_Deflate_Hashtable_Insert(
						bThreeBytes,
						lpThis->dwRawRingHeadAbsolute - dwCachedDataLookahead,
						lpThis->dwRawRingTail,
						lpThis->dwLZ77LookbackDistance,
						lpThis->lpLZ77HashTable,
						lpThis->lpSystemAPI
					);
					if(e != minorE_Ok) { return e; }
				}
				lpThis->dwRawRingTail = (lpThis->dwRawRingTail + 1) % lpThis->dwRawRingSize;
				dwCachedDataLookback = dwCachedDataLookback + 1;
				dwCachedDataLookahead = dwCachedDataLookahead - 1;
			}
		}

		while(dwCachedDataLookahead > 0) {
			/*
				Emit last literal (one or two) bytes - we haven't replaced
				them with an repeated sequence ...
			*/
			e = minorCompressor_Deflate__ProcessLZ77Symbol_Literal(lpThis, (uint16_t)(lpThis->lpRawRingBuffer[lpThis->dwRawRingTail]));
			if(e != minorE_Ok) { return e; }

			/* We don't have to register our symbols any more ... */
			lpThis->dwRawRingTail = (lpThis->dwRawRingTail + 1) % lpThis->dwRawRingSize;
			dwCachedDataLookback = dwCachedDataLookback + 1;
			dwCachedDataLookahead = dwCachedDataLookahead - 1;
		}

		/*
			We've flushed the block into the LZ77 buffer. Append end of block marker.
			We do not have to count the marker inside dwBlockBytesDone because the
			end of block marker is NOT appended to raw data segments.
		*/
		e = minorCompressor_Deflate__ProcessLZ77Symbol_Literal(lpThis, (uint16_t)256);
		if(e != minorE_Ok) { return e; }

		lpThis->stateInfo.flushState = minorCompressor_Deflate_FlushBlock_State__CandidateSelection;
	}

	if(lpThis->stateInfo.flushState == minorCompressor_Deflate_FlushBlock_State__CandidateSelection) {
		/*
			Now - depending on the supported types - check how large various block
			encodings would be
		*/
		dwCandidateSize_Raw = ~0;
		dwCandidateSize_Fixed = ~0;
		dwCandidateSize_Dynamic = ~0;

		if((lpThis->dwFlags & MINOR_COMPRESSOR_DEFLATE_FLAG__CANDIDATE_RAW) != 0) {
			dwCandidateSize_Raw = lpThis->dwBlockBytesDone;
			dwCandidateSize_Raw = dwCandidateSize_Raw + 4; /* LEN and NLEN fields */
		}

		if((lpThis->dwFlags & MINOR_COMPRESSOR_DEFLATE_FLAG__CANDIDATE_DYNAMIC) != 0) {

			/*
				We have to create a huffman dictionary (the best fitting one).
				We'll use the package merge algorithm to check which the best fitting one
				would be.
			*/
			e = minorCompressor_Deflate__PackageMerge_GenerateDictionary(lpThis);
			if(e != minorE_Ok) { return e; }

			/* Calculate the data size we'd see */
			dwCandidateSize_Dynamic = 0;
			for(i = 0; i < minorCompressor_Deflate__ALPHABETSIZE_LITERAL; i=i+1) 	{ dwCandidateSize_Dynamic = dwCandidateSize_Dynamic + lpThis->statLiteralLength.entry[i].dProbability * (lpThis->statLiteralLength.entry[i].huffLength + lpThis->statLiteralLength.entry[i].bAdditionalBits); 									}
			for(i = 0; i < minorCompressor_Deflate__ALPHABETSIZE_DISTANCE; i=i+1) 	{ dwCandidateSize_Dynamic = dwCandidateSize_Dynamic + lpThis->statDistance.entry[i].dProbability * (lpThis->statDistance.entry[i].huffLength + lpThis->statDistance.entry[i].bAdditionalBits); 													}
			for(i = 0; i < minorCompressor_Deflate__ALPHABETSIZE_CODELENGTH; i=i+1) { dwCandidateSize_Dynamic = dwCandidateSize_Dynamic + lpThis->alphabetCodelengthCodes.entry[i].dProbability * (lpThis->alphabetCodelengthCodes.entry[i].huffLength + lpThis->alphabetCodelengthCodes.entry[i].bAdditionalBits); 	}
			dwCandidateSize_Dynamic = dwCandidateSize_Dynamic >> 3; /* Bits -> Bytes */
		}
		if((lpThis->dwFlags & MINOR_COMPRESSOR_DEFLATE_FLAG__CANDIDATE_FIXED) != 0) {
			/*
				We'll calculate the size of the block in case of the predefined
				fixed dictionary.
			*/
			dwCandidateSize_Fixed = 0;
			for(i = 0; i < minorCompressor_Deflate__ALPHABETSIZE_LITERAL; i=i+1) {
				if(lpThis->statLiteralLength.entry[i].dwSymbol <= 143) {
					dwCandidateSize_Fixed = dwCandidateSize_Fixed + lpThis->statLiteralLength.entry[i].dProbability * (8 + lpThis->statLiteralLength.entry[i].bAdditionalBits);
				} else if(lpThis->statLiteralLength.entry[i].dwSymbol <= 255) {
					dwCandidateSize_Fixed = dwCandidateSize_Fixed + lpThis->statLiteralLength.entry[i].dProbability * (9 + lpThis->statLiteralLength.entry[i].bAdditionalBits);
				} else if(lpThis->statLiteralLength.entry[i].dwSymbol <= 279) {
					dwCandidateSize_Fixed = dwCandidateSize_Fixed + lpThis->statLiteralLength.entry[i].dProbability * (7 + lpThis->statLiteralLength.entry[i].bAdditionalBits);
				} else {
					dwCandidateSize_Fixed = dwCandidateSize_Fixed + lpThis->statLiteralLength.entry[i].dProbability * (8 + lpThis->statLiteralLength.entry[i].bAdditionalBits);
				}
			}
			for(i = 0; i < minorCompressor_Deflate__ALPHABETSIZE_DISTANCE; i=i+1) {
				dwCandidateSize_Fixed = dwCandidateSize_Fixed + lpThis->statDistance.entry[i].dProbability * (5 + lpThis->statDistance.entry[i].bAdditionalBits);
			}
			dwCandidateSize_Fixed = dwCandidateSize_Fixed >> 3; /* Bits -> Bytes */
		}

		#ifdef MINOR_DEFLATE_DEBUG
			printf("%s:%u Candidate sizes:\n", __FILE__, __LINE__);
			printf("\tUncompressed:\t%lu\n\tFixed trees:\t%lu (%lf%% compression)\n\tDynamic trees:\t%lu (%lf%% compression)\n", dwCandidateSize_Raw, dwCandidateSize_Fixed, (1.0-((double)dwCandidateSize_Fixed)/((double)dwCandidateSize_Raw))*100, dwCandidateSize_Dynamic, (1.0-((double)dwCandidateSize_Dynamic)/((double)dwCandidateSize_Raw))*100);
		#endif

		/*
			Decide which of the supported encoding formats is the
			"minimal length" one and emit a block with appropriate encoding.
		*/
		if(((dwCandidateSize_Raw <= dwCandidateSize_Fixed) && (dwCandidateSize_Raw <= dwCandidateSize_Dynamic)) && ((lpThis->dwFlags & MINOR_COMPRESSOR_DEFLATE_FLAG__CANDIDATE_RAW) != 0)) {
			/* Selected raw data output ... */
			#ifdef MINOR_DEFLATE_DEBUG
				printf("%s:%u Selected raw tree format\n", __FILE__, __LINE__);
			#endif
			lpThis->stateInfo.flushState = minorCompressor_Deflate_FlushBlock_State__RawSelectedHeader;
			lpThis->stateInfo.dwCurrentBit = 0;
		} else if(((dwCandidateSize_Dynamic <= dwCandidateSize_Fixed) && (dwCandidateSize_Dynamic <= dwCandidateSize_Raw)) && ((lpThis->dwFlags & MINOR_COMPRESSOR_DEFLATE_FLAG__CANDIDATE_DYNAMIC) != 0)) {
			/* Selected dynamic tree format */
			#ifdef MINOR_DEFLATE_DEBUG
				printf("%s:%u Selected dynamic tree format\n", __FILE__, __LINE__);
			#endif
			lpThis->stateInfo.flushState = minorCompressor_Deflate_FlushBlock_State__DynamicSelectedHeader;
			lpThis->stateInfo.dwCurrentBit = 0;
		} else if(((dwCandidateSize_Fixed <= dwCandidateSize_Dynamic) && (dwCandidateSize_Fixed <= dwCandidateSize_Raw)) && ((lpThis->dwFlags & MINOR_COMPRESSOR_DEFLATE_FLAG__CANDIDATE_FIXED) != 0)) {
			/* Selected fixed tree format */
			#ifdef MINOR_DEFLATE_DEBUG
				printf("%s:%u Selected fixed tree format\n", __FILE__, __LINE__);
			#endif
			lpThis->stateInfo.flushState = minorCompressor_Deflate_FlushBlock_State__FixedSelectedHeader;
			lpThis->stateInfo.dwCurrentBit = 0;
		} else {
			/* Should never happen because that means we do not have any supported block format */
			#ifdef MINOR_DEFLATE_DEBUG
				printf("%s:%u [IMPLEMENTATION ERROR] Undecided which block format to use\n", __FILE__, __LINE__);
			#endif
			lpThis->currentState = minorCompressor_Deflate_State__Error;
			return minorE_ImplementationError;
		}
	}

	/*
		Emit block header
	*/
	if(
		(
			(lpThis->stateInfo.flushState == minorCompressor_Deflate_FlushBlock_State__RawSelectedHeader) ||
			(lpThis->stateInfo.flushState == minorCompressor_Deflate_FlushBlock_State__DynamicSelectedHeader) ||
			(lpThis->stateInfo.flushState == minorCompressor_Deflate_FlushBlock_State__FixedSelectedHeader)
		) && (lpThis->stateInfo.dwCurrentBit == 0)
	) {
		/* The first bit in the block header indicates if we are the LAST block */
		if(lpThis->currentState == minorCompressor_Deflate_State__FlushBlock_2) {
			/* Last block */
			e = minorCompressor_Deflate__FlushBlock_EmitBit(lpThis, 1);
			#ifdef MINOR_DEFLATE_DEBUG
				printf("%s:%u Emitting end of block header", __FILE__, __LINE__);
			#endif
		} else {
			/* Not last block */
			e = minorCompressor_Deflate__FlushBlock_EmitBit(lpThis, 0);
			#ifdef MINOR_DEFLATE_DEBUG
				printf("%s:%u Emitting normal block header (not end of block)", __FILE__, __LINE__);
			#endif
		}
		if(e != minorE_Ok) { return e; }
		lpThis->stateInfo.dwCurrentBit = lpThis->stateInfo.dwCurrentBit + 1;
	}

	if(
		(
			(
				(lpThis->stateInfo.flushState == minorCompressor_Deflate_FlushBlock_State__RawSelectedHeader) ||
				(lpThis->stateInfo.flushState == minorCompressor_Deflate_FlushBlock_State__FixedSelectedHeader)
			) && (lpThis->stateInfo.dwCurrentBit < 3)
		) || (
			(lpThis->stateInfo.flushState == minorCompressor_Deflate_FlushBlock_State__DynamicSelectedHeader) &&
			(lpThis->stateInfo.dwCurrentBit < 18)
		)
	) {
		switch(lpThis->stateInfo.flushState) {
			case minorCompressor_Deflate_FlushBlock_State__RawSelectedHeader:
				if(lpThis->stateInfo.dwCurrentBit == 1) {
					if((e = minorCompressor_Deflate__FlushBlock_EmitBit(lpThis, 0)) != minorE_Ok) { return e; }
					lpThis->stateInfo.dwCurrentBit = lpThis->stateInfo.dwCurrentBit + 1;
				}
				/* Else: 2nd bit follows */
				if((e = minorCompressor_Deflate__FlushBlock_EmitBit(lpThis, 0)) != minorE_Ok) { return e; }
				lpThis->stateInfo.dwCurrentBit = lpThis->stateInfo.dwCurrentBit + 1;

				lpThis->stateInfo.flushState = minorCompressor_Deflate_FlushBlock_State__RawLENNLEN;
				lpThis->stateInfo.dwCurrentBit = 0;
				lpThis->stateInfo.dwCurrentByte = 0;
				#ifdef MINOR_DEFLATE_DEBUG
					printf("%s:%u Written raw block header\n", __FILE__, __LINE__);
				#endif

				break;

			case minorCompressor_Deflate_FlushBlock_State__DynamicSelectedHeader:
				/* Emit whole header (bitwise) */
				if(lpThis->stateInfo.dwCurrentBit == 1) {
					if((e = minorCompressor_Deflate__FlushBlock_EmitBit(lpThis, 0)) != minorE_Ok) { return e; }
					lpThis->stateInfo.dwCurrentBit = lpThis->stateInfo.dwCurrentBit + 1;
				}

				/* Calculate HCLEN, HLIT and HDIST */
				hclen = minorCompressor_Deflate__ALPHABETSIZE_CODELENGTH;
				while((lpThis->alphabetCodelengthCodes.entry[minorCompressor_Deflate__FlushBlock__CodelengthAlphabetOrder[hclen-1]].huffLength == 0) && (hclen > 4)) { hclen = hclen - 1; }
				hlit = minorCompressor_Deflate__ALPHABETSIZE_LITERAL;
				while((lpThis->statLiteralLength.entry[hlit-1].huffLength == 0) && (hlit > 257)) { hlit = hlit - 1; }
				hdist = minorCompressor_Deflate__ALPHABETSIZE_DISTANCE;
				while((lpThis->statDistance.entry[hdist-1].huffLength == 0) && (hdist > 1)) { hdist = hdist - 1; }

				if(lpThis->stateInfo.dwCurrentBit == 2) {
					if((e = minorCompressor_Deflate__FlushBlock_EmitBit(lpThis, 1)) != minorE_Ok) { return e; }
					lpThis->stateInfo.dwCurrentBit = lpThis->stateInfo.dwCurrentBit + 1;

					lpThis->stateInfo.lastByte = hlit - 257;
				}

				while(lpThis->stateInfo.dwCurrentBit < 8) { /* Hlit has bit indices 3-7 */
					if((e = minorCompressor_Deflate__FlushBlock_EmitBit(lpThis, lpThis->stateInfo.lastByte & 0x01)) != minorE_Ok) { return e;  }
					lpThis->stateInfo.dwCurrentBit = lpThis->stateInfo.dwCurrentBit + 1;
					lpThis->stateInfo.lastByte = lpThis->stateInfo.lastByte >> 1;

					if(lpThis->stateInfo.dwCurrentBit == 8) {
						lpThis->stateInfo.lastByte = hdist - 1;
					}
				}

				/* HDist has bit indices 8-12 */
				while(lpThis->stateInfo.dwCurrentBit < 13) {
					if((e = minorCompressor_Deflate__FlushBlock_EmitBit(lpThis, lpThis->stateInfo.lastByte & 0x01)) != minorE_Ok) { return e;  }
					lpThis->stateInfo.dwCurrentBit = lpThis->stateInfo.dwCurrentBit + 1;
					lpThis->stateInfo.lastByte = lpThis->stateInfo.lastByte >> 1;

					if(lpThis->stateInfo.dwCurrentBit == 13) {
						lpThis->stateInfo.lastByte = hclen - 4;
					}
				}

				/*HCLen has bit inidces 13-17 */
				while(lpThis->stateInfo.dwCurrentBit < 17) {
					if((e = minorCompressor_Deflate__FlushBlock_EmitBit(lpThis, lpThis->stateInfo.lastByte & 0x01)) != minorE_Ok) { return e;  }
					lpThis->stateInfo.dwCurrentBit = lpThis->stateInfo.dwCurrentBit + 1;
					lpThis->stateInfo.lastByte = lpThis->stateInfo.lastByte >> 1;

					if(lpThis->stateInfo.dwCurrentBit == 17) {
						lpThis->stateInfo.flushState = minorCompressor_Deflate_FlushBlock_State__DynamicCodeLengthLens;
						lpThis->stateInfo.dwCurrentBit = 0;
						lpThis->stateInfo.dwCurrentByte = 0;
						break;
					}
				}
				break;

			case minorCompressor_Deflate_FlushBlock_State__FixedSelectedHeader:

				if(lpThis->stateInfo.dwCurrentBit == 1) {
					if((e = minorCompressor_Deflate__FlushBlock_EmitBit(lpThis, 1)) != minorE_Ok) { return e; }
					lpThis->stateInfo.dwCurrentBit = lpThis->stateInfo.dwCurrentBit + 1;
				}
				/* Else: 2nd bit follows */
				if((e = minorCompressor_Deflate__FlushBlock_EmitBit(lpThis, 0)) != minorE_Ok) { return e; }
				lpThis->stateInfo.dwCurrentBit = lpThis->stateInfo.dwCurrentBit + 1;

				/* We have emitted the header, now initialize the huffman trees */
				codeCurrent = 0;
				/* Start with 7 bit codes */
				for(i = 256; i <= 279; i=i+1) {
					lpThis->statLiteralLength.entry[i].dwSymbol = i;
					lpThis->statLiteralLength.entry[i].huffCode = codeCurrent;
					lpThis->statLiteralLength.entry[i].huffMask = 0x7F;
					lpThis->statLiteralLength.entry[i].huffLength = 7;
					if(i < 265) {
						lpThis->statLiteralLength.entry[i].bAdditionalBits = 0;
					} else if((i >= 265) && (i < 269)) {
						lpThis->statLiteralLength.entry[i].bAdditionalBits = 1;
					} else if((i >= 269) && (i < 273)) {
						lpThis->statLiteralLength.entry[i].bAdditionalBits = 2;
					} else if((i >= 273) && (i < 277)) {
						lpThis->statLiteralLength.entry[i].bAdditionalBits = 3;
					} else if((i >= 277) && (i < 281)) {
						lpThis->statLiteralLength.entry[i].bAdditionalBits = 4;
					}
					codeCurrent = codeCurrent + 1;
				}
				codeCurrent = codeCurrent << 1;
				for(i = 0; i <= 143; i=i+1) {
					lpThis->statLiteralLength.entry[i].dwSymbol = i;
					lpThis->statLiteralLength.entry[i].huffCode = codeCurrent;
					lpThis->statLiteralLength.entry[i].huffMask = 0xFF;
					lpThis->statLiteralLength.entry[i].huffLength = 8;
					codeCurrent = codeCurrent + 1;
				}
				for(i = 280; i <= 287; i=i+1) {
					if(i < minorCompressor_Deflate__ALPHABETSIZE_LITERAL) {
						lpThis->statLiteralLength.entry[i].dwSymbol = i;
						lpThis->statLiteralLength.entry[i].huffCode = codeCurrent;
						lpThis->statLiteralLength.entry[i].huffMask = 0xFF;
						lpThis->statLiteralLength.entry[i].huffLength = 8;
						if(i == 280) {
							lpThis->statLiteralLength.entry[i].bAdditionalBits = 4;
						} else if(i <= 284) {
							lpThis->statLiteralLength.entry[i].bAdditionalBits = 5;
						} else {
							lpThis->statLiteralLength.entry[i].bAdditionalBits = 0;
						}
					}
					codeCurrent = codeCurrent + 1;
				}
				codeCurrent = codeCurrent << 1;
				for(i = 144; i <= 255; i=i+1) {
					lpThis->statLiteralLength.entry[i].dwSymbol = i;
					lpThis->statLiteralLength.entry[i].huffCode = codeCurrent;
					lpThis->statLiteralLength.entry[i].huffMask = 0x1FF;
					lpThis->statLiteralLength.entry[i].huffLength = 9;
					codeCurrent = codeCurrent + 1;
					lpThis->statLiteralLength.entry[i].bAdditionalBits = 0;
				}

				codeCurrent = 0;
				for(i = 0; i < minorCompressor_Deflate__ALPHABETSIZE_DISTANCE; i=i+1) {
					lpThis->statDistance.entry[i].dwSymbol = i;
					lpThis->statDistance.entry[i].huffCode = codeCurrent;
					lpThis->statDistance.entry[i].huffMask = 0x1F;
					lpThis->statDistance.entry[i].huffLength = 5;
					codeCurrent = codeCurrent + 1;
					if(i < 4) {
						lpThis->statLiteralLength.entry[i].bAdditionalBits = 0;
					} else if(i < 6) {
						lpThis->statLiteralLength.entry[i].bAdditionalBits = 1;
					} else if(i < 8) {
						lpThis->statLiteralLength.entry[i].bAdditionalBits = 2;
					} else if(i < 10) {
						lpThis->statLiteralLength.entry[i].bAdditionalBits = 3;
					} else if(i < 12) {
						lpThis->statLiteralLength.entry[i].bAdditionalBits = 4;
					} else if(i < 14) {
						lpThis->statLiteralLength.entry[i].bAdditionalBits = 5;
					} else if(i < 16) {
						lpThis->statLiteralLength.entry[i].bAdditionalBits = 6;
					} else if(i < 18) {
						lpThis->statLiteralLength.entry[i].bAdditionalBits = 7;
					} else if(i < 20) {
						lpThis->statLiteralLength.entry[i].bAdditionalBits = 8;
					} else if(i < 22) {
						lpThis->statLiteralLength.entry[i].bAdditionalBits = 9;
					} else if(i < 24) {
						lpThis->statLiteralLength.entry[i].bAdditionalBits = 10;
					} else if(i < 26) {
						lpThis->statLiteralLength.entry[i].bAdditionalBits = 11;
					} else if(i < 28) {
						lpThis->statLiteralLength.entry[i].bAdditionalBits = 12;
					} else {
						lpThis->statLiteralLength.entry[i].bAdditionalBits = 13;
					}
				}

				/*
				lpThis->stateInfo.flushState = minorCompressor_Deflate_FlushBlock_State__FixedEmitData;
				lpThis->stateInfo.dwCurrentBit = 0;
				lpThis->stateInfo.dwCurrentByte = 0;
				*/

				/* We use the same data emitting logic as dynamic data */
				lpThis->stateInfo.dwCurrentByte = 0;
				lpThis->stateInfo.dwCurrentBit = 0;
				lpThis->stateInfo.flushState = minorCompressor_Deflate_FlushBlock_State__DynamicData;

				#ifdef MINOR_DEFLATE_DEBUG
					/* DEBUG: Output */
					uint32_t code;
					printf("%s:%u\n", __FILE__, __LINE__);
					printf("Dumping generated literal/length codetables:\n");
					printf("Symbol\tLength\tCode\n");
					for(int j = 0; j < (sizeof(lpThis->statLiteralLength.entry)/sizeof(struct minorHuffutilAlphabetEntry_U32)); j=j+1) {
						printf("%3u\t%u\t", lpThis->statLiteralLength.entry[j].dwSymbol, lpThis->statLiteralLength.entry[j].huffLength);
						if(lpThis->statLiteralLength.entry[j].huffLength > 0) {
							code = lpThis->statLiteralLength.entry[j].huffCode << (32 - lpThis->statLiteralLength.entry[j].huffLength);
							for(i = 0; i < lpThis->statLiteralLength.entry[j].huffLength; i=i+1) {
								printf("%c", ((code & 0x80000000) != 0) ? '1' : '0');
								code = code << 1;
							}
						}
						printf("\n");
					}
					printf("\n\n");

					printf("%s:%u\n", __FILE__, __LINE__);
					printf("Dumping generated distance codetables:\n");
					printf("Symbol\tLength\tCode\n");
					for(int j = 0; j < (sizeof(lpThis->statDistance.entry)/sizeof(struct minorHuffutilAlphabetEntry_U32)); j=j+1) {
						printf("%3u\t%u\t", lpThis->statDistance.entry[j].dwSymbol, lpThis->statDistance.entry[j].huffLength);
						if(lpThis->statDistance.entry[j].huffLength > 0) {
							code = lpThis->statDistance.entry[j].huffCode << (32 - lpThis->statDistance.entry[j].huffLength);
							for(i = 0; i < lpThis->statDistance.entry[j].huffLength; i=i+1) {
								printf("%c", ((code & 0x80000000) != 0) ? '1' : '0');
								code = code << 1;
							}
						}
						printf("\n");
					}
					printf("\n\n");
				#endif

				break;
			default:
				return minorE_ImplementationError; /* Impossible due to outer if */
		}
	}

	/*
		minorCompressor_Deflate_FlushBlock_State__RawLENNLEN
		minorCompressor_Deflate_FlushBlock_State__DynamicCodeLengthLens
		minorCompressor_Deflate_FlushBlock_State__FixedEmitData
	*/

	if(lpThis->stateInfo.flushState == minorCompressor_Deflate_FlushBlock_State__RawLENNLEN) {
		/* Do byte alignment. This is idempotent ... */
		if((e = minorCompressor_Deflate__FlushBlock_EmitBitFlush(lpThis)) != minorE_Ok) { return e; }

		/*
			Raw data blocks can only be selected for maximum of 64 KBytes
		*/
		if(lpThis->dwBlockBytesDone > 0xFFFF) {
			#ifdef MINOR_DEFLATE_DEBUG
				printf("%s:%u [IMPLEMENTATION ERROR] Block too large for raw data storage\n", __FILE__, __LINE__);
			#endif
			return minorE_ImplementationError;
		}

		#ifdef MINOR_DEFLATE_DEBUG
			printf("%s:%u Writing block length and negated block length for %lu bytes\n", __FILE__, __LINE__, lpThis->dwBlockBytesDone);
		#endif
		if(lpThis->stateInfo.dwCurrentByte == 0) {
			bTemp = (uint8_t)((lpThis->dwBlockBytesDone % 256) & 0xFF);
			#ifdef MINOR_DEFLATE_DEBUG
				printf("%s:%u %02x ", __FILE__, __LINE__, bTemp);
			#endif
			if((e = lpThis->lpDataSink->write(lpThis->lpDataSink, &bTemp, 1, NULL)) != minorE_Ok) { return e; }
			lpThis->stateInfo.dwCurrentByte = lpThis->stateInfo.dwCurrentByte + 1;
		}
		if(lpThis->stateInfo.dwCurrentByte == 1) {
			bTemp = (uint8_t)((lpThis->dwBlockBytesDone / 256) & 0xFF);
			#ifdef MINOR_DEFLATE_DEBUG
				printf("%02x ", bTemp);
			#endif
			if((e = lpThis->lpDataSink->write(lpThis->lpDataSink, &bTemp, 1, NULL)) != minorE_Ok) { return e; }
			lpThis->stateInfo.dwCurrentByte = lpThis->stateInfo.dwCurrentByte + 1;
		}
		if(lpThis->stateInfo.dwCurrentByte == 2) {
			bTemp = ~(uint8_t)((lpThis->dwBlockBytesDone % 256) & 0xFF);
			#ifdef MINOR_DEFLATE_DEBUG
				printf("%02x ", bTemp);
			#endif
			if((e = lpThis->lpDataSink->write(lpThis->lpDataSink, &bTemp, 1, NULL)) != minorE_Ok) { return e; }
			lpThis->stateInfo.dwCurrentByte = lpThis->stateInfo.dwCurrentByte + 1;
		}
		if(lpThis->stateInfo.dwCurrentByte == 3) {
			bTemp = ~(uint8_t)((lpThis->dwBlockBytesDone / 256) & 0xFF);
			#ifdef MINOR_DEFLATE_DEBUG
				printf("%02x ", bTemp);
			#endif
			if((e = lpThis->lpDataSink->write(lpThis->lpDataSink, &bTemp, 1, NULL)) != minorE_Ok) { return e; }
			lpThis->stateInfo.dwCurrentByte = 0;
			lpThis->stateInfo.flushState = minorCompressor_Deflate_FlushBlock_State__RawBytes;
		}
	}
	if(lpThis->stateInfo.flushState == minorCompressor_Deflate_FlushBlock_State__RawBytes) {
		/*
			Flusing raw data. This means we have
				lpThis->dwBlockBytesDone bytes that we want to write.
				lpThis->dwRawRingHead = lpThis->dwRawRingTail; and points to the NEXT byte after the raw data

				We want to flush dwBlockBytesDone bytes in front of dwRawRingTail. The buffer may be wrapping
				around the circular buffer size. It starts at
					(lpThis->dwRawRingTail - lpThis->dwBlockBytesDone) % lpThis->dwRawRingSize
				and it ends at
					(lpThis->dwRawRingTail - 1)

				When dwBlockBytesDone < dwRawRingTail -> everything can be flushed in a single write
				starting from dwRawRingTail - dwBlockBytesDone having a length of dwBlockBytesDone.

				In case dwBlockBytesDone > dwRawRingTail the first segment is
					dwRawRingSize - (dwBlockBytesDone - dwRawRingTail)  with length (dwBlockBytesDon - dwRawRingTail)
				and the second segment is
					0  with length  dwRawRingTail

					First part:			[ lpThis->dwRawRingSize - (lpThis->dwBlockBytesDone - lpThis->dwRawRingTail) ; length = (lpThis->dwBlockBytesDone - lpThis->dwRawRingTail) ]
					Second part:		[ 0 ; length = lpThis->dwRawRingTail ]

				lpThis->stateInfo.dwCurrentByte contains always the already processed bytes (in case there
				have been partial writes)
		 */
		 #ifdef MINOR_DEFLATE_DEBUG
			 printf("%s:%u Writing raw blocks ...\n", __FILE__, __LINE__);
		 #endif
		 while(lpThis->stateInfo.dwCurrentByte < lpThis->dwBlockBytesDone) {
			 #ifdef MINOR_DEFLATE_DEBUG
 				printf("\t\t\t%lu of %lu\n", lpThis->stateInfo.dwCurrentByte, lpThis->dwBlockBytesDone);
 			#endif
			 /* We flush bytes as long as we have some in our buffer OR we are interrupted (update stateblock continuously) */
			 if(lpThis->dwBlockBytesDone <= lpThis->dwRawRingTail) {
				 /* Non wrap around situation */
				 e = lpThis->lpDataSink->write(lpThis->lpDataSink, &(lpThis->lpRawRingBuffer[lpThis->dwRawRingTail - lpThis->dwBlockBytesDone + lpThis->stateInfo.dwCurrentByte]), lpThis->dwBlockBytesDone - lpThis->stateInfo.dwCurrentByte, &dwBytesDone);
				 if(e != minorE_Ok) {
					 return e; /* Pass error to the next layer. We are restartable */
				 }
				 lpThis->stateInfo.dwCurrentByte = lpThis->stateInfo.dwCurrentByte + dwBytesDone;
			 } else {
				 /* Wrap around. Determine if we require bytes from the first segment */
				 if(lpThis->stateInfo.dwCurrentByte < (lpThis->dwBlockBytesDone - lpThis->dwRawRingTail)) {
					 e = lpThis->lpDataSink->write(lpThis->lpDataSink, &(lpThis->lpRawRingBuffer[lpThis->dwRawRingSize - (lpThis->dwBlockBytesDone - lpThis->dwRawRingTail) + lpThis->stateInfo.dwCurrentByte]), (lpThis->dwBlockBytesDone - lpThis->dwRawRingTail)-lpThis->stateInfo.dwCurrentByte, &dwBytesDone);
				 } else {
					 e = lpThis->lpDataSink->write(lpThis->lpDataSink, &(lpThis->lpRawRingBuffer[(lpThis->dwRawRingSize - (lpThis->dwBlockBytesDone - lpThis->dwRawRingTail) + lpThis->stateInfo.dwCurrentByte) % lpThis->dwRawRingSize]), (lpThis->dwBlockBytesDone - lpThis->stateInfo.dwCurrentByte), &dwBytesDone);
				 }
				 if(e != minorE_Ok) { return e; }
				 lpThis->stateInfo.dwCurrentByte = lpThis->stateInfo.dwCurrentByte + dwBytesDone;

			 }
		 }

		 /*
		 	Finished block flushing
		*/
		lpThis->dwOutputBufferUsed = 0;
		lpThis->dwBlockBytesDone = 0;
		lpThis->dwSymbolBufferUsed = 0;

		for(i = 0; i < (sizeof(lpThis->statLiteralLength.entry)/sizeof(struct minorHuffutilAlphabetEntry_U32)); i=i+1) {
			lpThis->statLiteralLength.entry[i].dProbability = 0;
			lpThis->statLiteralLength.entry[i].huffCode = 0;
			lpThis->statLiteralLength.entry[i].huffMask = 0;
			lpThis->statLiteralLength.entry[i].huffLength = 0;
		}
		for(i = 0; i < (sizeof(lpThis->statDistance.entry)/sizeof(struct minorHuffutilAlphabetEntry_U32)); i=i+1) {
			lpThis->statDistance.entry[i].dProbability = 0;
			lpThis->statDistance.entry[i].huffCode = 0;
			lpThis->statDistance.entry[i].huffMask = 0;
			lpThis->statDistance.entry[i].huffLength = 0;
		}

		switch(lpThis->currentState) {
			case minorCompressor_Deflate_State__FlushBlock_1:	/* This was a flush that was triggered by an full symbol or raw data buffer */
			case minorCompressor_Deflate_State__FlushBlock_3:	/* The application had requested an flush operation. Return to normal accepting data state */
				lpThis->currentState = minorCompressor_Deflate_State__AcceptingData;
				break;
			case minorCompressor_Deflate_State__FlushBlock_2:	/* This has been the last block of data */
				lpThis->currentState = minorCompressor_Deflate_State__Finished;
				break;
			default: /* Clear error indication (implementation error) */
				lpThis->currentState = minorCompressor_Deflate_State__Error;
				return minorE_ImplementationError;
		}
		return minorE_Ok;
	}

	if(lpThis->stateInfo.flushState == minorCompressor_Deflate_FlushBlock_State__DynamicCodeLengthLens) {
		/* (re)calculate HCLEN, HLIT and HDIST - in case we have been restarted. */
		if(hclen == 0) {
			hclen = minorCompressor_Deflate__ALPHABETSIZE_CODELENGTH;
			while((lpThis->alphabetCodelengthCodes.entry[minorCompressor_Deflate__FlushBlock__CodelengthAlphabetOrder[hclen-1]].huffLength == 0) && (hclen > 4)) { hclen = hclen - 1; }
		}

		/*
			Emit lengths for the code length alphabet if we haven't finished

			lpThis->stateInfo.dwCurrentByte tells us which code we are currently emitting
			lpThis->stateInfo.dwCurrentBit tells us at which bit position we are currently stopped
		*/

		while(lpThis->stateInfo.dwCurrentByte < hclen) {
			/* Load the next length into lpThis->stateBlock.lastByte and reset current bit index */
			if(lpThis->stateInfo.dwCurrentBit == 0) {
				/* Load next symbol length */
				lpThis->stateInfo.lastByte = lpThis->alphabetCodelengthCodes.entry[minorCompressor_Deflate__FlushBlock__CodelengthAlphabetOrder[lpThis->stateInfo.dwCurrentByte]].huffLength;
			}
			while(lpThis->stateInfo.dwCurrentBit < 3) {
				if((e = minorCompressor_Deflate__FlushBlock_EmitBit(lpThis, (lpThis->stateInfo.lastByte & 0x01))) != minorE_Ok) { return e; }
				lpThis->stateInfo.lastByte = lpThis->stateInfo.lastByte >> 1;
				lpThis->stateInfo.dwCurrentBit = lpThis->stateInfo.dwCurrentBit + 1;
			}

			lpThis->stateInfo.dwCurrentByte = lpThis->stateInfo.dwCurrentByte + 1;
			lpThis->stateInfo.dwCurrentBit = 0;
		}

		/* All lengths have been emitted (in correct order) */
		lpThis->stateInfo.dwCurrentByte = 0;
		lpThis->stateInfo.dwCurrentBit = 0;
		lpThis->stateInfo.flushState = minorCompressor_Deflate_FlushBlock_State__DynamicCodeLengths;
	}
	if(lpThis->stateInfo.flushState == minorCompressor_Deflate_FlushBlock_State__DynamicCodeLengths) {
		while((lpThis->stateInfo.dwCurrentByte < lpThis->codelengthcodeTable.dwUsedSymbols) || (lpThis->stateInfo.dwCurrentBit != 0)) {
			/*
				We have to encode the length of the dwCurrentByte HLIT or HDIST
				symbol into the output stream. We use the alphabetCodelengthCodes
				alphabet to encode them.

				The lengths of the codes have already been collected in codelengthcodeTable
				whose frequency analysis has been used for alphabet creation.

				Note that codelengthcodeTable is a series of codes (0-15) without
				additional bits OR codes 16 (2 additional bits), 17 (3 additional bits)
				or 18 (7 additional bits) which are encoded in the following byte.

				lpThis->stateInfo.dwCurrentBit != 0 is required to finalize output of the last
				symbol.
			*/
			if(lpThis->stateInfo.dwCurrentBit == 0) {
				/*
					We haven't emitted any bit from the current symbol so we fetch all
					information into our state block
				*/
				lpThis->stateInfo.nextSymbolBits 		= lpThis->alphabetCodelengthCodes.entry[lpThis->codelengthcodeTable.symbols[lpThis->stateInfo.dwCurrentByte]].huffCode;
				lpThis->stateInfo.dwSymbolBitCount 		= lpThis->alphabetCodelengthCodes.entry[lpThis->codelengthcodeTable.symbols[lpThis->stateInfo.dwCurrentByte]].huffLength;
				lpThis->stateInfo.dwAdditionalBitCount	= lpThis->alphabetCodelengthCodes.entry[lpThis->codelengthcodeTable.symbols[lpThis->stateInfo.dwCurrentByte]].bAdditionalBits;
				/*
					We output huffman codes in reverse order
				*/
				lpThis->stateInfo.nextSymbolBits = lpThis->stateInfo.nextSymbolBits << (32-lpThis->stateInfo.dwSymbolBitCount);

				/*
					If we have an additional byte we have to fetch additional bits too
				*/
				if(lpThis->codelengthcodeTable.symbols[lpThis->stateInfo.dwCurrentByte] >= 16) {
					lpThis->stateInfo.nextAdditionalBits = lpThis->codelengthcodeTable.symbols[lpThis->stateInfo.dwCurrentByte+1];
					lpThis->stateInfo.dwCurrentByte = lpThis->stateInfo.dwCurrentByte + 2;
				} else {
					lpThis->stateInfo.dwCurrentByte = lpThis->stateInfo.dwCurrentByte + 1;
				}

				/* We use the bit index 0 for our load state ... */
				lpThis->stateInfo.dwCurrentBit = 1;
			}

			/* Now we have to emit bits for the symbol itself */
			while(lpThis->stateInfo.dwCurrentBit < lpThis->stateInfo.dwSymbolBitCount+1) {
				if((e = minorCompressor_Deflate__FlushBlock_EmitBit(lpThis, ((lpThis->stateInfo.nextSymbolBits & 0x80000000) == 0) ? 0 : 1)) != minorE_Ok) { return e; }
				lpThis->stateInfo.nextSymbolBits = lpThis->stateInfo.nextSymbolBits << 1;
				lpThis->stateInfo.dwCurrentBit = lpThis->stateInfo.dwCurrentBit + 1;
			}
			while(lpThis->stateInfo.dwCurrentBit < lpThis->stateInfo.dwSymbolBitCount+1+lpThis->stateInfo.dwAdditionalBitCount) {
				if((e = minorCompressor_Deflate__FlushBlock_EmitBit(lpThis, (lpThis->stateInfo.nextAdditionalBits & 0x01))) != minorE_Ok) { return e; }
				lpThis->stateInfo.nextAdditionalBits = lpThis->stateInfo.nextAdditionalBits >> 1;
				lpThis->stateInfo.dwCurrentBit = lpThis->stateInfo.dwCurrentBit + 1;
			}

			/* We have emitted this ... */
			lpThis->stateInfo.dwCurrentBit = 0;
		}
		/* Codelengths have been written ... */
		lpThis->stateInfo.dwCurrentByte = 0;
		lpThis->stateInfo.dwCurrentBit = 0;
		lpThis->stateInfo.flushState = minorCompressor_Deflate_FlushBlock_State__DynamicData;
	}
	/* Dynamic data output */
	if(lpThis->stateInfo.flushState == minorCompressor_Deflate_FlushBlock_State__DynamicData) {
		/* Iterate over all symbols ... */
		while((lpThis->stateInfo.dwCurrentByte < lpThis->dwSymbolBufferUsed) || (lpThis->stateInfo.dwCurrentBit != 0)) {
			if(lpThis->stateInfo.dwCurrentBit == 0) {
				/*
					Encoding inside LZ77 codeword buffer:
						0-255		Literally stored as value
						256			End of block, literally stored
						257-285		Length; 9 Bits are used as "literal" length, the remaining bits are "extra" bits
				*/
				if(lpThis->lpSymbolBuffer[lpThis->stateInfo.dwCurrentByte] < 257) {
					/* Directly encode with literal/length alphabet */
					lpThis->stateInfo.nextSymbolBits = lpThis->statLiteralLength.entry[lpThis->lpSymbolBuffer[lpThis->stateInfo.dwCurrentByte]].huffCode;
					lpThis->stateInfo.dwSymbolBitCount = lpThis->statLiteralLength.entry[lpThis->lpSymbolBuffer[lpThis->stateInfo.dwCurrentByte]].huffLength;
					lpThis->stateInfo.dwAdditionalBitCount = 0;
					lpThis->stateInfo.nextAdditionalBits = 0;

					lpThis->stateInfo.dwDistanceSymbolBitCount = 0;
					lpThis->stateInfo.dwAdditionalDistanceBitCount = 0;

					lpThis->stateInfo.dwCurrentByte = lpThis->stateInfo.dwCurrentByte + 1;
					lpThis->stateInfo.dwCurrentBit = 1;

					/*
						Huffman codes are always encoded in inverse order
					*/
					lpThis->stateInfo.nextSymbolBits = lpThis->stateInfo.nextSymbolBits << (32-lpThis->stateInfo.dwSymbolBitCount);
				} else {
					/*TODO: Add assertion that we really have two symbols available */

					/* Extract additional bits AND append distance code */
					lpThis->stateInfo.nextSymbolBits = lpThis->statLiteralLength.entry[lpThis->lpSymbolBuffer[lpThis->stateInfo.dwCurrentByte] & 0x1FF].huffCode;
					lpThis->stateInfo.dwSymbolBitCount = lpThis->statLiteralLength.entry[lpThis->lpSymbolBuffer[lpThis->stateInfo.dwCurrentByte] & 0x1FF].huffLength;
					lpThis->stateInfo.dwAdditionalBitCount = lpThis->statLiteralLength.entry[lpThis->lpSymbolBuffer[lpThis->stateInfo.dwCurrentByte] & 0x1FF].bAdditionalBits;
					lpThis->stateInfo.nextAdditionalBits = lpThis->lpSymbolBuffer[lpThis->stateInfo.dwCurrentByte] >> 9;

					/*
						Because distance codes are literally (as distance) included in the symbol
						buffer we have to re-determine which symbol will be used
					*/
					dwTempDistance = lpThis->lpSymbolBuffer[lpThis->stateInfo.dwCurrentByte+1];
					for(i = 0; i < minorDeflate__LUT__DistanceCodes__CODECOUNT-1; i=i+1) {
						if(dwTempDistance < minorDeflate__LUT__DistanceCodes[i+1][1]) {
							/* Found entry i */
							lpThis->stateInfo.dwDistanceSymbolBitCount = lpThis->statDistance.entry[i].huffLength;
							lpThis->stateInfo.nextDistanceSymbolBits = lpThis->statDistance.entry[i].huffCode;
							lpThis->stateInfo.dwAdditionalDistanceBitCount = lpThis->statDistance.entry[i].bAdditionalBits;
							lpThis->stateInfo.nextAdditionalDistanceBits = (dwTempDistance - minorDeflate__LUT__DistanceCodes[i][1]);
							break;
						}
					}
					if(i == minorDeflate__LUT__DistanceCodes__CODECOUNT-1) {
						/* We use the last entry ... */
						lpThis->stateInfo.dwDistanceSymbolBitCount = lpThis->statDistance.entry[i].huffLength;
						lpThis->stateInfo.nextDistanceSymbolBits = lpThis->statDistance.entry[i].huffCode;
						lpThis->stateInfo.dwAdditionalDistanceBitCount = lpThis->statDistance.entry[i].bAdditionalBits;
						lpThis->stateInfo.nextAdditionalDistanceBits = (dwTempDistance - minorDeflate__LUT__DistanceCodes[i][1]);
					}

					lpThis->stateInfo.dwCurrentByte = lpThis->stateInfo.dwCurrentByte + 2;
					lpThis->stateInfo.dwCurrentBit = 1;

					/*
						Huffman codes are always encoded in inverse order
					*/
					lpThis->stateInfo.nextSymbolBits = lpThis->stateInfo.nextSymbolBits << (32-lpThis->stateInfo.dwSymbolBitCount);
					lpThis->stateInfo.nextDistanceSymbolBits = lpThis->stateInfo.nextDistanceSymbolBits << (32-lpThis->stateInfo.dwDistanceSymbolBitCount);
				}
			}
			while(lpThis->stateInfo.dwCurrentBit < 1+lpThis->stateInfo.dwSymbolBitCount) {
				if((e = minorCompressor_Deflate__FlushBlock_EmitBit(lpThis, ((lpThis->stateInfo.nextSymbolBits & 0x80000000) == 0 ? 0 : 1)) != minorE_Ok)) { return e; }
				lpThis->stateInfo.nextSymbolBits = lpThis->stateInfo.nextSymbolBits << 1;
				lpThis->stateInfo.dwCurrentBit = lpThis->stateInfo.dwCurrentBit + 1;
			}
			while(lpThis->stateInfo.dwCurrentBit < 1+lpThis->stateInfo.dwSymbolBitCount+lpThis->stateInfo.dwAdditionalBitCount) {
				if((e = minorCompressor_Deflate__FlushBlock_EmitBit(lpThis, (lpThis->stateInfo.nextAdditionalBits & 0x01)) != minorE_Ok)) { return e; }
				lpThis->stateInfo.nextAdditionalBits = lpThis->stateInfo.nextAdditionalBits >> 1;
				lpThis->stateInfo.dwCurrentBit = lpThis->stateInfo.dwCurrentBit + 1;
			}
			while(lpThis->stateInfo.dwCurrentBit < 1+lpThis->stateInfo.dwSymbolBitCount+lpThis->stateInfo.dwAdditionalBitCount+lpThis->stateInfo.dwDistanceSymbolBitCount) {
				if((e = minorCompressor_Deflate__FlushBlock_EmitBit(lpThis, ((lpThis->stateInfo.nextDistanceSymbolBits & 0x80000000) == 0 ? 0 : 1)) != minorE_Ok)) { return e; }
				lpThis->stateInfo.nextDistanceSymbolBits = lpThis->stateInfo.nextDistanceSymbolBits << 1;
				lpThis->stateInfo.dwCurrentBit = lpThis->stateInfo.dwCurrentBit + 1;
			}
			while(lpThis->stateInfo.dwCurrentBit < 1+lpThis->stateInfo.dwSymbolBitCount+lpThis->stateInfo.dwAdditionalBitCount+lpThis->stateInfo.dwDistanceSymbolBitCount+lpThis->stateInfo.dwAdditionalDistanceBitCount) {
				if((e = minorCompressor_Deflate__FlushBlock_EmitBit(lpThis, (lpThis->stateInfo.nextAdditionalDistanceBits & 0x01)) != minorE_Ok)) { return e; }
				lpThis->stateInfo.nextAdditionalDistanceBits = lpThis->stateInfo.nextAdditionalDistanceBits >> 1;
				lpThis->stateInfo.dwCurrentBit = lpThis->stateInfo.dwCurrentBit + 1;
			}
			lpThis->stateInfo.dwCurrentBit = 0;
		}

		if(lpThis->currentState == minorCompressor_Deflate_State__FlushBlock_2) {
			/*
				We are emitting the last block so we won't cache the
				remaining bits from an unfinished byte
			*/
			e = minorCompressor_Deflate__FlushBlock_EmitBitFlush(lpThis);
			if(e != minorE_Ok) {
				/*
					In case we restart the while loop above will never
					be true so we will automatically enter the last
					emitBitFlush again
				*/
				return e;
			}
		}
		/*
			Done emitting data

			Reset state to enter next block / finish finalization
		*/
		{
	   		lpThis->dwOutputBufferUsed = 0;
	   		lpThis->dwBlockBytesDone = 0;
	   		lpThis->dwSymbolBufferUsed = 0;
	   		for(i = 0; i < (sizeof(lpThis->statLiteralLength.entry)/sizeof(struct minorHuffutilAlphabetEntry_U32)); i=i+1) {
		   		lpThis->statLiteralLength.entry[i].dProbability = 0;
		   		lpThis->statLiteralLength.entry[i].huffCode = 0;
		   		lpThis->statLiteralLength.entry[i].huffMask = 0;
		   		lpThis->statLiteralLength.entry[i].huffLength = 0;
	   		}
	   		for(i = 0; i < (sizeof(lpThis->statDistance.entry)/sizeof(struct minorHuffutilAlphabetEntry_U32)); i=i+1) {
		   		lpThis->statDistance.entry[i].dProbability = 0;
		   		lpThis->statDistance.entry[i].huffCode = 0;
		   		lpThis->statDistance.entry[i].huffMask = 0;
		   		lpThis->statDistance.entry[i].huffLength = 0;
	   		}
		}
	   	switch(lpThis->currentState) {
		   	case minorCompressor_Deflate_State__FlushBlock_1:	/* This was a flush that was triggered by an full symbol or raw data buffer */
		   	case minorCompressor_Deflate_State__FlushBlock_3:	/* The application had requested an flush operation. Return to normal accepting data state */
			   	lpThis->currentState = minorCompressor_Deflate_State__AcceptingData;
			   	break;
		   	case minorCompressor_Deflate_State__FlushBlock_2:	/* This has been the last block of data */
			   	lpThis->currentState = minorCompressor_Deflate_State__Finished;
			   	break;
		   	default: /* Clear error indication (implementation error) */
			   	lpThis->currentState = minorCompressor_Deflate_State__Error;
			   	return minorE_ImplementationError;
	   	}
	   	return minorE_Ok;
	}

	return minorE_ImplementationError;
}

static enum minorError minorCompressor_Deflate__ProcessByte(
	struct minorCompressor_Deflate* lpThis,
	uint8_t bByte
) {
	unsigned long int dwCachedDataLookahead;
	unsigned long int dwCachedDataLookback;
	unsigned long int dwSkipIndex;

	unsigned long int dwBestMatch_Absolute;
	unsigned long int dwBestMatch_Distance;
	unsigned long int dwBestMatch_Length;
	unsigned long int dwBestMatch_LazyIndex;

	uint32_t bThreeBytes;

	enum minorError e;

	/* Check current state */
	if((lpThis->currentState == minorCompressor_Deflate_State__AcceptingData) || (lpThis->currentState == minorCompressor_Deflate_State__FlushData) || (lpThis->currentState == minorCompressor_Deflate_State__FlushLastData)) {
		/* Insert byte into our ring buffer */
		lpThis->lpRawRingBuffer[lpThis->dwRawRingHead] = bByte;
		lpThis->dwRawRingHead = (lpThis->dwRawRingHead + 1) % lpThis->dwRawRingSize;
		lpThis->dwRawRingHeadAbsolute = lpThis->dwRawRingHeadAbsolute + 1;
		if(lpThis->dwRawRingUsed < lpThis->dwRawRingSize) { lpThis->dwRawRingUsed = lpThis->dwRawRingUsed + 1; }

		lpThis->dwBlockBytesDone = lpThis->dwBlockBytesDone + 1;
		lpThis->stateInfo.lastByte = bByte;

		switch(lpThis->currentState) {
			case minorCompressor_Deflate_State__AcceptingData:	lpThis->currentState = minorCompressor_Deflate_State__ProcessByte_Normal; 			break;
			case minorCompressor_Deflate_State__FlushLastData:	lpThis->currentState = minorCompressor_Deflate_State__ProcessByte_FlushLastBlock; 	break;
			case minorCompressor_Deflate_State__FlushData:		lpThis->currentState = minorCompressor_Deflate_State__ProcessByte_FlushBlock; 		break;
			default:											return minorE_ImplementationError; /* This is impossible due to outer if */
		}
	} else if((lpThis->currentState == minorCompressor_Deflate_State__ProcessByte_Normal) || (lpThis->currentState == minorCompressor_Deflate_State__ProcessByte_FlushBlock) || (lpThis->currentState == minorCompressor_Deflate_State__ProcessByte_FlushLastBlock)) {
		/* Recover last used byte when called from continuiation function */
		bByte = lpThis->stateInfo.lastByte;
	} else {
		return minorE_InvalidState; /* This is an internal error. This function should never be called with an invalid state */
	}

	/* Calculate lookahead and lookback */
	if(lpThis->dwRawRingHead > lpThis->dwRawRingTail) {
		dwCachedDataLookahead = lpThis->dwRawRingHead - lpThis->dwRawRingTail;
	} else {
		dwCachedDataLookahead = (lpThis->dwRawRingSize - lpThis->dwRawRingTail) + lpThis->dwRawRingHead;
	}
	dwCachedDataLookback = lpThis->dwRawRingUsed - dwCachedDataLookahead;
	/*
		Check if we have enough data cached for a potential full match.
		In the worst case this means we have to have a dwLZ77MaximumMatchLength lookahead
		because the areas are allowed to overlap.
	*/
	while(dwCachedDataLookahead >= lpThis->dwLZ77MaximumMatchLength) {
		/*
			Perform insert(s) of the previous sequences. Note that
			duplicate inserts are ignored so we simply call hashtable
			insert routine for every possible combination
		*/
		if(dwCachedDataLookback >= 2) {
			bThreeBytes = ((uint32_t)(lpThis->lpRawRingBuffer[(lpThis->dwRawRingTail-2+lpThis->dwRawRingSize) % lpThis->dwRawRingSize])) |
				((uint32_t)(lpThis->lpRawRingBuffer[(lpThis->dwRawRingTail-1+lpThis->dwRawRingSize) % lpThis->dwRawRingSize]) << 8) |
				((uint32_t)(lpThis->lpRawRingBuffer[(lpThis->dwRawRingTail+lpThis->dwRawRingSize) % lpThis->dwRawRingSize]) << 16);

			e = minorCompressor_Deflate_Hashtable_Insert(
				bThreeBytes,
				lpThis->dwRawRingHeadAbsolute - dwCachedDataLookahead - 2,
				(lpThis->dwRawRingTail - 2 + lpThis->dwRawRingSize) % lpThis->dwRawRingSize,
				lpThis->dwLZ77LookbackDistance,
				lpThis->lpLZ77HashTable,
				lpThis->lpSystemAPI
			);
			if(e != minorE_Ok) { return e; }
		}
		if((dwCachedDataLookback >= 1) && (dwCachedDataLookahead >= 1)) {
			bThreeBytes = ((uint32_t)(lpThis->lpRawRingBuffer[(lpThis->dwRawRingTail-1+lpThis->dwRawRingSize) % lpThis->dwRawRingSize])) |
				((uint32_t)(lpThis->lpRawRingBuffer[(lpThis->dwRawRingTail-0+lpThis->dwRawRingSize) % lpThis->dwRawRingSize]) << 8) |
				((uint32_t)(lpThis->lpRawRingBuffer[(lpThis->dwRawRingTail+1+lpThis->dwRawRingSize) % lpThis->dwRawRingSize]) << 16);

			e = minorCompressor_Deflate_Hashtable_Insert(
				bThreeBytes,
				lpThis->dwRawRingHeadAbsolute - dwCachedDataLookahead - 1,
				(lpThis->dwRawRingTail - 1 + lpThis->dwRawRingSize) % lpThis->dwRawRingSize,
				lpThis->dwLZ77LookbackDistance,
				lpThis->lpLZ77HashTable,
				lpThis->lpSystemAPI
			);
			if(e != minorE_Ok) { return e; }
		}

		/*
			Perform a match ...
		*/
		e = minorCompressor_Deflate_Hashtable_Search(
			lpThis,
			lpThis->dwRawRingHeadAbsolute - dwCachedDataLookahead,
			lpThis->dwRawRingTail,
			&dwBestMatch_Absolute,
			&dwBestMatch_Distance,
			&dwBestMatch_Length,
			&dwBestMatch_LazyIndex
		);
		if(e != minorE_Ok) { return e; }

		if(dwBestMatch_Length > 2) {
			#ifdef MINOR_DEFLATE_DEBUG
				printf("%s:%u Match found: Absolute Offset: %lu, Distance: %lu, Length: %lu, Lazy: %lu\n", __FILE__, __LINE__, dwBestMatch_Absolute, dwBestMatch_Distance, dwBestMatch_Length, dwBestMatch_LazyIndex);
			#endif
			/*
				Our currently pointed at symbol has been detected as the start
				of a repeated sequence.
			*/
			while(dwBestMatch_LazyIndex > 0) {
				/* We first emit literals for the skipped elements and register them (correctly) */
				e = minorCompressor_Deflate__ProcessLZ77Symbol_Literal(lpThis, (uint16_t)(lpThis->lpRawRingBuffer[lpThis->dwRawRingTail] & 0xFF));
				if(e != minorE_Ok) { return e; }

				/* If lookahead is > 0 we load the next byte into the state block to allow safe restart */
				lpThis->stateInfo.lastByte = lpThis->lpRawRingBuffer[(lpThis->dwRawRingTail + 1) % lpThis->dwRawRingSize];

				/* Register sequence with us as base if possible ... */
				if(dwCachedDataLookahead >= 2) {
					bThreeBytes = ((uint32_t)(lpThis->lpRawRingBuffer[(lpThis->dwRawRingTail+0+lpThis->dwRawRingSize) % lpThis->dwRawRingSize])) |
						((uint32_t)(lpThis->lpRawRingBuffer[(lpThis->dwRawRingTail+1+lpThis->dwRawRingSize) % lpThis->dwRawRingSize]) << 8) |
						((uint32_t)(lpThis->lpRawRingBuffer[(lpThis->dwRawRingTail+2+lpThis->dwRawRingSize) % lpThis->dwRawRingSize]) << 16);

					e = minorCompressor_Deflate_Hashtable_Insert(
						bThreeBytes,
						lpThis->dwRawRingHeadAbsolute - dwCachedDataLookahead,
						lpThis->dwRawRingTail,
						lpThis->dwLZ77LookbackDistance,
						lpThis->lpLZ77HashTable,
						lpThis->lpSystemAPI
					);
					if(e != minorE_Ok) { return e; }
				}
				lpThis->dwRawRingTail = (lpThis->dwRawRingTail + 1) % lpThis->dwRawRingSize;
				dwCachedDataLookback = dwCachedDataLookback + 1;
				dwCachedDataLookahead = dwCachedDataLookahead - 1;
				dwBestMatch_LazyIndex = dwBestMatch_LazyIndex - 1;
			}

			/* Emit Symbol for this match ... */
			e = minorCompressor_Deflate__ProcessLZ77Symbol_Symbol(lpThis, (uint16_t)dwBestMatch_Distance, (uint16_t)dwBestMatch_Length);
			if(e != minorE_Ok) { return e; }

			/* Skip the following repeated sequences and REGISTER the entries (if possible) into the hashtable */
			for(dwSkipIndex = 0; dwSkipIndex < dwBestMatch_Length; dwSkipIndex = dwSkipIndex + 1) {
				if((dwCachedDataLookahead - dwSkipIndex) >= 2) {
					bThreeBytes = ((uint32_t)(lpThis->lpRawRingBuffer[(lpThis->dwRawRingTail+dwSkipIndex) % lpThis->dwRawRingSize])) |
						((uint32_t)(lpThis->lpRawRingBuffer[(lpThis->dwRawRingTail+dwSkipIndex+1) % lpThis->dwRawRingSize]) << 8) |
						((uint32_t)(lpThis->lpRawRingBuffer[(lpThis->dwRawRingTail+dwSkipIndex+2) % lpThis->dwRawRingSize]) << 16);

					e = minorCompressor_Deflate_Hashtable_Insert(
						bThreeBytes,
						lpThis->dwRawRingHeadAbsolute - dwCachedDataLookahead + dwSkipIndex,
						(lpThis->dwRawRingTail + dwSkipIndex) % lpThis->dwRawRingSize,
						lpThis->dwLZ77LookbackDistance,
						lpThis->lpLZ77HashTable,
						lpThis->lpSystemAPI
					);

					/*
						TODO: Is this in any way security relevant?

						If we cannot insert into the hashtable (e != minorE_Ok) IGNORE because we
						cannot simply restart (currently) the skipping process
					*/
				}
			}
			lpThis->dwRawRingTail = (lpThis->dwRawRingTail + dwBestMatch_Length) % lpThis->dwRawRingSize,
			dwCachedDataLookback = dwCachedDataLookback + dwBestMatch_Length;
			dwCachedDataLookahead = dwCachedDataLookahead - dwBestMatch_Length;
		} else {
			#ifdef MINOR_DEFLATE_DEBUG
				printf("%s:%u No match\n", __FILE__, __LINE__);
			#endif

			/*
				We have to emit a literal symbol for this tail element
			*/
			e = minorCompressor_Deflate__ProcessLZ77Symbol_Literal(lpThis, (uint16_t)(lpThis->lpRawRingBuffer[lpThis->dwRawRingTail]));
			if(e != minorE_Ok) { return e; }

			/* If lookahead is > 0 we load the next byte into the state block to allow safe restart */
			lpThis->stateInfo.lastByte = lpThis->lpRawRingBuffer[(lpThis->dwRawRingTail + 1) % lpThis->dwRawRingSize];

			/* Register sequence with us as base if possible ... */
			if(dwCachedDataLookahead >= 2) {
				bThreeBytes = ((uint32_t)(lpThis->lpRawRingBuffer[(lpThis->dwRawRingTail+0+lpThis->dwRawRingSize) % lpThis->dwRawRingSize])) |
					((uint32_t)(lpThis->lpRawRingBuffer[(lpThis->dwRawRingTail+1+lpThis->dwRawRingSize) % lpThis->dwRawRingSize]) << 8) |
					((uint32_t)(lpThis->lpRawRingBuffer[(lpThis->dwRawRingTail+2+lpThis->dwRawRingSize) % lpThis->dwRawRingSize]) << 16);

				e = minorCompressor_Deflate_Hashtable_Insert(
					bThreeBytes,
					lpThis->dwRawRingHeadAbsolute - dwCachedDataLookahead,
					lpThis->dwRawRingTail,
					lpThis->dwLZ77LookbackDistance,
					lpThis->lpLZ77HashTable,
					lpThis->lpSystemAPI
				);
				/*
					TODO: Is this in any way security relevant?

					If we cannot insert into the hashtable IGNORE because the task will be retried anyways via
					a restartable idempotent operation
				*/
			}
			lpThis->dwRawRingTail = (lpThis->dwRawRingTail + 1) % lpThis->dwRawRingSize;
			dwCachedDataLookback = dwCachedDataLookback + 1;
			dwCachedDataLookahead = dwCachedDataLookahead - 1;
		}
	}

	/*
		Check if we have reached an end of block condition (i.e. blocksize - 1 bytes)
	*/

	switch(lpThis->currentState) {
		case minorCompressor_Deflate_State__ProcessByte_Normal:			lpThis->currentState = minorCompressor_Deflate_State__AcceptingData;	break;
		case minorCompressor_Deflate_State__ProcessByte_FlushBlock:		lpThis->currentState = minorCompressor_Deflate_State__FlushLastData;	break;
		case minorCompressor_Deflate_State__ProcessByte_FlushLastBlock:	lpThis->currentState = minorCompressor_Deflate_State__FlushData; 		break;
		default:																																break;
	}

	e = minorE_Ok;
	if(lpThis->dwBlockBytesDone >= (lpThis->dwBlockSize-1)) {
		/* Finished block. Flush remaining bytes into LZ77 symbol buffer ... */
		e = minorCompressor_Deflate__FlushBlock(lpThis);
	}

	return e;
}



/*
	Public functions
*/
enum minorError minorCompressorCreate_Deflate(
	struct minorCompressor** 			lpOut,			/* Output location for the compressor object. There will be NO REUSE. Any previous object pointed to will be orphaned if not released before! */

	enum minorAlgorithm					algorithm,		/*  Allows to select the algorithm to be used during data compression */
	struct minorSystemInterface*		lpSystem,		/* Required system interface */
	struct minorConfigurationElement* 	lpConfiguration	/* Pointer to the first configuration element inside the configuration chain or NULL */
) {
	/* Sliding window configuration */
	unsigned long int 			dwCfg_SlidingWindowSize		= MINOR_DEFLATE__DEFAULT__BLOCK_SIZE;			/* The size of our sliding window (for lookback) */
	/* Preload Dictionary configuration */
	unsigned char*				lpCfg_PreloadDictionary		= NULL;
	unsigned long int			dwCfg_PreloadDictionary		= 0;
	/* Quality settings configuration */
	unsigned long int			dwCfg_LazyMatchDistance		= MINOR_DEFLATE__DEFAULT__LAZY_MATCH_DISTANCE; 	/* How far to look ahead for a better match */
	unsigned long int			dwCfg_BlockSize				= MINOR_DEFLATE__DEFAULT__BLOCK_SIZE;			/* The block size ... this determines after which number of bytes we want to regenerate new tables */
	unsigned long int 			dwCfg_LookbackDistance		= MINOR_DEFLATE__DEFAULT__LOOKBACK_DISTANCE;	/* How far we can lookback */
	unsigned long int			dwCfg_EarlyMatchLength		= MINOR_DEFLATE__DEFAULT__EARLY_MATCH_LENGTH;	/* If we ever find a match longer or equal to this length we immedatly accept it */
	/* Deviations from the spec would be configured here */
	unsigned long int			dwCfg_MaximumLookbackLength	= MINOR_DEFLATE__DEFAULT__MAX_LOOKBACK_LENGTH; /* maximum length of a LZ77 lookback <distance,length> code */
	/* Tuneables for the hashtable */
	unsigned long int			dwCfg_HashEntries			= MINOR_DEFLATE__DEFAULT__HASHTABLE_ENTRIES;
	unsigned long int			dwCfg_HashMaxDepth			= MINOR_DEFLATE__DEFAULT__HASHTABLE_MAXDEPTH;
	#if 0
		unsigned long int		dwCfg_HashMaxEntries		= MINOR_DEFLATE__DEFAULT__HASHTABLE_MAXENTRIES;
	#endif

	unsigned long int			dwCfg_OutputBufferSize		= 0; 										/* Adds an additional caching layer to the output stream ... normally not needed for compression ... */
	uint32_t					dwCfg_Flags					= MINOR_COMPRESSOR_DEFLATE_FLAG__CANDIDATE_RAW|MINOR_COMPRESSOR_DEFLATE_FLAG__CANDIDATE_FIXED|MINOR_COMPRESSOR_DEFLATE_FLAG__CANDIDATE_DYNAMIC;
	/*
		Can be set for debug purposes:

		uint32_t					dwCfg_Flags					= MINOR_COMPRESSOR_DEFLATE_FLAG__CANDIDATE_RAW|MINOR_COMPRESSOR_DEFLATE_FLAG__CANDIDATE_FIXED;
		uint32_t					dwCfg_Flags					= MINOR_COMPRESSOR_DEFLATE_FLAG__CANDIDATE_RAW;
	*/


	struct minorCompressor_Deflate* lpNew;
	struct minorConfigurationElement* lpCfg_Current;
	enum minorError e;
	unsigned long int dwRequiredBufferSpace;
	unsigned long int dwAlignmentPadding;
	uint32_t bThreeBytes;
	unsigned long int i;


	if((lpOut == NULL) || (algorithm != minorAlgorithm_Deflate) || (lpSystem == NULL)) { return minorE_InvalidParam; }

	if((lpSystem->alloc == NULL) || (lpSystem->free == NULL)) { return minorE_InvalidParam; }

	/* Read configuration options. Note that we do NOT validate if all critical options have been processed. Use the validation function for that */
	lpCfg_Current = lpConfiguration;
	while(lpCfg_Current != NULL) {
		if(lpCfg_Current->typeMajor == minorConfiguration_MajorId_Deflate) {
			if(((struct minorConfigurationElement_Deflate*)lpCfg_Current)->typeMinor == minorConfiguration_MinorID_Deflate_BackreferenceTableSize) {
				dwCfg_SlidingWindowSize = ((struct minorConfigurationElement_Deflate_BackreferenceTableSize*)lpCfg_Current)->slidingWindowSize;
				/* Sliding window sizes larger than the default blocksize would lead to deflate codes that cannot be decoded by RFC conformant decoders. */
				if(dwCfg_SlidingWindowSize > LIBMINOR_DEFLATE__BLOCKSIZE_K) 			{  return minorE_InvalidParam; }
			} else if(((struct minorConfigurationElement_Deflate*)lpCfg_Current)->typeMinor == minorConfiguration_MinorID_Deflate_PreloadDictionary) {
				lpCfg_PreloadDictionary 	= ((struct minorConfigurationElement_Deflate_PreloadDictionary*)lpCfg_Current)->lpDictionary;
				dwCfg_PreloadDictionary 	= ((struct minorConfigurationElement_Deflate_PreloadDictionary*)lpCfg_Current)->dwByteLength;

				if((lpCfg_PreloadDictionary == NULL) && (dwCfg_PreloadDictionary != 0)) { return minorE_InvalidParam; }
				if(dwCfg_PreloadDictionary > LIBMINOR_DEFLATE__BLOCKSIZE_K*1024) 		{ return minorE_InvalidParam; }
			} else if(((struct minorConfigurationElement_Deflate*)lpCfg_Current)->typeMinor == minorConfiguration_MinorID_Deflate_QualitySettings) {
				dwCfg_LazyMatchDistance 	= ((struct minorConfigurationElement_Deflate_QualitySettings*)lpCfg_Current)->dwLazyMatchDistance;
				dwCfg_BlockSize 			= ((struct minorConfigurationElement_Deflate_QualitySettings*)lpCfg_Current)->dwBlockSize;
				dwCfg_LookbackDistance 		= ((struct minorConfigurationElement_Deflate_QualitySettings*)lpCfg_Current)->dwLookbackDistance;
				dwCfg_EarlyMatchLength 		= ((struct minorConfigurationElement_Deflate_QualitySettings*)lpCfg_Current)->dwEarlyMatchLength;

				/* Check parameters are inside RFC specified boundaries. If we wouldn't check them we would be capable of producting encoded data that an RFC conformant decoder could not decode */
				if(dwCfg_LookbackDistance > MINOR_DEFLATE__DEFAULT__LOOKBACK_DISTANCE) 	{ return minorE_InvalidParam; }
				if(dwCfg_BlockSize > LIBMINOR_DEFLATE__BLOCKSIZE_K*1024) 				{ return minorE_InvalidParam; }
				if(dwCfg_LookbackDistance > MINOR_DEFLATE__DEFAULT__LOOKBACK_DISTANCE) 	{ return minorE_InvalidParam; }
			}
		}

		lpCfg_Current = lpCfg_Current->lpNext;
	}

	/* In case we only allow raw blocks we have to clamp the blocksize */
	if((dwCfg_Flags & (~MINOR_COMPRESSOR_DEFLATE_FLAG__CANDIDATE_RAW)) == 0) {
		dwCfg_BlockSize = 0xFFFF;
	}

	/* Allocate and initialize data structure */
	dwRequiredBufferSpace = sizeof(struct minorCompressor_Deflate);
	/*
		Raw Ring Buffer.
		This buffer buffers the raw data bytes. If an uncompressed raw data block is a candidate
		for emission the buffer has to be capable of holding a full raw data block as well as
		dwLZ77LookbackDistance bits. Else it has to hold at least
		(dwLZ77LookbackDistance+dwLZ77MaximumMatchLength+dwLazyMatchDistance+1) bytes. The last
		byte that's used is used to prevent head and tail crashing into each other.
	*/
	if((dwCfg_Flags & MINOR_COMPRESSOR_DEFLATE_FLAG__CANDIDATE_RAW) != 0) {
		dwRequiredBufferSpace = dwRequiredBufferSpace + (((dwCfg_BlockSize+dwCfg_LookbackDistance+1) > (dwCfg_LookbackDistance+dwCfg_LazyMatchDistance+dwCfg_MaximumLookbackLength+1)) ? (dwCfg_BlockSize+dwCfg_LookbackDistance+1) : (dwCfg_LookbackDistance+dwCfg_LazyMatchDistance+dwCfg_MaximumLookbackLength+1));
	} else {
		dwRequiredBufferSpace = dwRequiredBufferSpace + dwCfg_LookbackDistance+dwCfg_LazyMatchDistance+dwCfg_MaximumLookbackLength+1;
	}
	/*
		LZ77 symbol buffer.
		This buffer holds at least dwBlockSize unencoded SYMBOLS. Each symbol either represents
		an literal (0-255) or an <distance, length> code. Because each distance,length code
		replaces three literal bytes the maximum number of symbols is dwBlockSize + 1 (end of block).
		Each symbol is representable as a 16 bit value -> (dwBlockSize+1) * sizeof(uint16_t) bytes

		To provide adequate alignment of the buffer we add padding of size
		(sizeof(unsigned long int)-(dwRequiredBufferSpace % sizeof(unsigned long int))).
	*/
	dwAlignmentPadding = (sizeof(unsigned long int)-(dwRequiredBufferSpace % sizeof(unsigned long int)));
	dwRequiredBufferSpace = dwRequiredBufferSpace + dwAlignmentPadding + (dwCfg_BlockSize+1)*sizeof(uint16_t);

	/*
		Output buffer
		The output buffer can have a variable size (or be totally omitted). After a block has been
		cached inside the symbol buffer and the huffman tree has been built (if required and not
		a raw data block is used) the compressor starts to output bytes - either (in case of an omitted
		output buffer) one byte at a time into the attached sink or one output buffer block (or less for
		the last block).
	*/
	dwRequiredBufferSpace = dwRequiredBufferSpace + dwCfg_OutputBufferSize;

	if((e = lpSystem->alloc((void**)(&lpNew), dwRequiredBufferSpace, lpSystem->lpFreeParam_Alloc)) != minorE_Ok) { return e; }

	lpNew->base.algorithm = minorAlgorithm_Deflate;
	lpNew->lpSystemAPI = lpSystem;
	lpNew->dwFlags = dwCfg_Flags;
	lpNew->dwLazyMatchDistance = dwCfg_LazyMatchDistance;
	lpNew->dwBlockSize = dwCfg_BlockSize;
	for(i = 0; i < (sizeof(lpNew->statLiteralLength.entry)/sizeof(struct minorHuffutilAlphabetEntry_U32)); i=i+1) {
		lpNew->statLiteralLength.entry[i].dwSymbol = i;
		lpNew->statLiteralLength.entry[i].dProbability = 0;
		if(i < 265) { 			lpNew->statLiteralLength.entry[i].bAdditionalBits = 0;
		} else if(i < 269) { 	lpNew->statLiteralLength.entry[i].bAdditionalBits = 1;
		} else if(i < 273) { 	lpNew->statLiteralLength.entry[i].bAdditionalBits = 2;
		} else if(i < 277) { 	lpNew->statLiteralLength.entry[i].bAdditionalBits = 3;
		} else if(i < 281) { 	lpNew->statLiteralLength.entry[i].bAdditionalBits = 4;
		} else if(i < 285) { 	lpNew->statLiteralLength.entry[i].bAdditionalBits = 5;
		} else { 				lpNew->statLiteralLength.entry[i].bAdditionalBits = 0; }
		lpNew->statLiteralLength.entry[i].huffCode = 0;
		lpNew->statLiteralLength.entry[i].huffMask = 0;
		lpNew->statLiteralLength.entry[i].huffLength = 0;
	}
	lpNew->statLiteralLength.dwAlphabetSize = sizeof(lpNew->statLiteralLength.entry)/sizeof(struct minorHuffutilAlphabetEntry_U32);
	for(i = 0; i < (sizeof(lpNew->statDistance.entry)/sizeof(struct minorHuffutilAlphabetEntry_U32)); i=i+1) {
		lpNew->statDistance.entry[i].dwSymbol = i;
		lpNew->statDistance.entry[i].dProbability = 0;
		if(i < 4) { lpNew->statDistance.entry[i].bAdditionalBits = 0; }
		else if(i < 6) 	{ lpNew->statDistance.entry[i].bAdditionalBits = 1; }
		else if(i < 8) 	{ lpNew->statDistance.entry[i].bAdditionalBits = 2; }
		else if(i < 10) { lpNew->statDistance.entry[i].bAdditionalBits = 3; }
		else if(i < 12) { lpNew->statDistance.entry[i].bAdditionalBits = 4; }
		else if(i < 14) { lpNew->statDistance.entry[i].bAdditionalBits = 5; }
		else if(i < 16) { lpNew->statDistance.entry[i].bAdditionalBits = 6; }
		else if(i < 18) { lpNew->statDistance.entry[i].bAdditionalBits = 7; }
		else if(i < 20) { lpNew->statDistance.entry[i].bAdditionalBits = 8; }
		else if(i < 22) { lpNew->statDistance.entry[i].bAdditionalBits = 9; }
		else if(i < 24) { lpNew->statDistance.entry[i].bAdditionalBits = 10; }
		else if(i < 26) { lpNew->statDistance.entry[i].bAdditionalBits = 11; }
		else if(i < 28) { lpNew->statDistance.entry[i].bAdditionalBits = 12; }
		else 			{ lpNew->statDistance.entry[i].bAdditionalBits = 13; }
		lpNew->statDistance.entry[i].huffCode = 0;
		lpNew->statDistance.entry[i].huffMask = 0;
		lpNew->statDistance.entry[i].huffLength = 0;
	}
	lpNew->statDistance.dwAlphabetSize = sizeof(lpNew->statDistance.entry)/sizeof(struct minorHuffutilAlphabetEntry_U32);
	lpNew->dwLZ77LookbackDistance = dwCfg_LookbackDistance;
	lpNew->dwLZ77MaximumMatchLength = dwCfg_MaximumLookbackLength;
	lpNew->dwLZ77EarlyMatchLength = dwCfg_EarlyMatchLength;
	lpNew->lpRawDictionary = lpCfg_PreloadDictionary;
	lpNew->dwDictionaryLength = dwCfg_PreloadDictionary;
	lpNew->dwUnprocessedDictionaryBytes = dwCfg_PreloadDictionary;
	lpNew->lpDataSource = NULL; /* TODO Allow setting via configuration option */
	lpNew->lpDataSink = NULL; /* TODO: Allow setting via configuration option */
	lpNew->lpLZ77HashTable = NULL; /* Created afterwards */
	lpNew->dwBlockBytesDone = 0;

	/* Calculate buffer information */
	lpNew->lpRawRingBuffer = &(lpNew->bData[0]);
	if((dwCfg_Flags & MINOR_COMPRESSOR_DEFLATE_FLAG__CANDIDATE_RAW) != 0) {
		lpNew->dwRawRingSize = ((dwCfg_BlockSize+dwCfg_LookbackDistance+1) > (dwCfg_LookbackDistance+dwCfg_LazyMatchDistance+dwCfg_MaximumLookbackLength+1)) ? (dwCfg_BlockSize+dwCfg_LookbackDistance+1) : (dwCfg_LookbackDistance+dwCfg_LazyMatchDistance+dwCfg_MaximumLookbackLength+1);
	} else {
		lpNew->dwRawRingSize = dwCfg_LookbackDistance+dwCfg_LazyMatchDistance+dwCfg_MaximumLookbackLength+1;
	}

	lpNew->dwRawRingUsed = 0;
	lpNew->dwRawRingHead = 0;
	lpNew->dwRawRingTail = 0;
	lpNew->dwRawRingHeadAbsolute = 0;

	lpNew->shiftPosition = 0;
	lpNew->bitShiftreg = 0;

	lpNew->lpSymbolBuffer = (uint16_t*)(&(lpNew->bData[lpNew->dwRawRingSize+dwAlignmentPadding]));
	lpNew->dwSymbolBufferUsed = 0;

	if(dwCfg_OutputBufferSize != 0) {
		lpNew->lpOutputBuffer = &(lpNew->bData[lpNew->dwRawRingSize + (dwCfg_BlockSize + 1) * sizeof(uint16_t)]);
	} else {
		lpNew->lpOutputBuffer = NULL;
	}
	lpNew->dwOutputBufferSize = dwCfg_OutputBufferSize;
	lpNew->dwOutputBufferUsed = 0;

	/* Set current state */
	lpNew->currentState = minorCompressor_Deflate_State__AcceptingData; /* Now we are ready to accept data */

	/* Initialize hash table */
	if((e = minorCompressor_Deflate_Hashtable_Create(&(lpNew->lpLZ77HashTable), lpSystem, dwCfg_HashEntries, dwCfg_HashMaxDepth)) != minorE_Ok) { goto cleanup; }

	/*
		Start "new block" (the first one). Note that we have to honor the preload dictionary only
		for the first block! It is NOT reused for subsequent blocks. If we have a preload dictionary
		we will inject the data now and NOT produce any output
	*/
	bThreeBytes = 0;
	if(lpNew->dwDictionaryLength != 0) {
		/* Fill our dictionary into the ringbuffer, registering tripplets on the go ... */
		for(i = 0; i < lpNew->dwDictionaryLength; i=i+1) {
			lpNew->lpRawRingBuffer[lpNew->dwRawRingHead] = lpNew->lpRawDictionary[i];
			/*
				We a shift register to accumulate the 3 byte sequences. The lowest byte is always
				at lowest position s owe have to insert in reverse order
			*/
			bThreeBytes = (bThreeBytes >> 8) | (((unsigned long int)(lpNew->lpRawDictionary[i])) << 16);
			if(i >= 3) {
				/* Register the last 3 byte sequence we've gathered */
				e = minorCompressor_Deflate_Hashtable_Insert(
					bThreeBytes,
					i, /* Our absolute position IS the position inside the dictionary */
					lpNew->dwRawRingHead,
					dwCfg_LookbackDistance,
					lpNew->lpLZ77HashTable,
					lpSystem
				);
				if(e != minorE_Ok) { goto cleanup; }
				lpNew->dwUnprocessedDictionaryBytes = lpNew->dwUnprocessedDictionaryBytes - 1;
			}

			if(lpNew->dwRawRingUsed < lpNew->dwRawRingSize) { lpNew->dwRawRingUsed = lpNew->dwRawRingUsed + 1; }
			lpNew->dwRawRingHead = (lpNew->dwRawRingHead + 1) % lpNew->dwRawRingSize;	/* Advance head pointer */
			lpNew->dwRawRingHeadAbsolute = lpNew->dwRawRingHeadAbsolute + 1;
			/*
				In case we ever reach the tail again discard data at the tail.
				We ALWAYS have to have a difference of 1 except during bootstrapping
				the buffer. Unused bytes of the dictionary (that are discarded) are
				not used by the lookback anyways ...
			*/
		}
		lpNew->dwRawRingTail = lpNew->dwRawRingHead;
	}

	/*
		Decompressor is now bootstrapped and able to process bytes
	*/
	(*lpOut) = (struct minorCompressor*)lpNew;
	return minorE_Ok;

cleanup:
	if(lpNew != NULL) {
		if(lpNew->lpLZ77HashTable != NULL) { minorCompressor_Deflate_Hashtable_Release(&(lpNew->lpLZ77HashTable), lpSystem); lpNew->lpLZ77HashTable = NULL; }
		lpSystem->free((void*)lpNew, lpSystem->lpFreeParam_Free); lpNew = NULL;
	}
	return e;
}

enum minorError minorCompressorRelease_Deflate(
	struct minorCompressor*				lpObject		/* Allows releasing the minor object */
) {
	struct minorCompressor_Deflate* lpThis;
	struct minorSystemInterface* lpSystem;
	enum minorError e;

	e = minorE_Ok;

	if(lpObject == NULL) { return minorE_InvalidParam; }
	if(((struct minorCompressor*)lpObject)->algorithm != minorAlgorithm_Deflate) { return minorE_InvalidParam; }

	lpThis = (struct minorCompressor_Deflate*)lpObject;
	lpSystem = lpThis->lpSystemAPI;

	/*
		Flush remaining block & last block
	*/
	lpThis->currentState = minorCompressor_Deflate_State__FlushLastData;
	e = minorCompressor_Deflate__FlushBlock(lpThis);
	if(e == minorE_Suspend) { return minorE_Suspend; } /* We have to continue the output before releasing. TODO: Add the ability to fully release even in case we have not finished output */

	/* In case our compressor finished we signal "ok" to the application */
	if(e == minorE_Finished) { e = minorE_Ok; }

	if(lpThis->lpLZ77HashTable != NULL) {
		minorCompressor_Deflate_Hashtable_Release(&(lpThis->lpLZ77HashTable), lpSystem);
		lpThis->lpLZ77HashTable = NULL;
	}
	lpSystem->free((void*)lpThis, lpSystem->lpFreeParam_Free);

	return e;
}

enum minorError minorCompressorAttachSource_Deflate(
	struct minorCompressor*				lpObject,
	struct minorStreamSource*			lpSource
) {
	struct minorCompressor_Deflate* lpThis;

	if(lpObject == NULL) { return minorE_InvalidParam; }
	if(((struct minorCompressor*)lpObject)->algorithm != minorAlgorithm_Deflate) { return minorE_InvalidParam; }

	lpThis = (struct minorCompressor_Deflate*)lpObject;

	lpThis->lpDataSource = lpSource;
	return minorE_Ok;
}

enum minorError minorCompressorAttachSink_Deflate(
	struct minorCompressor*				lpObject,
	struct minorStreamSink*				lpSink
) {
	struct minorCompressor_Deflate* lpThis;

	if(lpObject == NULL) { return minorE_InvalidParam; }
	if(((struct minorCompressor*)lpObject)->algorithm != minorAlgorithm_Deflate) { return minorE_InvalidParam; }

	lpThis = (struct minorCompressor_Deflate*)lpObject;

	lpThis->lpDataSink = lpSink;
	return minorE_Ok;
}

enum minorError minorCompressorContinue_Deflate(
	struct minorCompressor				*lpObject
) {
	struct minorCompressor_Deflate* lpThis;

	if(lpObject == NULL) { return minorE_InvalidParam; }
	if(((struct minorCompressor*)lpObject)->algorithm != minorAlgorithm_Deflate) { return minorE_InvalidParam; }
	lpThis = (struct minorCompressor_Deflate*)lpObject;

	/*
		Check which state we are currently in and if it
		is continue-able
	*/
	switch(lpThis->currentState) {
		case minorCompressor_Deflate_State__AcceptingData:
			/*
				Nothing to do if we are already in accepting data
				state.
			*/
			return minorE_Ok;
		/*
			The following states are LZ77 processing states. We
			can re-enter them by calling process byte with any
			arbitrary byte value (it is not used while
			in any of these states).
		*/
		case minorCompressor_Deflate_State__ProcessByte_Normal:
		case minorCompressor_Deflate_State__ProcessByte_FlushBlock:
		case minorCompressor_Deflate_State__ProcessByte_FlushLastBlock:
			return minorCompressor_Deflate__ProcessByte(lpThis, 0x00);
		/*
			The following states are all handeled by the flushBlock
			state. If we are in FlushLastData or FlushData state
			we wanted to but haven't managed to enter flush
			state, flush 1-3 are the flush block states themselves.
		*/
		case minorCompressor_Deflate_State__FlushLastData:
		case minorCompressor_Deflate_State__FlushData:
		case minorCompressor_Deflate_State__FlushBlock_1:
		case minorCompressor_Deflate_State__FlushBlock_2:
		case minorCompressor_Deflate_State__FlushBlock_3:
			return minorCompressor_Deflate__FlushBlock(lpThis);
		case minorCompressor_Deflate_State__Error:
			return minorE_AlreadyFailed;
		case minorCompressor_Deflate_State__Finished:
			return minorE_Finished;
		default:
			return minorE_ImplementationError;
	}
}

enum minorError minorCompressor_TransferFromInput_Deflate(
	struct minorCompressor				*lpObject,
	unsigned long int 					dwBytesToRead,
	unsigned long int					*lpBytesDone
) {
	struct minorCompressor_Deflate* lpThis;
	unsigned long int dwBytesToGo;
	unsigned long int dwBytesRead;
	enum minorError e;
	uint8_t nextByte;

	if(lpObject == NULL) { return minorE_InvalidParam; }
	if(((struct minorCompressor*)lpObject)->algorithm != minorAlgorithm_Deflate) { return minorE_InvalidParam; }
	lpThis = (struct minorCompressor_Deflate*)lpObject;

	/* Check if we have attached source and sink */
	if(lpThis->lpDataSource == NULL) { return minorE_MissingDataSource; }
	if(lpThis->lpDataSink == NULL) { return minorE_MissingDataSink; }

	/* Initialize the number of bytes we've written */
	if(lpBytesDone != NULL) { (*lpBytesDone) = 0; }

	if(lpThis->currentState != minorCompressor_Deflate_State__AcceptingData) {
		/* Continue background processing if we are not ready to accept next data ... */
		if((e = minorCompressorContinue_Deflate(lpObject)) != minorE_Ok) { return e; } /* Continue tries to put the reader into one of the stable states ... */
		if(lpThis->currentState != minorCompressor_Deflate_State__AcceptingData) {
			return e;
		}
	}

	dwBytesToGo = dwBytesToRead;
	while(dwBytesToGo > 0) {
		e = lpThis->lpDataSource->read(
			lpThis->lpDataSource,
			&nextByte,
			1,
			&dwBytesRead
		);
		if(e == minorE_Ok) {
			/* Weve received the next byte ... execute processing in our compressor routines ... */
			e = minorCompressor_Deflate__ProcessByte(lpThis, nextByte);
			if((e != minorE_Ok) && (e != minorE_Finished)) {
				if((e == minorE_Suspend) && (lpBytesDone == NULL)) {
					/* TODO: Is this required or can we expect the source to implement tracking? Then the lpBytesDone would not be required? */
					/*
						In this case we are not capable of restart because we cannot
						tell the application layer that we've only processed a
						smaller amount of data and will process the rest later on
					*/
					lpThis->currentState = minorCompressor_Deflate_State__Error;
					return minorE_FailedWouldBlock;
				}
				return e;
			}

			if(lpBytesDone != NULL) { (*lpBytesDone) = (*lpBytesDone) + 1; }
			if(e == minorE_Finished) {
				/* TODO FLUSH if we implement output buffering */
				return e;
			}
			dwBytesToGo = dwBytesToGo - 1;
		} else if(e == minorE_Suspend) {
			if(lpBytesDone == NULL) {
				/* TODO: Is this required or can we expect the source to implement tracking? Then the lpBytesDone would not be required? */
				/*
					In this case we are not capable of restart because we cannot
					tell the application layer that we've only processed a
					smaller amount of data and will process the rest later on
				*/
				lpThis->currentState = minorCompressor_Deflate_State__Error;
				return minorE_FailedWouldBlock;
			}
			return minorE_Suspend;
		} else {
			return e; /* Default error case */
		}
	}
	return minorE_Ok;
}

enum minorError minorCompressor_Execute_Deflate(
	struct minorCompressor*				lpObject
) {
	enum minorError e;
	struct minorCompressor_Deflate* lpSelf;
	unsigned long int dwBytesRead;
	uint8_t nextByte;

	if(lpObject == NULL) { return minorE_InvalidParam; }

	lpSelf = (struct minorCompressor_Deflate*)lpObject;

	/*
		Check if we have attached input and output streams
	*/
	if(lpSelf->lpDataSource == NULL) { return minorE_MissingDataSource; }
	if(lpSelf->lpDataSink == NULL) { return minorE_MissingDataSink; }

	for(;;) {
		e = lpSelf->lpDataSource->read(
			lpSelf->lpDataSource,
			&nextByte,
			1,
			&dwBytesRead
		);
		if(e == minorE_Ok) {
			/* We have received the next byte, push into our state machine ... */
			e = minorCompressor_Deflate__ProcessByte(lpSelf, nextByte);
			if((e != minorE_Ok) && (e != minorE_Finished)) { return e; }

			if(e == minorE_Finished) {
				/* TODO FLUSH if we implement output buffering */
				return e;
			}
		} else if(e == minorE_Suspend) {
			return minorE_Suspend;
		} else {
			/* An error occured */
			return e;
		}
	}
}



struct minorCompressor_WriteMem__LocalSource {
	struct minorStreamSource			base;

	uint8_t*							lpBase;
	unsigned long int					dwBytesToGo;
	unsigned long int					dwCurrentOffset;
};

static enum minorError minorCompressor_WriteMem__LocalSource__Read(
	struct minorStreamSource* 		lpSelf,
	uint8_t* 						lpDestinationBuffer,
	unsigned long int				dwBytesToRead,
	unsigned long int				*lpBytesRead
) {
	struct minorCompressor_WriteMem__LocalSource* lpInfo;
	unsigned long int dwPassBytes;
	unsigned long int i;

	if(lpSelf == NULL) { return minorE_InvalidParam; }
	lpInfo = (struct minorCompressor_WriteMem__LocalSource*)lpSelf;

	if(dwBytesToRead < lpInfo->dwBytesToGo) { dwPassBytes = dwBytesToRead; } else { dwPassBytes = lpInfo->dwBytesToGo; }
	if(dwPassBytes == 0) { return minorE_EndOfStream; }

	for(i = 0; i < dwPassBytes; i=i+1) {
		lpDestinationBuffer[i] = lpInfo->lpBase[lpInfo->dwCurrentOffset];
		lpInfo->dwCurrentOffset = lpInfo->dwCurrentOffset + 1;
		lpInfo->dwBytesToGo = lpInfo->dwBytesToGo - 1;
	}

	return minorE_Ok;
}

enum minorError minorCompressor_WriteMem_Deflate(
	struct minorCompressor*				lpObjectParam,

	uint8_t* 							lpDataIn,
	unsigned long int 					dwBytesToWrite,
	unsigned long int					*dwBytesDone
) {
	enum minorError e;
	struct minorStreamSource* oldSource;

	struct minorCompressor_WriteMem__LocalSource localSource;
	struct minorCompressor_Deflate* lpObject;

	lpObject = (struct minorCompressor_Deflate*)lpObjectParam;

	if(lpObject == NULL) { return minorE_InvalidParam; }

	if(lpObject->lpDataSink == NULL) { return minorE_MissingDataSink; }
	oldSource = lpObject->lpDataSource; /* Backup if we MAY require this again later on ... */

	/* We setup our own "temporary" source ... */
	localSource.base.read = &minorCompressor_WriteMem__LocalSource__Read;
	localSource.lpBase = lpDataIn;
	localSource.dwBytesToGo = dwBytesToWrite;
	localSource.dwCurrentOffset = 0;
	lpObject->lpDataSource = (struct minorStreamSource*)&localSource;

	e = minorCompressor_TransferFromInput_Deflate(
		lpObjectParam,
		dwBytesToWrite,
		dwBytesDone
	);

	lpObject->lpDataSource = oldSource;
	return e;
}

enum minorError minorCompressor_CheckConfiguration_Deflate(
	struct minorConfigurationElement* lpConfiguration
) {
	struct minorConfigurationElement* lpCur;

	lpCur = lpConfiguration;
	while(lpCur != NULL) {
		if(lpCur->typeMajor == minorConfiguration_MajorId_Deflate) {
			if(((struct minorConfigurationElement_Deflate*)lpCur)->typeMinor == minorConfiguration_MinorID_Deflate_BackreferenceTableSize) {
				/*
					We are a compressor and want to produce deflate conforming output so our blocksize has to be
					at maximum LIBMINOR_DEFLATE__BLOCKSIZE_K
				*/
				if(((struct minorConfigurationElement_Deflate_BackreferenceTableSize*)lpCur)->slidingWindowSize > LIBMINOR_DEFLATE__BLOCKSIZE_K) {
					return minorE_InvalidParam;
				}

				lpCur->dwFlags = lpCur->dwFlags | minorConfiguration__Flag__Processed; /* Mark supported */
			} else if(((struct minorConfigurationElement_Deflate*)lpCur)->typeMinor == minorConfiguration_MinorID_Deflate_PreloadDictionary) {
				/*
					Check if there is a preload dictionary and if it fits into the first block
				*/
				if((((struct minorConfigurationElement_Deflate_PreloadDictionary*)lpCur)->lpDictionary != NULL) || (((struct minorConfigurationElement_Deflate_PreloadDictionary*)lpCur)->dwByteLength != 0)) {
					if(((struct minorConfigurationElement_Deflate_PreloadDictionary*)lpCur)->lpDictionary == NULL) { return minorE_InvalidParam; }
					if(((struct minorConfigurationElement_Deflate_PreloadDictionary*)lpCur)->dwByteLength == 0) { return minorE_InvalidParam; }

					if(((struct minorConfigurationElement_Deflate_PreloadDictionary*)lpCur)->dwByteLength > (LIBMINOR_DEFLATE__BLOCKSIZE_K*1024)) { return minorE_InvalidParam; }
				}

				lpCur->dwFlags = lpCur->dwFlags | minorConfiguration__Flag__Processed; /* Mark supported */
			} else if(((struct minorConfigurationElement_Deflate*)lpCur)->typeMinor == minorConfiguration_MinorID_Deflate_QualitySettings) {
				/* ToDo: Are there any problems with accepting arbitrary values? */
				lpCur->dwFlags = lpCur->dwFlags | minorConfiguration__Flag__Processed; /* Mark supported */
			}
		}
		lpCur = lpCur->lpNext;
	}

	return minorE_Ok;
}


#ifdef __cplusplus
	} /* extern "C" { */
#endif
