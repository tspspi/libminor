#define __is_inside_libminor_E7B72649_9D2F_4078_8EA0_5D7B61775925 1

#include <string.h> /* Used for memset */
#ifdef MINOR_GZIP_DEBUG
	#include <stdio.h>
#endif
#if 0
	#include <time.h> /* Required for file modification time default */
#endif

#ifndef MINOR_GZIP_DEFAULTSTRINGBLOCKSIZE
	#define MINOR_GZIP_DEFAULTSTRINGBLOCKSIZE 64
#endif

#include "../include/minor.h"

#ifdef __cplusplus
	extern "C" {
#endif

enum minorDecompressor_GZip_State {
	minorDecompressor_GZip_State__Header			= 0,
	minorDecompressor_GZip_State__ExtraLength		= 1,
	minorDecompressor_GZip_State__Extra				= 2,
	minorDecompressor_GZip_State__OriginalFilename	= 3,
	minorDecompressor_GZip_State__Comment			= 4,
	minorDecompressor_GZip_State__HeaderCRC16		= 5,
	minorDecompressor_GZip_State__Compressed		= 6,
	minorDecompressor_GZip_State__CRC32_ISIZE		= 7,

	minorDecompressor_GZip_State__Done				= 8,
	minorDecompressor_GZip_State__Error				= 9,
};

enum minorDecompressor_GZip_CompressionMethod {
	minorDecompressor_GZip_CompressionMethod__Deflate = 8,
};

#define minorDecompressor_GZip__Flags__FTEXT		0x01		/* The file that has been compressed was a text file (for line ending conversion) */
#define minorDecompressor_GZip__Flags__FHCRC		0x02		/* The header CRC16 is present */
#define minorDecompressor_GZip__Flags__FEXTRA		0x04		/* Extra area is present */
#define minorDecompressor_GZip__Flags__FNAME		0x08		/* The original filename is present */
#define minorDecompressor_GZip__Flags__FCOMMENT		0x10		/* Comment area is present */

#define minorDecompressor__GZip__Flags__ALLOWEDFLAGS (minorDecompressor_GZip__Flags__FTEXT | minorDecompressor_GZip__Flags__FHCRC | minorDecompressor_GZip__Flags__FEXTRA | minorDecompressor_GZip__Flags__FNAME | minorDecompressor_GZip__Flags__FCOMMENT)

/* Locally used wrapper structure */
struct minorDecompressor_GZip_SinkWrapper {
	struct minorStreamSink							base;
	struct minorDecompressor_GZip*					lpSelf;
};

struct minorDecompressor_GZip_StringPart {
	struct minorDecompressor_GZip_StringPart*		lpNext;
	unsigned long int								dwPartLength;
	unsigned long int								dwUsed;
	uint8_t											data[];
};

struct minorDecompressor_GZip {
	struct minorDecompressor						base;
	struct minorSystemInterface*					lpSystemAPI;
	struct minorConfigurationElement*				lpConfigurationRoot;

	/* Current state of our GZip file handler */
	enum minorDecompressor_GZip_State				currentState;
	unsigned long int								byteCounter; 		/* Used by many of the states that read a fixed number of bytes */

	uint8_t											flags;
	uint8_t											extraFlags;
	uint8_t											osIdentifier;
	uint8_t											modificationTimeByte[4];
	uint8_t											crcISizeBuffer[8];

	uint16_t										extraLength;
	uint32_t										crc32_Header;
	uint32_t										crc32_Payload;
	uint16_t										crc16Header;
	uint32_t										isizeCounter;

	/* Status for our original filename and comment string routines */
	struct minorDecompressor_GZip_StringPart*		lpOriginalFilenameStringChain;
	struct minorDecompressor_GZip_StringPart*		lpOriginalFilenameStringChain_Tail;
	struct minorDecompressor_GZip_StringPart*		lpCommentStringChain;
	struct minorDecompressor_GZip_StringPart*		lpCommentStringChain_Tail;
	uint8_t*										lpOriginalFilenameString;
	uint8_t*										lpCommentString;

	/* We require a decompressor for our specific algorithm */
	struct minorDecompressor*						decompressor;

	/*
		Attached stream interfaces
	*/
	struct minorStreamSource*						lpDataSource;
	struct minorStreamSink*							lpDataSink;

	/*
		Sink wrapper
	*/
	struct minorDecompressor_GZip_SinkWrapper		sinkWrapper;

	/*
		Configureable parameters
	*/
	unsigned long int														dwStringBlocksize;
	lpfnMinorConfigurationElement_GZIP_OriginalFilenameCallback_Function	cbOriginalFilename;
	void* 																	lpOriginalFilenameFreeParam;
	lpfnMinorConfigurationElement_GZIP_CommentCallback_Function 			cbComment;
	void* 																	lpCommentFreeParam;
};

/* CRC32 Helper and constants */

static unsigned long int rfc1951_crc32Table[256] = {
        0x0,0x77073096,0xee0e612c,0x990951ba,0x76dc419,0x706af48f,0xe963a535,0x9e6495a3,
        0xedb8832,0x79dcb8a4,0xe0d5e91e,0x97d2d988,0x9b64c2b,0x7eb17cbd,0xe7b82d07,0x90bf1d91,
        0x1db71064,0x6ab020f2,0xf3b97148,0x84be41de,0x1adad47d,0x6ddde4eb,0xf4d4b551,0x83d385c7,
        0x136c9856,0x646ba8c0,0xfd62f97a,0x8a65c9ec,0x14015c4f,0x63066cd9,0xfa0f3d63,0x8d080df5,
        0x3b6e20c8,0x4c69105e,0xd56041e4,0xa2677172,0x3c03e4d1,0x4b04d447,0xd20d85fd,0xa50ab56b,
        0x35b5a8fa,0x42b2986c,0xdbbbc9d6,0xacbcf940,0x32d86ce3,0x45df5c75,0xdcd60dcf,0xabd13d59,
        0x26d930ac,0x51de003a,0xc8d75180,0xbfd06116,0x21b4f4b5,0x56b3c423,0xcfba9599,0xb8bda50f,
        0x2802b89e,0x5f058808,0xc60cd9b2,0xb10be924,0x2f6f7c87,0x58684c11,0xc1611dab,0xb6662d3d,
        0x76dc4190,0x1db7106,0x98d220bc,0xefd5102a,0x71b18589,0x6b6b51f,0x9fbfe4a5,0xe8b8d433,
        0x7807c9a2,0xf00f934,0x9609a88e,0xe10e9818,0x7f6a0dbb,0x86d3d2d,0x91646c97,0xe6635c01,
        0x6b6b51f4,0x1c6c6162,0x856530d8,0xf262004e,0x6c0695ed,0x1b01a57b,0x8208f4c1,0xf50fc457,
        0x65b0d9c6,0x12b7e950,0x8bbeb8ea,0xfcb9887c,0x62dd1ddf,0x15da2d49,0x8cd37cf3,0xfbd44c65,
        0x4db26158,0x3ab551ce,0xa3bc0074,0xd4bb30e2,0x4adfa541,0x3dd895d7,0xa4d1c46d,0xd3d6f4fb,
        0x4369e96a,0x346ed9fc,0xad678846,0xda60b8d0,0x44042d73,0x33031de5,0xaa0a4c5f,0xdd0d7cc9,
        0x5005713c,0x270241aa,0xbe0b1010,0xc90c2086,0x5768b525,0x206f85b3,0xb966d409,0xce61e49f,
        0x5edef90e,0x29d9c998,0xb0d09822,0xc7d7a8b4,0x59b33d17,0x2eb40d81,0xb7bd5c3b,0xc0ba6cad,
        0xedb88320,0x9abfb3b6,0x3b6e20c,0x74b1d29a,0xead54739,0x9dd277af,0x4db2615,0x73dc1683,
        0xe3630b12,0x94643b84,0xd6d6a3e,0x7a6a5aa8,0xe40ecf0b,0x9309ff9d,0xa00ae27,0x7d079eb1,
        0xf00f9344,0x8708a3d2,0x1e01f268,0x6906c2fe,0xf762575d,0x806567cb,0x196c3671,0x6e6b06e7,
        0xfed41b76,0x89d32be0,0x10da7a5a,0x67dd4acc,0xf9b9df6f,0x8ebeeff9,0x17b7be43,0x60b08ed5,
        0xd6d6a3e8,0xa1d1937e,0x38d8c2c4,0x4fdff252,0xd1bb67f1,0xa6bc5767,0x3fb506dd,0x48b2364b,
        0xd80d2bda,0xaf0a1b4c,0x36034af6,0x41047a60,0xdf60efc3,0xa867df55,0x316e8eef,0x4669be79,
        0xcb61b38c,0xbc66831a,0x256fd2a0,0x5268e236,0xcc0c7795,0xbb0b4703,0x220216b9,0x5505262f,
        0xc5ba3bbe,0xb2bd0b28,0x2bb45a92,0x5cb36a04,0xc2d7ffa7,0xb5d0cf31,0x2cd99e8b,0x5bdeae1d,
        0x9b64c2b0,0xec63f226,0x756aa39c,0x26d930a,0x9c0906a9,0xeb0e363f,0x72076785,0x5005713,
        0x95bf4a82,0xe2b87a14,0x7bb12bae,0xcb61b38,0x92d28e9b,0xe5d5be0d,0x7cdcefb7,0xbdbdf21,
        0x86d3d2d4,0xf1d4e242,0x68ddb3f8,0x1fda836e,0x81be16cd,0xf6b9265b,0x6fb077e1,0x18b74777,
        0x88085ae6,0xff0f6a70,0x66063bca,0x11010b5c,0x8f659eff,0xf862ae69,0x616bffd3,0x166ccf45,
        0xa00ae278,0xd70dd2ee,0x4e048354,0x3903b3c2,0xa7672661,0xd06016f7,0x4969474d,0x3e6e77db,
        0xaed16a4a,0xd9d65adc,0x40df0b66,0x37d83bf0,0xa9bcae53,0xdebb9ec5,0x47b2cf7f,0x30b5ffe9,
        0xbdbdf21c,0xcabac28a,0x53b39330,0x24b4a3a6,0xbad03605,0xcdd70693,0x54de5729,0x23d967bf,
        0xb3667a2e,0xc4614ab8,0x5d681b02,0x2a6f2b94,0xb40bbe37,0xc30c8ea1,0x5a05df1b,0x2d02ef8d
};

static inline uint32_t rfc1951_CRC32Update(
	uint32_t dwCRCIn,
	uint8_t* lpBuffer,
	unsigned long int dwLength
) {
	uint32_t w;
	unsigned long int i;

	w = dwCRCIn ^ 0xFFFFFFFF;
	for(i = 0; i < dwLength; i=i+1) {
		w = rfc1951_crc32Table[(w ^ lpBuffer[i]) & 0xFF] ^ (w >> 8);
	}
	return w ^ 0xFFFFFFFF;
}
static inline uint32_t rfc1951_CRC32UpdateByte(
	uint32_t dwCRCIn,
	uint32_t data
) {
	uint32_t w;
	w = dwCRCIn ^ 0xFFFFFFFF;
	w = rfc1951_crc32Table[(w ^ data) & 0xFF] ^ (w >> 8);
	return w ^ 0xFFFFFFFF;
}

/* State machine */
static enum minorError minorGZip_ProcessByte(
	struct minorDecompressor_GZip* lpState,
	uint8_t byte
) {
	enum minorError e;
	uint32_t readCRC;
	uint32_t readISIZE;
	struct minorDecompressor_GZip_StringPart* lpStringPartNew;
	struct minorDecompressor_GZip_StringPart* lpStringPartCur;
	unsigned long int dwSize;

	if(lpState == NULL) { return minorE_InvalidParam; }

	switch(lpState->currentState) {
		case minorDecompressor_GZip_State__Header:
			#ifdef MINOR_GZIP_DEBUG
				printf("[GZIP] Header state\n");
			#endif
			/*
				In this state we are parsing the header of the GZIP File

				Bytes 0 and 1 	are identification (0x1F, 0x8B)
				Byte 2 			is the Compression Method (CM)
				Byte 3 			contains Flags (FLG)
				Bytes 4-7		Modification time (MTIME) as unix timestamp (Most recent modification time of file being compressed or the timestamp of the compression)
				Byte 8			Extra Flags (XFL)
				Byte 9			Operating system identification

				The byte counter counts upwards (byteCounter)
			*/

			/* Update header CRC (in case we have a FCRC later on) */
			lpState->crc32_Header = rfc1951_CRC32UpdateByte(lpState->crc32_Header, byte);

			if(lpState->byteCounter == 0) {
				if(byte != 0x1F) { lpState->currentState = minorDecompressor_GZip_State__Error; return minorE_EncodingError; }
			} else if(lpState->byteCounter == 1) {
				if(byte != 0x8B) { lpState->currentState = minorDecompressor_GZip_State__Error; return minorE_EncodingError; }
			} else if(lpState->byteCounter == 2) {
				/* Check if we support this compression method */
				if(byte == minorDecompressor_GZip_CompressionMethod__Deflate) {
					e = minorDecompressorCreate(
						&(lpState->decompressor),
						minorAlgorithm_Deflate,
						lpState->lpSystemAPI,
						lpState->lpConfigurationRoot
					);
					if(e != minorE_Ok) { return e; }

					if(lpState->lpDataSource != NULL) { e = minorDecompressorAttachSource(lpState->decompressor, lpState->lpDataSource); if(e != minorE_Ok) { return e; } }
					/* if(lpState->lpDataSink != NULL) { e = minorDecompressorAttachSink(lpState->decompressor, lpState->lpDataSink); if(e != minorE_Ok) { return e; } } */
					e = minorDecompressorAttachSink(lpState->decompressor, (struct minorStreamSink*)&(lpState->sinkWrapper)); if(e != minorE_Ok) { return e; }

					lpState->isizeCounter = 0;
					lpState->crc32_Payload = 0;
				} else {
					return minorE_EncodingError;
				}
			} else if(lpState->byteCounter == 3) {
				if((byte & ~(minorDecompressor__GZip__Flags__ALLOWEDFLAGS)) != 0) {
					/* There are bits set that MAY indicate new features ... so we cannot continue processing */
					return minorE_EncodingError;
				}
				lpState->flags = byte;
			} else if((lpState->byteCounter >= 4) && (lpState->byteCounter <= 7)) {
				lpState->modificationTimeByte[lpState->byteCounter-4] = byte;
			} else if(lpState->byteCounter == 8) {
				/* Extra flags */
				lpState->extraFlags = byte;
			} else if(lpState->byteCounter == 9) {
				/* Operating system identifier */
				lpState->osIdentifier = byte;

				/* Advance to next state */
				if((lpState->flags & minorDecompressor_GZip__Flags__FEXTRA) != 0) { lpState->currentState = minorDecompressor_GZip_State__ExtraLength; lpState->byteCounter = 0; }
				else if((lpState->flags & minorDecompressor_GZip__Flags__FNAME) != 0) { lpState->currentState = minorDecompressor_GZip_State__OriginalFilename; lpState->byteCounter = 0; }
				else if((lpState->flags & minorDecompressor_GZip__Flags__FCOMMENT) != 0) { lpState->currentState = minorDecompressor_GZip_State__Comment; lpState->byteCounter = 0; }
				else if((lpState->flags & minorDecompressor_GZip__Flags__FHCRC) != 0) { lpState->currentState = minorDecompressor_GZip_State__HeaderCRC16; lpState->byteCounter = 0; }
				else { lpState->currentState = minorDecompressor_GZip_State__Compressed; lpState->byteCounter = 0; }
				return minorE_Ok;
			} else {
				/* Other positions are NEVER possible */
				return minorE_ImplementationError;
			}
			lpState->byteCounter = lpState->byteCounter + 1;
			return minorE_Ok;

		case minorDecompressor_GZip_State__ExtraLength:
			#ifdef MINOR_GZIP_DEBUG
				printf("[GZIP] Extra length state\n");
			#endif

			/* Update header CRC (in case we have a FCRC later on) */
			lpState->crc32_Header = rfc1951_CRC32UpdateByte(lpState->crc32_Header, byte);

			if(lpState->byteCounter == 0) {
				lpState->byteCounter = 1;
				lpState->extraLength = ((uint16_t)byte) & 0x00FF;
				return minorE_Ok;
			}
			lpState->extraLength = lpState->extraLength | ((((uint16_t)byte) << 8) & 0xFF00);

			/* Decoded extra length, now read (or currently skip ...) extra bytes */
			lpState->byteCounter = lpState->extraLength;
			if(lpState->extraLength != 0) {
				lpState->currentState = minorDecompressor_GZip_State__Extra;
			} else {
				if((lpState->flags & minorDecompressor_GZip__Flags__FNAME) != 0) { lpState->currentState = minorDecompressor_GZip_State__OriginalFilename; lpState->byteCounter = 0; }
				else if((lpState->flags & minorDecompressor_GZip__Flags__FCOMMENT) != 0) { lpState->currentState = minorDecompressor_GZip_State__Comment; lpState->byteCounter = 0; }
				else if((lpState->flags & minorDecompressor_GZip__Flags__FHCRC) != 0) { lpState->currentState = minorDecompressor_GZip_State__HeaderCRC16; lpState->byteCounter = 0; }
				else { lpState->currentState = minorDecompressor_GZip_State__Compressed; lpState->byteCounter = 0; }
			}
			return minorE_Ok;
		case minorDecompressor_GZip_State__Extra:
			#ifdef MINOR_GZIP_DEBUG
				printf("[GZIP] Extra state\n");
			#endif

			/* Update header CRC (in case we have a FCRC later on) */
			lpState->crc32_Header = rfc1951_CRC32UpdateByte(lpState->crc32_Header, byte);

			/* Currently we skip extra fields (only Apollo filetype is possible) */
			if((lpState->byteCounter = lpState->byteCounter - 1) == 0) {
				/* We skipped all bytes. Simply advance state */
				if((lpState->flags & minorDecompressor_GZip__Flags__FNAME) != 0) { lpState->currentState = minorDecompressor_GZip_State__OriginalFilename; lpState->byteCounter = 0; }
				else if((lpState->flags & minorDecompressor_GZip__Flags__FCOMMENT) != 0) { lpState->currentState = minorDecompressor_GZip_State__Comment; lpState->byteCounter = 0; }
				else if((lpState->flags & minorDecompressor_GZip__Flags__FHCRC) != 0) { lpState->currentState = minorDecompressor_GZip_State__HeaderCRC16; lpState->byteCounter = 0; }
				else { lpState->currentState = minorDecompressor_GZip_State__Compressed; lpState->byteCounter = 0; }
			}
			return minorE_Ok;


		case minorDecompressor_GZip_State__OriginalFilename:
			/*
				The (original) filename we fetch is zero terminated.
				We have to read it byte by byte into our blockwise allocated buffer
			*/
			#ifdef MINOR_GZIP_DEBUG
				printf("[GZIP] Original filename state\n");
			#endif

			/* Update header CRC (in case we have a FCRC later on) */
			lpState->crc32_Header = rfc1951_CRC32UpdateByte(lpState->crc32_Header, byte);

			if(lpState->cbOriginalFilename == NULL) {
				/* We dont have a callback so we simply ignore the bytes ... */
				if(byte != 0x00) {
					return minorE_Ok;
				}
				/* Finished. Advance state ... */
				if((lpState->flags & minorDecompressor_GZip__Flags__FCOMMENT) != 0) { lpState->currentState = minorDecompressor_GZip_State__Comment; lpState->byteCounter = 0; }
				else if((lpState->flags & minorDecompressor_GZip__Flags__FHCRC) != 0) { lpState->currentState = minorDecompressor_GZip_State__HeaderCRC16; lpState->byteCounter = 0; }
				else { lpState->currentState = minorDecompressor_GZip_State__Compressed; lpState->byteCounter = 0; }
				return minorE_Ok;
			}

			/* We do have to do full processing of filename because the application is listening for it ... */

			if(byte == 0x00) {
				/* Done reading. Compose the full string and pass through the callback */

				/* Count bytesize of filename */
				dwSize = 0;
				lpStringPartCur = lpState->lpOriginalFilenameStringChain;
				while(lpStringPartCur != NULL) { dwSize = dwSize + lpStringPartCur->dwUsed; lpStringPartCur = lpStringPartCur->lpNext; }

				/* Allocate memory */
				if((e = lpState->lpSystemAPI->alloc((void**)(&(lpState->lpOriginalFilenameString)), dwSize + 1, lpState->lpSystemAPI->lpFreeParam_Alloc)) != minorE_Ok) { return e; }

				/* Transfer data ... */
				dwSize = 0; /* We use this as an offset pointer ... */
				lpStringPartCur = lpState->lpOriginalFilenameStringChain;
				while(lpStringPartCur != NULL) {
					memcpy(&(lpState->lpOriginalFilenameString[dwSize]), lpStringPartCur->data, lpStringPartCur->dwUsed);
					dwSize = dwSize + lpStringPartCur->dwUsed;
					lpStringPartCur = lpStringPartCur->lpNext;
				}
				lpState->lpOriginalFilenameString[dwSize] = 0x00; /* Write zero termination ... */

				/* Release our string chain */
				lpStringPartCur = lpState->lpOriginalFilenameStringChain;
				while(lpStringPartCur != NULL) {
					lpStringPartNew = lpStringPartCur->lpNext;
					lpState->lpSystemAPI->free((void*)lpStringPartCur, lpState->lpSystemAPI->lpFreeParam_Free);
					lpStringPartCur = lpStringPartNew;
				}
				lpState->lpOriginalFilenameStringChain = NULL;
				lpState->lpOriginalFilenameStringChain_Tail = NULL;

				/* Call our callback */
				lpState->cbOriginalFilename(lpState->lpOriginalFilenameString, dwSize, lpState->lpOriginalFilenameFreeParam);

				/* Finished. Advance state ... */
				if((lpState->flags & minorDecompressor_GZip__Flags__FCOMMENT) != 0) { lpState->currentState = minorDecompressor_GZip_State__Comment; lpState->byteCounter = 0; }
				else if((lpState->flags & minorDecompressor_GZip__Flags__FHCRC) != 0) { lpState->currentState = minorDecompressor_GZip_State__HeaderCRC16; lpState->byteCounter = 0; }
				else { lpState->currentState = minorDecompressor_GZip_State__Compressed; lpState->byteCounter = 0; }
				return minorE_Ok;
			}

			/* Compose string chain */
			if(lpState->lpOriginalFilenameStringChain_Tail == NULL) {
				/* We have to allocate a new segment ... */
				if((e = lpState->lpSystemAPI->alloc((void**)(&lpStringPartNew), sizeof(struct minorDecompressor_GZip_StringPart)+(lpState->dwStringBlocksize), lpState->lpSystemAPI->lpFreeParam_Alloc)) != minorE_Ok) { return e; }

				/* Initialize new part */
				lpStringPartNew->lpNext = NULL;
				lpStringPartNew->dwPartLength = lpState->dwStringBlocksize;
				lpStringPartNew->dwUsed = 0;
				memset(lpStringPartNew->data, 0, lpState->dwStringBlocksize);

				/* Attach the new string part to the old parts ... */
				if((lpStringPartCur = lpState->lpOriginalFilenameStringChain) == NULL) {
					lpState->lpOriginalFilenameStringChain = lpStringPartNew; /* Set as root */
				} else {
					while(lpStringPartCur->lpNext != NULL) { lpStringPartCur = lpStringPartCur->lpNext; }
					lpStringPartCur->lpNext = lpStringPartNew; /* Attach */
				}

				lpState->lpOriginalFilenameStringChain_Tail = lpStringPartNew;
			}

			lpState->lpOriginalFilenameStringChain_Tail->data[lpState->lpOriginalFilenameStringChain_Tail->dwUsed] = byte;
			if((lpState->lpOriginalFilenameStringChain_Tail->dwUsed = lpState->lpOriginalFilenameStringChain_Tail->dwUsed + 1) == lpState->lpOriginalFilenameStringChain_Tail->dwPartLength) {
				lpState->lpOriginalFilenameStringChain_Tail = NULL; /* Will be allocated at the next filename byte */
			}
			return minorE_Ok;

		case minorDecompressor_GZip_State__Comment:
			/*
				An additional comment which is zero terminated ...
			*/
			#ifdef MINOR_GZIP_DEBUG
				printf("[GZIP] Comment state\n");
			#endif

			/* Update header CRC (in case we have a FCRC later on) */
			lpState->crc32_Header = rfc1951_CRC32UpdateByte(lpState->crc32_Header, byte);

			if(lpState->cbComment == NULL) {
				/* We dont have a callback so we simply ignore the bytes ... */
				if(byte != 0x00) {
					return minorE_Ok;
				}
				/* Finished. Advance state ... */
				if((lpState->flags & minorDecompressor_GZip__Flags__FHCRC) != 0) { lpState->currentState = minorDecompressor_GZip_State__HeaderCRC16; lpState->byteCounter = 0; }
				else { lpState->currentState = minorDecompressor_GZip_State__Compressed; lpState->byteCounter = 0; }
				return minorE_Ok;
			}

			/* We do have to do full processing of comment because the application is listening for it ... */

			if(byte == 0x00) {
				/* Done reading. Compose the full string and pass through the callback */

				/* Count bytesize of comment */
				dwSize = 0;
				lpStringPartCur = lpState->lpCommentStringChain;
				while(lpStringPartCur != NULL) { dwSize = dwSize + lpStringPartCur->dwUsed; lpStringPartCur = lpStringPartCur->lpNext; }

				/* Allocate memory */
				if((e = lpState->lpSystemAPI->alloc((void**)(&(lpState->lpCommentString)), dwSize + 1, lpState->lpSystemAPI->lpFreeParam_Alloc)) != minorE_Ok) { return e; }

				/* Transfer data ... */
				dwSize = 0; /* We use this as an offset pointer ... */
				lpStringPartCur = lpState->lpCommentStringChain;
				while(lpStringPartCur != NULL) {
					memcpy(&(lpState->lpCommentString[dwSize]), lpStringPartCur->data, lpStringPartCur->dwUsed);
					dwSize = dwSize + lpStringPartCur->dwUsed;
					lpStringPartCur = lpStringPartCur->lpNext;
				}
				lpState->lpCommentString[dwSize] = 0x00; /* Write zero termination ... */

				/* Release our string chain */
				lpStringPartCur = lpState->lpCommentStringChain;
				while(lpStringPartCur != NULL) {
					lpStringPartNew = lpStringPartCur->lpNext;
					lpState->lpSystemAPI->free((void*)lpStringPartCur, lpState->lpSystemAPI->lpFreeParam_Free);
					lpStringPartCur = lpStringPartNew;
				}
				lpState->lpCommentStringChain = NULL;
				lpState->lpCommentStringChain_Tail = NULL;

				/* Call our callback */
				lpState->cbComment(lpState->lpCommentString, dwSize, lpState->lpCommentFreeParam);

				/* Finished. Advance state ... */
				if((lpState->flags & minorDecompressor_GZip__Flags__FHCRC) != 0) { lpState->currentState = minorDecompressor_GZip_State__HeaderCRC16; lpState->byteCounter = 0; }
				else { lpState->currentState = minorDecompressor_GZip_State__Compressed; lpState->byteCounter = 0; }
				return minorE_Ok;
			}

			/* Compose string chain */
			if(lpState->lpCommentStringChain_Tail == NULL) {
				/* We have to allocate a new segment ... */
				if((e = lpState->lpSystemAPI->alloc((void**)(&lpStringPartNew), sizeof(struct minorDecompressor_GZip_StringPart)+(lpState->dwStringBlocksize), lpState->lpSystemAPI->lpFreeParam_Alloc)) != minorE_Ok) { return e; }

				/* Initialize new part */
				lpStringPartNew->lpNext = NULL;
				lpStringPartNew->dwPartLength = lpState->dwStringBlocksize;
				lpStringPartNew->dwUsed = 0;
				memset(lpStringPartNew->data, 0, lpState->dwStringBlocksize);

				/* Attach the new string part to the old parts ... */
				if((lpStringPartCur = lpState->lpCommentStringChain) == NULL) {
					lpState->lpCommentStringChain = lpStringPartNew; /* Set as root */
				} else {
					while(lpStringPartCur->lpNext != NULL) { lpStringPartCur = lpStringPartCur->lpNext; }
					lpStringPartCur->lpNext = lpStringPartNew; /* Attach */
				}

				lpState->lpCommentStringChain_Tail = lpStringPartNew;
			}

			lpState->lpCommentStringChain_Tail->data[lpState->lpCommentStringChain_Tail->dwUsed] = byte;
			if((lpState->lpCommentStringChain_Tail->dwUsed = lpState->lpCommentStringChain_Tail->dwUsed + 1) == lpState->lpCommentStringChain_Tail->dwPartLength) {
				lpState->lpCommentStringChain_Tail = NULL; /* Will be allocated at the next filename byte */
			}
			return minorE_Ok;

		case minorDecompressor_GZip_State__HeaderCRC16:
			#ifdef MINOR_GZIP_DEBUG
				printf("[GZIP] Header CRC16 state\n");
			#endif
			if(lpState->byteCounter == 0) {
				lpState->crc16Header = ((uint16_t)byte) & 0xFF;
				return minorE_Ok;
			}
			lpState->crc16Header = lpState->crc16Header | ((((uint16_t)byte) << 8) & 0xFF00);

			if(lpState->crc16Header != ((uint16_t)(lpState->crc32_Header & 0xFFFF))) {
				#ifdef MINOR_GZIP_DEBUG
					printf("[GZIP]: %s:%u CRC16 header mismatch (%08x mismatches %04x)\n", __FILE__, __LINE__, lpState->crc32_Header, lpState->crc16Header);
				#endif
				return minorE_EncodingError;
			}
			#ifdef MINOR_GZIP_DEBUG
				else {
					printf("[GZIP]: CRC16 header ok\n");
				}
			#endif

			lpState->currentState = minorDecompressor_GZip_State__Compressed;
			return minorE_Ok;

		case minorDecompressor_GZip_State__Compressed:
			/* Pass compressed data through to underlying decompressor ... */
			#ifdef MINOR_GZIP_DEBUG
				printf("[GZIP] Compressed data state\n");
			#endif
			e = minorDecompressor_WriteMem(lpState->decompressor, &byte, 1, NULL);
			if(e == minorE_Finished) {
				/* We have done the deflate decompression .. now we enter CRC32 and ISIZE state */
				lpState->byteCounter = 0;
				lpState->currentState = minorDecompressor_GZip_State__CRC32_ISIZE;
				return minorE_Ok;
			}
			return e;

		case minorDecompressor_GZip_State__CRC32_ISIZE:
			#ifdef MINOR_GZIP_DEBUG
				printf("[GZIP] CRC32 and ISIZE state\n");
			#endif
			lpState->crcISizeBuffer[lpState->byteCounter] = byte;

			if((lpState->byteCounter = lpState->byteCounter + 1) == 8) {
				readCRC =
					((((uint32_t)(lpState->crcISizeBuffer[0])) & 0xFF) <<  0) |
					((((uint32_t)(lpState->crcISizeBuffer[1])) & 0xFF) <<  8) |
					((((uint32_t)(lpState->crcISizeBuffer[2])) & 0xFF) << 16) |
					((((uint32_t)(lpState->crcISizeBuffer[3])) & 0xFF) << 24);
				readISIZE =
					((((uint32_t)(lpState->crcISizeBuffer[4])) & 0xFF) <<  0) |
					((((uint32_t)(lpState->crcISizeBuffer[5])) & 0xFF) <<  8) |
					((((uint32_t)(lpState->crcISizeBuffer[6])) & 0xFF) << 16) |
					((((uint32_t)(lpState->crcISizeBuffer[7])) & 0xFF) << 24);
				#ifdef MINOR_GZIP_DEBUG
					printf("[GZIP] Read CRC32: %02x %02x %02x %02x %08x; Calculated CRC32: %08x\n", lpState->crcISizeBuffer[0], lpState->crcISizeBuffer[1], lpState->crcISizeBuffer[2], lpState->crcISizeBuffer[3], readCRC, lpState->crc32_Payload);
					printf("[GZIP] Read ISIZE: %02x %02x %02x %02x %u; Counted ISIZE: %u\n", lpState->crcISizeBuffer[4], lpState->crcISizeBuffer[5], lpState->crcISizeBuffer[6], lpState->crcISizeBuffer[7], readISIZE, lpState->isizeCounter);
				#endif
				if(readISIZE != lpState->isizeCounter) {
					lpState->currentState = minorDecompressor_GZip_State__Error;
					return minorE_EncodingError;
				}
				if(readCRC != lpState->crc32_Payload) {
					lpState->currentState = minorDecompressor_GZip_State__Error;
					return minorE_CRCError;
				}
				lpState->currentState = minorDecompressor_GZip_State__Done;
				return minorE_Finished;
			} else {
				return minorE_Ok;
			}

		case minorDecompressor_GZip_State__Done:
			#ifdef MINOR_GZIP_DEBUG
				printf("[GZIP] Done state\n");
			#endif
			return minorE_Finished;

		case minorDecompressor_GZip_State__Error:
			#ifdef MINOR_GZIP_DEBUG
				printf("[GZIP] Error state\n");
			#endif
			return minorE_AlreadyFailed;

		default: /* We can NEVER enter an undefined state ... */
			#ifdef MINOR_GZIP_DEBUG
				printf("[GZIP] Undefined state %u\n", lpState->currentState);
			#endif
			return minorE_ImplementationError;
	}
}


/*
	Sink wrapper in case we want to calculate CRC32 and ISIZE
	for validation
*/

static enum minorError minorDecompressor_SinkWrapper_Write(
	struct minorStreamSink*			lpSelf,
	uint8_t*						lpSourceBuffer,
	unsigned long int				dwBytesToWrite,
	unsigned long int				*lpBytesWritten
) {
	struct minorDecompressor_GZip* lpThis;
	enum minorError e;

	/* Recover real "self" pointer */
	lpThis = ((struct minorDecompressor_GZip_SinkWrapper*)lpSelf)->lpSelf;

	/* Update ISIZE and CRC32 before passthrough */
	if(lpBytesWritten == NULL) {
		#ifdef MINOR_GZIP_DEBUG
			printf("[GZIP] Writer that cannot reject data\n");
		#endif
		lpThis->isizeCounter = lpThis->isizeCounter + dwBytesToWrite;
		lpThis->crc32_Payload = rfc1951_CRC32Update(lpThis->crc32_Payload, lpSourceBuffer, dwBytesToWrite);
	}

	/* Passthrough to attached sink */
	e = lpThis->lpDataSink->write(lpThis->lpDataSink, lpSourceBuffer, dwBytesToWrite, lpBytesWritten);

	/* Update ISIZE and CRC32 after passthrough */
	if(lpBytesWritten != NULL) {
		#ifdef MINOR_GZIP_DEBUG
			printf("[GZIP] Writer can reject data\n");
		#endif
		lpThis->isizeCounter = lpThis->isizeCounter + (*lpBytesWritten);
		lpThis->crc32_Payload = rfc1951_CRC32Update(lpThis->crc32_Payload, lpSourceBuffer, (*lpBytesWritten));
	}

	return e;
}
static enum minorError minorDecompressor_SinkWrapper_Flush(
	struct minorStreamSink*			lpSelf
) {
	struct minorDecompressor_GZip* lpThis;

	/* Recover real "self" pointer */
	lpThis = ((struct minorDecompressor_GZip_SinkWrapper*)lpSelf)->lpSelf;

	/* Passthrough to attached sink */
	return lpThis->lpDataSink->flush(lpThis->lpDataSink);
}



/*
	Public API called from outside via our multiplexer
*/
enum minorError minorDecompressorCreate_GZip(
	struct minorDecompressor** 			lpOut,
	enum minorAlgorithm					algorithm,
	struct minorSystemInterface*		lpSystem,
	struct minorConfigurationElement*	lpConfiguration
) {
	enum minorError e;
	struct minorConfigurationElement* lpCurrentConf = NULL;

	unsigned long int dwStringBlocksize 													= MINOR_GZIP_DEFAULTSTRINGBLOCKSIZE;
	lpfnMinorConfigurationElement_GZIP_OriginalFilenameCallback_Function cbOriginalFilename	= NULL;
	void* lpOriginalFilenameCallbackFreeParam 												= NULL;
	lpfnMinorConfigurationElement_GZIP_CommentCallback_Function cbComment 					= NULL;
	void* lpCommentCallbackFreeParam														= NULL;

	if(lpOut == NULL) { return minorE_InvalidParam; }
	(*lpOut) = NULL;

	if(lpSystem == NULL) { return minorE_InvalidParam; }
	if(algorithm != minorAlgorithm_Gzip) { return minorE_InvalidParam; }

	/* Validate all passed configuration options */
	if((e = minorDecompressor_CheckConfiguration_GZip(lpConfiguration)) != minorE_Ok) { return e; }

	/*
		Load configuration options ...
	*/
	lpCurrentConf = lpConfiguration;
	while(lpCurrentConf != NULL) {
		if(lpCurrentConf->typeMajor == minorConfiguration_MajorId_GZip) {
			/* GZip specific configurations */
			if(((struct minorConfigurationElement_GZIP*)lpCurrentConf)->typeMinor == minorConfiguration_MinorID_GZIP_StringBlocksize) {
				dwStringBlocksize = ((struct minorConfigurationElement_GZIP_StringBlocksize*)lpCurrentConf)->blockSize;
			} else if(((struct minorConfigurationElement_GZIP*)lpCurrentConf)->typeMinor == minorConfiguration_MinorID_GZIP_OriginalFilenameCallback) {
				cbOriginalFilename = ((struct minorConfigurationElement_GZIP_OriginalFilenameCallback*)lpCurrentConf)->lpfnCallback;
				lpOriginalFilenameCallbackFreeParam = ((struct minorConfigurationElement_GZIP_OriginalFilenameCallback*)lpCurrentConf)->lpFreeParam;
			} else if(((struct minorConfigurationElement_GZIP*)lpCurrentConf)->typeMinor == minorConfiguration_MinorID_GZIP_CommentCallback) {
				cbComment = ((struct minorConfigurationElement_GZIP_CommentCallback*)lpCurrentConf)->lpfnCallback;
				lpCommentCallbackFreeParam = ((struct minorConfigurationElement_GZIP_CommentCallback*)lpCurrentConf)->lpFreeParam;
			}
		}
	}

	/* Check function pointers in system API are set */
	if((lpSystem->alloc == NULL) || (lpSystem->free == NULL)) {
		return minorE_InvalidParam;
	}

	/* Allocate statemachine and initialize ... */
	e = lpSystem->alloc((void**)lpOut, sizeof(struct minorDecompressor_GZip), lpSystem->lpFreeParam_Alloc);
	if(e != minorE_Ok) { return e; }

	/* Initialize structure */
	(*((struct minorDecompressor_GZip**)(lpOut)))->base.algorithm 					= minorAlgorithm_Gzip;
	(*((struct minorDecompressor_GZip**)(lpOut)))->lpSystemAPI 						= lpSystem;
	(*((struct minorDecompressor_GZip**)(lpOut)))->lpConfigurationRoot 				= lpConfiguration;

	(*((struct minorDecompressor_GZip**)(lpOut)))->currentState 					= minorDecompressor_GZip_State__Header;
	(*((struct minorDecompressor_GZip**)(lpOut)))->byteCounter 						= 0;

	(*((struct minorDecompressor_GZip**)(lpOut)))->flags 							= 0;
	(*((struct minorDecompressor_GZip**)(lpOut)))->extraFlags 						= 0;
	(*((struct minorDecompressor_GZip**)(lpOut)))->osIdentifier 					= 0;
	(*((struct minorDecompressor_GZip**)(lpOut)))->modificationTimeByte[0] 			= 0;
	(*((struct minorDecompressor_GZip**)(lpOut)))->modificationTimeByte[1] 			= 0;
	(*((struct minorDecompressor_GZip**)(lpOut)))->modificationTimeByte[2] 			= 0;
	(*((struct minorDecompressor_GZip**)(lpOut)))->modificationTimeByte[3] 			= 0;
	(*((struct minorDecompressor_GZip**)(lpOut)))->extraLength 						= 0;
	(*((struct minorDecompressor_GZip**)(lpOut)))->crc16Header 						= 0;
	(*((struct minorDecompressor_GZip**)(lpOut)))->crc32_Header 					= 0;
	(*((struct minorDecompressor_GZip**)(lpOut)))->crc32_Payload 					= 0;
	(*((struct minorDecompressor_GZip**)(lpOut)))->decompressor 					= NULL;

	(*((struct minorDecompressor_GZip**)(lpOut)))->sinkWrapper.base.write 			= &minorDecompressor_SinkWrapper_Write;
	(*((struct minorDecompressor_GZip**)(lpOut)))->sinkWrapper.base.flush 			= &minorDecompressor_SinkWrapper_Flush;
	(*((struct minorDecompressor_GZip**)(lpOut)))->sinkWrapper.lpSelf 				= (struct minorDecompressor_GZip*)(*lpOut);

	(*((struct minorDecompressor_GZip**)(lpOut)))->lpOriginalFilenameStringChain 	= NULL;
	(*((struct minorDecompressor_GZip**)(lpOut)))->lpOriginalFilenameStringChain_Tail = NULL;
	(*((struct minorDecompressor_GZip**)(lpOut)))->lpCommentStringChain 			= NULL;
	(*((struct minorDecompressor_GZip**)(lpOut)))->lpCommentStringChain_Tail		= NULL;
	(*((struct minorDecompressor_GZip**)(lpOut)))->lpOriginalFilenameString			= NULL;
	(*((struct minorDecompressor_GZip**)(lpOut)))->lpCommentString					= NULL;

	(*((struct minorDecompressor_GZip**)(lpOut)))->lpDataSource 					= NULL;
	(*((struct minorDecompressor_GZip**)(lpOut)))->lpDataSink 						= NULL;

	(*((struct minorDecompressor_GZip**)(lpOut)))->dwStringBlocksize 				= dwStringBlocksize;
	(*((struct minorDecompressor_GZip**)(lpOut)))->cbOriginalFilename 				= cbOriginalFilename;
	(*((struct minorDecompressor_GZip**)(lpOut)))->cbComment 						= cbComment;
	(*((struct minorDecompressor_GZip**)(lpOut)))->lpOriginalFilenameFreeParam 		= lpOriginalFilenameCallbackFreeParam;
	(*((struct minorDecompressor_GZip**)(lpOut)))->lpCommentFreeParam 				= lpCommentCallbackFreeParam;

	return minorE_Ok;
}

enum minorError minorDecompressorRelease_GZip(
	struct minorDecompressor* lpObject
) {
	struct minorSystemInterface* lpSys;
	struct minorDecompressor_GZip_StringPart* lpStringPart;
	struct minorDecompressor_GZip_StringPart* lpStringPartNext;

	if(lpObject == NULL) { return minorE_Ok; }

	lpSys = ((struct minorDecompressor_GZip*)lpObject)->lpSystemAPI;

	/* Release decompressor (this function accepts NULL as input parameter ...) */
	minorDecompressorRelease(((struct minorDecompressor_GZip*)lpObject)->decompressor); ((struct minorDecompressor_GZip*)lpObject)->decompressor = NULL;

	/* Release allocated string chains (Original filename and comment string) */
	lpStringPartNext = ((struct minorDecompressor_GZip*)lpObject)->lpOriginalFilenameStringChain;
	while(lpStringPartNext != NULL) { lpStringPart = lpStringPartNext; lpStringPartNext = lpStringPart->lpNext; lpSys->free((void*)lpStringPart, lpSys->lpFreeParam_Free); }
	((struct minorDecompressor_GZip*)lpObject)->lpOriginalFilenameStringChain = NULL; ((struct minorDecompressor_GZip*)lpObject)->lpOriginalFilenameStringChain_Tail = NULL;

	lpStringPartNext = ((struct minorDecompressor_GZip*)lpObject)->lpCommentStringChain;
	while(lpStringPartNext != NULL) { lpStringPart = lpStringPartNext; lpStringPartNext = lpStringPart->lpNext; lpSys->free((void*)lpStringPart, lpSys->lpFreeParam_Free); }
	((struct minorDecompressor_GZip*)lpObject)->lpCommentStringChain = NULL; ((struct minorDecompressor_GZip*)lpObject)->lpCommentStringChain_Tail = NULL;

	/* release allocated strings */
	if(((struct minorDecompressor_GZip*)lpObject)->lpOriginalFilenameString != NULL) { lpSys->free((void*)(((struct minorDecompressor_GZip*)lpObject)->lpOriginalFilenameString), lpSys->lpFreeParam_Free); ((struct minorDecompressor_GZip*)lpObject)->lpOriginalFilenameString = NULL; }
	if(((struct minorDecompressor_GZip*)lpObject)->lpCommentString != NULL) { lpSys->free((void*)(((struct minorDecompressor_GZip*)lpObject)->lpCommentString), lpSys->lpFreeParam_Free); ((struct minorDecompressor_GZip*)lpObject)->lpCommentString = NULL; }

	/* Release our own datablock ... */
	lpSys->free((void*)lpObject, lpSys->lpFreeParam_Free);
	return minorE_Ok;
}

enum minorError minorDecompressorAttachSource_GZip(
	struct minorDecompressor*			lpObject,
	struct minorStreamSource*			lpSource
) {
	if(lpObject == NULL) { return minorE_InvalidParam; }

	((struct minorDecompressor_GZip*)lpObject)->lpDataSource = lpSource;
	return minorE_Ok;
}

enum minorError minorDecompressorAttachSink_GZip(
	struct minorDecompressor*			lpObject,
	struct minorStreamSink*				lpSink
) {
	if(lpObject == NULL) { return minorE_InvalidParam; }

	((struct minorDecompressor_GZip*)lpObject)->lpDataSink = lpSink;
	return minorE_Ok;
}

enum minorError minorDecompressor_TransferFromInput_GZip(
	struct minorDecompressor*			lpObject,
	unsigned long int 					dwBytesToRead,
	unsigned long int					*lpBytesDone
) {
	enum minorError e;
	struct minorDecompressor_GZip* lpSelf;
	unsigned long int dwBytesToGo;
	unsigned long int dwBytesRead;
	uint8_t nextByte;

	if(lpObject == NULL) { return minorE_InvalidParam; }

	lpSelf = (struct minorDecompressor_GZip*)lpObject;

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
	while(dwBytesToGo > 0) {
		e = lpSelf->lpDataSource->read(
			lpSelf->lpDataSource,
			&nextByte,
			1,
			&dwBytesRead
		);
		if(e == minorE_Ok) {
			/* We have received the next byte, push into our state machine ... */
			e = minorGZip_ProcessByte(lpSelf, nextByte);
			if((e != minorE_Ok) && (e != minorE_Finished)) { return e; }

			if(lpBytesDone != NULL) { (*lpBytesDone) = (*lpBytesDone) + 1; }
			if(e == minorE_Finished) { return e; }
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

enum minorError minorDecompressor_Execute_GZip(
	struct minorDecompressor*			lpObject
) {
	enum minorError e;
	struct minorDecompressor_GZip* lpSelf;
	unsigned long int dwBytesRead;
	uint8_t nextByte;

	if(lpObject == NULL) { return minorE_InvalidParam; }

	lpSelf = (struct minorDecompressor_GZip*)lpObject;

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
			e = minorGZip_ProcessByte(lpSelf, nextByte);
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

enum minorError minorDecompressor_WriteMem_GZip(
	struct minorDecompressor*			lpObjectParam,

	uint8_t* 							lpDataIn,
	unsigned long int 					dwBytesToWrite,
	unsigned long int					*dwBytesDone
) {
	enum minorError e;
	struct minorStreamSource* oldSource;

	struct minorDecompressor_WriteMem__LocalSource localSource;
	struct minorDecompressor_GZip* lpObject;

	lpObject = (struct minorDecompressor_GZip*)lpObjectParam;

	if(lpObject == NULL) { return minorE_InvalidParam; }

	if(lpObject->lpDataSink == NULL) { return minorE_MissingDataSink; }
	oldSource = lpObject->lpDataSource; /* Backup if we MAY require this again later on ... */

	/* We setup our own "temporary" source ... */
	localSource.base.read = &minorDecompressor_WriteMem__LocalSource__Read;
	localSource.lpBase = lpDataIn;
	localSource.dwBytesToGo = dwBytesToWrite;
	localSource.dwCurrentOffset = 0;
	lpObject->lpDataSource = (struct minorStreamSource*)&localSource;

	e = minorDecompressor_TransferFromInput_GZip(
		lpObjectParam,
		dwBytesToWrite,
		dwBytesDone
	);

	lpObject->lpDataSource = oldSource;
	return e;
}

enum minorError minorDecompressor_CheckConfiguration_GZip(
	struct minorConfigurationElement* lpConfiguration
) {
	struct minorConfigurationElement* lpCurrentElement = lpConfiguration;
	struct minorConfigurationElement_GZIP* lpCurrentElement_GZIP;
	enum minorError e;

	if(lpConfiguration == NULL) { return minorE_Ok; }

	while(lpCurrentElement != NULL) {
		/*
			Check if it's our major ID. We can only validate settings that are specific to our
			module and that have not been processed until now
		*/
		if((lpCurrentElement->typeMajor == minorConfiguration_MajorId_GZip) && ((lpCurrentElement->dwFlags & minorConfiguration__Flag__Processed) == 0)) {
			lpCurrentElement_GZIP = (struct minorConfigurationElement_GZIP*)lpCurrentElement;
			switch(lpCurrentElement_GZIP->typeMinor) {
				case minorConfiguration_MinorID_GZIP_StringBlocksize:
					if(((struct minorConfigurationElement_GZIP_StringBlocksize*)lpCurrentElement_GZIP)->blockSize == 0) {
						return minorE_InvalidParam;
					}
					lpCurrentElement->dwFlags = lpCurrentElement->dwFlags | minorConfiguration__Flag__Processed; /* Mark supported */
					break;
				case minorConfiguration_MinorID_GZIP_OriginalFilenameCallback:
					lpCurrentElement->dwFlags = lpCurrentElement->dwFlags | minorConfiguration__Flag__Processed; /* Mark supported */
					break;
				case minorConfiguration_MinorID_GZIP_CommentCallback:
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

	/* We also support configuration options of deflate in case a deflate block is encountered */
	e = minorDecompressor_CheckConfiguration_Deflate(lpConfiguration);
	if(e != minorE_Ok) { return e; }

	/* Done */
	return minorE_Ok;
}

/*
	=================
	=	Compressor	=
	=================

	The following file area contains only code that is used during compression
*/

enum minorCompressor_GZip__State {
	minorCompressor_GZip__State__Header,
	minorCompressor_GZip__State__Extra,
	minorCompressor_GZip__State__Filename,
	minorCompressor_GZip__State__Comment,
	minorCompressor_GZip__State__HeaderCRC,
	minorCompressor_GZip__State__CompressedData,
	minorCompressor_GZip__State__CRC32AndIsize,
	minorCompressor_GZip__State__Finished
};

/*
	Attached stream source and sink wrapper structures
*/
struct minorCompressor_GZip__StreamSourceWrapper {
	struct minorStreamSource								base;
	struct minorCompressor_GZip*							lpCompressor;
};
struct minorCompressor_GZip__StreamSinkWrapper {
	struct minorStreamSink									base;
	struct minorCompressor_GZip*							lpCompressor;
};

/*
	Global compressor wrapper state
*/
struct minorCompressor_GZip {
	struct minorCompressor									base; /* Base structure that encodes the type of the compressor. Everything else is opaque to the application using our compressor */

	struct minorSystemInterface*							lpSystemAPI;
	uint32_t												dwFlags;

	/* Configuration options */
	uint32_t												cfgModificationTime;
	uint32_t												cfgFlags;
	enum minorConfigurationElement_GZIP_OperatingSystem 	cfgOperatingSystem;
	enum minorConfigurationElement_GZIP_CompressionMethod	cfgCompressionMethod;

	unsigned long int										cfgCommentLen;
	unsigned long int										cfgOriginalFilenameLen;
	char*													cfgComment;				/* Do NOT DEALLOCATE. Contained in state block */
	char*													cfgOriginalFilename;	/* Do NOT DEALLOCATE. Contained in state block */

	struct minorStreamSink*									lpDataSink;
	struct minorStreamSource*								lpDataSource;

	enum minorCompressor_GZip__State						state;
	unsigned long int										dwStageWritten;
	unsigned long int										dwUncompressedDataSize;
	unsigned long int										dwOutputWritten;
	uint32_t												crc16Header; /* Note: Calculated as CRC32 .... */
	uint32_t												crc32Data;

	struct minorCompressor*									lpAttachedCompressor;

	struct minorCompressor_GZip__StreamSourceWrapper		attachedSourceWrapper;
	struct minorCompressor_GZip__StreamSinkWrapper			attachedSinkWrapper;

	/* Following the structure is some binary payload */
	char													bBinaryPayload[];
};

/*
	Stream wrapper functions
*/
static enum minorError minorCompressor_GZip__StreamSourceWrapper__Read(
	struct minorStreamSource* 		lpSelf,
	uint8_t* 						lpDestinationBuffer,
	unsigned long int				dwBytesToRead,
	unsigned long int				*lpBytesRead
) {
	struct minorCompressor_GZip__StreamSourceWrapper* lpWrapper;
	enum minorError e;

	if(lpBytesRead != NULL) { (*lpBytesRead) = 0; }
	if(lpSelf == NULL) { return minorE_InvalidParam; }

	lpWrapper = (struct minorCompressor_GZip__StreamSourceWrapper*)lpSelf;

	e = lpWrapper->lpCompressor->lpDataSource->read(
		lpWrapper->lpCompressor->lpDataSource,
		lpDestinationBuffer,
		dwBytesToRead,
		lpBytesRead
	);

	if(lpBytesRead != NULL) {
		lpWrapper->lpCompressor->dwUncompressedDataSize = lpWrapper->lpCompressor->dwUncompressedDataSize + (*lpBytesRead);
		if((*lpBytesRead) > 0) {
			lpWrapper->lpCompressor->crc32Data = rfc1951_CRC32Update(lpWrapper->lpCompressor->crc32Data, lpDestinationBuffer, (*lpBytesRead));
		}
	} else {
		if(e == minorE_Ok) {
			lpWrapper->lpCompressor->dwUncompressedDataSize = lpWrapper->lpCompressor->dwUncompressedDataSize + dwBytesToRead;
			lpWrapper->lpCompressor->crc32Data = rfc1951_CRC32Update(lpWrapper->lpCompressor->crc32Data, lpDestinationBuffer, dwBytesToRead);
		}
	}

	return e;
}




static enum minorError minorCompressor_ContinueHeader_GZip(
	struct minorCompressor_GZip* lpThis,
	unsigned long int dwMaxBytesToWrite,
	unsigned long int* dwBytesWrittenResult
) {
	enum minorError e;
	unsigned long int dwBytesWritten;
	union {
		uint8_t headerBytes[10];
		uint8_t trailerBytes[8];
		uint8_t bTemp;
	} bTemp;

	if(dwBytesWrittenResult != NULL) { (*dwBytesWrittenResult) = 0; }

	if(lpThis == NULL) { return minorE_InvalidParam; }

	if(lpThis->state == minorCompressor_GZip__State__CompressedData) { return minorE_Ok; }
	if(lpThis->state == minorCompressor_GZip__State__Finished) { return minorE_Finished; }

	for(;;) {
		if(lpThis->state == minorCompressor_GZip__State__Header) {
			/* Build temporary copy of header */

			/* ID1, ID2 */
			bTemp.headerBytes[0] = 0x1F;
			bTemp.headerBytes[1] = 0x8B;

			/* CM */
			switch(lpThis->cfgCompressionMethod) {
				case minorConfigurationElement_GZIP_CompressionMethod__Deflate:
					bTemp.headerBytes[2] = 8;
					break;
				default:
					return minorE_ImplementationError; /* Should never be called with anything not allowed */
			}

			/* FLG */
			bTemp.headerBytes[3] = 0;
			if((lpThis->cfgFlags & minorConfigurationElement_GZIP_CompressionSettings__FLAG__SetASCIIHint) != 0) { bTemp.headerBytes[3] = bTemp.headerBytes[3] | 0x01; }
			if((lpThis->cfgFlags & minorConfigurationElement_GZIP_CompressionSettings__FLAG__EnableHeaderCRC16) != 0) { bTemp.headerBytes[3] = bTemp.headerBytes[3] | 0x02; }
			if(lpThis->cfgOriginalFilenameLen > 0) { bTemp.headerBytes[3] = bTemp.headerBytes[3] | 0x08;  }
			if(lpThis->cfgCommentLen > 0) { bTemp.headerBytes[3] = bTemp.headerBytes[3] | 0x10;  }

			/* MTIME */
			bTemp.headerBytes[4] = (uint8_t)((lpThis->cfgModificationTime >>  0) & 0xFF);
			bTemp.headerBytes[5] = (uint8_t)((lpThis->cfgModificationTime >>  8) & 0xFF);
			bTemp.headerBytes[6] = (uint8_t)((lpThis->cfgModificationTime >> 16) & 0xFF);
			bTemp.headerBytes[7] = (uint8_t)((lpThis->cfgModificationTime >> 24) & 0xFF);

			/* XFL */
			bTemp.headerBytes[8] = 0;

			/* OS */
			bTemp.headerBytes[9] = ((uint8_t)(lpThis->cfgOperatingSystem));

			if(lpThis->dwStageWritten == 0) {
				lpThis->crc16Header = 0;
			}

			/*
				Write header bytes (honor any already written ones) to
				the stream sink
			*/
			dwBytesWritten = 0;
			e = lpThis->lpDataSink->write(
				lpThis->lpDataSink,
				&(bTemp.headerBytes[lpThis->dwStageWritten]),
				10-lpThis->dwStageWritten,
				&dwBytesWritten
			);
			if(dwBytesWrittenResult != NULL) { (*dwBytesWrittenResult) = (*dwBytesWrittenResult) + dwBytesWritten; }
			if((dwBytesWritten > 0) && ((lpThis->cfgFlags & minorConfigurationElement_GZIP_CompressionSettings__FLAG__EnableHeaderCRC16) != 0)) {
				/* CRC Update */
				lpThis->crc16Header = rfc1951_CRC32Update(lpThis->crc16Header, &(bTemp.headerBytes[lpThis->dwStageWritten]), dwBytesWritten);
			}
			lpThis->dwStageWritten = lpThis->dwStageWritten + dwBytesWritten;
			if(lpThis->dwStageWritten == 10) {
				if(lpThis->cfgOriginalFilenameLen > 0) {
					lpThis->state = minorCompressor_GZip__State__Filename;
					lpThis->dwStageWritten = 0;
				} else if(lpThis->cfgCommentLen > 0) {
					lpThis->state = minorCompressor_GZip__State__Comment;
					lpThis->dwStageWritten = 0;
				} else if((lpThis->cfgFlags & minorConfigurationElement_GZIP_CompressionSettings__FLAG__EnableHeaderCRC16) != 0) {
					lpThis->state = minorCompressor_GZip__State__HeaderCRC;
					lpThis->dwStageWritten = 0;
				} else {
					lpThis->state = minorCompressor_GZip__State__CompressedData;
					lpThis->dwStageWritten = 0;
					lpThis->dwUncompressedDataSize = 0;
					lpThis->crc32Data = 0;
				}
			}

			if(e != minorE_Ok) { return e; }

			if(lpThis->state == minorCompressor_GZip__State__Header) { continue; } /* The last write was only partial but successful. Retry ... */
		}

		if(lpThis->state == minorCompressor_GZip__State__Extra) {
			return minorE_ImplementationError; /* Unsupported */
		}

		if(lpThis->state == minorCompressor_GZip__State__Filename) {
			/* Zero terminated filename */
			while(lpThis->dwStageWritten < lpThis->cfgOriginalFilenameLen) {
				dwBytesWritten = 0;
				e = lpThis->lpDataSink->write(
					lpThis->lpDataSink,
					(uint8_t*)(&(lpThis->cfgOriginalFilename[lpThis->dwStageWritten])),
					lpThis->cfgOriginalFilenameLen - lpThis->dwStageWritten,
					&dwBytesWritten
				);
				if(dwBytesWrittenResult != NULL) { (*dwBytesWrittenResult) = (*dwBytesWrittenResult) + dwBytesWritten; }
				if((dwBytesWritten > 0) && ((lpThis->cfgFlags & minorConfigurationElement_GZIP_CompressionSettings__FLAG__EnableHeaderCRC16) != 0)) {
					/* CRC Update */
					lpThis->crc16Header = rfc1951_CRC32Update(lpThis->crc16Header, (uint8_t*)&(lpThis->cfgOriginalFilename[lpThis->dwStageWritten]), dwBytesWritten);
				}
				lpThis->dwStageWritten = lpThis->dwStageWritten + dwBytesWritten;

				if(e != minorE_Ok) { return e; }
			}

			dwBytesWritten = 0;
			bTemp.bTemp = 0;
			e = lpThis->lpDataSink->write(
				lpThis->lpDataSink,
				(uint8_t*)(&(bTemp.bTemp)),
				1,
				&dwBytesWritten
			);
			if(dwBytesWrittenResult != NULL) { (*dwBytesWrittenResult) = (*dwBytesWrittenResult) + dwBytesWritten; }
			if((dwBytesWritten > 0) && ((lpThis->cfgFlags & minorConfigurationElement_GZIP_CompressionSettings__FLAG__EnableHeaderCRC16) != 0)) {
				/* CRC Update */
				lpThis->crc16Header = rfc1951_CRC32Update(lpThis->crc16Header, &(bTemp.bTemp), dwBytesWritten);
			}
			if(dwBytesWritten == 1) {
				if(lpThis->cfgCommentLen > 0) {
					lpThis->state = minorCompressor_GZip__State__Comment;
					lpThis->dwStageWritten = 0;
				} else if((lpThis->cfgFlags & minorConfigurationElement_GZIP_CompressionSettings__FLAG__EnableHeaderCRC16) != 0) {
					lpThis->state = minorCompressor_GZip__State__HeaderCRC;
					lpThis->dwStageWritten = 0;
				} else {
					lpThis->state = minorCompressor_GZip__State__CompressedData;
					lpThis->dwStageWritten = 0;
					lpThis->dwUncompressedDataSize = 0;
					lpThis->crc32Data = 0;
				}
			}
			if(e != minorE_Ok) { return e; }
			if(dwBytesWritten < 1) { continue; } /* Retry ... */
		}

		if(lpThis->state == minorCompressor_GZip__State__Comment) {
			/* Zero terminated comment */
			while(lpThis->dwStageWritten < lpThis->cfgCommentLen) {
				dwBytesWritten = 0;
				e = lpThis->lpDataSink->write(
					lpThis->lpDataSink,
					(uint8_t*)(&(lpThis->cfgComment[lpThis->dwStageWritten])),
					lpThis->cfgCommentLen - lpThis->dwStageWritten,
					&dwBytesWritten
				);
				if(dwBytesWrittenResult != NULL) { (*dwBytesWrittenResult) = (*dwBytesWrittenResult) + dwBytesWritten; }
				if((dwBytesWritten > 0) && ((lpThis->cfgFlags & minorConfigurationElement_GZIP_CompressionSettings__FLAG__EnableHeaderCRC16) != 0)) {
					/* CRC Update */
					lpThis->crc16Header = rfc1951_CRC32Update(lpThis->crc16Header, (uint8_t*)&(lpThis->cfgComment[lpThis->dwStageWritten]), dwBytesWritten);
				}
				lpThis->dwStageWritten = lpThis->dwStageWritten + dwBytesWritten;

				if(e != minorE_Ok) { return e; }
			}

			dwBytesWritten = 0;
			bTemp.bTemp = 0;
			e = lpThis->lpDataSink->write(
				lpThis->lpDataSink,
				(uint8_t*)(&(bTemp.bTemp)),
				1,
				&dwBytesWritten
			);
			if(dwBytesWrittenResult != NULL) { (*dwBytesWrittenResult) = (*dwBytesWrittenResult) + dwBytesWritten; }
			if((dwBytesWritten > 0) && ((lpThis->cfgFlags & minorConfigurationElement_GZIP_CompressionSettings__FLAG__EnableHeaderCRC16) != 0)) {
				/* CRC Update */
				lpThis->crc16Header = rfc1951_CRC32Update(lpThis->crc16Header, (uint8_t*)&(bTemp.bTemp), dwBytesWritten);
			}
			if(dwBytesWritten == 1) {
				if((lpThis->cfgFlags & minorConfigurationElement_GZIP_CompressionSettings__FLAG__EnableHeaderCRC16) != 0) {
					lpThis->state = minorCompressor_GZip__State__HeaderCRC;
					lpThis->dwStageWritten = 0;
				} else {
					lpThis->state = minorCompressor_GZip__State__CompressedData;
					lpThis->dwStageWritten = 0;
					lpThis->dwUncompressedDataSize = 0;
					lpThis->crc32Data = 0;
				}
			}
			if(e != minorE_Ok) { return e; }
			if(dwBytesWritten < 1) { continue; } /* Retry ... */
		}

		if(lpThis->state == minorCompressor_GZip__State__HeaderCRC) {
			if(lpThis->dwStageWritten == 0) {
				dwBytesWritten = 0;
				bTemp.bTemp = ((uint8_t)(lpThis->crc16Header & 0x000000FF));
				e = lpThis->lpDataSink->write(
					lpThis->lpDataSink,
					(uint8_t*)(&(bTemp.bTemp)),
					1,
					&dwBytesWritten
				);
				if(dwBytesWrittenResult != NULL) { (*dwBytesWrittenResult) = (*dwBytesWrittenResult) + dwBytesWritten; }
				lpThis->dwStageWritten = lpThis->dwStageWritten + dwBytesWritten;
				if((dwBytesWritten < 1) && (e == minorE_Ok)) { continue; }
				if(e != minorE_Ok) { return e; }
			}
			if(lpThis->dwStageWritten == 1) {
				dwBytesWritten = 0;
				bTemp.bTemp = (((uint8_t)(lpThis->crc16Header & 0x0000FF00)) >> 8);
				e = lpThis->lpDataSink->write(
					lpThis->lpDataSink,
					(uint8_t*)(&(bTemp.bTemp)),
					1,
					&dwBytesWritten
				);
				if(dwBytesWrittenResult != NULL) { (*dwBytesWrittenResult) = (*dwBytesWrittenResult) + dwBytesWritten; }
				lpThis->dwStageWritten = lpThis->dwStageWritten + dwBytesWritten;
				if((dwBytesWritten < 1) && (e == minorE_Ok)) { continue; }
				if(e != minorE_Ok) { return e; }
			}

			lpThis->dwStageWritten = 0;
			lpThis->dwUncompressedDataSize = 0;
			lpThis->state = minorCompressor_GZip__State__CompressedData;
			lpThis->crc32Data = 0;
		}

		/*
			Handle CRC32 and ISIZE here too
		*/
		if(lpThis->state == minorCompressor_GZip__State__CRC32AndIsize) {
			/* First build trailer */
			bTemp.trailerBytes[0] = (uint8_t)((lpThis->crc32Data >>  0) & 0xFF);
			bTemp.trailerBytes[1] = (uint8_t)((lpThis->crc32Data >>  8) & 0xFF);
			bTemp.trailerBytes[2] = (uint8_t)((lpThis->crc32Data >> 16) & 0xFF);
			bTemp.trailerBytes[3] = (uint8_t)((lpThis->crc32Data >> 24) & 0xFF);

			bTemp.trailerBytes[4] = (uint8_t)((lpThis->dwUncompressedDataSize >>  0) & 0xFF);
			bTemp.trailerBytes[5] = (uint8_t)((lpThis->dwUncompressedDataSize >>  8) & 0xFF);
			bTemp.trailerBytes[6] = (uint8_t)((lpThis->dwUncompressedDataSize >> 16) & 0xFF);
			bTemp.trailerBytes[7] = (uint8_t)((lpThis->dwUncompressedDataSize >> 24) & 0xFF);

			/* Now issue write request */
			dwBytesWritten = 0;
			e = lpThis->lpDataSink->write(
				lpThis->lpDataSink,
				bTemp.trailerBytes,
				8 - lpThis->dwStageWritten,
				&dwBytesWritten
			);
			if(dwBytesWrittenResult != NULL) { (*dwBytesWrittenResult) = (*dwBytesWrittenResult) + dwBytesWritten; }
			if((lpThis->dwStageWritten = lpThis->dwStageWritten + dwBytesWritten) < 8) {
				/* Not done */
				if(e == minorE_Ok) { continue; } /* Retry with a second write */
				return e; /* Else return an error code */
			} else {
				/* Done */
				lpThis->state = minorCompressor_GZip__State__Finished;
				return minorE_Finished;
			}

		}

		return minorE_Ok;
	}
}


enum minorError minorCompressorTransferFromInput_GZip(
	struct minorCompressor*				lpObject,
	unsigned long int 					dwBytesToRead,
	unsigned long int*					lpBytesDone
) {
	struct minorCompressor_GZip* lpThis;
	unsigned long int dwBytesDone;
	unsigned long int dwBytesDoneSum;
	enum minorError e;

	if(lpBytesDone != NULL) { (*lpBytesDone) = 0; }
	if(lpObject == NULL) { return minorE_InvalidParam; }
	if(lpObject->algorithm != minorAlgorithm_Gzip) { return minorE_InvalidParam; }
	lpThis = (struct minorCompressor_GZip*)lpObject;

	dwBytesDoneSum = 0;

	if(lpThis->lpDataSource == NULL) { return minorE_MissingDataSource; }
	if(lpThis->lpDataSink == NULL) { return minorE_MissingDataSink; }

	if(lpThis->state == minorCompressor_GZip__State__Finished) { return minorE_Finished; }
	if(lpThis->lpAttachedCompressor == NULL) { return minorE_InvalidState; }

	if((e = minorCompressorAttachSource(lpThis->lpAttachedCompressor, (struct minorStreamSource*)&(lpThis->attachedSourceWrapper))) != minorE_Ok) { return e; }
	if((e = minorCompressorAttachSink(lpThis->lpAttachedCompressor, lpThis->lpDataSink)) != minorE_Ok) { return e; }

	/*
		If we haven't fully processed our header we have to continue header
		processing
	*/
	while(lpThis->dwUncompressedDataSize < dwBytesToRead) {
		if(((lpThis->state < minorCompressor_GZip__State__CompressedData) || (lpThis->state > minorCompressor_GZip__State__CompressedData)) && (lpThis->state != minorCompressor_GZip__State__Finished)) {
			dwBytesDone = 0;
			e = minorCompressor_ContinueHeader_GZip(
				lpThis,
				dwBytesToRead - lpThis->dwUncompressedDataSize,
				&dwBytesDone
			);

			dwBytesDoneSum = dwBytesDoneSum + dwBytesDone;
			if(e != minorE_Ok) {
				if((e == minorE_Suspend) && (lpBytesDone == NULL)) {
					return minorE_FailedWouldBlock;
				}
				(*lpBytesDone) = dwBytesDoneSum;
				return e;
			}
		}

		if(lpThis->state == minorCompressor_GZip__State__CompressedData) {
			dwBytesDone = 0;
			e = minorCompressorTransferFromInput(lpThis->lpAttachedCompressor, dwBytesToRead - lpThis->dwUncompressedDataSize, &dwBytesDone);
			/* ToDo: COUNT UNCOMPRESSED DATA SIZE ... MAYBE WE DO THIS WITH A SOURCE WRAPPER? */
			dwBytesDoneSum = dwBytesDoneSum + dwBytesDone;
			if((e != minorE_Ok) && (e != minorE_Finished)) {
				if((e == minorE_Suspend) && (lpBytesDone == NULL)) {
					return minorE_FailedWouldBlock;
				}
				(*lpBytesDone) = dwBytesDoneSum;
				return e;
			} else if(e == minorE_Finished) {
				minorCompressorRelease(lpThis->lpAttachedCompressor);
				lpThis->lpAttachedCompressor = NULL;
				lpThis->state = minorCompressor_GZip__State__CRC32AndIsize;
				lpThis->dwStageWritten = 0;
			}
		}

		if(lpThis->state == minorCompressor_GZip__State__Finished) {
			if(lpBytesDone != NULL) {
				(*lpBytesDone) = dwBytesDoneSum;
				return minorE_Finished;
			} else {
				return minorE_Finished;
			}
		}
	}

	return minorE_Ok;
}
enum minorError minorCompressorTransferToOutput_GZip(
	struct minorCompressor*				lpObject,
	unsigned long int					dwBytesToWrite
) {
	// struct minorCompressor_GZip* lpThis;

	if(lpObject == NULL) { return minorE_InvalidParam; }
	if(lpObject->algorithm != minorAlgorithm_Gzip) { return minorE_InvalidParam; }
	// lpThis = (struct minorCompressor_GZip*)lpObject;

	return minorE_ImplementationError;
}

enum minorError minorCompressor_Execute_GZip(
	struct minorCompressor*				lpObject
) {
	struct minorCompressor_GZip* lpThis;
	unsigned long int dwBytesWritten;
	enum minorError e;

	if(lpObject == NULL) { return minorE_InvalidParam; }
	if(lpObject->algorithm != minorAlgorithm_Gzip) { return minorE_InvalidParam; }
	lpThis = (struct minorCompressor_GZip*)lpObject;

	if(lpThis->lpDataSource == NULL) { return minorE_MissingDataSource; }
	if(lpThis->lpDataSink == NULL) { return minorE_MissingDataSink; }

	for(;;) {
		e = minorCompressorTransferFromInput_GZip(lpObject, ~0, &dwBytesWritten);
		if(e != minorE_Ok) {
			break;
		}
	}

	return e;
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

	/* ToDo: Use memcpy instead? */
	for(i = 0; i < dwPassBytes; i=i+1) {
		lpDestinationBuffer[i] = lpInfo->lpBase[lpInfo->dwCurrentOffset];
		lpInfo->dwCurrentOffset = lpInfo->dwCurrentOffset + 1;
		lpInfo->dwBytesToGo = lpInfo->dwBytesToGo - 1;
	}

	return minorE_Ok;
}

enum minorError minorCompressor_WriteMem_GZip(
	struct minorCompressor*				lpObject,

	uint8_t* 							lpDataIn,
	unsigned long int 					dwBytesToWrite,
	unsigned long int					*dwBytesDone
) {
	struct minorCompressor_GZip* lpThis;

	enum minorError e;
	struct minorStreamSource* oldSource;
	struct minorCompressor_WriteMem__LocalSource localSource;

	if(lpObject == NULL) { return minorE_InvalidParam; }
	if(lpObject->algorithm != minorAlgorithm_Gzip) { return minorE_InvalidParam; }
	lpThis = (struct minorCompressor_GZip*)lpObject;

	/* Store old data source */
	oldSource = lpThis->lpDataSource;

	localSource.base.read = &minorCompressor_WriteMem__LocalSource__Read;
	localSource.lpBase = lpDataIn;
	localSource.dwBytesToGo = dwBytesToWrite;
	localSource.dwCurrentOffset = 0;
	lpThis->lpDataSource = (struct minorStreamSource*)(&localSource);

	e = minorCompressorTransferFromInput_GZip(
		lpObject,
		dwBytesToWrite,
		dwBytesDone
	);

	lpThis->lpDataSource = oldSource;

	return e;
}


enum minorError minorCompressorAttachSource_GZip(
	struct minorCompressor* 			lpObject,
	struct minorStreamSource* 			lpSource
) {
	struct minorCompressor_GZip* lpThis;

	if(lpObject == NULL) { return minorE_InvalidParam; }
	if(lpObject->algorithm != minorAlgorithm_Gzip) { return minorE_InvalidParam; }
	lpThis = (struct minorCompressor_GZip*)lpObject;

	lpThis->lpDataSource = lpSource;
	return minorE_Ok;
}

enum minorError minorCompressorAttachSink_GZip(
	struct minorCompressor* 			lpObject,
	struct minorStreamSink* 			lpSink
) {
	struct minorCompressor_GZip* lpThis;

	if(lpObject == NULL) { return minorE_InvalidParam; }
	if(lpObject->algorithm != minorAlgorithm_Gzip) { return minorE_InvalidParam; }

	lpThis = (struct minorCompressor_GZip*)lpObject;

	lpThis->lpDataSink = lpSink;
	return minorE_Ok;
}

enum minorError minorCompressorContinue_GZip(
	struct minorCompressor* lpObject
) {
	struct minorCompressor_GZip* lpThis;
	unsigned long int dwBytesWritten;
	enum minorError e;

	if(lpObject == NULL) { return minorE_InvalidParam; }
	if(lpObject->algorithm != minorAlgorithm_Gzip) { return minorE_InvalidParam; }

	lpThis = (struct minorCompressor_GZip*)lpObject;

	for(;;) {
		while((lpThis->state < minorCompressor_GZip__State__CompressedData) || (lpThis->state > minorCompressor_GZip__State__CompressedData)) {
			e = minorCompressor_ContinueHeader_GZip(lpThis, ~0, &dwBytesWritten);
			if(e != minorE_Ok) { return e; }
		}

		while(lpThis->state == minorCompressor_GZip__State__CompressedData) {
			e = minorCompressorContinue(lpThis->lpAttachedCompressor);
			if(e == minorE_Finished) {
				lpThis->state = minorCompressor_GZip__State__CRC32AndIsize;
				lpThis->dwStageWritten = 0;
				e = minorCompressorRelease(lpThis->lpAttachedCompressor);
				if(e != minorE_Ok) {
					return e;
				}
				lpThis->lpAttachedCompressor = NULL;
			} else if(e != minorE_Ok) {
				return e;
			}
		}
	}
	/* Not reachable ... */
}

enum minorError minorCompressorRelease_GZip(
	struct minorCompressor* 			lpObject
) {
		struct minorCompressor_GZip* lpThis;
		struct minorSystemInterface* lpSystem;
		enum minorError e;
		unsigned long int dwBytesDone;

		if(lpObject == NULL) { return minorE_InvalidParam; }
		if(lpObject->algorithm != minorAlgorithm_Gzip) { return minorE_InvalidParam; }

		lpThis = (struct minorCompressor_GZip*)lpObject;
		lpSystem = lpThis->lpSystemAPI;

		/* We may have to flush the remaining block ... and the trailer */
		if(lpThis->state != minorCompressor_GZip__State__Finished) {
			/* First check if we have to levae the compressed data state ... */
			if(lpThis->state == minorCompressor_GZip__State__CompressedData) {
				if(lpThis->lpAttachedCompressor != NULL) {
					e = minorCompressorRelease(lpThis->lpAttachedCompressor);
					if(e == minorE_Suspend) { return e; } /* Note that this function may result in suspend too ... ToDo: Add ability to release without writing */

					/* If we've successfully finished flusing the wrapped compressor - clear references and switch state to ISIZE and CRC32 */
					lpThis->lpAttachedCompressor = NULL;
				}
				lpThis->state = minorCompressor_GZip__State__CRC32AndIsize;
				lpThis->dwStageWritten = 0;
			}

			if(lpThis->state == minorCompressor_GZip__State__CRC32AndIsize) {
				do {
					dwBytesDone = 0;
					e = minorCompressor_ContinueHeader_GZip(
						lpThis,
						8 - lpThis->dwStageWritten,
						&dwBytesDone
					);
				} while(e == minorE_Ok);

				if(e == minorE_Suspend) { return minorE_Suspend; } /* Note that this function may result in suspend too ... ToDo: Add ability to release without writing */
			}
		}

		if(lpThis->lpAttachedCompressor != NULL) { minorCompressorRelease(lpThis->lpAttachedCompressor); lpThis->lpAttachedCompressor = NULL; }

		lpSystem->free((void*)lpThis, lpSystem->lpFreeParam_Free);
		return (e == minorE_Finished) ? minorE_Ok : e;
}

enum minorError minorCompressorCreate_GZip(
	struct minorCompressor** 			lpOut,			/* Output location for the compressor object. There will be NO REUSE. Any previous object pointed to will be orphaned if not released before! */

	enum minorAlgorithm					algorithm,		/*  Allows to select the algorithm to be used during data compression */
	struct minorSystemInterface*		lpSystem,		/* Required system interface */
	struct minorConfigurationElement* 	lpConfiguration	/* Pointer to the first configuration element inside the configuration chain or NULL */
) {
	enum minorError e;
	struct minorConfigurationElement* lpCurrentConf;

	/* Configuration variables (optionally overriden from lpConfiguration options) */
	unsigned char*											lpConfig_OrigFilename 		= NULL;
	unsigned long int										dwConfig_OrigFilenameLen 	= 0;
	unsigned char*											lpConfig_Comment 			= NULL;
	unsigned long int										dwConfig_CommentLen 		= 0;
	enum minorConfigurationElement_GZIP_OperatingSystem 	configOperatingSystem 		= minorConfigurationElement_GZIP_OperatingSystem__Unknown;
	enum minorConfigurationElement_GZIP_CompressionMethod	configCompressionMethod 	= minorConfigurationElement_GZIP_CompressionMethod__Deflate;
	uint32_t												dwConfigFlags 				= 0;
	uint32_t												dwConfigModificationTime	= 0;

	#if defined(__FreeBSD__)
		configOperatingSystem = minorConfigurationElement_GZIP_OperatingSystem__Unix;
	#elif defined(__Linux__)
		configOperatingSystem = minorConfigurationElement_GZIP_OperatingSystem__Unix;
	#elif defined(WIN32) || defined(_WIN32) || defined(__WINDOWS__) || defined(__WIN32__) || defined(_WIN16) || defined(_WIN64) || defined(MSDOS) || defined(__MSDOS__) || defined(_MSODS) || defined(__DOS__)
		configOperatingSystem = minorConfigurationElement_GZIP_OperatingSystem__FATFilesystem;
	#endif

	/* Input parameter validation */
	if(lpOut == NULL) { return minorE_InvalidParam; }
	(*lpOut) = NULL;

	if(lpSystem == NULL) { return minorE_InvalidParam; }
	if(algorithm != minorAlgorithm_Gzip) { return minorE_InvalidParam; }

	/* Check for system API pointers (memman) */
	if((lpSystem->alloc == NULL) || (lpSystem->free == NULL)) {
		return minorE_InvalidParam;
	}

	/* Validate all passed configuration options */
	if((e = minorCompressor_CheckConfiguration_GZip(lpConfiguration)) != minorE_Ok) { return e; }

	/* Load configurations and default values */
	lpCurrentConf = lpConfiguration;
	while(lpCurrentConf != NULL) {
		if(lpCurrentConf->typeMajor == minorConfiguration_MajorId_GZip) {
			/* GZip configuration options */
			if(((struct minorConfigurationElement_GZIP*)lpCurrentConf)->typeMinor == minorConfiguration_MinorID_GZIP_CompressionSettings) {
				configOperatingSystem = ((struct minorConfigurationElement_GZIP_CompressionSettings*)lpCurrentConf)->operatingSystem;
				dwConfigFlags = ((struct minorConfigurationElement_GZIP_CompressionSettings*)lpCurrentConf)->dwFlags;
				configCompressionMethod = ((struct minorConfigurationElement_GZIP_CompressionSettings*)lpCurrentConf)->compressionMethod;
			} else if(((struct minorConfigurationElement_GZIP*)lpCurrentConf)->typeMinor == minorConfiguration_MinorID_GZIP_OriginalFilename) {
				lpConfig_OrigFilename = ((struct minorConfigurationElement_GZIP_OriginalFilename*)lpCurrentConf)->lpFilename;
				dwConfig_OrigFilenameLen = ((struct minorConfigurationElement_GZIP_OriginalFilename*)lpCurrentConf)->dwFilenameLength;
			} else if(((struct minorConfigurationElement_GZIP*)lpCurrentConf)->typeMinor == minorConfiguration_MinorID_GZIP_Comment) {
				lpConfig_Comment = ((struct minorConfigurationElement_GZIP_Comment*)lpCurrentConf)->lpComment;
				dwConfig_CommentLen = ((struct minorConfigurationElement_GZIP_Comment*)lpCurrentConf)->dwCommentLength;
			} else if(((struct minorConfigurationElement_GZIP*)lpCurrentConf)->typeMinor == minorConfiguration_MinorID_GZIP_ModificationTime) {
				dwConfigModificationTime = ((struct minorConfigurationElement_GZIP_ModificationTime*)lpCurrentConf)->dwModificationTime;
			}
		}
	}

	/*
		Allocate and initialize the state block
	*/
	e = lpSystem->alloc((void**)lpOut, sizeof(struct minorCompressor_GZip)+dwConfig_CommentLen+dwConfig_OrigFilenameLen, lpSystem->lpFreeParam_Alloc);
	if(e != minorE_Ok) { return e; }

	/* Base information */
	(*((struct minorCompressor_GZip**)lpOut))->base.algorithm 			= minorAlgorithm_Gzip;
	(*((struct minorCompressor_GZip**)lpOut))->lpSystemAPI 				= lpSystem;
	(*((struct minorCompressor_GZip**)lpOut))->dwFlags 					= 0;

	/* Intiialize configuration variables */
	(*((struct minorCompressor_GZip**)lpOut))->cfgModificationTime 		= dwConfigModificationTime;
	(*((struct minorCompressor_GZip**)lpOut))->cfgFlags 				= dwConfigFlags;
	(*((struct minorCompressor_GZip**)lpOut))->cfgOperatingSystem 		= configOperatingSystem;
	(*((struct minorCompressor_GZip**)lpOut))->cfgCompressionMethod 	= configCompressionMethod;
	(*((struct minorCompressor_GZip**)lpOut))->cfgCommentLen 			= dwConfig_CommentLen;
	(*((struct minorCompressor_GZip**)lpOut))->cfgOriginalFilenameLen 	= dwConfig_OrigFilenameLen;
	(*((struct minorCompressor_GZip**)lpOut))->cfgOriginalFilename 		= &((*((struct minorCompressor_GZip**)lpOut))->bBinaryPayload[0]);
	(*((struct minorCompressor_GZip**)lpOut))->cfgComment		 		= &((*((struct minorCompressor_GZip**)lpOut))->bBinaryPayload[dwConfig_OrigFilenameLen]);

	/* State initialization */
	(*((struct minorCompressor_GZip**)lpOut))->state 					= minorCompressor_GZip__State__Header;
	(*((struct minorCompressor_GZip**)lpOut))->dwStageWritten 			= 0;
	(*((struct minorCompressor_GZip**)lpOut))->dwUncompressedDataSize 	= 0;
	(*((struct minorCompressor_GZip**)lpOut))->dwOutputWritten			= 0;
	(*((struct minorCompressor_GZip**)lpOut))->crc32Data 				= 0;
	(*((struct minorCompressor_GZip**)lpOut))->crc16Header				= 0;
	(*((struct minorCompressor_GZip**)lpOut))->lpAttachedCompressor		= NULL;

	/* Wrapper preparation */
	(*((struct minorCompressor_GZip**)lpOut))->attachedSourceWrapper.lpCompressor 	= (*((struct minorCompressor_GZip**)lpOut));
	(*((struct minorCompressor_GZip**)lpOut))->attachedSourceWrapper.base.read 		= &minorCompressor_GZip__StreamSourceWrapper__Read;

	/* Copy configured comment and original filename */
	if(dwConfig_CommentLen > 0) { memcpy((*((struct minorCompressor_GZip**)lpOut))->cfgComment, lpConfig_Comment, dwConfig_CommentLen); }
	if(dwConfig_OrigFilenameLen > 0) { memcpy((*((struct minorCompressor_GZip**)lpOut))->cfgOriginalFilename, lpConfig_OrigFilename, dwConfig_OrigFilenameLen); }

	/* Create wrapped compressor */
	switch(configCompressionMethod) {
		case minorConfigurationElement_GZIP_CompressionMethod__Deflate:
			e = minorCompressorCreate(
				&((*((struct minorCompressor_GZip**)lpOut))->lpAttachedCompressor),
				minorAlgorithm_Deflate,
				lpSystem,
				lpConfiguration
			);
			if(e != minorE_Ok) {
				lpSystem->free((void*)(*lpOut), lpSystem->lpFreeParam_Free);
				(*lpOut) = NULL;
				return e;
			}
			break;
		default:
			lpSystem->free((void*)(*lpOut), lpSystem->lpFreeParam_Free);
			(*lpOut) = NULL;
			return minorE_ImplementationError;
	}

	return minorE_Ok;
}

enum minorError minorCompressor_CheckConfiguration_GZip(
	struct minorConfigurationElement* lpConfiguration
) {
	struct minorConfigurationElement* lpCurrentElement = lpConfiguration;
	struct minorConfigurationElement_GZIP* lpCurrentElement_GZIP;
	enum minorError e;
	unsigned long int i;

	if(lpConfiguration == NULL) { return minorE_Ok; }

	while(lpCurrentElement != NULL) {
		/*
			Check if it's our major ID. We can only validate settings that are specific to our
			module and that have not been processed until now
		*/
		if((lpCurrentElement->typeMajor == minorConfiguration_MajorId_GZip) && ((lpCurrentElement->dwFlags & minorConfiguration__Flag__Processed) == 0)) {
			lpCurrentElement_GZIP = (struct minorConfigurationElement_GZIP*)lpCurrentElement;
			switch(lpCurrentElement_GZIP->typeMinor) {
				case minorConfiguration_MinorID_GZIP_CompressionSettings:
					if((((struct minorConfigurationElement_GZIP_CompressionSettings*)lpCurrentElement_GZIP)->operatingSystem > minorConfigurationElement_GZIP_OperatingSystem__AcornRISCOS) && (((struct minorConfigurationElement_GZIP_CompressionSettings*)lpCurrentElement_GZIP)->operatingSystem < minorConfigurationElement_GZIP_OperatingSystem__Unknown)) { return minorE_InvalidParam; }
					if((((struct minorConfigurationElement_GZIP_CompressionSettings*)lpCurrentElement_GZIP)->dwFlags & (~minorConfigurationElement_GZIP_CompressionSettings__FLAG__ALLOWEDFLAGS)) != 0) { return minorE_InvalidParam; }
					if(((struct minorConfigurationElement_GZIP_CompressionSettings*)lpCurrentElement_GZIP)->compressionMethod != minorConfigurationElement_GZIP_CompressionMethod__Deflate) { return minorE_InvalidParam; }

					/* Validated */
					lpCurrentElement->dwFlags = lpCurrentElement->dwFlags | minorConfiguration__Flag__Processed; /* Mark supported */
					break;
				case minorConfiguration_MinorID_GZIP_OriginalFilename:
					if(((struct minorConfigurationElement_GZIP_OriginalFilename*)lpCurrentElement_GZIP)->dwFilenameLength != 0) {
						/* Check there are no zeros contained ... */
						if(((struct minorConfigurationElement_GZIP_OriginalFilename*)lpCurrentElement_GZIP)->lpFilename == NULL) {
							return minorE_InvalidParam;
						}
						for(i = 0; i < ((struct minorConfigurationElement_GZIP_OriginalFilename*)lpCurrentElement_GZIP)->dwFilenameLength; i=i+1) {
							if(((struct minorConfigurationElement_GZIP_OriginalFilename*)lpCurrentElement_GZIP)->lpFilename[i] == 0x00) {
								return minorE_InvalidParam;
							}
						}
					}

					/* Validated */
					lpCurrentElement->dwFlags = lpCurrentElement->dwFlags | minorConfiguration__Flag__Processed; /* Mark supported */
					break;
				case minorConfiguration_MinorID_GZIP_Comment:
					if(((struct minorConfigurationElement_GZIP_Comment*)lpCurrentElement_GZIP)->dwCommentLength != 0) {
						/* Check there are no zeros contained ... */
						if(((struct minorConfigurationElement_GZIP_Comment*)lpCurrentElement_GZIP)->lpComment == NULL) {
							return minorE_InvalidParam;
						}
						for(i = 0; i < ((struct minorConfigurationElement_GZIP_Comment*)lpCurrentElement_GZIP)->dwCommentLength; i=i+1) {
							if(((struct minorConfigurationElement_GZIP_Comment*)lpCurrentElement_GZIP)->lpComment[i] == 0x00) {
								return minorE_InvalidParam;
							}
						}
					}

					/* Validated */
					lpCurrentElement->dwFlags = lpCurrentElement->dwFlags | minorConfiguration__Flag__Processed; /* Mark supported */
					break;
				case minorConfiguration_MinorID_GZIP_ModificationTime:
					/*
						We do NOT validate this modification time depending on sane values (not in future, etc - this
						has to be done by the applciation)
					*/

					/* Validated */
					lpCurrentElement->dwFlags = lpCurrentElement->dwFlags | minorConfiguration__Flag__Processed; /* Mark supported */
					break;
				default:
					/* Unknown configuration option */
					if((lpCurrentElement->dwFlags & minorConfiguration__Flag__Critical) != 0) { return minorE_UnsupportedConfiguration; }
					lpCurrentElement->dwFlags = lpCurrentElement->dwFlags | minorConfiguration__Flag__Processed; /* Mark supported */
					break;
			}
		}

		/* Iterate to the next element */
		lpCurrentElement = lpCurrentElement->lpNext;
	}

	/* If the compression method is deflate we also support deflate configurations */
	e = minorDecompressor_CheckConfiguration_Deflate(lpConfiguration);
	if(e != minorE_Ok) { return e; }

	/* Done */
	return minorE_Ok;
}

#ifdef __cplusplus
	} /* extern "C" { */
#endif
