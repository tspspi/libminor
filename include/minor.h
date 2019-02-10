#ifndef __is_included__c59c25d7_46fe_4664_bf95_4ab6dbd3f431
#define __is_included__c59c25d7_46fe_4664_bf95_4ab6dbd3f431 1

#ifndef LIBMINOR_DEFLATE__BLOCKSIZE_K
	#define LIBMINOR_DEFLATE__BLOCKSIZE_K		32
#endif

#define MINOR_DEFLATE_STREAMING_ENABLE 1		/* Currently we ALWAYS enable the streaming & dictionary preloading feature */

#include <stdint.h>

#ifndef __cplusplus
	#ifndef true
		#define true 1
		#define false 0
		typedef uint8_t bool;
	#endif
#endif

#ifndef NULL
	#define NULL ((void*)0)
#endif

#define minorCompressor_Deflate__ALPHABETSIZE_LITERAL		286
#define minorCompressor_Deflate__ALPHABETSIZE_DISTANCE		30
#define minorCompressor_Deflate__ALPHABETSIZE_CODELENGTH	19

#ifdef __cplusplus
	extern "C" {
#endif

/*
	Basic return code definition used throughout minor
*/

enum minorError {
	minorE_Ok								= 0,

	minorE_OutOfMemory						= 1,
	minorE_InvalidParam						= 2,
	minorE_UnknownAlgorithm					= 3,
	minorE_UnsupportedConfiguration			= 4,
	minorE_EncodingError					= 5,
	minorE_AlreadyFailed					= 6,
	minorE_MissingDataSource				= 7,
	minorE_MissingDataSink					= 8,
	minorE_Suspend							= 9,
	minorE_EndOfStream						= 10,
	minorE_NoSolution						= 11,
	minorE_InvalidState						= 12,

	minorE_FailedWouldBlock					= 13,		/* The function would be required to block but this is not possible with the current configuration or parameters. The compressor / decompressor has entered failed state */
	minorE_Finished							= 14,

	minorE_CRCError							= 15,		/* used to signal an encoding error because of a mismatching CRC */

	minorE_ImplementationError				= 255,

	minorE_OkAlignByte,									/* Used internally. The (bytewise) reader will align to the next byte before continuing */
};


/*
	Enumeration of internally used algorithm identifiers
*/
enum minorAlgorithm {
	minorAlgorithm_Uncompressed				= 0,

	minorAlgorithm_Deflate					= 1,				/* Deflate algorithm, RFC 1951 ("ZIP") */
	minorAlgorithm_ZLib						= 2,				/* ZLib "algorithm", RFC 1950 */
	minorAlgorithm_Gzip						= 3,				/* GZip File Format. Normally the same as Deflate but with an additional header and supports additional callbacks */
};


/*
	System interface functions

	The minor library swaps access to basic system functions outside the library
	to support various configurations (even ones with preallocated memory).

	The following functions are contained inside the system abstraction interface:

	- Alloc:	Allocates a block of memory with the supplied size. This function
				accepts a free parameter which might be used by the unterlying
				implementation.
	- Free:		Releases a block of memory previously allocated via alloc. This
				function accepts a free parameter which might be used by the
				unterlying implementation.
*/

typedef enum minorError (*minorSystemAPI_Alloc)(void** lpOut, unsigned long int dwSize, void* lpFreeParam);
typedef enum minorError (*minorSystemAPI_Free)(void* lpOffset, void* lpFreeParam);

struct minorSystemInterface {
	minorSystemAPI_Alloc				alloc;			void* lpFreeParam_Alloc;
	minorSystemAPI_Free					free;			void* lpFreeParam_Free;
};

/*
	Configuration structures

	Configuration values are passed to minor as a set of linked configuration
	structures. They are managed by the calling library or program. Options
	may either be specified with ot without the critical flag. If an option
	with set cricital flag is not recognized then the function to which it
	has been passed failes. If the cricital flag is not set any unknown
	options are silently ignored.
*/
#define minorConfiguration__Flag__Critical				0x00000001		/* If set the option is required to be known by the library */
#define minorConfiguration__Flag__Processed				0x80000000		/* Used internally to mark that an option has been processed */

enum minorConfiguration_MajorId {
	minorConfiguration_MajorId_Undefined				= 0,

	minorConfiguration_MajorId_Deflate					= 1,			/* Deflate specific configurations */
	minorConfiguration_MajorId_GZip						= 2,			/* GZIP specific configurations */
};

struct minorConfigurationElement {
	struct minorConfigurationElement*					lpNext;			/* Links to the next configuration option. NULL means the end of configuration chain is reached */
	uint32_t											dwFlags;		/* Specifies behaviour of the configuration. Currently only the "critical" flag is supported */
	unsigned long int									dwLength;		/* Length of the whole structure (required for replication in case the configuration has to be cached) */
	enum minorConfiguration_MajorId						typeMajor;		/* Specifies the major type of the configuration group */
};

/*
	Deflate specific configuration options
*/

enum minorConfiguration_MinorID_Deflate {
	minorConfiguration_MinorID_Deflate_Undefined				= 0,

	minorConfiguration_MinorID_Deflate_BackreferenceTableSize 	= 1,		/* Allows one to specify the backreference table size. Default are 32 KByte which leads to a 32 KByte sliding window inside two buffers (i.e. one single 64 KByte output buffer) */
	minorConfiguration_MinorID_Deflate_PreloadDictionary		= 2,		/* Allows preloading of a custom (< BackreferenceTableSize) dictionary that will neither be encoded in the output nor contained inside the compressed data stream - the same dictionary has to be preloaded at compression and decompression! This may be used by ZLib methods */
	minorConfiguration_MinorID_Deflate_QualitySettings			= 3,		/* Allows settings of quality settings ... */
};

struct minorConfigurationElement_Deflate {
	struct minorConfigurationElement				base;
	enum minorConfiguration_MinorID_Deflate			typeMinor;
};
struct minorConfigurationElement_Deflate_BackreferenceTableSize {
	struct minorConfigurationElement_Deflate		base;
	unsigned long int								slidingWindowSize; /* Specified in KByte! */
};
/*@UNTESTED*/
struct minorConfigurationElement_Deflate_PreloadDictionary {
	struct minorConfigurationElement_Deflate		base;
	uint8_t*										lpDictionary;
	unsigned long int								dwByteLength;
};

struct minorConfigurationElement_Deflate_QualitySettings { /* Compressor only */
	struct minorConfigurationElement_Deflate		base;
	unsigned long int								dwLazyMatchDistance;		/* Default 1. Number of bytes to lookahead for a better match than the current byte position */
	unsigned long int 								dwBlockSize;				/* Maximum (symbol) size of an output block. This determines how often huffman tables are generated minimally. A block can always be produced via explicit flush */
	unsigned long int								dwLookbackDistance;			/* Lookback distance for LZ77. Default is 32*1024 Byte (32 kByte) */
	unsigned long int								dwEarlyMatchLength;			/* Length of a lookback match that should be immediately accepted instead of continuing to scan (default: 0 - disables this shortcut) */
};

/*
	GZIP specific configuration options
*/
enum minorConfiguration_MinorID_GZIP {
	minorConfiguration_MinorID_GZIP_Undefined					= 0,

	minorConfiguration_MinorID_GZIP_StringBlocksize				= 1,
	minorConfiguration_MinorID_GZIP_OriginalFilenameCallback	= 2,
	minorConfiguration_MinorID_GZIP_CommentCallback				= 3,

	minorConfiguration_MinorID_GZIP_CompressionSettings			= 4,
	minorConfiguration_MinorID_GZIP_OriginalFilename			= 5,
	minorConfiguration_MinorID_GZIP_Comment						= 6,
	minorConfiguration_MinorID_GZIP_ModificationTime			= 7,
};

struct minorConfigurationElement_GZIP {
	struct minorConfigurationElement				base;
	enum minorConfiguration_MinorID_GZIP			typeMinor;
};
struct minorConfigurationElement_GZIP_StringBlocksize {
	struct minorConfigurationElement_GZIP			base;
	unsigned long int								blockSize;
};


typedef void (*lpfnMinorConfigurationElement_GZIP_OriginalFilenameCallback_Function)(
	uint8_t* lpFilename,
	unsigned long int dwFilenameLength,
	void* lpFreeParam
);
typedef void (*lpfnMinorConfigurationElement_GZIP_CommentCallback_Function)(
	uint8_t* lpComment,
	unsigned long int dwCommentLength,
	void* lpFreeParam
);
struct minorConfigurationElement_GZIP_OriginalFilenameCallback {
	struct minorConfigurationElement_GZIP									base;
	lpfnMinorConfigurationElement_GZIP_OriginalFilenameCallback_Function	lpfnCallback;
	void*																	lpFreeParam;
};
struct minorConfigurationElement_GZIP_CommentCallback {
	struct minorConfigurationElement_GZIP									base;
	lpfnMinorConfigurationElement_GZIP_CommentCallback_Function				lpfnCallback;
	void*																	lpFreeParam;
};

enum minorConfigurationElement_GZIP_OperatingSystem {
	minorConfigurationElement_GZIP_OperatingSystem__FATFilesystem		= 0,
	minorConfigurationElement_GZIP_OperatingSystem__Amiga				= 1,
	minorConfigurationElement_GZIP_OperatingSystem__VMS					= 2,
	minorConfigurationElement_GZIP_OperatingSystem__Unix				= 3,
	minorConfigurationElement_GZIP_OperatingSystem__VM_CMS				= 4,
	minorConfigurationElement_GZIP_OperatingSystem__AtariTOS			= 5,
	minorConfigurationElement_GZIP_OperatingSystem__HPFSFilesystem		= 6,
	minorConfigurationElement_GZIP_OperatingSystem__Macintosh			= 7,
	minorConfigurationElement_GZIP_OperatingSystem__ZSystem				= 8,
	minorConfigurationElement_GZIP_OperatingSystem__CPM					= 9,
	minorConfigurationElement_GZIP_OperatingSystem__TOPS20				= 10,
	minorConfigurationElement_GZIP_OperatingSystem__NTFSFilesystem		= 11,
	minorConfigurationElement_GZIP_OperatingSystem__QDOS				= 12,
	minorConfigurationElement_GZIP_OperatingSystem__AcornRISCOS			= 13,

	minorConfigurationElement_GZIP_OperatingSystem__Unknown				= 255
};

enum minorConfigurationElement_GZIP_CompressionMethod {
	minorConfigurationElement_GZIP_CompressionMethod__Deflate			= 0,
};

#define minorConfigurationElement_GZIP_CompressionSettings__FLAG__EnableHeaderCRC16	0x00000001
#define minorConfigurationElement_GZIP_CompressionSettings__FLAG__SetASCIIHint		0x00000002

#define minorConfigurationElement_GZIP_CompressionSettings__FLAG__ALLOWEDFLAGS (minorConfigurationElement_GZIP_CompressionSettings__FLAG__EnableHeaderCRC16|minorConfigurationElement_GZIP_CompressionSettings__FLAG__SetASCIIHint)

struct minorConfigurationElement_GZIP_CompressionSettings {
	struct minorConfigurationElement_GZIP									base;
	enum minorConfigurationElement_GZIP_OperatingSystem						operatingSystem;		/* Default: Platform specific */
	uint32_t																dwFlags;				/* Default: 0 (no CRC16, no ASCII hint)*/
	enum minorConfigurationElement_GZIP_CompressionMethod					compressionMethod;		/* Default: Deflate */
};
struct minorConfigurationElement_GZIP_OriginalFilename {
	struct minorConfigurationElement_GZIP									base;
	unsigned char*															lpFilename;
	unsigned long int														dwFilenameLength;
};
struct minorConfigurationElement_GZIP_Comment {
	struct minorConfigurationElement_GZIP									base;
	unsigned char*															lpComment;
	unsigned long int														dwCommentLength;
};
struct minorConfigurationElement_GZIP_ModificationTime {
	struct minorConfigurationElement_GZIP									base;
	uint32_t																dwModificationTime;
};


/*
	Data sourcing and sinking interfaces used to query data. Normally
	data will be read one byte at a time and written one block at
	a time, if the interface is used. If the wrapper functions are
	used whole blocks may be supplied or only partial data will
	be read.

	The minorStreamSource and minorStreamSink structures may be
	used directly or may be inherited by corresponding application
	interfaces.

	All functions will receive the offset of this structure as
	their first parameter. This can be upcasted to the inherited
	structure type to allow attachment of supplemental information.
*/

struct minorStreamSource;
struct minorStreamSink;

/*
	Read from a data source.

	The return values known by the minor library are:
		minorE_Ok			Successfully fulfilled the request (at least partially)
		minorE_Suspend		Do not ask the stream source again until some kind of external
							event has happened. This is passed to the outside from the
							read and write loops but is NOT threatened as an error.

	All other return values are threatened as an error.
*/
typedef enum minorError (*minorStreamSource_Read)(
	struct minorStreamSource* 		lpSelf,
	uint8_t* 						lpDestinationBuffer,
	unsigned long int				dwBytesToRead,
	unsigned long int				*lpBytesRead
);
typedef enum minorError (*minorStreamSink_Write)(
	struct minorStreamSink*			lpSelf,
	uint8_t*						lpSourceBuffer,
	unsigned long int				dwBytesToWrite,
	unsigned long int				*lpBytesWritten
);
typedef enum minorError (*minorStreamSink_Flush)(
	struct minorStreamSink*			lpSelf
);

struct minorStreamSource {
	minorStreamSource_Read			read;
};
struct minorStreamSink {
	minorStreamSink_Write			write;
	minorStreamSink_Flush			flush;
};

/*
	Utility functions to work with configuration lists
*/

/*
	Check if there are any unprocessed critical configuration options
	inside a configuration chain.

	Returns:
		minorE_Ok						All critical configuration options have been processed
		minorE_UnsupportedConfiguration	At least one configuration option that has been marked
										cirtical has not been processed.
*/
enum minorError minorConfiguration_CheckUnprocessedCritical(
	struct minorConfigurationElement* lpRootConfiguration
);

/*
	Reset processed flag on configuration chain

	This method has to be called on a configuration chain if this chain
	is to be reused on a different compressor or decompressor. In case
	the same compressor or decompressor is used, this routine does
	not have to be called.

	Returns:
		minorE_Ok						Success
*/
enum minorError minorConfiguration_ResetProcessedOptions(
	struct minorConfigurationElement* lpRootConfiguration
);

/*
	Resource management of compressor and decompressor instances
	and definition of basic data types;
*/
struct minorCompressor;
struct minorDecompressor;

/*
	Create a new compressor

	Tries to instantiate a new compressor that can be used
	to compress data.

	This function expects an optional configuration chain and
	an mandatory algorithm identifier.

	Return values:
		minorE_Ok						Successfully created the object

		minorE_InvalidParam				An invalid parameter has been passed
		minorE_UnknownAlgorithm			The supplied algorithm identifier is unknown
		minorE_OutOfMemory				A memory allocation failed. No object has been created
		minorE_UnsupportedConfiguration	At least one of the supplied configurations that
										had been marked critical is not supported by the requested
										compressor
*/
enum minorError minorCompressorCreate(
	struct minorCompressor** 			lpOut,			/* Output location for the compressor object. There will be NO REUSE. Any previous object pointed to will be orphaned if not released before! */

	enum minorAlgorithm					algorithm,		/*  Allows to select the algorithm to be used during data compression */
	struct minorSystemInterface*		lpSystem,		/* Required system interface */
	struct minorConfigurationElement* 	lpConfiguration	/* Pointer to the first configuration element inside the configuration chain or NULL */
);

/*
	Release a compressor

	This function releases any passed compressor object. Note that this
	routine will NOT flush any cached data that has not been flushed until
	now. All internally cached data will be discarded.

	Return values:
		minorE_Ok						Successfully set
		minorE_InvalidParam				An invalid parameter has been passed
*/
enum minorError minorCompressorRelease(
	struct minorCompressor*				lpObject		/* Allows releasing the minor object */
);

/*
	Attach a stream source to the compressor

	A stream source allows the compressor to source uncompressed data from the
	stream source to produce more compressed data.

	Any potentially previously attached stream source gets discarded (the
	application will have to handle the release). Setting the streamsource
	to NULL will detach any sources.

	Return values:
		minorE_Ok						Successfully set
		minorE_InvalidParam				An invalid parameter has been passed
*/
enum minorError minorCompressorAttachSource(
	struct minorCompressor*				lpObject,
	struct minorStreamSource*			lpSource
);

/*
	Attach a stream sink to the compressor

	A stream sink allows the compressor to eject data from its internal
	block buffers to any data store and release them internally.

	Any potentially previously attached stream sink gets discarded (the
	application will have to handle the release). Setting the streamsink
	to NULL will detach any sources.

	Return values:
		minorE_Ok						Successfully set
		minorE_InvalidParam				An invalid parameter has been passed
*/
enum minorError minorCompressorAttachSink(
	struct minorCompressor*				lpObject,
	struct minorStreamSink*				lpSink
);

enum minorError minorCompressorTransferFromInput(
	struct minorCompressor*				lpObject,
	unsigned long int 					dwBytesToRead,
	unsigned long int*					lpBytesDone
);
enum minorError minorCompressorTransferToOutput(
	struct minorCompressor*				lpObject,
	unsigned long int					dwBytesToWrite
);
enum minorError minorCompressor_Execute(
	struct minorCompressor*				lpObject
);
enum minorError minorCompressor_WriteMem(
	struct minorCompressor*				lpObject,

	uint8_t* 							lpDataIn,
	unsigned long int 					dwBytesToWrite,
	unsigned long int					*dwBytesDone
);

/*
	Continuation function

	The contiuation function allows to continue processing any cached
	data. It CAN be called explicity - but it's not required to be called.
	Other functions will do continuation internally if required.

	Return values:
		minorE_Ok
		minorE_InvalidParam
		minorE_Finished
		...
*/
enum minorError minorCompressorContinue(
	struct minorCompressor				*lpObject
);



/*
	Create a new decompressor

	Tries to instantiate a new decompressor that can be used
	to decompress data.

	This function expects an optional configuration chain and
	an mandatory algorithm identifier.

	Return values:
		minorE_Ok						Successfully created the object

		minorE_InvalidParam				An invalid parameter has been passed
		minorE_UnknownAlgorithm			The supplied algorithm identifier is unknown
		minorE_OutOfMemory				A memory allocation failed. No object has been created
		minorE_UnsupportedConfiguration	At least one of the supplied configurations that
										had been marked critical is not supported by the requested
										decompressor
*/
enum minorError minorDecompressorCreate(
	struct minorDecompressor** 			lpOut,			/* Output location for the decompressor object. There will be NO REUSE. Any previous object pointed to will be orphaned if not released before! */

	enum minorAlgorithm					algorithm,		/*  Allows to select the algorithm to be used during data compression */
	struct minorSystemInterface*		lpSystem,		/* Required system interface */
	struct minorConfigurationElement* 	lpConfiguration	/* Pointer to the first configuration element inside the configuration chain or NULL */
);

/*
	Release a decompressor

	This function releases any passed decompressor object. Note that this
	routine will NOT flush any cached data that has not been flushed until
	now. All internally cached data will be discarded.

	Return values:
		minorE_Ok						Successfully set
		minorE_InvalidParam				An invalid parameter has been passed
*/
enum minorError minorDecompressorRelease(
	struct minorDecompressor*			lpObject		/* Allows releasing the minor object */
);

/*
	Attach a stream source to the decompressor

	A stream source allows the decompressor to source compressed data from the
	stream source to produce more decompressed data.

	Any potentially previously attached stream source gets discarded (the
	application will have to handle the release). Setting the streamsource
	to NULL will detach any sources.

	Return values:
		minorE_Ok						Successfully set
		minorE_InvalidParam				An invalid parameter has been passed
*/
enum minorError minorDecompressorAttachSource(
	struct minorDecompressor*			lpObject,
	struct minorStreamSource*			lpSource
);

/*
	Attach a stream sink to the decompressor

	A stream sink allows the decompressor to eject data from its internal
	block buffers to any data store and release them internally.

	Any potentially previously attached stream sink gets discarded (the
	application will have to handle the release). Setting the streamsink
	to NULL will detach any sources.

	Return values:
		minorE_Ok						Successfully set
		minorE_InvalidParam				An invalid parameter has been passed
*/
enum minorError minorDecompressorAttachSink(
	struct minorDecompressor*			lpObject,
	struct minorStreamSink*				lpSink
);

/*
	Getting data into/out from the decompressor or compressor
*/

enum minorError minorDecompressor_TransferFromInput(
	struct minorDecompressor*			lpObject,
	unsigned long int 					dwBytesToRead,
	unsigned long int					*lpBytesDone
);
enum minorError minorDecompressor_TransferToOutput(
	struct minorDecompressor*			lpObject,
	unsigned long int					dwBytesToWrite
);

/*
	This method transfers as much bytes as possible
	from the input to the output. It should be limited by reaching
	either end of stream or an finished or error state of the
	decompressor
*/
enum minorError minorDecompressor_Execute(
	struct minorDecompressor*			lpObject
);

/*
	Write compressed data from memory into the decompressor
*/
enum minorError minorDecompressor_WriteMem(
	struct minorDecompressor*			lpObject,

	uint8_t* 							lpDataIn,
	unsigned long int 					dwBytesToWrite,
	unsigned long int					*dwBytesDone
);

/*
	Read uncompressed data from the decompressor into memory
*/
enum minorError minorDecompressor_ReadMem(
	struct minorDecompressor*			lpObject,

	uint8_t* 							lpDataOut,
	unsigned long int 					dwBytesToWrite
);

/*
	Base structures used for compressor and decompressor
*/
struct minorDecompressor {
	enum minorAlgorithm					algorithm; /* The algorithm field is used by the multiplexer */
	unsigned long int 					dwReserved;
};
struct minorCompressor {
	enum minorAlgorithm					algorithm; /* The algorithm field is used by the multiplexer */
	unsigned long int 					dwReserved;
};


/*
	Internal-only functions and definitions ...

	NEVER EVER use them inside your application!
*/

#ifdef __is_inside_libminor_E7B72649_9D2F_4078_8EA0_5D7B61775925
	/*
		Huffman utility functions
	*/

	/*!
		\brief Create huffman codes from alphabet and frequencies

		\param lpAlphabet 	Is used to input alphabet, frequencies and
							additional bit count. The huffman code, mask
							and length fields are filled by this generation
							routine.
		\param lpSystem 	References the system API to dynamically allocate
							the single required memory block.
		\param dwMaximumCodeBits Specifies the maximum bitlength of each
							generated huffman code. The package-merge algorithm
							generates shorter codes if possible but is
							guaranteed to generate no code longer than
							dwMaximumCodeBits bits. If a correct huffman code
							is not possible the function returns minorE_NoSolution.

		The minorHuffutilCreateCodes function creates huffman codes, masks
		and codelengths from a list of all supported alphabet codes (uint32_t)
		and their frequencies (note that frequencies can also be counts or
		probabilities - they don't have to be normalized).

		The function currently uses the package merge algorithm and requires
		O(n * (maxBits+2) * alphabetSymbols) space, runtime is also linear
		in alphabet symbols and maxbits

		\returns minorE_Ok	Successfully filled the alphabet structure
		\returns minorE_InvalidParam Any parameter has not been passed correclty.
							This may be NULL pointers for alphabet or system API
		\returns minorE_NoSolution There is no solution to encode the specified
							alphabet inside the specified bitlength. Note that
							the algorithm only considers only alphabet symbols
							with a frequency / count / probability greater than
							zero.
		\returns minorE_OutOfMemory There is not enough memory available to
							perform the allocation of the state block.

		Any other error code can be passed through from the underlying
		allocation functions (minorE_OutOfMemory for example)
	*/

	struct minorHuffutilAlphabetEntry_U32 {
		uint32_t												dwSymbol;
		double													dProbability;
		uint8_t													bAdditionalBits;

		uint32_t 												huffCode;
		uint32_t												huffMask;
		uint32_t 												huffLength;
	};

	struct minorHuffutilAlphabet {
		unsigned long int										dwAlphabetSize;
		struct minorHuffutilAlphabetEntry_U32					entry[];
	};

	enum minorError minorHuffutilCreateCodes(
		struct minorHuffutilAlphabet							*lpAlphabet,
		struct minorSystemInterface								*lpSystem,
		unsigned long int										dwMaximumCodeBits
	);

	/*
		Deflate (RFC1951)
	*/
	enum minorError minorDecompressorCreate_Deflate(
	 	struct minorDecompressor** 			lpOut,			/* Output location for the compressor object. There will be NO REUSE. Any previous object pointed to will be orphaned if not released before! */

	 	enum minorAlgorithm 				algorithm,		/*  Allows to select the algorithm to be used during data compression */
	 	struct minorSystemInterface* 		lpSystem,		/* Required system interface */
	 	struct minorConfigurationElement* 	lpConfiguration	/* Pointer to the first configuration element inside the configuration chain or NULL */
	);

	enum minorError minorDecompressorRelease_Deflate(
	 	struct minorDecompressor* 			lpObject
	);

	enum minorError minorDecompressorAttachSource_Deflate(
	 	struct minorDecompressor* 			lpObject,
	 	struct minorStreamSource* 			lpSource
	);

	enum minorError minorDecompressorAttachSink_Deflate(
	 	struct minorDecompressor* 			lpObject,
	 	struct minorStreamSink* 			lpSink
	);

	enum minorError minorDecompressor_TransferFromInput_Deflate(
	 	struct minorDecompressor* 			lpObject,
	 	unsigned long int 					dwBytesToRead,
	 	unsigned long int *lpBytesDone
	);

	enum minorError minorDecompressor_Execute_Deflate(
	 	struct minorDecompressor* lpObject
	);

	enum minorError minorDecompressor_WriteMem_Deflate(
	 	struct minorDecompressor* lpObjectParam,

	 	uint8_t* lpDataIn,
	 	unsigned long int dwBytesToWrite,
	 	unsigned long int *dwBytesDone
	);

	enum minorError minorDecompressor_CheckConfiguration_Deflate(
		struct minorConfigurationElement*	lpConfiguration
	);



	enum minorError minorCompressorCreate_Deflate(
		struct minorCompressor** 			lpOut,			/* Output location for the compressor object. There will be NO REUSE. Any previous object pointed to will be orphaned if not released before! */

		enum minorAlgorithm					algorithm,		/*  Allows to select the algorithm to be used during data compression */
		struct minorSystemInterface*		lpSystem,		/* Required system interface */
		struct minorConfigurationElement* 	lpConfiguration	/* Pointer to the first configuration element inside the configuration chain or NULL */
	);
	enum minorError minorCompressorRelease_Deflate(
		struct minorCompressor*				lpObject		/* Allows releasing the minor object */
	);
	enum minorError minorCompressorAttachSource_Deflate(
		struct minorCompressor*				lpObject,
		struct minorStreamSource*			lpSource
	);
	enum minorError minorCompressorAttachSink_Deflate(
		struct minorCompressor*				lpObject,
		struct minorStreamSink*				lpSink
	);
	enum minorError minorCompressor_TransferFromInput_Deflate(
		struct minorCompressor				*lpObject,
		unsigned long int 					dwBytesToRead,
		unsigned long int					*lpBytesDone
	);
	enum minorError minorCompressor_Execute_Deflate(
		struct minorCompressor*				lpObject
	);
	enum minorError minorCompressor_WriteMem_Deflate(
		struct minorCompressor*				lpObjectParam,

		uint8_t* 							lpDataIn,
		unsigned long int 					dwBytesToWrite,
		unsigned long int					*dwBytesDone
	);
	enum minorError minorCompressorContinue_Deflate(
		struct minorCompressor				*lpObject
	);




	/*
		GZIP (RFC1952)
	*/
	enum minorError minorDecompressorCreate_GZip(
		struct minorDecompressor** 			lpOut,			/* Output location for the compressor object. There will be NO REUSE. Any previous object pointed to will be orphaned if not released before! */

		enum minorAlgorithm 				algorithm,		/*  Allows to select the algorithm to be used during data compression */
		struct minorSystemInterface* 		lpSystem,		/* Required system interface */
		struct minorConfigurationElement* 	lpConfiguration	/* Pointer to the first configuration element inside the configuration chain or NULL */
	);

	enum minorError minorDecompressorRelease_GZip(
		struct minorDecompressor* 			lpObject
	);

	enum minorError minorDecompressorAttachSource_GZip(
		struct minorDecompressor* 			lpObject,
		struct minorStreamSource* 			lpSource
	);

	enum minorError minorDecompressorAttachSink_GZip(
		struct minorDecompressor* 			lpObject,
		struct minorStreamSink* 			lpSink
	);

	enum minorError minorDecompressor_TransferFromInput_GZip(
		struct minorDecompressor* 			lpObject,
		unsigned long int 					dwBytesToRead,
		unsigned long int *lpBytesDone
	);

	enum minorError minorDecompressor_Execute_GZip(
		struct minorDecompressor* lpObject
	);

	enum minorError minorDecompressor_WriteMem_GZip(
		struct minorDecompressor* lpObjectParam,

		uint8_t* lpDataIn,
		unsigned long int dwBytesToWrite,
		unsigned long int *dwBytesDone
	);

	enum minorError minorDecompressor_CheckConfiguration_GZip(
		struct minorConfigurationElement* lpConfiguration
	);


	enum minorError minorCompressorTransferFromInput_GZip(
		struct minorCompressor*				lpObject,
		unsigned long int 					dwBytesToRead,
		unsigned long int*					lpBytesDone
	);
	enum minorError minorCompressorTransferToOutput_GZip(
		struct minorCompressor*				lpObject,
		unsigned long int					dwBytesToWrite
	);
	enum minorError minorCompressor_Execute_GZip(
		struct minorCompressor*				lpObject
	);
	enum minorError minorCompressor_WriteMem_GZip(
		struct minorCompressor*				lpObject,

		uint8_t* 							lpDataIn,
		unsigned long int 					dwBytesToWrite,
		unsigned long int					*dwBytesDone
	);
	enum minorError minorCompressorAttachSource_GZip(
		struct minorCompressor* 			lpObject,
		struct minorStreamSource* 			lpSource
	);
	enum minorError minorCompressorAttachSink_GZip(
		struct minorCompressor* 			lpObject,
		struct minorStreamSink* 			lpSink
	);
	enum minorError minorCompressorContinue_GZip(
		struct minorCompressor* lpObject
	);
	enum minorError minorCompressorRelease_GZip(
		struct minorCompressor* 			lpObject
	);
	enum minorError minorCompressorCreate_GZip(
		struct minorCompressor** 			lpOut,			/* Output location for the compressor object. There will be NO REUSE. Any previous object pointed to will be orphaned if not released before! */

		enum minorAlgorithm					algorithm,		/*  Allows to select the algorithm to be used during data compression */
		struct minorSystemInterface*		lpSystem,		/* Required system interface */
		struct minorConfigurationElement* 	lpConfiguration	/* Pointer to the first configuration element inside the configuration chain or NULL */
	);
	enum minorError minorCompressor_CheckConfiguration_GZip(
		struct minorConfigurationElement* lpConfiguration
	);
#endif


#ifdef __cplusplus
	} /* extern "C" { */
#endif

#endif /* #ifndef __is_included__c59c25d7_46fe_4664_bf95_4ab6dbd3f431 */
