#define __is_inside_libminor_E7B72649_9D2F_4078_8EA0_5D7B61775925 1
#include "../include/minor.h"

#ifdef __cplusplus
	extern "C" {
#endif


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
) {
	if(lpOut == NULL) { return minorE_InvalidParam; }

	switch(algorithm) {
		case minorAlgorithm_Deflate:		return minorDecompressorCreate_Deflate(lpOut, algorithm, lpSystem, lpConfiguration);
		case minorAlgorithm_Gzip:			return minorDecompressorCreate_GZip(lpOut, algorithm, lpSystem, lpConfiguration);
		default:							return minorE_UnknownAlgorithm;
	}
}

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
) {
	if(lpObject == NULL) { return minorE_InvalidParam; }

	switch(lpObject->algorithm) {
		case minorAlgorithm_Deflate:		return minorDecompressorRelease_Deflate(lpObject);
		case minorAlgorithm_Gzip:			return minorDecompressorRelease_GZip(lpObject);
		default:							return minorE_ImplementationError;
	}
}

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
) {
	if(lpObject == NULL) { return minorE_InvalidParam; }

	switch(lpObject->algorithm) {
		case minorAlgorithm_Deflate:		return minorDecompressorAttachSource_Deflate(lpObject, lpSource);
		case minorAlgorithm_Gzip:			return minorDecompressorAttachSource_GZip(lpObject, lpSource);
		default:							return minorE_ImplementationError;
	}
}

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
) {
	if(lpObject == NULL) { return minorE_InvalidParam; }

	switch(lpObject->algorithm) {
		case minorAlgorithm_Deflate:		return minorDecompressorAttachSink_Deflate(lpObject, lpSink);
		case minorAlgorithm_Gzip:			return minorDecompressorAttachSink_GZip(lpObject, lpSink);
		default:							return minorE_ImplementationError;
	}
}

/*
	Getting data into/out from the decompressor or compressor
*/

enum minorError minorDecompressor_TransferFromInput(
	struct minorDecompressor*			lpObject,
	unsigned long int 					dwBytesToRead,
	unsigned long int					*lpBytesDone
) {
	if(lpObject == NULL) { return minorE_InvalidParam; }

	switch(lpObject->algorithm) {
		case minorAlgorithm_Deflate:		return minorDecompressor_TransferFromInput_Deflate(lpObject, dwBytesToRead, lpBytesDone);
		case minorAlgorithm_Gzip:			return minorDecompressor_TransferFromInput_GZip(lpObject, dwBytesToRead, lpBytesDone);
		default:							return minorE_ImplementationError;
	}
}
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
) {
	if(lpObject == NULL) { return minorE_InvalidParam; }

	switch(lpObject->algorithm) {
		case minorAlgorithm_Deflate:		return minorDecompressor_Execute_Deflate(lpObject);
		case minorAlgorithm_Gzip:			return minorDecompressor_Execute_GZip(lpObject);
		default:							return minorE_ImplementationError;
	}
}

/*
	Write compressed data from memory into the decompressor
*/
enum minorError minorDecompressor_WriteMem(
	struct minorDecompressor*			lpObject,

	uint8_t* 							lpDataIn,
	unsigned long int 					dwBytesToWrite,
	unsigned long int					*dwBytesDone
) {
	if(lpObject == NULL) { return minorE_InvalidParam; }

	switch(lpObject->algorithm) {
		case minorAlgorithm_Deflate:		return minorDecompressor_WriteMem_Deflate(lpObject, lpDataIn, dwBytesToWrite, dwBytesDone);
		case minorAlgorithm_Gzip:			return minorDecompressor_WriteMem_GZip(lpObject, lpDataIn, dwBytesToWrite, dwBytesDone);
		default:							return minorE_ImplementationError;
	}
}

/*
	Read uncompressed data from the decompressor into memory
*/
enum minorError minorDecompressor_ReadMem(
	struct minorDecompressor*			lpObject,

	uint8_t* 							lpDataOut,
	unsigned long int 					dwBytesToWrite
);

/*
		=============================
		=	COMPRESSOR FUNCTIONS	=
		=============================
*/


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
) {
	if(lpOut == NULL) { return minorE_InvalidParam; }
	(*lpOut) = NULL;

	switch(algorithm) {
		case minorAlgorithm_Deflate:		return minorCompressorCreate_Deflate(lpOut, algorithm, lpSystem, lpConfiguration);
		case minorAlgorithm_Gzip:			return minorCompressorCreate_GZip(lpOut, algorithm, lpSystem, lpConfiguration);
		default:							return minorE_ImplementationError;
	}
}

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
) {
	if(lpObject == NULL) { return minorE_InvalidParam; }

	switch(lpObject->algorithm) {
		case minorAlgorithm_Deflate:		return minorCompressorRelease_Deflate(lpObject);
		case minorAlgorithm_Gzip:			return minorCompressorRelease_GZip(lpObject);
		default:							return minorE_ImplementationError;
	}
}

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
) {
	if(lpObject == NULL) { return minorE_InvalidParam; }

	switch(lpObject->algorithm) {
		case minorAlgorithm_Deflate:		return minorCompressorAttachSource_Deflate(lpObject, lpSource);
		case minorAlgorithm_Gzip:			return minorCompressorAttachSource_GZip(lpObject, lpSource);
		default:							return minorE_ImplementationError;
	}
}

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
) {
	if(lpObject == NULL) { return minorE_InvalidParam; }

	switch(lpObject->algorithm) {
		case minorAlgorithm_Deflate:		return minorCompressorAttachSink_Deflate(lpObject, lpSink);
		case minorAlgorithm_Gzip:			return minorCompressorAttachSink_GZip(lpObject, lpSink);
		default:							return minorE_ImplementationError;
	}
}

enum minorError minorCompressorTransferFromInput(
	struct minorCompressor*				lpObject,
	unsigned long int 					dwBytesToRead,
	unsigned long int*					lpBytesDone
) {
	if(lpObject == NULL) { return minorE_InvalidParam; }

	switch(lpObject->algorithm) {
		case minorAlgorithm_Deflate:		return minorCompressor_TransferFromInput_Deflate(lpObject, dwBytesToRead, lpBytesDone);
		case minorAlgorithm_Gzip:			return minorCompressorTransferFromInput_GZip(lpObject, dwBytesToRead, lpBytesDone);
		default:							return minorE_ImplementationError;
	}
}

enum minorError minorCompressorTransferToOutput(
	struct minorCompressor*				lpObject,
	unsigned long int					dwBytesToWrite
) {
	if(lpObject == NULL) { return minorE_InvalidParam; }

	switch(lpObject->algorithm) {
		default:							return minorE_ImplementationError;
	}
}

enum minorError minorCompressor_Execute(
	struct minorCompressor*					lpObject
) {
	if(lpObject == NULL) { return minorE_InvalidParam; }

	switch(lpObject->algorithm) {
		case minorAlgorithm_Deflate:		return minorCompressor_Execute_Deflate(lpObject);
		case minorAlgorithm_Gzip:			return minorCompressor_Execute_GZip(lpObject);
		default:							return minorE_ImplementationError;
	}
}

enum minorError minorCompressor_WriteMem(
	struct minorCompressor*				lpObject,

	uint8_t* 							lpDataIn,
	unsigned long int 					dwBytesToWrite,
	unsigned long int					*dwBytesDone
) {
	if(lpObject == NULL) { return minorE_InvalidParam; }

	switch(lpObject->algorithm) {
		case minorAlgorithm_Deflate:		return minorCompressor_WriteMem_Deflate(lpObject, lpDataIn, dwBytesToWrite, dwBytesDone);
		case minorAlgorithm_Gzip:			return minorCompressor_WriteMem_GZip(lpObject, lpDataIn, dwBytesToWrite, dwBytesDone);
		default:							return minorE_ImplementationError;
	}
}

enum minorError minorCompressorContinue(
	struct minorCompressor*				lpObject
) {
	if(lpObject == NULL) { return minorE_InvalidParam; }

	switch(lpObject->algorithm) {
		case minorAlgorithm_Deflate:		return minorCompressorContinue_Deflate(lpObject);
		case minorAlgorithm_Gzip:			return minorCompressorContinue_GZip(lpObject);
		default:							return minorE_ImplementationError;
	}
}


#ifdef __cplusplus
	} /* extern "C" { */
#endif
