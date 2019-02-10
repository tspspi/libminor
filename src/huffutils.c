#define __is_inside_libminor_E7B72649_9D2F_4078_8EA0_5D7B61775925 1

#include "../include/minor.h"

#ifdef __cplusplus
	extern "C" {
#endif

struct minorHuffutil_TempQEntry {
	unsigned long int										idxLeft;
	unsigned long int										idxRight;
	double													dProbability;
};
struct minorHuffutil_Codelengths {
	unsigned long int 										idx;
	uint32_t												huffLength;
};

/*
	Sort a codelength / index table by codelength (ascending order)

	Currently use bubblesort because alphabet sizes are soo small that
	overhead of alternative algorithms is too large.
*/
static void minorHuffutil_SortCLen(
	struct minorHuffutilAlphabet *lpAlphabet,
	struct minorHuffutil_Codelengths* lpCLen,
	unsigned long int dwEntries
) {
	struct minorHuffutil_Codelengths tmp;
	bool bDone;
	unsigned long int i;

	if((lpCLen == NULL) || (dwEntries < 2) || (lpAlphabet == NULL)) { return; }

	do {
		bDone = true;
		for(i = 0; i < dwEntries-1; i=i+1) {
			if(
				(lpCLen[i].huffLength > lpCLen[i+1].huffLength)
				|| ((lpCLen[i].huffLength == lpCLen[i+1].huffLength) && (lpAlphabet->entry[lpCLen[i].idx].dwSymbol > lpAlphabet->entry[lpCLen[i+1].idx].dwSymbol))
			) {
				bDone = false;

				tmp.huffLength 				= lpCLen[i].huffLength;
				tmp.idx 					= lpCLen[i].idx;

				lpCLen[i].huffLength 		= lpCLen[i+1].huffLength;
				lpCLen[i].idx 				= lpCLen[i+1].idx;

				lpCLen[i+1].huffLength 		= tmp.huffLength;
				lpCLen[i+1].idx				= tmp.idx;
			}
		}
	} while(bDone != true);

	return;
}

/*
	Sort a merged array Q[n] by probability (ascending)
*/
static void minorHuffutil_SortQ(
	struct minorHuffutil_TempQEntry* lpEntries,
	unsigned long int dwBase,
	unsigned long int dwCount
) {
	struct minorHuffutil_TempQEntry tmp;
	bool bDone;
	unsigned long int i;

	if((lpEntries == NULL) || (dwCount < 2)) { return; }

	/* We do bubblesort ... */
	do {
		bDone = true;
		for(i = 0; i < dwCount-1; i=i+1) {
			if(lpEntries[dwBase + i].dProbability > lpEntries[dwBase + i + 1].dProbability) {
				tmp.idxLeft 							= lpEntries[dwBase + i].idxLeft;
				tmp.idxRight 							= lpEntries[dwBase + i].idxRight;
				tmp.dProbability 						= lpEntries[dwBase + i].dProbability;

				lpEntries[dwBase + i].idxLeft 			= lpEntries[dwBase + i + 1].idxLeft;
				lpEntries[dwBase + i].idxRight 			= lpEntries[dwBase + i + 1].idxRight;
				lpEntries[dwBase + i].dProbability 		= lpEntries[dwBase + i + 1].dProbability;

				lpEntries[dwBase + i + 1].idxLeft 		= tmp.idxLeft;
				lpEntries[dwBase + i + 1].idxRight 		= tmp.idxRight;
				lpEntries[dwBase + i + 1].dProbability 	= tmp.dProbability;

				bDone = false;
			}
		}
	} while(bDone != true);

	return;
}

/*
	Count through packages (recursive) until we hit an alphabet
	literal.
*/
static void minorHuffutilCreateCodes_CountRecursive(
	struct minorHuffutilAlphabet							*lpAlphabet,

	struct minorHuffutil_TempQEntry*						lpQP,
	unsigned long int										dwCurrentLevel,
	unsigned long int										dwQWidth,
	unsigned long int										dwCurrentIndex
) {
	if(lpQP[dwCurrentLevel*dwQWidth + dwCurrentIndex].idxLeft == (~0)) {
		lpAlphabet->entry[lpQP[dwCurrentLevel*dwQWidth + dwCurrentIndex].idxRight].huffLength = lpAlphabet->entry[lpQP[dwCurrentLevel*dwQWidth + dwCurrentIndex].idxRight].huffLength + 1;
	} else {
		minorHuffutilCreateCodes_CountRecursive(lpAlphabet, lpQP, dwCurrentLevel-1, dwQWidth, lpQP[dwCurrentLevel*dwQWidth + dwCurrentIndex].idxLeft);
		minorHuffutilCreateCodes_CountRecursive(lpAlphabet, lpQP, dwCurrentLevel-1, dwQWidth, lpQP[dwCurrentLevel*dwQWidth + dwCurrentIndex].idxRight);
	}
}

enum minorError minorHuffutilCreateCodes(
	struct minorHuffutilAlphabet							*lpAlphabet,
	struct minorSystemInterface								*lpSystem,
	unsigned long int										dwMaximumCodeBits
) {
	unsigned long int dwUsedAlphabetSymbols;
	unsigned long int dwMinimumCodeLength;
	unsigned long int i,j,k;
	unsigned long int dwBufferSize;
	unsigned long int dwUsedPackage;
	uint32_t dwTemp;
	enum minorError e;

	unsigned char*						lpBuffer;
	struct minorHuffutil_TempQEntry*	lpQP; /* Q[i] and temporary package buffer */
	struct minorHuffutil_TempQEntry*	lpPackage; /* Package buffer */
	unsigned long int*					lpUsedQ; /* Counter for used entries inside the Q buffers */
	struct minorHuffutil_Codelengths*	lpCLens; /* Codeword lengths */

	uint32_t							dwCurrentCode;
	uint32_t							dwCurrentMask;
	unsigned long int					dwCurrentCodeLength;

	if((lpAlphabet == NULL) || (lpSystem == NULL)) { return minorE_InvalidParam; }

	/* Count used symbols in alphabet */
	dwUsedAlphabetSymbols = 0;
	for(i = 0; i < lpAlphabet->dwAlphabetSize; i=i+1) { if(lpAlphabet->entry[i].dProbability != 0) { dwUsedAlphabetSymbols = dwUsedAlphabetSymbols + 1; } }

	/* Calculate minimum codelength to be used ... */
	dwMinimumCodeLength = 0;
	dwTemp = 1;
	while(dwTemp < dwUsedAlphabetSymbols) { dwTemp = dwTemp << 1; dwMinimumCodeLength = dwMinimumCodeLength + 1; }

	/* Check requested maximum code bits >= required minimum length */
	if(dwMinimumCodeLength > dwMaximumCodeBits) { return minorE_NoSolution; }

	/* Allocate buffers */
	dwBufferSize = sizeof(struct minorHuffutil_TempQEntry)*2*dwUsedAlphabetSymbols*(dwMinimumCodeLength+1 + 1); /* Temp Q and package */
	dwBufferSize = dwBufferSize + sizeof(unsigned long int)*(dwMinimumCodeLength+1); /* Used Q */

	if(dwBufferSize < sizeof(struct minorHuffutil_Codelengths)*dwUsedAlphabetSymbols) {
		dwBufferSize = sizeof(struct minorHuffutil_Codelengths)*dwUsedAlphabetSymbols;
	}

	if((e = lpSystem->alloc((void**)(&lpBuffer), dwBufferSize, lpSystem->lpFreeParam_Alloc)) != minorE_Ok) {
		return e;
	}

	/* Generate local pointers into the data buffer */
	lpQP = (struct minorHuffutil_TempQEntry*)lpBuffer;
	lpPackage = &(lpQP[(dwMinimumCodeLength+1)*2*dwUsedAlphabetSymbols]);
	lpUsedQ = (unsigned long int*)(&(lpBuffer[sizeof(struct minorHuffutil_TempQEntry)*2*dwUsedAlphabetSymbols*(dwMinimumCodeLength+1+1)]));
	lpCLens = (struct minorHuffutil_Codelengths*)lpBuffer;

	/* Fill Q[0] */
	lpUsedQ[0] = 0;
	for(i = 0; i < lpAlphabet->dwAlphabetSize; i=i+1) {
		if(lpAlphabet->entry[i].dProbability != 0) {
			lpQP[lpUsedQ[0]].idxLeft = ~0;
			lpQP[lpUsedQ[0]].idxRight = i;
			lpQP[lpUsedQ[0]].dProbability = lpAlphabet->entry[i].dProbability;
			lpUsedQ[0] = lpUsedQ[0] + 1;
		}
	}

	/* Sort Q[0] */
	minorHuffutil_SortQ(lpQP, 0, lpUsedQ[0]);

	/* Package merge cycles */
	for(i = 1; i < dwMinimumCodeLength+1; i=i+1) {
		/*
			Create package by merging Q[i-1] entries into the package. In case of an odd
		 	number of coins simply discard the highest probable one.
		*/
		for(j = 0; j < lpUsedQ[i-1]/2; j=j+1) {
			lpPackage[j].idxLeft = 2*j + 0;
			lpPackage[j].idxRight = 2*j + 1;
			lpPackage[j].dProbability = lpQP[2*dwUsedAlphabetSymbols*(i-1) + 2*j + 0].dProbability + lpQP[2*dwUsedAlphabetSymbols*(i-1) + 2*j + 1].dProbability;
		}
		dwUsedPackage = j;

		/* Merge package and Q[0] into Q[i] */
		lpUsedQ[i] = 0;
		j = 0; k = 0;

		while((j < lpUsedQ[0]) || (k < dwUsedPackage)) {
			while(
				((k >= dwUsedPackage) && (j < lpUsedQ[0])) ||
				((j < lpUsedQ[0]) && (lpPackage[k].dProbability >= lpQP[j].dProbability))
			) {
				lpQP[2*dwUsedAlphabetSymbols*i + lpUsedQ[i]].idxLeft = ~0;
				lpQP[2*dwUsedAlphabetSymbols*i + lpUsedQ[i]].idxRight = lpQP[j].idxRight;
				lpQP[2*dwUsedAlphabetSymbols*i + lpUsedQ[i]].dProbability = lpQP[j].dProbability;

				lpUsedQ[i] = lpUsedQ[i] + 1;
				j = j + 1;
			}

			while(
				((j >= lpUsedQ[0]) && (k < dwUsedPackage)) ||
				((k < dwUsedPackage) && (lpPackage[k].dProbability < lpQP[j].dProbability))
			) {
				lpQP[2*dwUsedAlphabetSymbols*i + lpUsedQ[i]].idxLeft = lpPackage[k].idxLeft;
				lpQP[2*dwUsedAlphabetSymbols*i + lpUsedQ[i]].idxRight = lpPackage[k].idxRight;
				lpQP[2*dwUsedAlphabetSymbols*i + lpUsedQ[i]].dProbability = lpPackage[k].dProbability;

				lpUsedQ[i] = lpUsedQ[i] + 1;
				k = k + 1;
			}
		}
	}

	/* Calculate codelengths */
	for(j = 0; j < lpAlphabet->dwAlphabetSize; j=j+1) { lpAlphabet->entry[j].huffLength = 0; }
	for(j = 0; j < lpUsedQ[dwMinimumCodeLength] - (lpUsedQ[dwMinimumCodeLength] % 2); j=j+1) { minorHuffutilCreateCodes_CountRecursive(lpAlphabet, lpQP, dwMinimumCodeLength, 2*dwUsedAlphabetSymbols, j); }

	/* Now create index / length table, sort by length and generate huffman codes ... */
	j = 0;
	for(i = 0; i < lpAlphabet->dwAlphabetSize; i=i+1) {
		if(lpAlphabet->entry[i].huffLength > 0) {
			lpCLens[j].idx = i;
			lpCLens[j].huffLength = lpAlphabet->entry[i].huffLength;
			j = j + 1;
		}
	}

	/* Sort by huffman symbol length ... */
	minorHuffutil_SortCLen(lpAlphabet, lpCLens, j);

	/* Generate codes ... */
	dwCurrentCode = 0;
	dwCurrentCodeLength = 0;
	dwCurrentMask = 0;

	for(i = 0; i < j; i=i+1) {
		/* "i" is the current index into CLen array */
		while(dwCurrentCodeLength < lpCLens[i].huffLength) {
			dwCurrentCode = dwCurrentCode << 1;
			dwCurrentMask = (dwCurrentMask << 1) | 0x00000001;
			dwCurrentCodeLength = dwCurrentCodeLength + 1;
		}

		/* Assign the next code */
		lpAlphabet->entry[lpCLens[i].idx].huffCode = dwCurrentCode;
		lpAlphabet->entry[lpCLens[i].idx].huffMask = dwCurrentMask;
		dwCurrentCode = dwCurrentCode + 1;
	}

	if(lpBuffer != NULL) { lpSystem->free(lpBuffer, lpSystem->lpFreeParam_Free); lpBuffer = NULL; lpQP = NULL; lpUsedQ = NULL; lpCLens = NULL; }
	return e;
}

#ifdef __cplusplus
	} /* extern "C" { */
#endif
