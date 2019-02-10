#define __is_inside_libminor_E7B72649_9D2F_4078_8EA0_5D7B61775925 1 /* Because we test internal functions ... */

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






struct testData_Alphabet {
	uint32_t dwSymbol;
	double dProbability;
	uint8_t additionalBits;
};

// #define testdata_Alphabet_1_CodelengthLimit 16
/* struct testData_Alphabet testData_Alphabet_1[8+2] = {
	{ 0, 0.001, 0 },
	{ 1, 0.05, 0 },
	{ 2, 0.1, 0 },
	{ 3, 0.15, 0 },
	{ 4, 0.2, 0 },
	{ 5, 0.22, 0 },
	{ 6, 0.3, 0 },
	{ 7, 0.007, 0 },

	{ 8, 0.13, 0 },
	{ 9, 0.8, 0 }
};
*/

#define testdata_Alphabet_1_CodelengthLimit 32
struct testData_Alphabet testData_Alphabet_1[6] = {
	{ 1, 0.05, 0 },
	{ 2, 0.1, 0 },
	{ 3, 0.15, 0 },
	{ 4, 0.2, 0 },
	{ 5, 0.2, 0 },
	{ 6, 0.3, 0 }
};



#define testdata_Alphabet_2_CodelengthLimit 32
#define testdata_Alphabet_2_AlphabetSize 259


int main(int argc, char* argv[]) {
	/* Test alphabet generation ... */
	struct minorHuffutilAlphabet* lpAlpha;
	unsigned long int i,j;
	uint32_t dwTemp;
	uint32_t dwTempMask;
	enum minorError e;
	bool bSuccess = true;
	double dAverageLen;

	srand(1234); /* Always use the same seed to have a reproduceable expected output */

	{
		lpAlpha = (struct minorHuffutilAlphabet*)malloc(sizeof(struct minorHuffutilAlphabet) + sizeof(testData_Alphabet_1)/sizeof(struct testData_Alphabet) * sizeof(struct minorHuffutilAlphabetEntry_U32));
		if(lpAlpha == NULL) {
			printf("%s:%u Allocation failed\n", __FILE__, __LINE__);
			return -1;
		}

		lpAlpha->dwAlphabetSize = sizeof(testData_Alphabet_1)/sizeof(struct testData_Alphabet);
		for(i = 0; i < sizeof(testData_Alphabet_1)/sizeof(struct testData_Alphabet); i=i+1) {
			lpAlpha->entry[i].dwSymbol = testData_Alphabet_1[i].dwSymbol;
			lpAlpha->entry[i].dProbability = testData_Alphabet_1[i].dProbability;
			lpAlpha->entry[i].bAdditionalBits = testData_Alphabet_1[i].additionalBits;
		}

		e = minorHuffutilCreateCodes(lpAlpha, &sysApi, testdata_Alphabet_1_CodelengthLimit);
		if(e != minorE_Ok) {
			printf("%s:%u Generation failed. Status %u\n", __FILE__, __LINE__, e);
			free((void*)lpAlpha);
			return -1;
		}

		/* Dump alphabet */
		printf("%s:%u Dumping alphabet\n", __FILE__, __LINE__);
		printf("Symbol\t\tProb.\t\tAdd Bits\tHuffCode\t\tHuffMask\tHuffLength\n");
		for(i = 0; i < lpAlpha->dwAlphabetSize; i=i+1) {
			printf("0x%08x\t%lf\t%4u\t\t",  lpAlpha->entry[i].dwSymbol, lpAlpha->entry[i].dProbability, lpAlpha->entry[i].bAdditionalBits);

			dwTemp = (uint32_t)lpAlpha->entry[i].huffCode;
			dwTemp = dwTemp << (32 - lpAlpha->entry[i].huffLength);

			for(j = 0; j < lpAlpha->entry[i].huffLength; j=j+1) { printf("%s", ((dwTemp & 0x80000000) == 0) ? "0" : "1"); dwTemp = dwTemp << 1; }
			for(j = 0; j < (16 - lpAlpha->entry[i].huffLength); j=j+1) { printf(" "); }

			printf("\t0x%08x\t%u\n",lpAlpha->entry[i].huffMask, lpAlpha->entry[i].huffLength);
		}

		/* Check that the alphabet is really possible and is really prefix free ... */
		for(i = 0; i < lpAlpha->dwAlphabetSize; i=i+1) {
			if((lpAlpha->entry[i].huffLength == 0) && (lpAlpha->entry[i].dProbability == 0)) { continue; }
			if((lpAlpha->entry[i].huffLength == 0) && (lpAlpha->entry[i].dProbability != 0)) {
				printf("FAILED: Probability for symbol 0x%08x is not zero but huffman code length is zero\n", lpAlpha->entry[i].dwSymbol);
				bSuccess = false;
			}
			if((lpAlpha->entry[i].huffLength != 0) && (lpAlpha->entry[i].dProbability == 0)) {
				printf("FAILED: Probability for symbol 0x%08x is zero but huffman code exists\n", lpAlpha->entry[i].dwSymbol);
				bSuccess = false;
			}

			/* Check if THIS code is prefix for any OTHER code */
			dwTemp = lpAlpha->entry[i].huffCode << (32 - lpAlpha->entry[i].huffLength);
			dwTempMask = lpAlpha->entry[i].huffMask << (32 - lpAlpha->entry[i].huffLength);
			for(j = 0; j < lpAlpha->dwAlphabetSize; j=j+1) {
				if(i == j) { continue; }
				if((lpAlpha->entry[j].huffLength == 0) || (lpAlpha->entry[j].dProbability == 0)) { continue; }

				if(((lpAlpha->entry[j].huffCode << (32 - lpAlpha->entry[j].huffLength)) & dwTempMask) == dwTemp) {
					printf("FAILED: Code for symbol 0x%08x is prefix to symbol 0x%08x or vice versa\n", lpAlpha->entry[i].dwSymbol, lpAlpha->entry[j].dwSymbol);
					bSuccess = false;
				}
			}
		}
		if(bSuccess != true) { return -1; }
		printf("Codes are prefix free, OK\n");

		/* Check if codes honor length limit */
		for(i = 0; i < lpAlpha->dwAlphabetSize; i=i+1) {
			if(lpAlpha->entry[i].huffLength > testdata_Alphabet_1_CodelengthLimit) {
				printf("FAILED: Code for symbol 0x%08x violated codelength constraint (should be max. %u bits)\n", lpAlpha->entry[i].dwSymbol, testdata_Alphabet_1_CodelengthLimit);
				bSuccess = false;
			}
		}
		if(bSuccess != true) { return -1; }
		printf("Codes honor length limit, OK\n");

		/* Calculate average symbol length. To do so we normalize probability first then then use expectation value */
		dAverageLen = 0;
		for(i = 0; i < lpAlpha->dwAlphabetSize; i=i+1) { dAverageLen = dAverageLen + lpAlpha->entry[i].dProbability; }
		for(i = 0; i < lpAlpha->dwAlphabetSize; i=i+1) { lpAlpha->entry[i].dProbability = lpAlpha->entry[i].dProbability / dAverageLen; }
		dAverageLen = 0;
		for(i = 0; i < lpAlpha->dwAlphabetSize; i=i+1) { dAverageLen = dAverageLen + lpAlpha->entry[i].dProbability * lpAlpha->entry[i].huffLength; }
		printf("Average code length: %lf\n", dAverageLen);
	}

	{
		lpAlpha = (struct minorHuffutilAlphabet*)malloc(sizeof(struct minorHuffutilAlphabet) + testdata_Alphabet_2_AlphabetSize * sizeof(struct minorHuffutilAlphabetEntry_U32));
		if(lpAlpha == NULL) {
			printf("%s:%u Allocation failed\n", __FILE__, __LINE__);
			return -1;
		}

		lpAlpha->dwAlphabetSize = testdata_Alphabet_2_AlphabetSize;
		for(i = 0; i < testdata_Alphabet_2_AlphabetSize; i=i+1) {
			lpAlpha->entry[i].dwSymbol = i;
			lpAlpha->entry[i].dProbability = (double)rand()/(double)RAND_MAX;
			lpAlpha->entry[i].bAdditionalBits = 0;
		}
		lpAlpha->entry[0].dProbability = 1e5;
		lpAlpha->entry[1].dProbability = 1e4;


		e = minorHuffutilCreateCodes(lpAlpha, &sysApi, testdata_Alphabet_2_CodelengthLimit);
		if(e != minorE_Ok) {
			printf("%s:%u Generation failed. Status %u\n", __FILE__, __LINE__, e);
			free((void*)lpAlpha);
			return -1;
		}

		/* Dump alphabet */
		printf("%s:%u Dumping alphabet\n", __FILE__, __LINE__);
		printf("Symbol\t\tProb.\t\tAdd Bits\tHuffCode\t\tHuffMask\tHuffLength\n");
		for(i = 0; i < lpAlpha->dwAlphabetSize; i=i+1) {
			printf("0x%08x\t%lf\t%4u\t\t",  lpAlpha->entry[i].dwSymbol, lpAlpha->entry[i].dProbability, lpAlpha->entry[i].bAdditionalBits);

			dwTemp = (uint32_t)lpAlpha->entry[i].huffCode;
			dwTemp = dwTemp << (32 - lpAlpha->entry[i].huffLength);

			for(j = 0; j < lpAlpha->entry[i].huffLength; j=j+1) { printf("%s", ((dwTemp & 0x80000000) == 0) ? "0" : "1"); dwTemp = dwTemp << 1; }
			for(j = 0; j < (32 - lpAlpha->entry[i].huffLength); j=j+1) { printf(" "); }

			printf("\t0x%08x\t%u\n",lpAlpha->entry[i].huffMask, lpAlpha->entry[i].huffLength);
		}

		/* Check that the alphabet is really possible and is really prefix free ... */
		for(i = 0; i < lpAlpha->dwAlphabetSize; i=i+1) {
			if((lpAlpha->entry[i].huffLength == 0) && (lpAlpha->entry[i].dProbability == 0)) { continue; }
			if((lpAlpha->entry[i].huffLength == 0) && (lpAlpha->entry[i].dProbability != 0)) {
				printf("FAILED: Probability for symbol 0x%08x is not zero but huffman code length is zero\n", lpAlpha->entry[i].dwSymbol);
				bSuccess = false;
			}
			if((lpAlpha->entry[i].huffLength != 0) && (lpAlpha->entry[i].dProbability == 0)) {
				printf("FAILED: Probability for symbol 0x%08x is zero but huffman code exists\n", lpAlpha->entry[i].dwSymbol);
				bSuccess = false;
			}

			/* Check if THIS code is prefix for any OTHER code */
			dwTemp = lpAlpha->entry[i].huffCode << (32 - lpAlpha->entry[i].huffLength);
			dwTempMask = lpAlpha->entry[i].huffMask << (32 - lpAlpha->entry[i].huffLength);
			for(j = 0; j < lpAlpha->dwAlphabetSize; j=j+1) {
				if(i == j) { continue; }
				if((lpAlpha->entry[j].huffLength == 0) || (lpAlpha->entry[j].dProbability == 0)) { continue; }

				if(((lpAlpha->entry[j].huffCode << (32 - lpAlpha->entry[j].huffLength)) & dwTempMask) == dwTemp) {
					printf("FAILED: Code for symbol 0x%08x is prefix to symbol 0x%08x or vice versa\n", lpAlpha->entry[i].dwSymbol, lpAlpha->entry[j].dwSymbol);
					bSuccess = false;
				}
			}
		}
		if(bSuccess != true) { return -1; }
		printf("Codes are prefix free, OK\n");

		/* Check if codes honor length limit */
		for(i = 0; i < lpAlpha->dwAlphabetSize; i=i+1) {
			if(lpAlpha->entry[i].huffLength > testdata_Alphabet_2_CodelengthLimit) {
				printf("FAILED: Code for symbol 0x%08x violated codelength constraint (should be max. %u bits)\n", lpAlpha->entry[i].dwSymbol, testdata_Alphabet_1_CodelengthLimit);
				bSuccess = false;
			}
		}
		if(bSuccess != true) { return -1; }
		printf("Codes honor length limit, OK\n");

		/* Calculate average symbol length. To do so we normalize probability first then then use expectation value */
		dAverageLen = 0;
		for(i = 0; i < lpAlpha->dwAlphabetSize; i=i+1) { dAverageLen = dAverageLen + lpAlpha->entry[i].dProbability; }
		for(i = 0; i < lpAlpha->dwAlphabetSize; i=i+1) { lpAlpha->entry[i].dProbability = lpAlpha->entry[i].dProbability / dAverageLen; }
		dAverageLen = 0;
		for(i = 0; i < lpAlpha->dwAlphabetSize; i=i+1) { dAverageLen = dAverageLen + lpAlpha->entry[i].dProbability * lpAlpha->entry[i].huffLength; }
		printf("Average code length: %lf\n", dAverageLen);
	}

	free((void*)lpAlpha);
	if(bSuccess != false) {
		return 0;
	} else {
		return -1;
	}
}
