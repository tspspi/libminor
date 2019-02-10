/*
	Codegenerator for CRC32 as used in RFC1951 (GZip 4.3)
	This is the same CRC32 as specified in ISO 3309 and ITU-T V.42
*/
/*
	Copyright (c) 2018, Thomas Spielauer
	All rights reserved.

	Redistribution and use in source and binary forms, with or without
	modification, are permitted provided that the following conditions are met:
	1. Redistributions of source code must retain the above copyright
	   notice, this list of conditions and the following disclaimer.
	2. Redistributions in binary form must reproduce the above copyright
	   notice, this list of conditions and the following disclaimer in the
	   documentation and/or other materials provided with the distribution.
	3. All advertising materials mentioning features or use of this software
	   must display the following acknowledgement:
	   This product includes software developed by Thomas Spielauer
	4. Neither the name of the Thomas Spielauer may be used to endorse or
	   promote products derived from this software without specific prior written
	   permission.

	THIS SOFTWARE IS PROVIDED BY Thomas Spielauer ''AS IS'' AND ANY
	EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
	WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
	DISCLAIMED. IN NO EVENT SHALL Thomas Spielauer BE LIABLE FOR ANY
	DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
	(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
	LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
	ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
	(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
	SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <stdio.h>
#include <stdlib.h>

static unsigned long int crc32Table[256];

int main(int argc, char* argv[]) {
	/* CRC32 Table Generator ... */

	unsigned long int c;
	unsigned int n, k;
	unsigned long int i,j;

	/* Calculate CRC32 table */

	for(n = 0; n < 256; n=n+1) {
		c = n;
		for(k = 0; k < 8; k=k+1) {
			if((c & 1) != 0) {
				c = 0xedb88320L ^ (c >> 1);
			} else {
				c = c >> 1;
			}
		}
		crc32Table[n] = c;
	}

	/* Generate sourcecode ... */
	
	printf("unsigned long int rfc1951_crc32Table[256] = {\n");
	for(i = 0; i < 32; i=i+1) {
		printf("\t");
		for(j = 0; j < 8; j=j+1) {
			printf("0x%lx", crc32Table[j + i*8]);
			if(!((j == 7) && (i == 31))) { printf(","); }
		}
		printf("\n");
	}
	printf("};\n");
}