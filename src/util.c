#define __is_inside_libminor_E7B72649_9D2F_4078_8EA0_5D7B61775925 1

#include "../include/minor.h"

#ifdef __cplusplus
	extern "C" {
#endif

enum minorError minorConfiguration_CheckUnprocessedCritical(
	struct minorConfigurationElement* lpRootConfiguration
) {
	struct minorConfigurationElement* lpCur = lpRootConfiguration;

	while(lpCur != NULL) {
		if((lpCur->dwFlags & (minorConfiguration__Flag__Critical|minorConfiguration__Flag__Processed)) == minorConfiguration__Flag__Critical) {
			return minorE_UnsupportedConfiguration;
		}
		lpCur = lpCur->lpNext;
	}

	return minorE_Ok;
}

enum minorError minorConfiguration_ResetProcessedOptions(
	struct minorConfigurationElement* lpRootConfiguration
) {
	struct minorConfigurationElement* lpCur = lpRootConfiguration;

	while(lpCur != NULL) {
		lpCur->dwFlags = lpCur->dwFlags & (~(minorConfiguration__Flag__Processed));
		lpCur = lpCur->lpNext;
	}
	return minorE_Ok;
}



#ifdef __cplusplus
	} /* extern "C" { */
#endif
