#ifndef __PLATFORM_HPP__
#define __PLATFORM_HPP__

/*
* Predefined OS macros
* ====================
* http://stackoverflow.com/questions/142508/how-do-i-check-os-with-a-preprocessor-directive
*/

#if defined(__APPLE__) || defined(__MACH__) /* mac os x */

#	define USE_UNIX_STD
#   define USE_APPLE_STD
#	define USE_APPLE_SEMAPHORE
#	define USE_PTHREAD
#	define USE_BSD_SOCKET
#	define USE_SIGNAL

#	include <unistd.h>
#	include <sys/time.h>
#	include <sys/stat.h>
#	include <sys/types.h>
#	include <ctime>
#	include <dirent.h>
#	include <fcntl.h>
#	include <signal.h>
#   include <mach/clock.h>
#   include <mach/mach.h>

#	define TIME time_t

#elif defined(unix) || defined(__unix__) || defined(__unix) /* unix or linux */

#	define USE_UNIX_STD
#   define USE_POSIX_STD
#	define USE_POSIX_SEMAPHORE
#	define USE_PTHREAD
#	define USE_BSD_SOCKET
#	define USE_SIGNAL

#	if !defined(__CYGWIN__)
#		define USE_PRCTL
#	endif

#	include <unistd.h>
#	include <sys/time.h>
#	include <sys/stat.h>
#	include <sys/types.h>
#	include <ctime>
#	include <dirent.h>
#	include <errno.h>
#	include <fcntl.h>
#	include <signal.h>

#	define TIME time_t

#elif defined(_WIN32) || defined(_WIN64) /* windows */

// #	define _WIN32_WINNT 0x501 /* force to use windows xp APIs */

#	define USE_MS_WIN
#	define USE_WIN_SEMAPHORE
#	define USE_WIN_THREAD
#	define USE_WINSOCK2

#if _MSC_VER < 1900
#	define snprintf _snprintf_s
#endif

#if _MSC_VER < 1400
#	define strncpy strncpy_s
#endif

#	define strcasecmp(x,y) _stricmp((x),(y))
#	define strncasecmp(x,y,z) _strnicmp((x),(y),(z))

#	define TIME SYSTEMTIME

#	include <direct.h>
#	include <sys/stat.h>
#	include <io.h>

#	define __func__ __FUNCTION__

#endif 

#endif /* Platform Detection */
