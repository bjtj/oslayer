#include "Environment.hpp"


using namespace std;

#if !defined(USE_MS_WIN)
extern char ** environ;
#endif



namespace osl {

    Optional<string> Environment::get(const string & key) {
#if defined(USE_MS_WIN)
	/* https://docs.microsoft.com/en-us/windows/desktop/api/winbase/nf-winbase-getenvironmentvariable */
	size_t buffer_size = 32767;
	char * buffer = (char*)malloc(buffer_size);
	memset(buffer, 0, buffer_size);
	if (GetEnvironmentVariable(key.c_str(), buffer, buffer_size) == 0) {
	    free(buffer);
	    return Optional<string>();
	}
	string ret(buffer);
	free(buffer);
	return Optional<string>(ret);
#else
	char * env = getenv(key.c_str());
	if (env == NULL) {
	    return Optional<string>();
	}
	return Optional<string>(string(env));
#endif	
    }

    void Environment::set(const string & key, const string & value) {
#if defined(USE_MS_WIN)
	/* https://docs.microsoft.com/en-gb/windows/desktop/api/winbase/nf-winbase-setenvironmentvariable */
	SetEnvironmentVariable(key.c_str(), value.c_str());
#else
	setenv(key.c_str(), value.c_str(), 1);
#endif
    }

    void Environment::unset(const string & key) {
#if defined(USE_MS_WIN)
	/* https://docs.microsoft.com/en-gb/windows/desktop/api/winbase/nf-winbase-setenvironmentvariable */
	SetEnvironmentVariable(key.c_str(), NULL);
#else
	unsetenv(key.c_str());
#endif	
    }

    map<string, string> Environment::all() {

	map<string, string> env;
	
#if defined(USE_MS_WIN)
	LPTSTR lpszVariable;
	LPTCH lpvEnv;
	lpvEnv = GetEnvironmentStrings();
	if (lpvEnv == NULL)
	{
	    fprintf(stderr, "GetEnvironmentStrings() failed\n");
	    return NULL;
	}
	for (lpszVariable = (LPTSTR)lpvEnv; *lpszVariable; lpszVariable++)
	{
	    string line(*lpszVariable);
	    size_t f = line.find("=");
	    if (f == string::npos) {
		env[line] = "";
	    } else {
		env[line.substr(0, f)] = line.substr(f+1);
	    }
	}
    
	FreeEnvironmentStrings(lpvEnv);
	return env;
#else
	char ** ptr = environ;
	for (; *ptr; ptr++)
	{
	    string line(*ptr);
	    size_t f = line.find("=");
	    if (f == string::npos) {
		env[line] = "";
	    } else {
		env[line.substr(0, f)] = line.substr(f+1);
	    }
	}
	return env;
#endif	
    }
}
