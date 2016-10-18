#include "File.hpp"

namespace OS {

	using namespace std;
	
#if defined(USE_UNIX_STD)

	static Date s_time_to_date(time_t t) {
		Date date;
        struct tm info;
		localtime_r(&t, &info); // GMT - gmtime_r(&t, &info);
        date.setYear(1900 + info.tm_year);
        date.setMonth(info.tm_mon);
        date.setDay(info.tm_mday);
		date.setDayOfWeek(info.tm_wday);
        date.setHour(info.tm_hour);
        date.setMinute(info.tm_min);
        date.setSecond(info.tm_sec);
		return date;
	}
    
    static bool s_is_separator(char c, const string & separators);

    static const string s_get_separators() {
        return "/";
    }
    static bool s_is_separator(char c) {
        return s_is_separator(c, s_get_separators());
    }
    static bool s_is_separator(char c, const string & separators) {
        for (string::const_iterator iter = separators.begin(); iter != separators.end(); iter++) {
            char sep = *iter;
            if (sep == c) {
                return true;
            }
        }
        return false;
    }
    static string s_append_separator_if_not(const string & path, const string & separators) {
        
        if (path.empty() || separators.empty()) {
            return path;
        }
        
        char end = *(path.rbegin());
        for (string::const_iterator iter = separators.begin(); iter != separators.end(); iter++) {
            char sep = *iter;
            if (sep == end) {
                return path;
            }
        }
        
        return path + *(separators.begin());
    }
    static string s_remove_last_separator(const string & path) {
        if (!path.empty() && path.length() > 1 && s_is_separator(*(path.rbegin())) ) {
            return path.substr(0, path.length() - 1); // trailing last / character
        }
        return path;
    }
	static bool s_is_fullpath(const string & path) {
		return !path.empty() && s_is_separator(path[0]);
	}
    static string s_get_cwd() {
        char buffer[2048] = {0,};
        if (getcwd(buffer, sizeof(buffer)) == NULL) {
            throw IOException("getcwd() error", -1, 0);
        }
        return string(buffer);
    }
	static bool s_is_root_path(const string & path) {
		return !path.compare("/");
	}
	static bool s_exists(const string & path) {

		if (path.empty()) {
			return false;
		}
		
		// http://stackoverflow.com/questions/12774207/fastest-way-to-check-if-a-file-exist-using-standard-c-c11-c
		struct stat st;
		memset(&st, 0, sizeof(struct stat));
		return (stat(path.c_str(), &st) == 0);
	}
	static bool s_is_file(const string & path) {

		if (path.empty()) {
			return false;
		}
		
		// http://stackoverflow.com/questions/3536781/accessing-directories-in-c/3536853#3536853
		struct stat st;
		memset(&st, 0, sizeof(struct stat));
		lstat(path.c_str(), &st);
		return (S_ISDIR(st.st_mode) ? false : true);
	}
	static bool s_is_directory(const string & path) {

		if (path.empty()) {
			return false;
		}
		
		struct stat st;
		memset(&st, 0, sizeof(struct stat));
		lstat(path.c_str(), &st);
		return (S_ISDIR(st.st_mode) ? true : false);
	}
	static bool s_is_writable(const string & path) {
		return (access(path.c_str(), W_OK) == 0);
	}

	static string s_get_path_part(const string & path) {
		
		if (path.empty() || s_is_directory(path) || s_is_root_path(path)) {
			return s_remove_last_separator(path);
		}

		size_t f = path.find_last_of(s_get_separators());
		if (f == string::npos) {
			return "";
		}

		return path.substr(0, f + 1);
	}
	static string s_get_filename_part(const string & path) {

		if (path.empty()) {
			return "";
		}
		
		size_t f = path.find_last_of(s_get_separators());
		if (f == string::npos) {
			return path;
		}

		return path.substr(f + 1);
	}
	static string s_get_ext(const string & path) {
		string name = s_get_filename_part(path);
		size_t f = name.find_last_of(".");
		if (f == string::npos || f == 0) {
			return "";
		}
		return name.substr(f+1);
	}

	// http://stackoverflow.com/questions/2336242/recursive-mkdir-system-call-on-unix
	static int s_mkdir(const char *dir, mode_t mode) {
	
		char tmp[256];
		char *p = NULL;
		size_t len;

		snprintf(tmp, sizeof(tmp),"%s",dir);
		len = strlen(tmp);

		if(tmp[len - 1] == '/') {
			tmp[len - 1] = 0;
		}

		for(p = tmp + 1; *p; p++) {
			if(*p == '/') {
				*p = 0;
				mkdir(tmp, mode); // ignore error (just try)
				*p = '/';
			}
		}
	
		return mkdir(tmp, mode);
	}

	static bool s_remove_file(const char * path) {
		return (remove(path) == 0 ? true : false);
	}

	static Date s_get_creation_date(const string & path) {
		struct stat st;
		memset(&st, 0, sizeof(struct stat));
		if (stat(path.c_str(), &st) != 0) {
			throw Exception("stat() failed");
		}
		return s_time_to_date(st.st_ctime);
	}

	static Date s_get_modified_date(const string & path) {
		struct stat st;
		memset(&st, 0, sizeof(struct stat));
		if (stat(path.c_str(), &st) != 0) {
			throw Exception("stat() failed");
		}
		return s_time_to_date(st.st_mtime);
	}
    
    static filesize_t s_get_file_size(const string & path) {
        
        struct stat st;
		memset(&st, 0, sizeof(struct stat));
        lstat(path.c_str(), &st);
        
        return st.st_size;
    }

	static vector<File> s_list(const string & path) {
		vector<File> ret;
		struct dirent * ent = NULL;
		struct dirent ** list = NULL;
		int cnt;
		cnt = scandir(path.c_str(), &list, NULL, alphasort);
		if (cnt < 0) {
			return ret;
		}
		for (int i = 0; i < cnt; i++) {
			ent = list[i];
			ret.push_back(File::fullpath(path, ent->d_name));
			free(ent);
		}
		free(list);
		return ret;
	}

	static string s_get_absolute_path(const string & path) {
		bool abs = (path[0] == '/');
		vector<string> toks;
		string p = path;

		if (!abs) {
			p = s_get_cwd() + "/" + p;
		}
	
		string buf;
		for (string::iterator iter = p.begin(); iter != p.end(); iter++) {
			char ch = *iter;

			if (ch != '/') {
				buf.append(1, ch);
			}
		
			if (ch == '/' || (iter + 1 == p.end())) {
				if (!buf.empty()) {
					if (buf == ".") {
						// no op
					} else if (buf == "..") {
						toks.pop_back();
					} else {
						toks.push_back(buf);
					}
				}
				buf = "";
			}
		}

		string ret;
		for (vector<string>::iterator iter = toks.begin(); iter != toks.end(); iter++) {
			ret.append("/" + *iter);
		}

		return ret;
	}
	
#elif defined(USE_MS_WIN)

#define STAT_STRUCT struct _stat64
#define STAT_FUNC __stat64

	static const string s_get_separators();
	static bool s_is_separator(char c);
	static bool s_is_separator(char c, const string & separators);
	static string s_remove_if_last(const string & path, char m);
	static bool s_is_fullpath(const string & path);
	static bool s_is_root_path(const string & path);
	static bool s_exists(const string & path);
	static bool s_is_file(const string & path);
	static bool s_is_directory(const string & path);
	static string s_get_path_part(const string & path);
	static string s_get_filename_part(const string & path);
	static string s_get_ext(const string & path);
	static int s_mkdir(const char *dir, int mode);
	static Date s_get_creation_date(const string & path);
	static Date s_get_modified_date(const string & path);

	static const string s_get_separators() {
		return "\\/";
	}
	static bool s_is_separator(char c) {
		return s_is_separator(c, s_get_separators());
	}
	static bool s_is_separator(char c, const string & separators) {
		for (string::const_iterator iter = separators.begin(); iter != separators.end(); iter++) {
			char sep = *iter;
			if (sep == c) {
				return true;
			}
		}
		return false;
	}
	static char s_get_default_seprator() {
		return *(s_get_separators().begin());
	}
	static string s_get_default_seprator_in_string() {
		return string(1, s_get_default_seprator());
	}
	static string s_append_separator_if_not(const string & path) {
		if (!s_is_separator(*(path.rbegin()))) {
			return path + s_get_default_seprator();
		}
		return path;
	}
	static string s_append_separator_if_not(const string & path, const string & separators) {

		if (path.empty() || separators.empty()) {
			return path;
		}

		char end = *(path.rbegin());
		for (string::const_iterator iter = separators.begin(); iter != separators.end(); iter++) {
			char sep = *iter;
			if (sep == end) {
				return path;
			}
		}

		return path + *(separators.begin());
	}
	static string s_remove_last_separator(const string & path) {
		if (!path.empty() && path.length() > 1 && s_is_separator(*(path.rbegin())) ) {
			return path.substr(0, path.length() - 1); // trailing last / character
		}
		return path;
	}
	static bool s_is_fullpath(const string & path) {
		return !path.empty() && s_is_separator(path[0]);
	}

	static string s_get_cwd() {
		char buffer[2048] = {0,};
		if (_getcwd(buffer, sizeof(buffer)) == NULL) {
			throw IOException("_getcwd() error", -1, 0);
		}
		return string(buffer);
	}

	static bool s_is_root_path(const string & path) {
		return (path.length() == 1 && s_is_separator(*path.begin()));
	}
	static bool s_exists(const string & path) {

		if (path.empty()) {
			return false;
		}

		if (s_is_directory(path) || s_is_file(path)) {
			return true;
		}
		
		return false;
	}
	static bool s_is_file(const string & path) {

		if (path.empty()) {
			return false;
		}

		STAT_STRUCT s;
		memset(&s, 0, sizeof(STAT_STRUCT));
		if (STAT_FUNC(path.c_str(), &s) != 0) {
			// error
			return false;
		}

		return (s.st_mode & S_IFREG ? true : false);
	}
	static bool s_is_directory(const string & path) {

		if (path.empty()) {
			return false;
		}
		
		STAT_STRUCT s;
		memset(&s, 0, sizeof(STAT_STRUCT));
		if (STAT_FUNC(path.c_str(), &s) != 0) {
			// error
			return false;
		}

		return (s.st_mode & S_IFDIR ? true : false);
	}
	static bool s_is_writable(const string & path) {
		if (!s_exists(path)) {
			return false;
		}
		return (_access(path.c_str(), 2) == 0);
	}

	static string s_get_path_part(const string & path) {
		
		if (path.empty() || s_is_directory(path) || s_is_root_path(path)) {
			return s_remove_last_separator(path);
		}

		int f = path.find_last_of(s_get_separators());
		if (f == string::npos) {
			return "";
		}

		return path.substr(0, f + 1);
	}
	static string s_get_filename_part(const string & path) {

		if (path.empty()) {
			return "";
		}
		
		int f = path.find_last_of(s_get_separators());
		if (f == string::npos) {
			return path;
		}

		return path.substr(f + 1);
	}
	static string s_get_ext(const string & path) {
		string name = s_get_filename_part(path);
		size_t f = name.find_last_of(".");
		if (f == string::npos || f == 0) {
			return "";
		}
		return name.substr(f+1);
	}

	// http://stackoverflow.com/questions/2336242/recursive-mkdir-system-call-on-unix
	static int s_mkdir(const char *dir, int mode) {
	
		// https://msdn.microsoft.com/en-us/library/2fkk4dzw.aspx

		char tmp[256];
		char *p = NULL;
		size_t len;

		snprintf(tmp, sizeof(tmp),"%s",dir);
		len = strlen(tmp);

		if(s_is_separator(tmp[len - 1])) {
			tmp[len - 1] = 0;
		}

		for(p = tmp + 1; *p; p++) {
			if(s_is_separator(*p)) {
				*p = 0;
				_mkdir(tmp);
				*p = s_get_default_seprator();
			}
		}
	
		return _mkdir(tmp);
	}

	static bool s_remove_file(const char * path) {
		return DeleteFile(path) == TRUE ? true : false;
	}
	
	static Date s_get_creation_date(const string & path) {
		HANDLE hFile;
		FILETIME ftCreate, ftAccess, ftWrite;
		long int ret = 0;

		memset(&ftCreate, 0, sizeof(ftCreate));
		memset(&ftAccess, 0, sizeof(ftAccess));
		memset(&ftWrite, 0, sizeof(ftWrite));

		hFile = CreateFile(path.c_str(), GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL);
		if (hFile == INVALID_HANDLE_VALUE) {
			throw Exception("CreateFile() failed - INVALID_HANDLE_VALUE");
		}

		if (!GetFileTime(hFile, &ftCreate, &ftAccess, &ftWrite)) {
			CloseHandle(hFile);
			throw Exception("GetFileTime() failed");
		}

		CloseHandle(hFile);

		return Date(ftCreate);
	}
	static Date s_get_modified_date(const string & path) {
		HANDLE hFile;
		FILETIME ftCreate, ftAccess, ftWrite;
		long int ret = 0;

		memset(&ftCreate, 0, sizeof(ftCreate));
		memset(&ftAccess, 0, sizeof(ftAccess));
		memset(&ftWrite, 0, sizeof(ftWrite));

		hFile = CreateFile(path.c_str(), GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL);
		if (hFile == INVALID_HANDLE_VALUE) {
			throw Exception("CreateFile() failed - INVALID_HANDLE_VALUE");
		}

		if (!GetFileTime(hFile, &ftCreate, &ftAccess, &ftWrite)) {
			CloseHandle(hFile);
			throw Exception("GetFileTime() failed");
		}

		CloseHandle(hFile);

		return Date(ftWrite);
	}

	static filesize_t s_get_file_size(const string & path) {

		STAT_STRUCT st;
		memset(&st, 0, sizeof(STAT_STRUCT));
		int ret = STAT_FUNC(path.c_str(), &st);
		if (ret != 0) {
			// error
			throw Exception("stat() failed", -1, 0);
		}

		return (filesize_t)st.st_size;
	}

	static vector<File> s_list(const string & path) {

		vector<File> ret;

		string dir = path;
		dir.append("\\*");

		WIN32_FIND_DATAA ffd;
		HANDLE hFind = INVALID_HANDLE_VALUE;
		DWORD dwError = 0;

		hFind = FindFirstFileA(dir.c_str(), &ffd);
		if (hFind == INVALID_HANDLE_VALUE) {
			throw IOException("FindFirstFileA() failed", -1, 0);
		}

		do {
			
			ret.push_back(File::fullpath(path, ffd.cFileName));

		} while (FindNextFileA(hFind, &ffd) != 0);

		FindClose(hFind);

		return ret;
	}

	static string s_get_absolute_path(const string & path) {

		static const unsigned int BUFFER_SIZE = 4096;

		DWORD  retval = 0;	
		char buffer[BUFFER_SIZE] = {0,}; 
		char ** lpppart = {NULL};

		// https://msdn.microsoft.com/en-us/library/windows/desktop/aa364963(v=vs.85).aspx
		retval = GetFullPathName(path.c_str(), BUFFER_SIZE, buffer, lpppart);

		if (retval == 0)  {
			throw Exception("GetFullPathName failed\n", GetLastError(), 0);
		}

		return string(buffer);
	}

#else

	/* other platform */
	
#endif

	/**
	 * @brief file constructor
	 */
	File::File() {
	}
	
	File::File(const string & path) : path(path) {
	}

	File::~File() {
	}

	string File::getSeparators() {
		return s_get_separators();
	}

	string File::mergePaths(const string & dir, const string & filename) {
		return fullpath(dir, filename);
	}

	string File::mergePaths(const string & dir, const string & filename, const string & separators) {
		return fullpath(dir, filename, separators);
	}

	string File::fullpath(const string & dir, const string & filename) {
		return fullpath(dir, filename, s_get_separators());
	}

	string File::fullpath(const string & dir, const string & filename, const string & separators) {
		
		string d = dir;
		string fname = filename;

		d = s_append_separator_if_not(d, separators);

		if (!fname.empty()) {
			size_t f = fname.find_first_not_of(separators);
			fname = (f != string::npos ? fname.substr(f) : "");
		}

		return (d + fname);
	}

	string File::getCwd() {
		return s_get_cwd();
	}

	bool File::isRootPath(const string & path){
		return s_is_root_path(path);
	}

	bool File::isFullpath(const string & path) {
		return s_is_fullpath(path);
	}
	
	bool File::exists(const string & path){
		return s_exists(path);
	}

	bool File::isFile(const string & path){
		return s_is_file(path);
	}

	bool File::isDirectory(const string & path){
		return s_is_directory(path);
	}

	bool File::isWritable(const string & path) {
		return s_is_writable(path);
	}

	string File::getAbsolutePath(const string & path) {
		return s_get_absolute_path(path);
	}

	string File::getDirectory(const string & path){
		return s_get_path_part(path);
	}

	string File::getName(const string & path) {
		return getFileName(path);
	}

	string File::getFileName(const string & path){
		return s_get_filename_part(path);
	}

	string File::getFileNameWithoutExtension(const string & path) {
		string fn = getFileName(path);
		size_t dot = fn.find_last_of(".");
		if (dot != string::npos && dot > 0) {
			return fn.substr(0, dot);
		}

		return fn;
	}

	string File::getExtension(const string & path){
		return s_get_ext(path);
	}

	bool File::compareExtension(const string & path, string extension){
		
		string a = s_get_ext(path);
		string b = s_get_ext(extension);

		if (a.empty() || b.empty()) {
			return false;
		}

		return (!a.compare(b));
	}

	int File::mkdir(const string & path) {
		return s_mkdir(path.c_str(), 0755);
	}

	bool File::remove(const string & path) {
		return s_remove_file(path.c_str());
	}

	Date File::creationDate(const string & path) {
		return s_get_creation_date(path);
	}
	Date File::lastModifiedDate(const string & path) {
		return s_get_modified_date(path);
	}
	osl_time_t File::creationTime(const string & path) {
		return creationDate(path).getTime();
	}
	osl_time_t File::lastModifiedTime(const string & path) {
		return lastModifiedDate(path).getTime();
	}
	filesize_t File::getSize(const string & path) {
		return s_get_file_size(path);
	}
	vector<File> File::list(const string & path) {
		return s_list(path);
	}
	string File::getPath() const {
		return path;
	}
	bool File::isRootPath() const {
		return File::isRootPath(path);
	}
	bool File::isFullpath() const {
		return File::isFullpath(path);
	}
	bool File::exists() const {
		return File::exists(path);
	}
	bool File::isFile() const {
		return File::isFile(path);
	}
	bool File::isDirectory() const {
		return File::isDirectory(path);
	}
	bool File::isWritable() const {
		return File::isWritable(path);
	}
	string File::getAbsolutePath() {
		return File::getAbsolutePath(path);
	}
	string File::getDirectory() const {
		return File::getDirectory(path);
	}
	string File::getName() const {
		return getFileName();
	}
	string File::getFileName() const {
		return File::getFileName(path);
	}
	string File::getFileNameWithoutExtension() const {
		return File::getFileNameWithoutExtension(path);
	}
	string File::getExtension() const {
		return File::getExtension(path);
	}
	bool File::compareExtension(string extension) const {
		return File::compareExtension(path, extension);
	}
	int File::mkdir() const {
		return File::mkdir(path);
	}
	bool File::remove() {
		return File::remove(path);
	}
	Date File::creationDate() {
		return creationDate(path);
	}
	Date File::lastModifiedDate() {
		return lastModifiedDate(path);
	}
	osl_time_t File::creationTime() {
		return creationDate(path).getTime();
	}
	osl_time_t File::lastModifiedTime() {
		return lastModifiedDate(path).getTime();
	}
	filesize_t File::getSize() const {
		return File::getSize(path);
	}
	vector<File> File::list() const {
		return File::list(path);
	}

	string File::toString() const {
		return path;
	}
	
}
