#include "FileStream.hpp"
#include <cstdio>

#if defined(USE_MS_WIN)
#	define _INIT ,handle(NULL), _eof(false)
#endif

namespace UTIL {

	using namespace std;
	using namespace OS;

	static void testFileOpen(FILE * fp) {
		if (fp == NULL) {
			throw IOException("Invalid file");
		}
	}

#if defined(USE_MS_WIN)
	static void testHandleOpen(HANDLE handle) {
		if (handle == NULL) {
			throw IOException("Invalid handle");
		}
	}
#endif
	
	FileStream::FileStream() : fp(NULL) _INIT {
		/**/
	}

	FileStream::FileStream(FILE * fp) : fp(fp) _INIT {
		if (!fp) {
			throw IOException("invalid file poitner");
		}
	}

	FileStream::FileStream(File file, const string & flags) : fp(NULL) _INIT {
		open(file.getPath(), flags);
	}

	FileStream::FileStream(const string & path, const string & flags) : fp(NULL) _INIT {
		open(path, flags);
	}

	FileStream::~FileStream() {
		/**/
	}

	void FileStream::testOpen() {
		if (win32HandleMode()) {
#if defined(USE_MS_WIN)
			testHandleOpen(handle);
#else
			throw Exception("Wrong operation");
#endif
		} else {
			testFileOpen(fp);
		}
	}

	void FileStream::open(const string & path, const string & flags) {

#if defined(USE_UNIX_STD)
		
		fp = fopen(path.c_str(), flags.c_str());
		if (!fp) {
			throw IOException("fopen() error - '" + path + "'");
		}

#elif defined(USE_MS_WIN)

		if (fopen_s(&fp, path.c_str(), flags.c_str()) != 0) {
			throw IOException("fopen_s() error - '" + path + "'");
		}

#endif
		
	}

	bool FileStream::eof() {

		if (win32HandleMode()) {
			return eofWin32();
		}

		testOpen();
		return feof(fp) ? true : false;
	}

	int FileStream::read() {

		if (win32HandleMode()) {
			return readWin32();
		}

		testOpen();
		int ch = fgetc(fp);
		if (ch == EOF) {
			ch = -1;
		}
		return ch;
	}

	size_t FileStream::read(char * buffer, size_t len) {

		if (win32HandleMode()) {
			return readWin32(buffer, len);
		}

		testOpen();
		return fread(buffer, 1, len, fp);
	}

	string FileStream::readline() {
		testOpen();
		string ret;
		int ch;
		while (ch = read(), (ch >= 0 && ch != '\n')) {
			ret.append(1, (char)ch);
		}
		return ret;
	}

	string FileStream::readFullAsString() {
		return readFullAsString(1024);
	}

	string FileStream::readFullAsString(size_t bufferSize) {
		string ret;
		char * buffer = new char[bufferSize];
		size_t len;
		while ((len = read(buffer, bufferSize)) > 0) {
			ret.append(buffer, len);
		}
		delete[] buffer;
		return ret;
	}

	void FileStream::write(int ch) {

		if (win32HandleMode()) {
			writeWin32(ch);
		}

		testOpen();
		fputc(ch, fp);
	}

	size_t FileStream::write(const char * buffer, size_t len) {

		if (win32HandleMode()) {
			return writeWin32(buffer, len);
		}

		testOpen();
		return fwrite(buffer, 1, len, fp);
	}

	void FileStream::write(const string & data) {
		write(data.c_str(), data.size());
	}

	void FileStream::writeline(const string & line) {
		testOpen();
		string nlstr = line + "\n";
		write(nlstr.c_str(), nlstr.length());
	}

	void FileStream::rewind() {
		testOpen();
		::rewind(fp);
	}

	void FileStream::seek(size_t pos) {

		if (win32HandleMode()) {
			seekWin32(pos);
		}

		testOpen();
		fseek(fp, pos, SEEK_SET);
	}

	void FileStream::seekEnd(size_t pos) {

		if (win32HandleMode()) {
			seekEndWin32(pos);
		}

		testOpen();
		fseek(fp, pos, SEEK_END);
	}

	void FileStream::seekOffset(long offset) {

		if (win32HandleMode()) {
			seekOffsetWin32(offset);
		}

		testOpen();
		fseek(fp, offset, SEEK_CUR);
	}

	size_t FileStream::position() {

		if (win32HandleMode()) {
			positionWin32();
		}

		testOpen();
		long pos = ftell(fp);
		if (pos < 0) {
			throw IOException("ftell() error", -1, 0);
		}
		return (size_t)pos;
	}

	void FileStream::close() {
		if (fp) {
			fclose(fp);
			fp = NULL;
		}
	}

	/**
	 *
	 */
	bool FileStream::win32HandleMode() {
#if defined(USE_MS_WIN)
		return (fp == NULL && handle != NULL);
#else
		return false;
#endif
	}

	/**
	 *
	 */

#if defined(USE_MS_WIN)
	FileStream::FileStream(HANDLE handle) : fp(NULL), handle(handle), _eof(false) {
		if (!handle) {
			throw IOException("invalid handle");
		}
	}

	bool FileStream::eofWin32() {
		return _eof;
	}

	int FileStream::readWin32() {
		char ch;
		int ret = readWin32(&ch, 1);
		if (ret > 0) {
			return ch;
		}
		return -1;
	}

	size_t FileStream::readWin32(char * buffer, size_t size) {
		testOpen();
		DWORD dwRead = 0;
		BOOL bSuccess = ReadFile(handle, buffer, size, &dwRead, NULL);
		_eof = (!bSuccess || dwRead == 0);
		return (_eof ? 0 : dwRead);
	}

	void FileStream::writeWin32(int ch) {
		testOpen();
		writeWin32((const char*)&ch, 1);
	}

	size_t FileStream::writeWin32(const char * buffer, size_t len) {
		testOpen();
		DWORD dwWritten = 0;
		BOOL bSuccess = WriteFile(handle, buffer, len, &dwWritten, NULL);
		return (size_t)dwWritten;
	}

	void FileStream::rewindWin32() {
		seekWin32(0);
	}

	void FileStream::seekWin32(size_t pos) {
		testOpen();
		SetFilePointer(handle, (long)pos, NULL, FILE_BEGIN);
	}

	void FileStream::seekEndWin32(size_t pos) {
		testOpen();
		SetFilePointer(handle, (long)pos, NULL, FILE_END);
	}

	void FileStream::seekOffsetWin32(long offset) {
		testOpen();
		SetFilePointer(handle, offset, NULL, FILE_CURRENT);
	}

	size_t FileStream::positionWin32() {
		testOpen();
		return (size_t)SetFilePointer(handle, 0, NULL, FILE_CURRENT);
	}

	void FileStream::closeWin32() {
		if (handle) {
			CloseHandle(handle);
			handle = NULL;
		}
	}
#endif
	
}
