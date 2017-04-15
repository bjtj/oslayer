#include "FileStream.hpp"
#include <cstdio>

#if !defined(USE_MS_WIN)
	typedef void * HANDLE;
#endif

namespace UTIL {

	using namespace std;
	using namespace OS;
	
	FileStream::FileStream() : _fp(NULL) {
		_init();
	}

	FileStream::FileStream(FILE * fp) : _fp(fp) {
		_init();
		if (_fp == NULL) {
			throw IOException("invalid file poitner");
		}
	}

	FileStream::FileStream(File file, const string & flags) : _fp(NULL) {
		_init();
		_fp = s_open(file.getPath(), flags);
	}

	FileStream::FileStream(const string & path, const string & flags) : _fp(NULL) {
		_init();
		_fp = s_open(path, flags);
	}

	FileStream::~FileStream() {
		/* do nothing */
	}

	void FileStream::_init() {
#if defined(USE_MS_WIN)
		_handle = NULL;
		_eof = false;
#endif
	}

	void FileStream::testFileOpen() {
		if (_fp == NULL) {
			throw IOException("Invalid file");
		}
	}

	void FileStream::testOpen() {
		if (isWin32Mode()) {
			testHandleOpen();
		} else {
			testFileOpen();
		}
	}

	void FileStream::open(const std::string & path, const std::string & flags) {
		_fp = s_open(path, flags);
	}

	FILE * FileStream::s_open(const string & path, const string & flags) {
#if defined(USE_MS_WIN)
		FILE * fp = NULL;
		if (fopen_s(&fp, path.c_str(), flags.c_str()) != 0) {
			throw IOException("fopen_s() error - '" + path + "'");
		}
		return fp;
#else
		FILE * fp = fopen(path.c_str(), flags.c_str());
		if (fp == NULL) {
			throw IOException("fopen() error - '" + path + "'");
		}
		return fp;
#endif
	}

	bool FileStream::eof() {

		if (isWin32Mode()) {
			return eofWin32();
		}

		testOpen();
		return feof(_fp) ? true : false;
	}

	int FileStream::read() {

		if (isWin32Mode()) {
			return readWin32();
		}

		testOpen();
		int ch = fgetc(_fp);
		if (ch == EOF) {
			ch = -1;
		}
		return ch;
	}

	size_t FileStream::read(char * buffer, size_t len) {

		if (isWin32Mode()) {
			return readWin32(buffer, len);
		}

		testOpen();
		return fread(buffer, 1, len, _fp);
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
		if (isWin32Mode()) {
			writeWin32(ch);
		}
		testOpen();
		fputc(ch, _fp);
	}

	size_t FileStream::write(const char * buffer, size_t len) {
		if (isWin32Mode()) {
			return writeWin32(buffer, len);
		}
		testOpen();
		return fwrite(buffer, 1, len, _fp);
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
		::rewind(_fp);
	}

	void FileStream::seek(size_t pos) {
		if (isWin32Mode()) {
			seekWin32(pos);
		}
		testOpen();
		fseek(_fp, pos, SEEK_SET);
	}

	void FileStream::seekEnd(size_t pos) {

		if (isWin32Mode()) {
			seekEndWin32(pos);
		}

		testOpen();
		fseek(_fp, pos, SEEK_END);
	}

	void FileStream::seekOffset(long offset) {

		if (isWin32Mode()) {
			seekOffsetWin32(offset);
		}

		testOpen();
		fseek(_fp, offset, SEEK_CUR);
	}

	size_t FileStream::position() {

		if (isWin32Mode()) {
			positionWin32();
		}

		testOpen();
		long pos = ftell(_fp);
		if (pos < 0) {
			throw IOException("ftell() error", -1, 0);
		}
		return (size_t)pos;
	}

	void FileStream::close() {
		if (_fp) {
			fclose(_fp);
			_fp = NULL;
		}
	}

	/**
	 *
	 */
	bool FileStream::isWin32Mode() {
#if defined(USE_MS_WIN)
		return (_fp == NULL && _handle != NULL);
#else
		return false;
#endif
	}

	void FileStream::testHandleOpen() {
#if defined(USE_MS_WIN)
		if (_handle == NULL) {
			throw IOException("Invalid handle");
		}
#else
		throw Exception("Wrong operation");
#endif
	}

#if defined(USE_MS_WIN)
	FileStream::FileStream(HANDLE handle) : _fp(NULL), _handle(handle), _eof(false) {
		if (!_handle) {
			throw IOException("invalid handle");
		}
	}
#endif

	bool FileStream::eofWin32() {
#if defined(USE_MS_WIN)
		return _eof;
#else
		throw Exception("Not Supported");
#endif
	}

	int FileStream::readWin32() {
#if defined(USE_MS_WIN)
		char ch;
		int ret = readWin32(&ch, 1);
		if (ret > 0) {
			return ch;
		}
		return -1;
#else
		throw Exception("Not Supported");
#endif
	}

	size_t FileStream::readWin32(char * buffer, size_t size) {
#if defined(USE_MS_WIN)
		testOpen();
		DWORD dwRead = 0;
		BOOL bSuccess = ReadFile(_handle, buffer, size, &dwRead, NULL);
		_eof = (!bSuccess || dwRead == 0);
		return (_eof ? 0 : dwRead);
#else
		throw Exception("Not Supported");
#endif
	}

	void FileStream::writeWin32(int ch) {
#if defined(USE_MS_WIN)
		testOpen();
		writeWin32((const char*)&ch, 1);
#else
		throw Exception("Not Supported");
#endif
	}

	size_t FileStream::writeWin32(const char * buffer, size_t len) {
#if defined(USE_MS_WIN)
		testOpen();
		DWORD dwWritten = 0;
		BOOL bSuccess = WriteFile(_handle, buffer, len, &dwWritten, NULL);
		return (size_t)dwWritten;
#else
		throw Exception("Not Supported");
#endif
	}

	void FileStream::rewindWin32() {
#if defined(USE_MS_WIN)
		seekWin32(0);
#else
		throw Exception("Not Supported");
#endif
	}

	void FileStream::seekWin32(size_t pos) {
#if defined(USE_MS_WIN)
		testOpen();
		SetFilePointer(_handle, (long)pos, NULL, FILE_BEGIN);
#else
		throw Exception("Not Supported");
#endif
	}

	void FileStream::seekEndWin32(size_t pos) {
#if defined(USE_MS_WIN)
		testOpen();
		SetFilePointer(_handle, (long)pos, NULL, FILE_END);
#else
		throw Exception("Not Supported");
#endif
	}

	void FileStream::seekOffsetWin32(long offset) {
#if defined(USE_MS_WIN)
		testOpen();
		SetFilePointer(_handle, offset, NULL, FILE_CURRENT);
#else
		throw Exception("Not Supported");
#endif
	}

	size_t FileStream::positionWin32() {
#if defined(USE_MS_WIN)
		testOpen();
		return (size_t)SetFilePointer(_handle, 0, NULL, FILE_CURRENT);
#else
		throw Exception("Not Supported");
#endif
	}

	void FileStream::closeWin32() {
#if defined(USE_MS_WIN)
		if (_handle) {
			CloseHandle(_handle);
			_handle = NULL;
		}
#else
		throw Exception("Not Supported");
#endif
	}
}
