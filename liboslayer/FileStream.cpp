#include "FileStream.hpp"
#include <cstdio>

namespace UTIL {

	using namespace std;
	using namespace OS;

	static void testFileOpen(FILE * fp) {
		if (fp == NULL) {
			throw IOException("file not opened", -1, 0);
		}
	}
	
	FileStream::FileStream() : fp(NULL) {
	}
	FileStream::FileStream(FILE * fp) : fp(fp) {
	}
	FileStream::FileStream(File file, const string & flags) {
		open(file.getPath(), flags);
	}
	FileStream::FileStream(const string & path, const string & flags) {
		open(path, flags);
	}
	FileStream::~FileStream() {
	}

	void FileStream::open(const string & path, const string & flags) {

#if defined(USE_UNIX_STD)
		
		fp = fopen(path.c_str(), flags.c_str());
		if (!fp) {
			throw IOException("fopen() error", -1, 0);
		}


#elif defined(USE_MS_WIN)

		if (fopen_s(&fp, path.c_str(), flags.c_str()) != 0) {
			throw IOException("fopen_s() error", -1, 0);
		}

#endif
		
	}

	bool FileStream::eof() {
		testFileOpen(fp);
		return feof(fp) ? true : false;
	}
	int FileStream::read() {
		testFileOpen(fp);
		int ch = fgetc(fp);
		if (ch == EOF) {
			ch = -1;
		}
		return ch;
	}
	size_t FileStream::read(char * buffer, size_t len) {
		testFileOpen(fp);
		return fread(buffer, 1, len, fp);
	}
	string FileStream::readline() {
		testFileOpen(fp);
		string ret;
		int ch;
		while (ch = read(), (ch >= 0 && ch != '\n')) {
			ret.append(1, (char)ch);
		}
		return ret;
	}
	void FileStream::write(int ch) {
		testFileOpen(fp);
		fputc(ch, fp);
	}
	size_t FileStream::write(const char * buffer, size_t len) {
		testFileOpen(fp);
		return fwrite(buffer, 1, len, fp);
	}
	void FileStream::writeline(const std::string & line) {
		testFileOpen(fp);
		string nlstr = line + "\n";
		write(nlstr.c_str(), nlstr.length());
	}
	void FileStream::rewind() {
		testFileOpen(fp);
		::rewind(fp);
	}
	void FileStream::seek(size_t pos) {
		testFileOpen(fp);
		fseek(fp, pos, SEEK_SET);
	}
	void FileStream::seekEnd(size_t pos) {
		testFileOpen(fp);
		fseek(fp, pos, SEEK_END);
	}
	void FileStream::seekOffset(long offset) {
		testFileOpen(fp);
		fseek(fp, offset, SEEK_CUR);
	}
	size_t FileStream::position() {
		testFileOpen(fp);
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
	
}
