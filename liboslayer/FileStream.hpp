#ifndef __FILE_STREAM_HPP__
#define __FILE_STREAM_HPP__

#include "os.hpp"
#include <string>

namespace UTIL {

	/**
	 * @brief file stream utility
	 */
	class FileStream {
	private:
		FILE * fp;		
	public:
		FileStream();
		FileStream(FILE * fp);
		FileStream(OS::File file, const std::string & flags);
		FileStream(const std::string & path, const std::string & flags);
		virtual ~FileStream();
		void testOpen();
		void open(const std::string & path, const std::string & flags);
		bool eof();
		int read();
		size_t read(char * buffer, size_t len);
		std::string readline();
		std::string readFullAsString();
		std::string readFullAsString(size_t bufferSize);
		void write(int ch);
		size_t write(const char * buffer, size_t len);
		void write(const std::string & data);
		void writeline(const std::string & line);
		void rewind();
		void seek(size_t pos);
		void seekEnd(size_t pos);
		void seekOffset(long offset);
		size_t position();
		void close();

	private:
		bool win32HandleMode();

#if defined(USE_MS_WIN)
	private:
		HANDLE handle;
		bool _eof;
	public:
		FileStream(HANDLE handle);
	private:
		bool eofWin32();
		int readWin32();
		size_t readWin32(char * buffer, size_t size);
		void writeWin32(int ch);
		size_t writeWin32(const char * buffer, size_t len);
		void rewindWin32();
		void seekWin32(size_t pos);
		void seekEndWin32(size_t pos);
		void seekOffsetWin32(long offset);
		size_t positionWin32();
		void closeWin32();
#endif
	};

}

#endif
