#ifndef __FILE_READER_WRITER_HPP__
#define __FILE_READER_WRITER_HPP__

#include "os.hpp"
#include <string>

namespace UTIL {

	/**
	 * @brief FileReader
	 */
    class FileReader {
	private:
		FileReader * impl;
		OS::File file;

	protected:
		FileReader(FileReader * impl, OS::File & file);

	public:
		FileReader(OS::File & file);
		virtual ~FileReader();

		virtual void open(OS::File & file);
		virtual size_t read(char * buffer, size_t len);
		virtual void seek(size_t pos);
		virtual void close();

		std::string dumpAsString();
		std::string dumpAsStringWithLimit(size_t limit);

		OS::File & getFile();
	};

	/**
	 * @brief FileWriter
	 */
	class FileWriter {
	private:
		FileWriter * impl;
		OS::File file;

	protected:
		FileWriter(FileWriter * impl, OS::File & file);

	public:
		FileWriter(OS::File & file);
		virtual ~FileWriter();

		virtual void open(OS::File & file);
		virtual size_t write(const char * data, size_t len);
		virtual void seek(size_t pos);
		virtual void close();

		OS::File & getFile();
	};

	/**
	 * @brief RandomAccessFile
	 */
	class RandomAccessFile {
	private:
		RandomAccessFile * impl;
		OS::File file;
	protected:
		RandomAccessFile(RandomAccessFile * impl, OS::File & file);
	public:
		RandomAccessFile(OS::File & file, const std::string & mode);
		virtual ~RandomAccessFile();

		virtual void open(OS::File & file, const std::string & mode);
		virtual size_t read(char * buffer, size_t len);
		virtual size_t write(const char * data, size_t len);
		virtual void seek(size_t pos);
		virtual void close();

		OS::File & getFile();
	};

}

#endif
