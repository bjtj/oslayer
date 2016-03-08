#include "FileReaderWriter.hpp"

#define CHECK_NOT_IMPL_THROW(x) if(!x){throw NotImplementedException();}

namespace UTIL {

	using namespace std;
	using namespace OS;

	/**
	 * FileReaderImpl
	 */

#if defined(USE_UNIX_STD)

	class FileReaderImpl : public FileReader {
	private:
		FILE * fd;
	public:
		FileReaderImpl(File & file) : FileReader(NULL, file), fd(NULL) {
			open(file);
		}
		virtual ~FileReaderImpl() {
			close();
		}
		virtual void open(File & file) {
			fd = fopen(file.getPath().c_str(), "rb");
			if (!fd) {
				throw IOException("fopen() error", -1, 0);
			}
		}
		virtual size_t read(char * buffer, size_t len) {
			return fread(buffer, 1, len, fd);
		}
		virtual void seek(size_t pos) {
			fseek(fd, pos, SEEK_SET);
		}
		virtual void close() {
			if (fd) {
				fclose(fd);
				fd = NULL;
			}
		}
	};

#elif defined(USE_MS_WIN)

	class FileReaderImpl : public FileReader {
	private:
		FILE * fd;
	public:
		FileReaderImpl(File & file) : FileReader(NULL, file), fd(NULL) {
			open(file);
		}
		virtual ~FileReaderImpl() {
			close();
		}
		virtual void open(File & file) {
			if (fopen_s(&fd, file.getPath().c_str(), "rb") != 0) {
				throw IOException("fopen() error", -1, 0);
			}
		}
		virtual size_t read(char * buffer, size_t len) {
			size_t ret = fread(buffer, 1, len, fd);
			return ret;
		}
		virtual void seek(size_t pos) {
			fseek(fd, pos, SEEK_SET);
		}
		virtual void close() {
			if (fd) {
				fclose(fd);
				fd = NULL;
			}
		}
	};

#endif

	/**
	 * @brief FileReader
	 */

	FileReader::FileReader(FileReader * impl, File & file) : impl(impl), file(file) {
	}

	FileReader::FileReader(File & file) : file(file) {
		impl = new FileReaderImpl(file);
	}

	FileReader::~FileReader() {
		if (impl) {
			delete impl;
		}
	}

	void FileReader::open(File & file) {
		CHECK_NOT_IMPL_THROW(impl);
		return impl->open(file);
	}

	size_t FileReader::read(char * buffer, size_t len) {
		CHECK_NOT_IMPL_THROW(impl);
		return impl->read(buffer, len);
	}

	void FileReader::seek(size_t pos) {
		CHECK_NOT_IMPL_THROW(impl);
		impl->seek(pos);
	}

	void FileReader::close() {
		CHECK_NOT_IMPL_THROW(impl);
		impl->close();
	}

	string FileReader::dumpAsString() {
		string ret;
		char buffer[1024] = {0,};
		size_t len;
		while ((len = read(buffer, sizeof(buffer))) > 0) {
			ret.append(buffer, len);
		}
		return ret;
	}

	string FileReader::dumpAsStringWithLimit(size_t limit) {
		string ret;
		char buffer[1024] = {0,};
		size_t total = 0;
		size_t len;
		while ((total < limit) && (len = read(buffer, sizeof(buffer))) > 0) {

			size_t writeLen = len;
			if (total + len > limit) {
				writeLen = limit - total;
			}

			ret.append(buffer, writeLen);
			total += writeLen;
		}
		return ret;
	}
	
	File & FileReader::getFile() {
		return file;
	}

	/**
	 * @brief FileWriterImpl
	 */
		
#if defined(USE_UNIX_STD)

	class FileWriterImpl : public FileWriter {
	private:
		FILE * fd;
	public:
		FileWriterImpl(File & file) : FileWriter(NULL, file), fd(NULL) {
			open(file);
		}
		virtual ~FileWriterImpl() {
			close();
		}
		virtual void open(OS::File & file) {
			fd = fopen(file.getPath().c_str(), "wb");
			if (!fd) {
				throw IOException("fopen() error", -1, 0);
			}
		}
		virtual size_t write(const char * data, size_t len) {
			return fwrite(data, 1, len, fd);
		}
		virtual void seek(size_t pos) {
			fseek(fd, pos, SEEK_SET);
		}
		virtual void close() {
			if (fd) {
				fclose(fd);
				fd = NULL;
			}
		}
	};

#elif defined(USE_MS_WIN)

	class FileWriterImpl : public FileWriter {
	private:
		FILE * fd;
	public:
		FileWriterImpl(File & file) : FileWriter(NULL, file), fd(NULL) {
			open(file);
		}
		virtual ~FileWriterImpl() {
			close();
		}
		virtual void open(File & file) {
			if (fopen_s(&fd, file.getPath().c_str(), "wb") != 0) {
				throw IOException("fopen() error", -1, 0);
			}
		}
		virtual size_t write(const char * data, size_t len) {
			return fwrite(data, 1, len, fd);
		}
		virtual void seek(size_t pos) {
			fseek(fd, pos, SEEK_SET);
		}
		virtual void close() {
			if (fd) {
				fclose(fd);
				fd = NULL;
			}
		}
	};

#endif

	/**
	 * @brief FileWriter
	 */

	FileWriter::FileWriter(FileWriter * impl, File & file) : impl(impl), file(file){
	}
	
	FileWriter::FileWriter(File & file) : file(file) {
		impl = new FileWriterImpl(file);
	}

	FileWriter::~FileWriter() {
		if (impl) {
			delete impl;
		}
	}

	void FileWriter::open(File & file) {
		CHECK_NOT_IMPL_THROW(impl);
		return impl->open(file);
	}

	size_t FileWriter::write(const char * data, size_t len) {
		CHECK_NOT_IMPL_THROW(impl);
		return impl->write(data, len);
	}

	void FileWriter::seek(size_t pos) {
		CHECK_NOT_IMPL_THROW(impl);
		impl->seek(pos);
	}

	void FileWriter::close() {
		CHECK_NOT_IMPL_THROW(impl);
		impl->close();
	}
	
	File & FileWriter::getFile() {
		return file;
	}

#if defined(USE_UNIX_STD)

	class RandomAccessFileImpl : public RandomAccessFile {
	private:
		FILE * fd;
	public:
		RandomAccessFileImpl(File & file, const string & mode) : RandomAccessFile(NULL, file), fd(NULL) {
			open(file, mode);
		}
		virtual ~RandomAccessFileImpl() {
			close();
		}
		virtual void open(OS::File & file, const string & mode) {
			fd = fopen(file.getPath().c_str(), mode.c_str());
			if (!fd) {
				throw IOException("fopen() error", -1, 0);
			}
		}
		virtual size_t read(char * buffer, size_t len) {
			return fread(buffer, 1, len, fd);
		}
		virtual size_t write(const char * data, size_t len) {
			return fwrite(data, 1, len, fd);
		}
		virtual void seek(size_t pos) {
			fseek(fd, pos, SEEK_SET);
		}
		virtual void close() {
			if (fd) {
				fclose(fd);
				fd = NULL;
			}
		}
	};

#elif defined(USE_MS_WIN)

	class RandomAccessFileImpl : public RandomAccessFile {
	private:
		FILE * fd;
	public:
		RandomAccessFileImpl(File & file, const string & mode) : RandomAccessFile(NULL, file), fd(NULL) {
			open(file, mode);
		}
		virtual ~RandomAccessFileImpl() {
			close();
		}
		virtual void open(File & file, const string & mode) {
			if (fopen_s(&fd, file.getPath().c_str(), mode.c_str()) != 0) {
				throw IOException("fopen() error", -1, 0);
			}
		}
		virtual size_t read(char * buffer, size_t len) {
			size_t ret = fread(buffer, 1, len, fd);
			return ret;
		}
		virtual size_t write(const char * data, size_t len) {
			return fwrite(data, 1, len, fd);
		}
		virtual void seek(size_t pos) {
			fseek(fd, pos, SEEK_SET);
		}
		virtual void close() {
			if (fd) {
				fclose(fd);
				fd = NULL;
			}
		}
	};

#endif

	/**
	 * @brief FileWriter
	 */

	RandomAccessFile::RandomAccessFile(RandomAccessFile * impl, File & file) : impl(impl), file(file){
	}
	
	RandomAccessFile::RandomAccessFile(File & file, const string & mode) : file(file) {
		impl = new RandomAccessFileImpl(file, mode);
	}

	RandomAccessFile::~RandomAccessFile() {
		if (impl) {
			delete impl;
		}
	}

	void RandomAccessFile::open(File & file, const string & mode) {
		CHECK_NOT_IMPL_THROW(impl);
		return impl->open(file, mode);
	}

	size_t RandomAccessFile::read(char * buffer, size_t len) {
		CHECK_NOT_IMPL_THROW(impl);
		return impl->read(buffer, len);
	}

	size_t RandomAccessFile::write(const char * data, size_t len) {
		CHECK_NOT_IMPL_THROW(impl);
		return impl->write(data, len);
	}

	void RandomAccessFile::seek(size_t pos) {
		CHECK_NOT_IMPL_THROW(impl);
		impl->seek(pos);
	}

	void RandomAccessFile::close() {
		CHECK_NOT_IMPL_THROW(impl);
		impl->close();
	}
	
	File & RandomAccessFile::getFile() {
		return file;
	}


} // namespace
