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
			fd = fopen(file.getPath().c_str(), "rb");
			if (!fd) {
				throw IOException("fopen() error", -1, 0);
			}
		}
		virtual ~FileReaderImpl() {
			close();
		}

		virtual size_t read(char * buffer, size_t len) {
			return fread(buffer, 1, len, fd);
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
			if (fopen_s(&fd, file.getPath().c_str(), "rb") != 0) {
				throw IOException("fopen() error", -1, 0);
			}
		}
		virtual ~FileReaderImpl() {
			close();
		}

		virtual size_t read(char * buffer, size_t len) {
			size_t ret = fread(buffer, 1, len, fd);
			return ret;
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

	size_t FileReader::read(char * buffer, size_t len) {
		CHECK_NOT_IMPL_THROW(impl);
		return impl->read(buffer, len);
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
			fd = fopen(file.getPath().c_str(), "wb");
			if (!fd) {
				throw IOException("fopen() error", -1, 0);
			}
		}
		virtual ~FileWriterImpl() {
			close();
		}

		virtual size_t write(const char * data, size_t len) {
			return fwrite(data, 1, len, fd);
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
			errno_t ret = fopen_s(&fd, file.getPath().c_str(), "wb");
			if (ret != 0) {
				throw IOException("fopen() error", -1, 0);
			}
		}
		virtual ~FileWriterImpl() {
			close();
		}

		virtual size_t write(const char * data, size_t len) {
			return fwrite(data, 1, len, fd);
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

	size_t FileWriter::write(const char * data, size_t len) {
		CHECK_NOT_IMPL_THROW(impl);
		return impl->write(data, len);
	}

	void FileWriter::close() {
		CHECK_NOT_IMPL_THROW(impl);
		impl->close();
	}
	
	File & FileWriter::getFile() {
		return file;
	}

}