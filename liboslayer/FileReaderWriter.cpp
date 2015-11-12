#include "FileReaderWriter.hpp"

#define CHECK_NOT_IMPL_THROW(x) if(!x){throw NotImplementedException();}

namespace UTIL {

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

		virtual int read(char * buffer, size_t len) {
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

		virtual int read(char * buffer, size_t len) {
			int ret = fread(buffer, 1, len, fd);
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

	int FileReader::read(char * buffer, size_t len) {
		CHECK_NOT_IMPL_THROW(impl);
		return impl->read(buffer, len);
	}

	void FileReader::close() {
		CHECK_NOT_IMPL_THROW(impl);
		impl->close();
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

		virtual int write(const char * data, size_t len) {
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

		virtual int write(const char * data, size_t len) {
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

	int FileWriter::write(const char * data, size_t len) {
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