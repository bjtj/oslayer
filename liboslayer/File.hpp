#ifndef __FILE_HPP__
#define __FILE_HPP__

#include "os.hpp"
#include "Date.hpp"

namespace OS {

#if defined(USE_UNIX_STD)
	typedef off_t filesize_t; // file size type
#elif defined(USE_MS_WIN)
    typedef unsigned long long filesize_t; // file size type
#else
	//
#endif

	/**
	 * @brife File
	 */
	class File {
	private:
		std::string path;

	public:
		File();
		File(const std::string & path);
		virtual ~File();

		static std::string getSeparators();

		static std::string mergePaths(const std::string & dir, const std::string & filename);
		static std::string mergePaths(const std::string & dir, const std::string & filename, const std::string & separators);
		static std::string fullpath(const std::string & dir, const std::string & filename);
		static std::string fullpath(const std::string & dir, const std::string & filename, const std::string & separators);
		static std::string getCwd();

		/* */
		static bool isRootPath(const std::string & path);
		static bool isFullpath(const std::string & path);
		static bool exists(const std::string & path);
		static bool isFile(const std::string & path);
		static bool isDirectory(const std::string & path);
		static bool isWritable(const std::string & path);
		static std::string getAbsolutePath(const std::string & path);
		static std::string getDirectory(const std::string & path);
		static std::string getName(const std::string & path);
		static std::string getFileName(const std::string & path);
		static std::string getFileNameWithoutExtension(const std::string & path);
		static std::string getExtension(const std::string & path);
		static bool compareExtension(const std::string & path, std::string extension);
		static int mkdir(const std::string & path);
		static bool remove(const std::string & path);
		static Date creationDate(const std::string & path);
		static Date lastModifiedDate(const std::string & path);
		static osl_time_t creationTime(const std::string & path);
		static osl_time_t lastModifiedTime(const std::string & path);
		static filesize_t getSize(const std::string & path);
		static std::vector<File> list(const std::string & path);

		std::string getPath() const;
		bool isRootPath() const;
		bool isFullpath() const;
		bool exists() const;
		bool isFile() const;
		bool isDirectory() const;
		bool isWritable() const;
		std::string getAbsolutePath();
		std::string getDirectory() const;
		std::string getName() const;
		std::string getFileName() const;
		std::string getFileNameWithoutExtension() const;
		std::string getExtension() const;
		bool compareExtension(std::string extension) const;
		int mkdir() const;
		bool remove();
		Date creationDate();
		Date lastModifiedDate();
		osl_time_t creationTime();
		osl_time_t lastModifiedTime();
		filesize_t getSize() const;
		std::vector<File> list() const;
		virtual std::string toString() const;
	};	
	
}

#endif
