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
		std::string _path;

	public:
		File();
		File(const std::string & path);
		virtual ~File();

		static std::string separators();

		static std::string merge(const std::string & dir, const std::string & filename);
		static std::string merge(const std::string & dir, const std::string & filename, const std::string & separators);
		static std::string fullpath(const std::string & dir, const std::string & filename);
		static std::string fullpath(const std::string & dir, const std::string & filename, const std::string & separators);
		static std::string getCwd();

		/* */
		static bool isRootPath(const std::string & path);
		static bool isAbsolutePath(const std::string & path);
		static bool exists(const std::string & path);
		static bool isFile(const std::string & path);
		static bool isDirectory(const std::string & path);
		static bool isWritable(const std::string & path);
		static std::string absolutePath(const std::string & path);
		static std::string dirname(const std::string & path);
		static std::string name(const std::string & path);
		static std::string basename(const std::string & path);
		static std::string extension(const std::string & path);
		static int mkdir(const std::string & path);
		static bool remove(const std::string & path);
		static Date creationDate(const std::string & path);
		static Date lastModifiedDate(const std::string & path);
		static osl_time_t creationTime(const std::string & path);
		static osl_time_t lastModifiedTime(const std::string & path);
		static filesize_t size(const std::string & path);
		static std::vector<File> list(const std::string & path);

		std::string path() const;
		bool isRootPath() const;
		bool isAbsolutePath() const;
		bool exists() const;
		bool isFile() const;
		bool isDirectory() const;
		bool isWritable() const;
		std::string absolutePath();
		std::string dirname() const;
		std::string name() const;
		std::string basename() const;
		std::string extension() const;
		int mkdir() const;
		bool remove();
		Date creationDate() const;
		Date lastModifiedDate() const;
		osl_time_t creationTime() const;
		osl_time_t lastModifiedTime() const;
		filesize_t size() const;
		std::vector<File> list() const;
		virtual std::string toString() const;
	};	
}

#endif
