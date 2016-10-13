#ifndef __PROCESS_HPP__
#define __PROCESS_HPP__

#include "platform.hpp"
#include "os.hpp"
#include <string>
#include <vector>

namespace OS {

#if defined(USE_UNIX_STD)

	/**
	 * @brief 
	 */
	class Process {
	private:
		pid_t pid;
		std::string cmd;
		std::vector<std::string> env;
		int pipe_in[2];
		int pipe_out[2];
		int pipe_err[2];
		FILE * fdin;
		FILE * fdout;
		FILE * fderr;
		int _exitCode;

	private:
		Process(const Process & other);
	    Process & operator=(const Process & other);
		
	public:
		Process(const std::string & cmd);
		Process(const std::string & cmd, const std::vector<std::string> & env);
		virtual ~Process();
		void start();
		FILE * in();
		FILE * out();
		FILE * err();
		void wait();
		bool exited();
		void close();
		int exitCode();
	};

#elif defined(USE_MS_WIN)

	class Process {
	private:

	private:
		Process(const Process & other);
	    Process & operator=(const Process & other);
		
	public:
		Process(const std::string & cmd);
		Process(const std::string & cmd, const std::vector<std::string> & env);
		virtual ~Process();
	};

	
#else

	// not implemented

	class Process {
	private:
	public:
		Process();
		virtual ~Process();
	};


#endif // USE_UNIX_STD

}

#endif
