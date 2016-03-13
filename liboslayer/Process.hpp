#ifndef __PROCESS_HPP__
#define __PROCESS_HPP__

#include "platform.hpp"
#include "os.hpp"
#include <string>
#include <vector>

namespace OS {
	
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
		size_t read(char * buffer, size_t size);
		size_t readerr(char * buffer, size_t size);
		size_t write(const char * buffer, size_t size);
		void wait();
		bool exited();
		void close();
		int exitCode();
	};
}

#endif
