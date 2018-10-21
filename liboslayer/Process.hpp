#ifndef __PROCESS_HPP__
#define __PROCESS_HPP__

#include "platform.hpp"
#include "os.hpp"
#include <string>
#include <vector>

namespace osl {

#if defined(USE_UNIX_STD)

    /**
     * @brief 
     */
    class Process {
    private:
	pid_t _pid;
	std::string _cmd;
	std::vector<std::string> _env;
	int _pipe_in[2];
	int _pipe_out[2];
	int _pipe_err[2];
	FILE * _fdin;
	FILE * _fdout;
	FILE * _fderr;
	int _exitCode;

    private:
	Process(const Process & other);
	Process & operator=(const Process & other);
		
    public:
	Process(const std::string & cmd);
	Process(const std::string & cmd, const std::vector<std::string> & env);
	virtual ~Process();
	void start();
	long pid();
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
	PROCESS_INFORMATION piProcInfo;
	std::string _cmd;
	HANDLE in_read;
	HANDLE in_write;
	HANDLE out_read;
	HANDLE out_write;
	HANDLE err_read;
	HANDLE err_write;
    private:
	Process(const Process & other);
	Process & operator=(const Process & other);
		
    public:
	Process(const std::string & cmd);
	Process(const std::string & cmd, const std::vector<std::string> & env);
	virtual ~Process();
	void start();
	long pid();
	HANDLE in();
	HANDLE out();
	HANDLE err();
	void wait();
	bool exited();
	void close();
	int exitCode();
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
