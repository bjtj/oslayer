#include "Process.hpp"

namespace osl {

    using namespace std;

#if defined(USE_UNIX_STD)

    Process::Process(const std::string & cmd) : _pid(0), _cmd(cmd), _fdin(NULL), _fdout(NULL), _fderr(NULL), _exitCode(0)  {
	/**/
    }
    Process::Process(const std::string & cmd, const std::vector<std::string> & env) :
	_pid(0), _cmd(cmd), _env(env), _fdin(NULL), _fdout(NULL), _fderr(NULL), _exitCode(0)
    {
	/**/
    }
    Process::~Process() {
	/**/
    }
    void Process::start() {
	pid_t pid = 0;

	if (pipe(_pipe_in) != 0 || pipe(_pipe_out) != 0 || pipe(_pipe_err) != 0) {
	    throw IOException("pipe() error", -1, 0);
	}
	
	pid = fork();
	if (pid == 0) {

	    // child

	    ::close(_pipe_in[1]);
	    ::close(_pipe_out[0]);
	    ::close(_pipe_err[0]);

	    dup2(_pipe_in[0], STDIN_FILENO);
	    dup2(_pipe_out[1], STDOUT_FILENO);
	    dup2(_pipe_err[1], STDERR_FILENO);

	    ::close(_pipe_in[0]);
	    ::close(_pipe_out[1]);
	    ::close(_pipe_err[1]);

	    execlp("sh", "sh", "-c", _cmd.c_str(), (char*)NULL);
		
	    // execl do not return if successful
	    // -- http://stackoverflow.com/a/16089327/5676460

	    perror("execl() error");
	    exit(1);
			
	} else {

	    // parent

	    this->_pid = pid;
			
	    _fdin = fdopen(_pipe_in[1], "w");
	    if (!_fdin) {
		throw IOException("fdopen() error", -1, 0);
	    }
	    _fdout = fdopen(_pipe_out[0], "r");
	    if (!_fdout) {
		throw IOException("fdopen() error", -1, 0);
	    }
	    _fderr = fdopen(_pipe_err[0], "r");
	    if (!_fderr) {
		throw IOException("fdopen() error", -1, 0);
	    }

	    ::close(_pipe_in[0]);
	    ::close(_pipe_out[1]);
	    ::close(_pipe_err[1]);
	}
    }

    long Process::pid() {
	return (long)_pid;
    }
	
    FILE * Process::in() {
	return _fdin;
    }
	
    FILE * Process::out() {
	return _fdout;
    }
	
    FILE * Process::err() {
	return _fderr;
    }

    void Process::wait() {
	while (!exited()) {
	    idle(10);
	}
    }

    bool Process::exited() {

	// http://linux.die.net/man/2/wait
		
	bool ret = false;
	int status;
	if (waitpid(_pid, &status, WUNTRACED | WCONTINUED) == -1) {
	    perror("waitpid() error");
	    throw IOException("waitpid() error", -1, 0);
	}

	if (WIFEXITED(status)) {
	    ret = true;
	    _exitCode = WEXITSTATUS(status);
	} else if (WIFSIGNALED(status)) {
	    ret = true;
	} else if (WIFSTOPPED(status)) {
	    // stopped
	} else if (WIFCONTINUED(status)) {
	    // continued
	}
	return ret;
    }

    void Process::close() {
	if (_fdin) {
	    fclose(_fdin);
	    _fdin = NULL;
	}
	if (_fdout) {
	    fclose(_fdout);
	    _fdout = NULL;
	}
	if (_fderr) {
	    fclose(_fderr);
	    _fderr = NULL;
	}

	::close(_pipe_in[1]);
	::close(_pipe_out[0]);
	::close(_pipe_err[0]);
	_pid = 0;
    }

    int Process::exitCode() {
	return _exitCode;
    }

#elif defined(USE_MS_WIN)

    // win32

    Process::Process(const string & cmd)
	: _cmd(cmd), in_read(NULL), in_write(NULL), out_read(NULL), out_write(NULL), err_read(NULL), err_write(NULL)
    {
	memset(&piProcInfo, 0, sizeof(PROCESS_INFORMATION));
    }
    Process::Process(const string & cmd, const vector<string> & env) 
	: _cmd(cmd), in_read(NULL), in_write(NULL), out_read(NULL), out_write(NULL), err_read(NULL), err_write(NULL)
    {
	memset(&piProcInfo, 0, sizeof(PROCESS_INFORMATION));
    }
    Process::~Process(){
    }

    void Process::start() {

	/* make pipe */

	SECURITY_ATTRIBUTES saAttr = {0,};
	saAttr.nLength = sizeof(SECURITY_ATTRIBUTES);
	saAttr.bInheritHandle = TRUE;
	saAttr.lpSecurityDescriptor = NULL;
	if (!CreatePipe(&out_read, &out_write, &saAttr, 0)) {
	    throw Exception("CreatePipe() failed - stdout read");
	}	
	if (!SetHandleInformation(out_read, HANDLE_FLAG_INHERIT, 0)) {
	    throw Exception("SetHandleInformation() failed - stdout");
	}
	if (!CreatePipe(&err_read, &err_write, &saAttr, 0)) {
	    throw Exception("CreatePipe() failed - stderr read");
	}	
	if (!SetHandleInformation(err_read, HANDLE_FLAG_INHERIT, 0)) {
	    throw Exception("SetHandleInformation() failed - stderr");
	}
	if (!CreatePipe(&in_read, &in_write, &saAttr, 0))  {
	    throw Exception("CreatePipe() failed - stdin read");
	}
	if (!SetHandleInformation(in_write, HANDLE_FLAG_INHERIT, 0)) {
	    throw Exception("SetHandleInformation() failed - stdin");
	}
		
	/* create process */
	BOOL bSuccess = FALSE;
	STARTUPINFO siStartInfo = {0,};

	siStartInfo.cb = sizeof(STARTUPINFO); 
	siStartInfo.hStdError = err_write;
	siStartInfo.hStdOutput = out_write;
	siStartInfo.hStdInput = in_read;		
	siStartInfo.dwFlags |= STARTF_USESTDHANDLES;

	string cmd = "cmd /C " + _cmd;
		
	bSuccess = CreateProcess(NULL, 
				 (LPSTR)cmd.c_str(),
				 NULL,          // process security attributes 
				 NULL,          // primary thread security attributes 
				 TRUE,          // handles are inherited 
				 0,             // creation flags 
				 NULL,          // use parent's environment 
				 NULL,          // use parent's current directory 
				 &siStartInfo,  // STARTUPINFO pointer 
				 &piProcInfo);

	if (!bSuccess) {
	    throw Exception("CreateProcess() failed");
	}

	CloseHandle(err_write);
	CloseHandle(out_write);
	CloseHandle(in_read);
    }

    long Process::pid() {
	return (long)piProcInfo.dwProcessId;
    }
	
    HANDLE Process::in() {
	return in_write;
    }
	
    HANDLE Process::out() {
	return out_read;
    }
	
    HANDLE Process::err() {
	return err_read;
    }
	
    void Process::wait() {
	while (!exited()) {
	    idle(10);
	}
    }
	
    bool Process::exited() {
	return (exitCode() != STILL_ACTIVE);
    }
	
    void Process::close() {
	CloseHandle(piProcInfo.hProcess);
	CloseHandle(piProcInfo.hThread);
	CloseHandle(in_write);
	CloseHandle(out_read);
	CloseHandle(err_read);
    }
	
    int Process::exitCode() {
	DWORD code;
	if (!GetExitCodeProcess(piProcInfo.hProcess, &code)) {
	    throw Exception("GetExitCodeProcess() failed");
	}
	return (int)code;
    }

#else
	
    // not implemented
	
#endif

}
