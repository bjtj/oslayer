#include <iostream>
#include "Process.hpp"

namespace OS {

	using namespace std;

	Process::Process(const std::string & cmd) : pid(0), cmd(cmd), fdin(NULL), fdout(NULL), fderr(NULL), _exitCode(0)  {
	}
	Process::Process(const std::string & cmd, const std::vector<std::string> & env) : pid(0), cmd(cmd), env(env), fdin(NULL), fdout(NULL), fderr(NULL), _exitCode(0)  {
	}
	Process::~Process() {
	}
	void Process::start() {
		pid_t pid = 0;
		char buf[1024] = {0,};

		if (pipe(pipe_in) != 0 || pipe(pipe_out) != 0 || pipe(pipe_err) != 0) {
			throw IOException("pipe() error", -1, 0);
		}
	
		pid = fork();
		if (pid == 0) {

			// child

			::close(pipe_in[1]);
			::close(pipe_out[0]);
			::close(pipe_err[0]);

			dup2(pipe_in[0], STDIN_FILENO);
			dup2(pipe_out[1], STDOUT_FILENO);
			dup2(pipe_err[1], STDERR_FILENO);

			::close(pipe_in[0]);
			::close(pipe_out[1]);
			::close(pipe_err[1]);

			execlp("sh", "sh", "-c", cmd.c_str(), NULL);
		
			// execl do not return if successful
			// -- http://stackoverflow.com/a/16089327/5676460

			perror("execl() error");
			exit(1);
			
		} else {

			// parent

			this->pid = pid;
			
			fdin = fdopen(pipe_in[1], "w");
			if (!fdin) {
				throw IOException("fdopen() error", -1, 0);
			}
			fdout = fdopen(pipe_out[0], "r");
			if (!fdout) {
				throw IOException("fdopen() error", -1, 0);
			}
			fderr = fdopen(pipe_err[0], "r");
			if (!fderr) {
				throw IOException("fdopen() error", -1, 0);
			}

			::close(pipe_in[0]);
			::close(pipe_out[1]);
			::close(pipe_err[1]);
		}
	}

	size_t Process::read(char * buffer, size_t size) {
		return fread(buffer, 1, size, fdout);
	}

	size_t Process::readerr(char * buffer, size_t size) {
		return fread(buffer, 1, size, fderr);
	}
	
	size_t Process::write(const char * buffer, size_t size) {
		size_t ret = fwrite(buffer, 1, size, fdin);
		fflush(fdin);
		return ret;
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
		if (waitpid(pid, &status, WUNTRACED | WCONTINUED) == -1) {
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
		if (fdin) {
			fclose(fdin);
			fdin = NULL;
		}
		if (fdout) {
			fclose(fdout);
			fdout = NULL;
		}
		if (fderr) {
			fclose(fderr);
			fderr = NULL;
		}

		::close(pipe_in[1]);
		::close(pipe_out[0]);
		::close(pipe_err[0]);
		pid = 0;
	}

	int Process::exitCode() {
		return _exitCode;
	}
}
