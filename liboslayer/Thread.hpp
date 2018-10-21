#ifndef __THREAD_HPP__
#define __THREAD_HPP__

#include "os.hpp"

namespace osl {

    /*
     * @brief Thread
     */
    class Thread {
    private:
	THREAD_HANDLE handle;
	bool signal_interrupt;
	bool running;
	size_t stack_size;
    private:
	static unsigned int s_thread_id_seed;
    public:
	unsigned int id;
    private:
	void init();
    public:
	Thread();
	Thread(size_t stack_size);
	virtual ~Thread();
	unsigned int getId();
	void reset();
	bool start();
	virtual void interrupt();
	bool interrupted();
	bool isRunning();
	virtual void onInterrupt();
	void wait(); // deprecated
	void join();
	void waitFor();
	virtual void run() = 0;
    };

}

#endif
