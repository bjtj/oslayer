#include "Thread.hpp"

namespace osl {

	/* THREAD */
#if defined(USE_PTHREAD)

	// unix or linux

	typedef void * (*thread_func)(void *);
	
	static void * s_thread_wrapper(void * arg) {
		Thread * thread = (Thread*)arg;
#if defined(USE_PRCTL)
		{
			char name[16] = {0,};
			snprintf(name, sizeof(name), "tid:0x%x", (unsigned int)thread->getId());
			prctl(PR_SET_NAME, name, 0, 0, 0);
		}
#endif
		thread->run();
		thread->reset();
		return 0;
	}

	/**
	 * @brief pthread_create
	 */
	static bool s_startThread(THREAD_HANDLE * handle, thread_func func, Thread * thread, size_t stack_size) {
		pthread_attr_t attr;
		bool started = false;
		if (!handle || !thread) {
			return false;
		}
		if (pthread_attr_init(&attr) != 0) {
			return false;
		}
		try {
			if (pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED) != 0) {
				throw Exception("pthread_attr_setdetachstate() failed");
			}
			if (stack_size > 0) {
				if (pthread_attr_setstacksize(&attr, stack_size) != 0) {
					throw Exception("pthread_attr_setstacksize() failed");
				}
			}
			if (pthread_create(handle, &attr, func, (void*)thread) != 0) {
				throw Exception("pthread_create() failed");
			}
			started = true;
		} catch (Exception e) {
			// ...
		}
		pthread_attr_destroy(&attr);
		return started;
	}

#elif defined(USE_WIN_THREAD)

	// windows

	typedef UINT (WINAPI thread_func)(void *);

	static UINT WINAPI s_thread_wrapper(void * arg) {
		Thread * thread = (Thread*)arg;
		thread->run();
		thread->reset();
		return 0;
	}

	/**
	 * @brief win32 thread start
	 */
	static bool s_startThread(THREAD_HANDLE * handle, thread_func func, Thread * thread, size_t stack_size) {
		UINT dwThreadID;
		HANDLE h;
		if (!handle || !thread) {
			return false;
		}
		h = (HANDLE)_beginthreadex(NULL, stack_size, func, (void*)thread, 0, &dwThreadID);
		*handle = h;
		return false;
	}

#endif

	/**
	 * @brief thread id seed - 0 means error
	 */
	unsigned int Thread::s_thread_id_seed = 0;

	/**
	 * @brief thread constructor
	 */
	Thread::Thread() : handle(0), signal_interrupt(false), stack_size(0) {
		init();
	}

	Thread::Thread(size_t stack_size) : handle(0), signal_interrupt(false), stack_size(stack_size) {
		init();
	}
	
	Thread::~Thread() {
	}

	void Thread::init() {
		id = (++s_thread_id_seed == 0) ? ++s_thread_id_seed : s_thread_id_seed;
		reset();
	}

	unsigned int Thread::getId() {
		return id;
	}

	void Thread::reset() {
		signal_interrupt = false;
		running = false;
	}

	bool Thread::start() {
		if (!isRunning()) {
			bool ret = s_startThread(&handle, s_thread_wrapper, this, stack_size);
			running = true;
			return ret;
		}
		return false;
	}
	
	void Thread::interrupt() {
		signal_interrupt = true;
		onInterrupt();
	}

	bool Thread::interrupted() {
		bool ret = signal_interrupt;
		signal_interrupt = false;
		return ret;
	}

	bool Thread::isRunning() {
		return running;
	}

	void Thread::onInterrupt() {
		/* virtual */
	}

	void Thread::join() {
		while (running) { idle(10); }
	}

}
