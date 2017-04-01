#include "utils.hpp"
#include <liboslayer/TaskThreadPool.hpp>

using namespace std;
using namespace OS;
using namespace UTIL;

class MyTask : public Task {
private:
	static Semaphore sem;
public:
	static int count;
public:
    MyTask() {}
    virtual ~MyTask() {}
	virtual void doTask() {
		sem.wait();
		cout << "task :: " << count++ << endl;
		sem.post();
	}
};

Semaphore MyTask::sem(1);
int MyTask::count = 0;

static void test_task_thread_pool() {
	TaskThreadPool pool(10);

	pool.start();

	for (int i = 0; i < 100; i++) {
		pool.setTaskWaitIfFull(AutoRef<Task>(new MyTask));
	}

	idle(100);

	pool.stop();

	ASSERT(MyTask::count, ==, 100);
}

class InfiniteTask : public Task {
private:
	static bool done;
public:
	InfiniteTask() {}
	virtual ~InfiniteTask() {}
	virtual void doTask() {
		while (!done) {
			idle(10);
		}
		cout << "infinite task::done" << endl;
	}
	static void setDone() {
		done = true;
	}
};

bool InfiniteTask::done = false;

static void test_full() {

	string err;
	unsigned long tick, dur;
	unsigned long timeout;
	
	size_t size = 5;
	TaskThreadPool pool(size);

	pool.start();

	for (size_t i = 0; i < size; i++) {
		pool.setTask(AutoRef<Task>(new InfiniteTask));
	}

	tick = tick_milli();
	try {
		pool.setTask(AutoRef<Task>(new InfiniteTask));
	} catch (Exception e) {
		err = e.toString();
	}
	dur = tick_milli() - tick;
	ASSERT(err.empty(), ==, false);
	ASSERT(dur, <, 100);

	err = "";
	tick = tick_milli();
	timeout = 1000;
	try {
		pool.setTaskWaitIfFullWithTimeout(AutoRef<Task>(new InfiniteTask), timeout);
	} catch (Exception e) {
		err = e.toString();
	}
	dur = tick_milli() - tick;
	ASSERT(err.empty(), ==, false);
	ASSERT(dur, >=, timeout);

	InfiniteTask::setDone();

	pool.stop();
}

int main(int argc, char *args[]) {

	test_task_thread_pool();
	test_full();
    
    return 0;
}
