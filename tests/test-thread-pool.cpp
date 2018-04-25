#include "utils.hpp"
#include <liboslayer/os.hpp>
#include <liboslayer/Task.hpp>
#include <liboslayer/ThreadPool.hpp>
#include <liboslayer/AutoRef.hpp>

using namespace std;
using namespace osl;


/**
 * 
 */
class WorkerThreadPool : public ThreadPool {
private:
public:
    WorkerThreadPool(size_t count) :  ThreadPool(count) {}
    virtual ~WorkerThreadPool() {}
	void setTask(AutoRef<Task> task) {
		StatefulThread * thread;
		while ((thread = acquire()) == NULL) {
			idle(10);
		}
		thread->task() = task;
		enqueue(thread);
	}
};

class WorkerTask : public Task {
public:
	static int count;
private:
	static Semaphore sem;
public:
    WorkerTask() {}
    virtual ~WorkerTask() {}
	virtual void onTask() {
		sem.wait();
		cout << " @@ task :: " << count++ << endl;
		sem.post();
	}
};

int WorkerTask::count = 0;
Semaphore WorkerTask::sem(1);

/**
 * 
 */
class WorkerThreadObserver : public Observer {
private:
	int count;
public:
	WorkerThreadObserver()
		: count(0) {}
	virtual ~WorkerThreadObserver()
		{}
	virtual void onUpdate(Observable * target) {
		count--;
	}
	int getCount() {
		return count;
	}
};


static void test_thread_pool() {

	WorkerThreadPool pool(5);
	pool.start();

	WorkerThreadObserver observer;
	pool.addObserver(&observer);

	ASSERT(pool.freeCount(), ==, 5);
	ASSERT(pool.workingCount(), ==, 0);

	for (int i = 0; i < 100; i++) {
		cout << (i+1) << " try" << endl;
		pool.setTask(AutoRef<Task>(new WorkerTask));
	}

	idle(100);

	ASSERT(pool.freeCount(), ==, 5);
	ASSERT(pool.workingCount(), ==, 0);

	cout << "++ [stop]" << endl;
	pool.stop();
	cout << "-- [stop]" << endl;

	ASSERT(WorkerTask::count, ==, 100);
	ASSERT(pool.freeCount(), ==, 5);
	ASSERT(pool.workingCount(), ==, 0);

	ASSERT(observer.getCount(), ==, -100);
}

int main(int argc, char *args[]) {

	test_thread_pool();
    
    return 0;
}
