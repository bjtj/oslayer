#include "utils.hpp"
#include <liboslayer/os.hpp>
#include <liboslayer/Task.hpp>
#include <liboslayer/ThreadPool.hpp>
#include <liboslayer/AutoRef.hpp>

using namespace std;
using namespace OS;
using namespace UTIL;

class WorkerThread : public FlaggableThread {
private:
	AutoRef<Task> task;
public:
    WorkerThread() : FlaggableThread(false) {}
	virtual ~WorkerThread() {}
	void setTask(AutoRef<Task> task) {
		this->task = task;
	}
	virtual void run() {
		while (!interrupted()) {
			if (!flagged()) {
				idle(10);
				continue;
			}

			task->doTask();

			setFlag(false);
		}
	}
};


class WorkerInstanceCreator : public InstanceCreator<FlaggableThread*> {
private:
public:
    WorkerInstanceCreator() {}
    virtual ~WorkerInstanceCreator() {}
	virtual FlaggableThread * createInstance() {
		return new WorkerThread;
	}
	virtual void releaseInstance(FlaggableThread * instance) {
		delete instance;
	}
};

static WorkerInstanceCreator s_creator;

class WorkerThreadPool : public ThreadPool {
private:
public:
    WorkerThreadPool(size_t count) :  ThreadPool(count, s_creator) {}
    virtual ~WorkerThreadPool() {}
	void setTask(AutoRef<Task> task) {
		WorkerThread * thread = NULL;
		while ((thread = (WorkerThread*)acquire()) == NULL) {
			idle(10);
		}
		thread->setTask(task);
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
	virtual void doTask() {
		sem.wait();
		cout << "task :: " << count++ << endl;
		sem.post();
	}
};

int WorkerTask::count = 0;
Semaphore WorkerTask::sem(1);

class WorkerThreadObserver : public Observer {
private:
	int count;
public:
	WorkerThreadObserver() : count(0) {}
	virtual ~WorkerThreadObserver() {}
	virtual void update(Observable * target) {
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
		pool.setTask(AutoRef<Task>(new WorkerTask));
	}

	idle(100);

	ASSERT(pool.freeCount(), ==, 5);
	ASSERT(pool.workingCount(), ==, 0);

	pool.stop();

	ASSERT(WorkerTask::count, ==, 100);
	ASSERT(pool.freeCount(), ==, 0);
	ASSERT(pool.workingCount(), ==, 0);

	ASSERT(observer.getCount(), ==, -100);
}

int main(int argc, char *args[]) {

	test_thread_pool();
    
    return 0;
}
