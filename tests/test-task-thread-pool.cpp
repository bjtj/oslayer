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
		pool.setTask(AutoRef<Task>(new MyTask));
	}

	idle(100);

	pool.stop();

	ASSERT(MyTask::count, ==, 100);
}

int main(int argc, char *args[]) {

	test_task_thread_pool();
    
    return 0;
}
