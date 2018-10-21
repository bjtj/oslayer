#include <iostream>
#include <liboslayer/AutoLock.hpp>
#include <liboslayer/Thread.hpp>

using namespace std;
using namespace osl;


class Worker : public Thread
{
private:
public:
    Worker() {}
    virtual ~Worker() {}
    virtual void run() {
	for (int i = 0; i < 100; i++) {
	    cout << "worker print -- " << i << endl;
	    idle(10);
	}
    }
};

class SyncWorker : public Thread
{
private:
    Ref<Semaphore> _sem;
public:
    SyncWorker(Ref<Semaphore> sem) : _sem(sem) {}
    virtual ~SyncWorker() {}
    virtual void run() {
	for (int i = 0; i < 100; i++) {
	    auto_lock(_sem) {
		cout << "[sync] worker print -- " << i << endl;
	    }
	    idle(10);
	}
    }
};


int main(int argc, char *argv[])
{

    Worker worker1;
    Worker worker2;

    worker1.start();
    worker2.start();

    worker1.join();
    worker2.join();
	
    Ref<Semaphore> sem(new Semaphore(1));
    SyncWorker sworker1(sem);
    SyncWorker sworker2(sem);
    sworker1.start();
    sworker2.start();

    sworker1.join();
    sworker2.join();
    sem.dealloc();
    return 0;
}
