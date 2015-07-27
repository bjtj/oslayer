#include <liboslayer/os.hpp>
#include <iostream>

using namespace OS;
using namespace std;

class MyThread : public Thread {
private:
public:
    MyThread() {
	}
    virtual ~MyThread() {
	}

	virtual void run() {
		for (int i = 0; i < 10; i++) {
			cout << "count " << i << endl;
			idle(100);
		}
	}
};


int main(int argc, char *args[]) {

	MyThread t;

	t.start();
	t.interrupt();
	t.join();
    
    return 0;
}
