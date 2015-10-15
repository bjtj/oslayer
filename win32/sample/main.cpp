#include <iostream>
#include <liboslayer/os.hpp>

using namespace std;
using namespace OS;

class MyThread : public Thread {
private:
public:
	MyThread() {}
	virtual ~MyThread() {}

	virtual void run() {
		cout << "hello in thread" << endl;
	}
};

int main(int argc, char * args[]) {

	MyThread mt;

	mt.start();

	Sleep(100);

	return 0;
}