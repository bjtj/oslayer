#include <liboslayer/os.hpp>
#include <liboslayer/Text.hpp>
#include <iostream>

using namespace OS;
using namespace std;
using namespace UTIL;


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

static void test_thread() {
	MyThread t;

	t.start();
	t.interrupt();
	t.join();
}

static void test_toint() {
	int num;
	num = Text::toInt("10\r\n");
	cout << num << endl;
	num = Text::toInt("ff", 16);
	cout << num << endl;
}

int main(int argc, char *args[]) {

	//test_thread();
	test_toint();
    
    return 0;
}
