#include <iostream>
#include <liboslayer/MessageQueue.hpp>
#include "utils.hpp"

using namespace std;
using namespace OS;
using namespace UTIL;

static int s_refcount = 0;

/**
 * custom object
 */
class MyObj {
private:
	string _str;
public:
    MyObj(const string & str) : _str(str) {
		s_refcount++;
	}
    virtual ~MyObj() {
		s_refcount--;
	}
	string & str() {return _str;}
};

typedef Message<AutoRef<MyObj> > MyObjMessage; // convenient typedef

/**
 * test basic
 */
static void test_basic() {
	MessageQueue<AutoRef<MyObj> > mq;
	mq.enqueue(MyObjMessage(0));
	mq.enqueue(MyObjMessage(1, 2, 3));
	mq.enqueue(MyObjMessage(2, 0, 1, AutoRef<MyObj>(new MyObj("hello"))));

	ASSERT(mq.dequeue().what(), ==, 0);
	ASSERT(mq.dequeue().what(), ==, 1);
	MyObjMessage msg = mq.dequeue();
	ASSERT(msg.what(), ==, 2);
	ASSERT(msg.arg1(), ==, 0);
	ASSERT(msg.arg2(), ==, 1);
	ASSERT(msg.obj()->str(), ==, "hello");
}

/**
 * main
 */
int main(int argc, char *args[]) {

	test_basic();

	ASSERT(s_refcount, ==, 0);
	
    return 0;
}
