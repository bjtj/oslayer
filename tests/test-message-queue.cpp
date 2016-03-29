#include <iostream>
#include <liboslayer/MessageQueue.hpp>
#include "utils.hpp"

using namespace std;
using namespace UTIL;

static int s_refcount = 0;

class MyObj : public Object {
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


static void test_basic() {
	MessageQueue mq;
	mq.enqueue(Message(0));
	mq.enqueue(Message(1, 2, 3));
	mq.enqueue(Message(2, 0, 1, AutoRef<Object>(new MyObj("hello"))));

	ASSERT(mq.dequeue().what(), ==, 0);
	ASSERT(mq.dequeue().what(), ==, 1);
	Message msg = mq.dequeue();
	ASSERT(msg.what(), ==, 2);
	ASSERT(msg.arg1(), ==, 0);
	ASSERT(msg.arg2(), ==, 1);
	ASSERT(((MyObj*)(&msg.obj()))->str(), ==, "hello");
}

int main(int argc, char *args[]) {

	test_basic();

	ASSERT(s_refcount, ==, 0);
	
    return 0;
}
