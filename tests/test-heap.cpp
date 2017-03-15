#include <liboslayer/Heap.hpp>
#include <liboslayer/TestSuite.hpp>
#include <vector>

using namespace std;
using namespace OS;
using namespace UTIL;

/**
 * 
 */
class Object
{
private:
	int _ival;
public:
    Object() {
		_init();
	}
	Object(int ival) : _ival(ival) {
		_init();
	}
    virtual ~Object() {
		cout << "~Object()" << endl;
	}
	void _init() {
		cout << "Object()" << endl;
	}
	int & ival() { return _ival; }
};


/**
 * 
 */
class Device {
private:
	GCRef<Device> _parent;
public:
	Device() {
	}
	Device(GCRef<Device> parent) : _parent(parent) {
	}
	virtual ~Device() {
	}
	GCRef<Device> parent() {
		return _parent;
	}
};

/**
 * 
 */
class HeapTestCase : public TestCase {
public:
	HeapTestCase() : TestCase("heap") {
	}
	virtual ~HeapTestCase() {
	}

	template <typename T>
	void print_heap(const string & tag, Heap<T> & heap) {
		cout << " ** [" << tag << "] heap debug **" << endl;
		heap.print_usage();
		cout << endl;
	}

	virtual void test() {
		Heap<Device> heap;
		cout << " -- test " << __LINE__ << endl;
		{
			GCRef<Device> root = heap.alloc(new Device);
			print_heap("1-IN", heap);
			GCRef<Device> child = heap.alloc(new Device(root));
			print_heap("2-IN", heap);
		}
		print_heap("OUT", heap);
		cout << "BEFORE GC - " << heap.size() << endl;
		unsigned long elapsed = heap.gc();
		cout << "AFTER GC - " << heap.size() << "/ elapsed: " << elapsed << " ms." << endl;
	}
};

/**
 * 
 */
class HeapClearTestCase : public TestCase {
public:
	HeapClearTestCase() : TestCase("heap-clear") {
	}
	virtual ~HeapClearTestCase() {
	}

	template <typename T>
	void print_heap(const string & tag, Heap<T> & heap) {
		cout << " ** [" << tag << "] heap debug **" << endl;
		heap.print_usage();
		cout << endl;
	}

	virtual void test() {
		Heap<Device> heap;
		cout << " -- test " << __LINE__ << endl;
		{
			GCRef<Device> root = heap.alloc(new Device);
			print_heap("1-IN", heap);
			GCRef<Device> child = heap.alloc(new Device(root));
			print_heap("2-IN", heap);
		}
		print_heap("OUT", heap);
		cout << "BEFORE GC - " << heap.size() << endl;
		heap.clear();
		cout << "AFTER GC - " << heap.size() << endl;
	}
};

/**
 * 
 */
class HeapObjTestCase : public TestCase {
public:
	HeapObjTestCase() : TestCase("heap-obj") {
	}
	virtual ~HeapObjTestCase() {
	}

	template <typename T>
	void print_heap(const string & tag, Heap<T> & heap) {
		cout << " ** [" << tag << "] heap debug **" << endl;
		heap.print_usage();
		cout << endl;
	}

	virtual void test() {
		Heap<Object> heap;
		cout << " -- test " << __LINE__ << endl;
		{
			GCRef<Object> obj = heap.alloc(new Object);
			obj = heap.alloc(new Object);

			map<string, GCRef<Object> > objs;
			objs["obj"] = heap.alloc(new Object(1000));
			objs["obj"] = heap.alloc(new Object(10));

			cout << "ival : " << objs["obj"]->ival() << endl;
		}
		print_heap("OUT", heap);
		cout << "BEFORE GC - " << heap.size() << endl;
		heap.clear();
		cout << "AFTER GC - " << heap.size() << endl;
	}
};

int main(int argc, char *args[]) {

	TestSuite ts;
	ts.addTestCase(AutoRef<TestCase>(new HeapTestCase));
	ts.addTestCase(AutoRef<TestCase>(new HeapClearTestCase));
	ts.addTestCase(AutoRef<TestCase>(new HeapObjTestCase));

	TestReport report(ts.testAll());
	report.validate();
    
    return 0;
}
