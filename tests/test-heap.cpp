#include <liboslayer/Heap.hpp>
#include <liboslayer/TestSuite.hpp>
#include <vector>

using namespace std;
using namespace OS;
using namespace UTIL;

class Device {
private:
	Obj<Device> _parent;
public:
	Device() {
	}
	Device(Obj<Device> parent) : _parent(parent) {
	}
	virtual ~Device() {
	}
	Obj<Device> parent() {
		return _parent;
	}
};

class HeapTestCase : public TestCase {
public:
	HeapTestCase() : TestCase("heap") {
	}
	virtual ~HeapTestCase() {
	}

	template <typename T>
	void print_heap(Heap<T> & heap) {
		cout << " ** heap debug **" << endl;
		heap.print_usage();
		cout << endl;
	}

	virtual void test() {
		Heap<Device> heap;

		cout << " -- test " << __LINE__ << endl;

		{
			Obj<Device> root = heap.alloc(new Device);
			print_heap(heap);
			Obj<Device> child = heap.alloc(new Device(root));
			print_heap(heap);
		}

		print_heap(heap);

		cout << heap.size() << endl;
		heap.gc();
		cout << heap.size() << endl;
	}
};


int main(int argc, char *args[]) {

	TestSuite ts;
	ts.addTestCase(AutoRef<TestCase>(new HeapTestCase));

	TestReport report(ts.testAll());
	report.validate();
    
    return 0;
}
