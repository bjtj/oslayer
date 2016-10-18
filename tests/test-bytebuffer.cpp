#include <liboslayer/TestSuite.hpp>
#include <liboslayer/ByteBuffer.hpp>

using namespace std;
using namespace OS;
using namespace UTIL;

class ByteBufferTestCase : public TestCase {
private:
public:
    ByteBufferTestCase() : TestCase("ByteBuffer test") {}
    virtual ~ByteBufferTestCase() {}
	virtual void test() {
		ByteBuffer buffer(10);
		buffer.put(1);
		buffer.put(2);
		buffer.put(3);
		buffer.put(4);
		ASSERT(buffer.remaining(), ==, 6);
		buffer.flip();
		ASSERT(buffer.remaining(), ==, 4);
		ASSERT(buffer.get(), ==, 1);
		ASSERT(buffer.get(), ==, 2);
		ASSERT(buffer.get(), ==, 3);
		ASSERT(buffer.get(), ==, 4);
		string err;
		try {
			buffer.get();
		} catch (Exception e) {
			err = e.getMessage();
		}
		ASSERT(err.empty(), ==, false);
	}
};


int main(int argc, char *args[]) {

	TestSuite ts;
	ts.addTestCase(AutoRef<TestCase>(new ByteBufferTestCase));

	TestReport report(ts.testAll());
	report.validate();
    
    return 0;
}
