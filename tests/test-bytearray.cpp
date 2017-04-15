#include <liboslayer/ByteArray.hpp>
#include <liboslayer/TestSuite.hpp>

using namespace std;
using namespace OS;
using namespace UTIL;

/**
 * byte array test case
 */
class ByteArrayTestCase : public TestCase {
public:
    ByteArrayTestCase() : TestCase("byte-array-test-case") {
	}
    virtual ~ByteArrayTestCase() {
	}
	virtual void test() {
		ByteArray arr(30);
		ByteArrayStream st(arr);

		st.write_i8(0x0f);
		st.write_i16(0x0f0e);
		st.write_i32(0x0f0e0d0c);
		st.write_i64(0x0f0e0d0c0b0a0908LL);
		st.write_ui8(0xf0);
		st.write_ui16(0xf0e0);
		st.write_ui32(0xf0e0d0c0);
		st.write_ui64(0xf0e0d0c0b0a9080ULL);

		try {
			st.write_i8(0x0f);
			throw "unexpected";
		} catch (OverflowException e) {
			// expected
		}

		st.position() = 0;

		ASSERT(st.read_i8(), ==, 0x0f);
		ASSERT(st.read_i16(), ==, 0x0f0e);
		ASSERT(st.read_i32(), ==, 0x0f0e0d0c);
		ASSERT(st.read_i64(), ==, 0x0f0e0d0c0b0a0908LL);
		ASSERT(st.read_ui8(), ==, 0xf0);
		ASSERT(st.read_ui16(), ==, 0xf0e0);
		ASSERT(st.read_ui32(), ==, 0xf0e0d0c0);
		ASSERT(st.read_ui64(), ==, 0xf0e0d0c0b0a9080ULL);

		try {
			st.read_i8();
			throw "unexpected";
		} catch (UnderflowException e) {
			// expected
		}
		
	}
};

/**
 * main
 */
int main(int argc, char *argv[]) {

	TestSuite ts;
	ts.addTestCase(AutoRef<TestCase>(new ByteArrayTestCase));;
	TestReport report(ts.testAll());
	report.validate();
    
    return 0;
}
