#include <liboslayer/TestSuite.hpp>
#include <liboslayer/Base64.hpp>

using namespace std;
using namespace osl;


class Base64TestCase : public TestCase {
private:
public:
	Base64TestCase() : TestCase("Base64") {
	}
	virtual ~Base64TestCase() {}

	virtual void test() {
		ASSERT(Base64::encode("Aladdin:OpenSesame"), ==, "QWxhZGRpbjpPcGVuU2VzYW1l");
		ASSERT(Base64::decode("QWxhZGRpbjpPcGVuU2VzYW1l"), ==, "Aladdin:OpenSesame");
	}
};

int main(int argc, char *args[]) {

	TestSuite ts;
	ts.addTestCase(AutoRef<TestCase>(new Base64TestCase));

	TestReport report(ts.testAll());
	report.validate();
    
    return 0;
}
