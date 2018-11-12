#include <liboslayer/TestSuite.hpp>
#include <liboslayer/Environment.hpp>

using namespace std;
using namespace osl;

class EnvironmentTestCase : public TestCase
{
public:
    EnvironmentTestCase() : TestCase("environment-testcase") {
    }
    virtual ~EnvironmentTestCase() {
    }
    virtual void test() {
	cout << *Environment::get("PATH") << endl;
	cout << *Environment::get("USER") << endl;

	Optional<string> msg = Environment::get("MY_MSG");
	ASSERT(msg.nil(), ==, true);
	Environment::set("MY_MSG", "hello");
	msg = Environment::get("MY_MSG");
	ASSERT(msg.nil(), ==, false);
	ASSERT(*msg, ==, "hello");
    }
};


int main(int argc, char *argv[])
{
    TestSuite ts;
    ts.addTestCase(AutoRef<TestCase>(new EnvironmentTestCase));
    TestReport report(ts.testAll());
    report.validate();
    return 0;
}
