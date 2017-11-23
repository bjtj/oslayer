#include <liboslayer/TestSuite.hpp>
#include <liboslayer/ArgumentParser.hpp>

using namespace std;
using namespace OS;
using namespace UTIL;

class ArgumentParserTestCase : public TestCase {
public:
	ArgumentParserTestCase() : TestCase("ArgumentParserTestCase") {
	}
	virtual ~ArgumentParserTestCase() {
	}

	virtual void test() {

		{
			const char * args[] = {
				"program",
				"a",
				"b"
			};

			ArgumentParser parser;
			Arguments arguments = parser.parse(3, args);

			ASSERT(arguments.programName(), ==, "program");
			ASSERT(arguments.texts().size(), ==, 2);
			ASSERT(arguments.texts()[0], ==, "a");
			ASSERT(arguments.texts()[1], ==, "b");
			
		}

		{
			const char * args[] = {
				"program",
				"a",
				"b"
			};

			ArgumentParser parser;
			Arguments arguments = parser.parse(3, args);
			arguments.obtainVar("greeting") = "hello";
			
			ASSERT(arguments.programName(), ==, "program");
			ASSERT(arguments.texts().size(), ==, 2);
			ASSERT(arguments.texts()[0], ==, "a");
			ASSERT(arguments.texts()[1], ==, "b");
			ASSERT(arguments.obtainVar("greeting").value(), ==, "hello");
		}

		{
			const char * args[] = {
				"program",
				"a",
				"b",
				"--greeting",
				"allo"
			};

			Arguments arguments;
			arguments.obtainVar("greeting") = "hello";

			ArgumentParser parser;
			arguments = parser.parse(5, args);

			ASSERT(arguments.programName(), ==, "program");
			ASSERT(arguments.texts().size(), ==, 2);
			ASSERT(arguments.texts()[0], ==, "a");
			ASSERT(arguments.texts()[1], ==, "b");
			ASSERT(arguments.obtainVar("greeting").value(), ==, "allo");
		}

		{
			const char * args[] = {
				"program",
				"--oneline"
			};

			ArgumentParser parser;
			Arguments arguments = parser.parse(2, args);

			ASSERT(arguments.programName(), ==, "program");
			ASSERT(arguments.varWithAlias("oneline").value(), ==, "yes");
		}

		{
			const char * args[] = {
				"program",
				"--oneline="
			};

			ArgumentParser parser;
			Arguments arguments = parser.parse(2, args);

			ASSERT(arguments.programName(), ==, "program");
			ASSERT(arguments.varWithAlias("oneline").value(), ==, "");
		}

		{
			const char * args[] = {
				"program",
				"-v",
				"--oneline",
				"--st=upnp:rootdevice",
				"text1",
				"text2",
				"--message",
				"hello world",
				"-x",
				"-x",
				"overwritten"
			};

			ArgumentParser parser;
			Arguments arguments = parser.parse(11, args);

			ASSERT(arguments.programName(), ==, "program");
			ASSERT(arguments.texts().size(), ==, 2);
			ASSERT(arguments.texts()[0], ==, "text1");
			ASSERT(arguments.texts()[1], ==, "text2");
			ASSERT(arguments.varWithShortAlias("v").value(), ==, "yes");
			ASSERT(arguments.varWithAlias("oneline").value(), ==, "yes");
			ASSERT(arguments.varWithAlias("oneline").valueAsBoolean(), ==, true);
			ASSERT(arguments.varWithAlias("st").value(), ==, "upnp:rootdevice");
			ASSERT(arguments.varWithAlias("message").value(), ==, "hello world");
			ASSERT(arguments.varWithAlias("x").value(), ==, "overwritten");
			ASSERT(arguments.varAsBoolean("notspecified", false), ==, false);
			ASSERT(arguments.varAsBoolean("notspecified", true), ==, true);
		}
	}
};


int main(int argc, char *args[]) {

	TestSuite ts;
	ts.addTestCase(AutoRef<TestCase>(new ArgumentParserTestCase));

	TestReport report(ts.testAll());
	ASSERT(report.failed(), ==, 0);
    
    return 0;
}

