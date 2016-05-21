#include <liboslayer/TestSuite.hpp>
#include <liboslayer/ArgumentParser.hpp>

using namespace std;
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
			parser.parse(3, args);

			ASSERT(parser.getProgramName(), ==, "program");
			ASSERT(parser.getTexts().size(), ==, 2);
			ASSERT(parser.getTexts()[0], ==, "a");
			ASSERT(parser.getTexts()[1], ==, "b");
			
		}

		{
			const char * args[] = {
				"program",
				"a",
				"b"
			};

			ArgumentParser parser;
			parser.var("greeting") = "hello";
			
			parser.parse(3, args);

			ASSERT(parser.getProgramName(), ==, "program");
			ASSERT(parser.getTexts().size(), ==, 2);
			ASSERT(parser.getTexts()[0], ==, "a");
			ASSERT(parser.getTexts()[1], ==, "b");
			ASSERT(parser.var("greeting").value(), ==, "hello");
		}

		{
			const char * args[] = {
				"program",
				"a",
				"b",
				"--greeting",
				"allo"
			};

			ArgumentParser parser;
			parser.var("greeting") = "hello";
			
			parser.parse(5, args);

			ASSERT(parser.getProgramName(), ==, "program");
			ASSERT(parser.getTexts().size(), ==, 2);
			ASSERT(parser.getTexts()[0], ==, "a");
			ASSERT(parser.getTexts()[1], ==, "b");
			ASSERT(parser.var("greeting").value(), ==, "allo");
		}

		{
			const char * args[] = {
				"program",
				"--oneline"
			};

			ArgumentParser parser;
			parser.parse(2, args);

			ASSERT(parser.getProgramName(), ==, "program");
			ASSERT(parser.varWithAlias("oneline").value(), ==, "yes");
		}

		{
			const char * args[] = {
				"program",
				"--oneline="
			};

			ArgumentParser parser;
			parser.parse(2, args);

			ASSERT(parser.getProgramName(), ==, "program");
			ASSERT(parser.varWithAlias("oneline").value(), ==, "");
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
			parser.parse(11, args);

			ASSERT(parser.getProgramName(), ==, "program");
			ASSERT(parser.getTexts().size(), ==, 2);
			ASSERT(parser.getTexts()[0], ==, "text1");
			ASSERT(parser.getTexts()[1], ==, "text2");
			ASSERT(parser.varWithShortAlias("v").value(), ==, "yes");
			ASSERT(parser.varWithAlias("oneline").value(), ==, "yes");
			ASSERT(parser.varWithAlias("oneline").valueAsBoolean(), ==, true);
			ASSERT(parser.varWithAlias("st").value(), ==, "upnp:rootdevice");
			ASSERT(parser.varWithAlias("message").value(), ==, "hello world");
			ASSERT(parser.varWithAlias("x").value(), ==, "overwritten");
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

