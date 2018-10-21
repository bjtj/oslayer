#include <liboslayer/TestSuite.hpp>
#include <liboslayer/Arguments.hpp>

using namespace std;
using namespace osl;



class ArgumentsTestCase : public TestCase {
public:
    ArgumentsTestCase() : TestCase("ArgumentsTestCase") {
    }
    virtual ~ArgumentsTestCase() {
    }

    virtual void test() {

	{
	    const char * args[] = {
		"program",
		"a",
		"b"
	    };

	    Arguments arguments = ArgumentParser::parse(3, (char**)args);

	    ASSERT(arguments.program(), ==, "program");
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
			
	    Arguments arguments = ArgumentParser::parse(3, (char**)args);
	    arguments.set("greeting", "hello");
			
	    ASSERT(arguments.program(), ==, "program");
	    ASSERT(arguments.texts().size(), ==, 2);
	    ASSERT(arguments.texts()[0], ==, "a");
	    ASSERT(arguments.texts()[1], ==, "b");
	    ASSERT(arguments.named("greeting").str(), ==, "hello");
	}

	{
	    const char * args[] = {
		"program",
		"a",
		"b",
		"--greeting", "allo"
	    };

	    Arguments arguments;
	    arguments.set("greeting", "hello");

	    arguments = ArgumentParser::parse(5, (char**)args);

	    ASSERT(arguments.program(), ==, "program");
	    ASSERT(arguments.texts().size(), ==, 2);
	    ASSERT(arguments.texts()[0], ==, "a");
	    ASSERT(arguments.texts()[1], ==, "b");
	    ASSERT(arguments.named("greeting").str(), ==, "allo");
	}

	{
	    const char * args[] = {
		"program",
		"--oneline"
	    };

	    Arguments arguments = ArgumentParser::parse(2, (char**)args);

	    ASSERT(arguments.program(), ==, "program");
	    ASSERT(arguments.is_set("oneline"), ==, true);
	}

	{
	    const char * args[] = {
		"program",
		"--oneline="
	    };

	    Arguments arguments = ArgumentParser::parse(2, (char**)args);

	    ASSERT(arguments.program(), ==, "program");
	    ASSERT(arguments.named("oneline").str(), ==, "");
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
		"-x", "overwritten"
	    };

	    Arguments arguments = ArgumentParser::parse(11, (char**)args);

	    ASSERT(arguments.program(), ==, "program");
	    ASSERT(arguments.texts().size(), ==, 2);
	    ASSERT(arguments.texts()[0], ==, "text1");
	    ASSERT(arguments.texts()[1], ==, "text2");
	    ASSERT(arguments.is_set("v"), ==, true);
	    ASSERT(arguments.named("oneline").str(), ==, "");
	    ASSERT(arguments.is_set("oneline"), ==, true);
	    ASSERT(arguments.named("st").str(), ==, "upnp:rootdevice");
	    ASSERT(arguments.named("message").str(), ==, "hello world");
	    ASSERT(arguments.named("x").str(), ==, "overwritten");
	    ASSERT(arguments.is_set("notspecified"), ==, false);
	}

	{
	    const char * args[] = {
		"program",
		"a",
		"b",
		"--num", "10",
		"c",
		"--long-num", "1234"
	    };

	    Arguments arguments = ArgumentParser::parse(8, (char**)args);

	    ASSERT(arguments.program(), ==, "program");
	    ASSERT(arguments.texts().size(), ==, 3);
	    ASSERT(arguments.texts()[0], ==, "a");
	    ASSERT(arguments.texts()[1], ==, "b");
	    ASSERT(arguments.texts()[2], ==, "c");
	    ASSERT(arguments.named("num").to_int(), ==, 10);
	    ASSERT(arguments.named("long-num").to_long(), ==, 1234);
	}
    }
};


int main(int argc, char *args[]) {

    TestSuite ts;
    ts.addTestCase(AutoRef<TestCase>(new ArgumentsTestCase));

    TestReport report(ts.testAll());
    ASSERT(report.failed(), ==, 0);
    
    return 0;
}

