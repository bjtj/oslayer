#include <liboslayer/TestSuite.hpp>
#include <liboslayer/Text.hpp>
#include <liboslayer/Logger.hpp>

using namespace std;
using namespace UTIL;

class StaticPrinter : public LogPrinter {
public:
	static vector<string> logs;
public:
	StaticPrinter() {}
	virtual ~StaticPrinter() {}
	virtual void print(const LogSession & session, const string & msg) {
		logs.push_back(msg);
	}
};

vector<string> StaticPrinter::logs;


class LoggerTestCase : public TestCase {
private:
	AutoRef<Logger> logger;
public:
	LoggerTestCase() : TestCase("LoggerTestCase") {}
	virtual ~LoggerTestCase() {}
	virtual void setUp(TestEnvironment & env) {
		LoggerFactory::getInstance().setPrinter("static", AutoRef<LogPrinter>(new StaticPrinter));
		logger = LoggerFactory::getInstance().getLogger(__FILE__);
	}
	virtual void tearDown() {
	}
	virtual void test() {
		LoggerFactory::getInstance().setLoggerDescriptorSimple("*", "basic", "static");
		logger->logd("debug1");
		logger->logd("debug2");

		ASSERT(Text::endsWith(StaticPrinter::logs[0], "debug1"), ==, true);
		ASSERT(Text::endsWith(StaticPrinter::logs[1], "debug2"), ==, true);

	}
};


int main(int argc, char *args[]) {

	TestSuite ts;
	ts.addTestCase(AutoRef<TestCase>(new LoggerTestCase));

	TestReport report(ts.testAll());
	ASSERT(report.failed(), ==, 0);
    
    return 0;
}
