#include <iostream>
#include <liboslayer/TestSuite.hpp>
#include <liboslayer/Text.hpp>
#include <liboslayer/Logger.hpp>

using namespace std;
using namespace OS;
using namespace UTIL;

/**
 * logger factory observer
 */
class LoggerFactoryObserver : public Observer {
private:
	bool _updated;
public:
    LoggerFactoryObserver() : _updated(false) {}
    virtual ~LoggerFactoryObserver() {}

	virtual void update(Observable * target) {
		_updated = true;
	}

	bool & updated() {
		return _updated;
	}
};

/**
 * static printer
 */
class StaticPrinter : public LogPrinter {
public:
	static vector<string> logs;
public:
	StaticPrinter() {}
	virtual ~StaticPrinter() {}
	virtual void print(const LogSession & session, const string & msg) {
		cout << "[StaticPrinter] :: " << msg << endl;
		logs.push_back(msg);
	}
};

vector<string> StaticPrinter::logs;

/**
 * logger test case
 */
class LoggerTestCase : public TestCase {
private:
	AutoRef<Logger> logger;
	LoggerFactoryObserver observer;
public:
	LoggerTestCase() : TestCase("LoggerTestCase") {}
	virtual ~LoggerTestCase() {}
	virtual void setUp(TestEnvironment & env) {
		LoggerFactory::getInstance().addObserver(&observer);
		LoggerFactory::getInstance().setPrinter("static", AutoRef<LogPrinter>(new StaticPrinter));
		logger = LoggerFactory::getInstance().getObservingLogger(__FILE__);
	}
	virtual void tearDown() {
	}
	
	virtual void test() {
		LoggerFactory::getInstance().setLoggerDescriptorSimple("*", "basic", "static");
		ASSERT(LoggerFactory::getInstance().getPrinter("static").nil(), ==, false);
		ASSERT(LoggerFactory::getInstance().observerCount(), ==, 2);
		ASSERT(observer.updated(), ==, true);
		logger->logd("debug1");
		logger->logd("debug2");
		ASSERT(StaticPrinter::logs.size(), ==, 2);
		ASSERT(Text::endsWith(StaticPrinter::logs[0], "debug1"), ==, true);
		ASSERT(Text::endsWith(StaticPrinter::logs[1], "debug2"), ==, true);
	}
};

/**
 * main
 */
int main(int argc, char *args[]) {

	TestSuite ts;
	ts.addTestCase(AutoRef<TestCase>(new LoggerTestCase));

	TestReport report(ts.testAll());
	ASSERT(report.failed(), ==, 0);
    
    return 0;
}
