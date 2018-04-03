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
    LoggerFactoryObserver() : _updated(false) {
	}
    virtual ~LoggerFactoryObserver() {
	}
	virtual void onUpdate(Observable * target) {
		_updated = true;
	}
	bool & updated() {
		return _updated;
	}
};

/**
 * static writer
 */
class StaticWriter : public LogWriter {
public:
	static vector<string> logs;
public:
	StaticWriter() {}
	virtual ~StaticWriter() {}
	virtual void write(const LogSession & session, const string & msg) {
		cout << "[StaticWriter] :: " << msg << endl;
		logs.push_back(msg);
	}
};

vector<string> StaticWriter::logs;

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
		LoggerFactory::inst().addObserver(&observer);
		LoggerFactory::inst().registerWriter("static", AutoRef<LogWriter>(new StaticWriter));
		logger = LoggerFactory::inst().getObservingLogger(__FILE__);
	}
	virtual void tearDown() {
	}
	
	virtual void test() {
		LoggerFactory::inst().setProfile("*", "basic", "static");
		ASSERT(LoggerFactory::inst().writer("static").nil(), ==, false);
		ASSERT(LoggerFactory::inst().observerCount(), ==, 2);
		ASSERT(observer.updated(), ==, true);
		logger->debug("debug1");
		logger->debug("debug2");
		ASSERT(StaticWriter::logs.size(), ==, 2);
		ASSERT(Text::endsWith(StaticWriter::logs[0], "debug1"), ==, true);
		ASSERT(Text::endsWith(StaticWriter::logs[1], "debug2"), ==, true);
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
