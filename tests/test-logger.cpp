#include <iostream>
#include <liboslayer/TestSuite.hpp>
#include <liboslayer/Text.hpp>
#include <liboslayer/Logger.hpp>
#include <liboslayer/File.hpp>

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
	virtual void write(const string & str) {
		cout << "[StaticWriter] :: " << str << endl;
		logs.push_back(str);
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
		LoggerFactory::instance().addObserver(&observer);
		LoggerFactory::instance().registerWriter("static", AutoRef<LogWriter>(new StaticWriter));
		logger = LoggerFactory::instance().getObservingLogger(File::basename(__FILE__));
	}
	virtual void tearDown() {
	}
	
	virtual void test() {
		LoggerFactory::instance().setProfile("*", "basic", "static");
		ASSERT(LoggerFactory::instance().writer("static").nil(), ==, false);
		ASSERT(LoggerFactory::instance().observerCount(), ==, 2);
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
