#include <liboslayer/os.hpp>
#include <liboslayer/TestSuite.hpp>
#include <liboslayer/CountDownLatch.hpp>
#include <liboslayer/TaskThreadPool.hpp>

using namespace std;
using namespace OS;
using namespace UTIL;

static size_t s_count = 0;

class WorkerTask : public Task {
private:
	CountDownLatch & startSignal;
	CountDownLatch & doneSignal;
public:
	WorkerTask(CountDownLatch & startSignal, CountDownLatch & doneSignal) : startSignal(startSignal), doneSignal(doneSignal) {}
	virtual ~WorkerTask() {}
	virtual void doTask() {
		startSignal.await();
		cout << "+ do task" << endl;
		s_count++;
		doneSignal.countDown();
	}
};

class BasicCountDownLatchTestCase : public TestCase {
public:
	BasicCountDownLatchTestCase() : TestCase("BasicCountDownLatchTestCase") {}
	virtual ~BasicCountDownLatchTestCase() {}
	virtual void test() {
		CountDownLatch signal(10);
		ASSERT(signal.getCount(), ==, 10);
		signal.countDown();
		ASSERT(signal.getCount(), ==, 9);
	}
};

class CountDownLatchTestCase : public TestCase {
public:
	CountDownLatchTestCase() : TestCase("CountDownLatchTestCase") {}
	virtual ~CountDownLatchTestCase() {}
	virtual void test() {
		size_t count_worker = 10;
		CountDownLatch startSignal(1);
		CountDownLatch doneSignal(count_worker);
		TaskThreadPool pool(count_worker);

		pool.start();

		for (size_t i = 0; i < count_worker; i++) {
			pool.setTask(AutoRef<Task>(new WorkerTask(startSignal, doneSignal)));
		}

		idle(500);
		ASSERT(s_count, ==, 0);
		startSignal.countDown();
		doneSignal.await();
		ASSERT(s_count, ==, count_worker);

		pool.stop();
	}
};

class CountDownLatchTimeoutTestCase : public TestCase {
public:
	CountDownLatchTimeoutTestCase() : TestCase("CountDownLatchTimeoutTestCase") {}
	virtual ~CountDownLatchTimeoutTestCase() {}
	virtual void test() {
		string err;
		CountDownLatch signal(1);
		unsigned long tick = tick_milli();
		try {
			signal.await(1000);
		} catch (Exception & e) {
			cout << e.getMessage() << endl;
			err = e.getMessage();
		}
		ASSERT(tick_milli() - tick, >=, 1000);
		ASSERT(err.empty(), ==, false);
	}
};

int main(int argc, char *args[]) {

	TestSuite ts;
	ts.addTestCase(AutoRef<TestCase>(new BasicCountDownLatchTestCase));
	ts.addTestCase(AutoRef<TestCase>(new CountDownLatchTestCase));
	ts.addTestCase(AutoRef<TestCase>(new CountDownLatchTimeoutTestCase));
	ts.testAll();
    
    return 0;
}
