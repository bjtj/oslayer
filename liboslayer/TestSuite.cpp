#include "TestSuite.hpp"
#include <sstream>

namespace UTIL {

	using namespace std;
	using namespace OS;

	/**
	 *
	 */
	TestEnvironment::TestEnvironment() {
	}
	TestEnvironment::~TestEnvironment() {
	}

	/**
	 *
	 */
	TestResult::TestResult(const string & name) : name(name), result(false) {
	}
	TestResult::TestResult(const string & name, bool result) : name(name), result(result) {
	}
	TestResult::TestResult(const string & name, bool result, const string & message) : name(name), result(result), message(message) {
	}
	TestResult::~TestResult() {
	}

	string TestResult::getName() {
		return name;
	}

	bool TestResult::getResult() {
		return result;
	}
	void TestResult::setResult(bool result) {
		this->result = result;
	}
	string TestResult::getMessage() {
		return message;
	}
	void TestResult::setMessage(const string & message) {
		this->message = message;
	}


	/**
	 *
	 */
	TestCase::TestCase(const string & name) : name(name) {
	}
	TestCase::~TestCase() {
	}
	void TestCase::setUp(TestEnvironment & env) {
	}
	void TestCase::tearDown() {
	}
	string TestCase::getName() {
		return name;
	}
	
	/**
	 *
	 */
	TestSuite::TestSuite(TestEnvironment & env) : env(env) {
	}
	TestSuite::TestSuite() {
	}
	TestSuite::~TestSuite() {
	}
	void TestSuite::addTestCase(AutoRef<TestCase> testCase) {
		testCases.push_back(testCase);
	}
	void TestSuite::removeTestCase(AutoRef<TestCase> testCase) {
		for (vector<AutoRef<TestCase> >::iterator iter = testCases.begin(); iter != testCases.end(); iter++) {
			if ((*iter) == testCase) {
				testCases.erase(iter);
				return;
			}
		}
	}
	vector<TestResult> TestSuite::testAll() {
		vector<TestResult> results;
		for (vector<AutoRef<TestCase> >::iterator iter = testCases.begin(); iter != testCases.end(); iter++) {
			results.push_back(test(*iter));
		}
		return results;
	}

	TestResult TestSuite::test(AutoRef<TestCase> testCase) {
		TestResult result(testCase->getName());
		cout << "[TEST]: " << testCase->getName() << endl;
		testCase->setUp(env);
		try {
			testCase->test();
			result.setResult(true);
		} catch (AssertException & e) {
			cerr << " [Assert]: " << e.getMessage() << endl;
			result.setResult(false);
			result.setMessage(e.getMessage());
		} catch (Exception & e) {
			cerr << " [Exception]: " << e.getMessage() << endl;
			result.setResult(false);
			result.setMessage(e.getMessage());
		} catch (...) {
			cerr << " [unknown exception]" << endl;
			result.setResult(false);
			result.setMessage("unknown exception");
		}
		testCase->tearDown();
		cout << endl;
		return result;
	}

	
	TestReport::TestReport(vector<TestResult> results) : results(results) {
		_total = results.size();
		_passed = 0;
		_failed = 0;
		for (vector<TestResult>::iterator iter = results.begin(); iter != results.end(); iter++) {
			if (iter->getResult()) {
				_passed++;
			} else {
				_failed++;
			}
		}
	}
	TestReport::~TestReport() {
	}
	size_t TestReport::total() {
		return _total;
	}
	size_t TestReport::passed() {
		return _passed;
	}
	size_t TestReport::failed() {
		return _failed;
	}
	string TestReport::toString() {
		stringstream ss;
		ss << "Total: " << _total << endl;
		ss << "Passed: " << _passed << endl;
		ss << "Failed: " << _failed << endl;
		return ss.str();
	}
	void TestReport::validate() {
		ASSERT(failed(), ==, 0);
	}
}
