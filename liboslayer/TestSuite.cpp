#include "TestSuite.hpp"

namespace UTIL {

	using namespace std;

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
		cout << "TEST : " << testCase->getName() << endl;
		testCase->setUp(env);
		try {
			testCase->test();
			result.setResult(true);
		} catch (AssertException e) {
			result.setResult(false);
			result.setMessage(e.getMessage());
		}
		testCase->tearDown();
		return result;
	}
}
