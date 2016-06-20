#ifndef __TEST_SUITE_HPP__
#define __TEST_SUITE_HPP__

#include <string>
#include <iostream>
#include <sstream>
#include "os.hpp"
#include "AutoRef.hpp"
#include "Properties.hpp"

#define ASSERT(A,CMP,B)													\
	do {																\
		std::cout << "* " << #A << "(" << #CMP << " " << B << ")";		\
		if (!(A CMP B)) {												\
			std::cout << " - FAIL" << std::endl;						\
			std::stringstream ss;										\
			ss << #A << " should be [" << #CMP << " " <<  B << "] but [" << A << "]"; \
			std::cerr << " [!] " << ss.str() << std::endl;						\
			throw UTIL::AssertException(ss.str());						\
		} else {														\
			std::cout << " - PASS" << std::endl;						\
		}																\
	} while(0);

namespace UTIL {

	DECL_NAMED_ONLY_EXCEPTION(AssertException);

	/**
	 *
	 */
	class TestEnvironment : public Properties {
	public:
		TestEnvironment();
		virtual ~TestEnvironment();
	};

	/**
	 *
	 */
	class TestResult {
	private:
		std::string name;
		bool result;
		std::string message;
	public:
		TestResult(const std::string & name);
		TestResult(const std::string & name, bool result);
		TestResult(const std::string & name, bool result, const std::string & message);
		virtual ~TestResult();
		std::string getName();
		bool getResult();
		void setResult(bool result);
		std::string getMessage();
		void setMessage(const std::string & message);
	};


	/**
	 *
	 */
	class TestCase {
	private:
		std::string name;
	public:
		TestCase(const std::string & name);
		virtual ~TestCase();
		virtual void setUp(TestEnvironment & env);
		virtual void tearDown();
		virtual void test() = 0;
		std::string getName();
	};

	/**
	 *
	 */
	class TestSuite {
	private:
		TestEnvironment env;
		std::vector<AutoRef<TestCase> > testCases;
	public:
		TestSuite(TestEnvironment & env);
		TestSuite();
		virtual ~TestSuite();
		void addTestCase(AutoRef<TestCase> testCase);
		void removeTestCase(AutoRef<TestCase> testCase);
		std::vector<TestResult> testAll();
		TestResult test(AutoRef<TestCase> testCase);
	};

	class TestReport {
	private:
		std::vector<TestResult> results;
		size_t _total;
		size_t _passed;
		size_t _failed;
	public:
		TestReport(std::vector<TestResult> results);
		virtual ~TestReport();
		size_t total();
		size_t passed();
		size_t failed();
		std::string toString();
		void validate();
	};

}

#endif
