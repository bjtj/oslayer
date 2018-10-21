#include <liboslayer/TestSuite.hpp>
#include <liboslayer/Text.hpp>

using namespace std;
using namespace osl;


class Caculator {
public:
    Caculator() {}
    virtual ~Caculator() {}
    int evaluate(const string & exp) {
	int sum = 0;
	vector<string> tokens = Text::split(exp, "+");
	for (vector<string>::iterator iter = tokens.begin(); iter != tokens.end(); iter++) {
	    sum += Text::toInt(*iter);
	}
	return sum;
    }
};

class BuggyCaculator {
public:
    BuggyCaculator() {}
    virtual ~BuggyCaculator() {}
    int evaluate(const string & exp) {
	int sum = 0;
	vector<string> tokens = Text::split(exp, "+");
	for (vector<string>::iterator iter = tokens.begin(); iter != tokens.end(); iter++) {
	    sum -= Text::toInt(*iter);
	}
	return sum;
    }
};


class CaculatorTest : public TestCase {
public:
    CaculatorTest() : TestCase("CalculatorTest") {}
    virtual ~CaculatorTest() {}
    virtual void test() {
	Caculator calculator;
	int sum = calculator.evaluate("1+2+3");
	ASSERT(sum, ==, 6);
    }
};

class BuggyCaculatorTest : public TestCase {
public:
    BuggyCaculatorTest() : TestCase("BuggyCaculatorTest") {}
    virtual ~BuggyCaculatorTest() {}
    virtual void test() {
	BuggyCaculator calculator;
	int sum = calculator.evaluate("1+2+3");
	ASSERT(sum, ==, 6);
    }
};


int main(int argc, char *args[]) {

    TestSuite suite;
    suite.addTestCase(AutoRef<TestCase>(new CaculatorTest));
    suite.addTestCase(AutoRef<TestCase>(new BuggyCaculatorTest));

    vector<TestResult> results = suite.testAll();

    TestReport report(results);
    cout << report.toString() << endl;
	
    return 0;
}
