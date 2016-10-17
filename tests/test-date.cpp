#include <iostream>
#include <liboslayer/TestSuite.hpp>

using namespace std;
using namespace OS;
using namespace UTIL;


class DateTestCase : public TestCase{
private:
public:
    DateTestCase() : TestCase("") {
	}
    virtual ~DateTestCase() {
	}
	virtual void test() {
		{
			Date date = Date::now();
			cout << Date::format(date) << endl;
		}

		{
			Date date(Date::now().getTime());
			cout << Date::format(date) << endl;
		}

		{
			cout << Date::now().getTime().sec << endl;
			cout << osl_system_time_to_unix_time(Date::now().getTime()).sec << endl;
			cout << osl_system_time_to_network_time(Date::now().getTime()).sec << endl;
			cout << osl_unix_time_to_system_time(osl_system_time_to_unix_time(
													 Date::now().getTime())).sec << endl;
			cout << osl_network_time_to_system_time(osl_system_time_to_network_time(
														Date::now().getTime())).sec << endl;
		}
	}
};

/**
 * 
 */
int main(int argc, char *args[]) {

	TestSuite ts;
	ts.addTestCase(AutoRef<TestCase>(new DateTestCase));

	TestReport report(ts.testAll());
	report.validate();
    
    return 0;
}
