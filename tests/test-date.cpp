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
			cout << osl_get_time().sec << endl;
		}

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
			cout << "unix time[1970]: " << osl_system_time_to_unix_time(Date::now().getTime()).sec << endl;
			cout << "network time[1900]: " << osl_system_time_to_network_time(Date::now().getTime()).sec << endl;
			cout << "ut -> system time: " << osl_unix_time_to_system_time(osl_system_time_to_unix_time(
													 Date::now().getTime())).sec << endl;
			cout << "nt -> system time: " << osl_network_time_to_system_time(osl_system_time_to_network_time(
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
