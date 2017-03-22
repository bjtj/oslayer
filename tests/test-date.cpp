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

	static void print_date(const Date & date) {
		cout << Date::format(date) << "/" << Date::formatRfc1123(date) << endl;
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

		{
			Date date = Date::now();

			cout << "[now]" << endl;
			print_date(date);
			cout << "[gmt]" << endl;
			print_date(date.toGmt());
			cout << "[gmt now-1]" << endl;
			date.setGmtOffset(date.getGmtOffset() - 60);
			print_date(date.toGmt());
			cout << "[gmt 0]" << endl;
			date.setGmtOffset(0);
			print_date(date.toGmt());

			cout << "[OS date test]" << endl;
			cout << " * offset: " << date.getGmtOffset() << " (" << ((double)date.getGmtOffset() / 60.0) << ")" << endl;
			cout << " * time: " << date.getTime().sec << endl;
		}

		{
			Date d;
			d.setYear(2017);
			d.setMonth(3 - 1);
			d.setDay(22);
			d.setHour(23);
			d.setMinute(33);
			d.setSecond(52);
			d.setGmtOffset(9 * 60);

			ASSERT(Date::formatRfc1123(d), ==, "Wed, 22 Mar 2017 14:33:52 GMT");
			ASSERT(Date::formatRfc1036(d), ==, "Wednesday, 22-Mar-17 14:33:52 GMT");
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
