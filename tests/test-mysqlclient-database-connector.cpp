/**
 * @ref http://egloos.zum.com/hanaduri/v/26427
 * @ref https://dev.mysql.com/doc/refman/5.7/en/c-api-function-overview.html
 */
#include <liboslayer/TestSuite.hpp>
#include <liboslayer/os.hpp>
#include <liboslayer/Library.hpp>
#include <liboslayer/DatabaseConnection.hpp>
#include <liboslayer/DatabaseDriver.hpp>

using namespace std;
using namespace OS;
using namespace UTIL;

/**
 * 
 */
class MysqlClientTestCase : public TestCase {
private:
	string username;
	string password;
	string dbname;
public:
    MysqlClientTestCase(const string & username, const string & password, const string & dbname)
		: TestCase("mysql-client-test-case"), username(username), password(password), dbname(dbname) {
	}
    virtual ~MysqlClientTestCase() {
	}
	virtual void test() {
		// connect
		// create table
		// insert
		// select
		// result set
		// update
		// delete
		// drop table
		// disconnect

		// CREATE TABLE address (
		//   name varchar(25) default NULL,
		//   address text,
		//   tel varchar(25) default NULL
		// );

		DatabaseDriver & driver = DatabaseDriver::instance();
		driver.load("mysql", AutoRef<Library>(new Library("../ext/.libs/", "mysqlconnector")));
		AutoRef<DatabaseConnection> conn = driver.getConnection("mysql");
		conn->connect("localhost", 3306, username, password, dbname);

		AutoRef<ResultSet> result = conn->query("select * from address");
		cout << "[";
		for (int i = 0; i < result->fieldCount(); i++) {
			if (i > 0) {
				cout << " | ";
			}
			cout << result->fieldName(i);
		}
		cout << "]" << endl;
		while (result->next()) {
			cout << "name: " << result->getString(0) <<
				", address: " << result->getString(1) <<
				", tel: " << result->getString(2) << endl;
		}

		int ret = conn->queryUpdate("insert into address values('name', 'addr', 'tel')");
		cout << "insert result: " << ret << endl;

		conn->disconnect();
	}
};

/**
 * 
 */
int main(int argc, char *argv[])
{
	bool enable = false;
	string username = "user";
	string password = "pass";
	string dbname = "mydb";
	
	TestSuite ts;
	if (enable) {
		ts.addTestCase(AutoRef<TestCase>(new MysqlClientTestCase(username, password, dbname)));
	}

	TestReport report(ts.testAll());
	report.validate();
    
    return 0;
}
