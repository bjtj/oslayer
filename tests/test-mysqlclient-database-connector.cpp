/**
 * @ref http://egloos.zum.com/hanaduri/v/26427
 */
#include <liboslayer/TestSuite.hpp>
#include <liboslayer/os.hpp>
#include <liboslayer/DatabaseConnection.hpp>

#if (HAVE_MYSQLCLIENT == 1)
#include <mysql.h>
#endif

using namespace std;
using namespace OS;
using namespace UTIL;

#if (HAVE_MYSQLCLIENT == 1)
class MysqlDatabaseConnection : public DatabaseConnection {
private:
	MYSQL * _conn;
public:
    MysqlDatabaseConnection() : _conn(NULL) {
	}
    virtual ~MysqlDatabaseConnection() {
		
	}
	virtual string getDriverName() {
		return "mysqlclient";
	}
	virtual void connect(const string & url) {
		MYSQL conn;
		mysql_init(&conn);
		_conn = mysql_real_connect(&conn, "localhost", "user", "pass", "mydb", 3306, (char*)NULL, 0); 
	}
	virtual void connect(const string & url, const string & username, const string & password) {
	}
	virtual void disconnect() {
		mysql_close(_conn);
	}
};

#endif

/**
 * 
 */
class MysqlClientTestCase : public TestCase {
public:
    MysqlClientTestCase() : TestCase("mysql-client-test-case") {
	}
    virtual ~MysqlClientTestCase() {
	}
	virtual void test() {
#if (HAVE_MYSQLCLIENT == 1)
		// connect
		// create table
		// insert
		// select
		// result set
		// update
		// delete
		// drop table
		// disconnect
#endif
	}
};

/**
 * 
 */
int main(int argc, char *argv[])
{
	TestSuite ts;
	ts.addTestCase(AutoRef<TestCase>(new MysqlClientTestCase));

	TestReport report(ts.testAll());
	report.validate();
    
    return 0;
}
