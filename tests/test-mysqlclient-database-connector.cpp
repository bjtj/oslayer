/**
 * @ref http://egloos.zum.com/hanaduri/v/26427
 * @ref https://dev.mysql.com/doc/refman/5.7/en/c-api-function-overview.html
 */
#include <liboslayer/TestSuite.hpp>
#include <liboslayer/os.hpp>
#include <liboslayer/DatabaseConnection.hpp>

#if (HAVE_MYSQLCLIENT == 1)
#include <mysql.h>
#endif

#define DB_USER "user"
#define DB_PASS "pass"
#define DB_NAME "my_db"

using namespace std;
using namespace OS;
using namespace UTIL;

#if (HAVE_MYSQLCLIENT == 1)

class MysqlClientResultSet : public ResultSet {
private:
	MYSQL_RES * _res;
	MYSQL_ROW _row;
public:
    MysqlClientResultSet(MYSQL_RES * res) : _res(res) {
	}
    virtual ~MysqlClientResultSet() {
		close();
	}
	virtual bool next() {
		return ((_row = mysql_fetch_row(_res)) != NULL);
	}
	virtual int fieldCount() {
		return mysql_num_fields(_res);
	}
	virtual string fieldName(size_t idx) {
		return string(mysql_fetch_field_direct(_res, idx)->name);
	}
	virtual string getString(size_t idx) {
		return _row[idx];
	}
	virtual void close() {
		if (_res) {
			mysql_free_result(_res);
			_res = NULL;
		}
	}
};


class MysqlDatabaseConnection : public DatabaseConnection {
private:
	MYSQL * _real_conn, _conn;
public:
    MysqlDatabaseConnection() : _real_conn(NULL) {
	}
    virtual ~MysqlDatabaseConnection() {
	}
	virtual string getDriverName() {
		return "mysqlclient";
	}
	virtual void connect(const string & url) {
		connect(url, DB_USER, DB_PASS);
	}
	virtual void connect(const string & url, const string & username, const string & password) {
		mysql_init(&_conn);
		_real_conn = mysql_real_connect(&_conn, url.c_str(), username.c_str(), password.c_str(),
										DB_NAME, 3306, (char*)NULL, 0);
		if (_real_conn == NULL) {
			throw "mysql_real_connect() failed";
		}
	}
	virtual AutoRef<ResultSet> query(const string & statement) {
		if (mysql_query(_real_conn, statement.c_str()) != 0) {
			throw "mysql_query() failed";
		}
		return AutoRef<ResultSet>(new MysqlClientResultSet(mysql_store_result(_real_conn)));
	}
	virtual size_t queryUpdate(const string & statement) {
		if (mysql_query(_real_conn, statement.c_str()) != 0) {
			throw "mysql_query() failed";
		}
		return mysql_affected_rows(_real_conn);
	}
	virtual size_t lastInsertId() {
		return mysql_insert_id(_real_conn);
	}
	virtual void disconnect() {
		mysql_close(_real_conn);
		_real_conn = NULL;
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

		// CREATE TABLE address (
		//   name varchar(25) default NULL,
		//   address text,
		//   tel varchar(25) default NULL
		// );

		MysqlDatabaseConnection conn;
		conn.connect("localhost", DB_USER, DB_PASS);

		AutoRef<ResultSet> result = conn.query("select * from address");
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

		int ret = conn.queryUpdate("insert into address values('name', 'addr', 'tel')");
		cout << "insert result: " << ret << endl;

		conn.disconnect();
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
