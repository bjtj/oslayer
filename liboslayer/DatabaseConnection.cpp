#include "DatabaseConnection.hpp"
#include "os.hpp"

namespace UTIL {

	using namespace std;
	using namespace OS;
	
	/**
	 * @brief 
	 */

	ResultSet::ResultSet() {
	}
	ResultSet::~ResultSet() {
	}
	size_t ResultSet::size() {
		throw NotImplementedException("size()");
	}
	bool ResultSet::next() {
		throw NotImplementedException("next()");
	}
	string ResultSet::getString(size_t idx) {
		throw NotImplementedException("getString()");
	}
	string ResultSet::getString(const string & column) {
		throw NotImplementedException("getString()");
	}


	/**
	 * @brief 
	 */

	PreparedStatement::PreparedStatement(const string & sql) : _sql(sql) {
	}
	PreparedStatement::~PreparedStatement() {
	}
	string & PreparedStatement::sql() {
		return _sql;
	}
	bool PreparedStatement::execute() {
		throw NotImplementedException("execute()");
	}
	AutoRef<ResultSet> PreparedStatement::executeQuery() {
		throw NotImplementedException("executeQuery()");
	}
	size_t PreparedStatement::executeUpdate() {
		throw NotImplementedException("executeUpdate()");
	}
	void PreparedStatement::setString(size_t idx, const string & str) {
		throw NotImplementedException("setString()");
	}
	void PreparedStatement::setInteger(size_t idx, const int & str) {
		throw NotImplementedException("setInteger()");
	}
	void PreparedStatement::setLong(size_t idx, const long & str) {
		throw NotImplementedException("setLong()");
	}

	/**
	 * @brief 
	 */
	
	DatabaseConnection::DatabaseConnection() {
	}
	DatabaseConnection::~DatabaseConnection() {
	}
	string DatabaseConnection::getDriverName() {
		throw NotImplementedException("getDriverName()");
	}
	void DatabaseConnection::connect(const string & url) {
		throw NotImplementedException("connect()");
	}
	void DatabaseConnection::connect(const string & url, const string & username, const string & password) {
		throw NotImplementedException("connect()");
	}
	void DatabaseConnection::disconnect() {
		throw NotImplementedException("disconnect()");
	}
	void DatabaseConnection::setSchema(const string & schema) {
		throw NotImplementedException("setSchema()");
	}
	AutoRef<PreparedStatement> DatabaseConnection::prepareStatement(const string & sql) {
		throw NotImplementedException("prepareStatement()");
	}
	bool DatabaseConnection::getAutoCommit() {
		throw NotImplementedException("getAutoCommit()");
	}
	void DatabaseConnection::setAutoCommit(bool autoCommit) {
		throw NotImplementedException("setAutoCommit()");
	}
	void DatabaseConnection::beginTransaction() {
		throw NotImplementedException("beginTransaction()");
	}
	void DatabaseConnection::commit() {
		throw NotImplementedException("commit()");
	}
	void DatabaseConnection::rollback() {
		throw NotImplementedException("rollback()");
	}
}
