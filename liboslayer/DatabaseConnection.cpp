#include "DatabaseConnection.hpp"

namespace UTIL {

	using namespace std;
	
	/**
	 * @brief 
	 */

	ResultSet::Row::Row() {
	}
	ResultSet::Row::Row(const vector<string> & row) : _row(row) {
	}
	ResultSet::Row::~Row() {
	}
	size_t ResultSet::Row::size() {
		return _row.size();
	}
	string & ResultSet::Row::operator[] (size_t idx) {
		return _row[idx];
	}

	/**
	 * @brief 
	 */
	
	ResultSet::ResultSet() {}
	ResultSet::~ResultSet() {}

	vector<string> & ResultSet::cols() {
		return _cols;
	}

	void ResultSet::append(const vector<string> & strs) {
		_rows.push_back(Row(strs));
	}

	size_t ResultSet::size() {
		return _rows.size();
	}

	ResultSet::Row & ResultSet::operator[] (size_t idx) {
		return _rows[idx];
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

	/**
	 * @brief 
	 */
	
	DatabaseConnection::DatabaseConnection() {}
	DatabaseConnection::~DatabaseConnection() {}
	
}
