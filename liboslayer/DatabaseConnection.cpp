#include "DatabaseConnection.hpp"
#include "os.hpp"

namespace UTIL {

	using namespace std;
	using namespace OS;
	
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
	
	ResultSet::ResultSet() : _row(-1) {
	}
	ResultSet::~ResultSet() {
	}

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

	bool ResultSet::next() {
		return ((size_t)++_row < _rows.size());
	}

	string ResultSet::getString(size_t idx) {
		return _rows[_row][idx];
	}
	
	string ResultSet::getString(const std::string & column) {
		for (size_t i = 0; i < _cols.size(); i++) {
			if (_cols[i] == column) {
				return _rows[_row][i];
			}
		}
		throw Exception("no column name found - '" + column + "'");
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
