#if HAVE_CONFIG_H
#	include <config.h>
#endif

#if HAVE_SQLITE3

#include <sqlite3.h>
#include <liboslayer/Text.hpp>
#include <liboslayer/DatabaseConnection.hpp>

using namespace std;
using namespace osl;


/**
 * 
 */
class SqliteResultSet : public ResultSet {
private:
	vector<string> _columns;
	vector< vector<string> > _rows;
	int _row;
public:
    SqliteResultSet() : _row(-1) {
	}
    virtual ~SqliteResultSet() {
	}
	vector<string> & columns() {
		return _columns;
	}
	vector< vector<string> > & rows() {
		return _rows;
	}
	size_t size() {
		return _rows.size();
	}
	bool next() {
		_row++;
		return (_row < size());
	}
	string getString(size_t idx) {
		return _rows[_row][idx];
	}
	string getString(const std::string & column) {
		return "";
	}
};


/**
 * @brief 
 */
class Sqlite3PreparedStatement : public PreparedStatement {
private:
	sqlite3 * _db;
public:
	Sqlite3PreparedStatement(sqlite3 * db, const string & sql);
	virtual ~Sqlite3PreparedStatement();
	virtual bool execute();
	virtual AutoRef<ResultSet> executeQuery();
	virtual AutoRef<ResultSet> executeStep();
	static int cb_query_handler(void * user, int argc, char ** args, char ** cols);
};

/**
 * @brief 
 */
Sqlite3PreparedStatement::Sqlite3PreparedStatement(sqlite3 * db, const string & sql)
	: PreparedStatement(sql), _db(db) {
}
Sqlite3PreparedStatement::~Sqlite3PreparedStatement() {
}

bool Sqlite3PreparedStatement::execute() {
	char * err_msg = NULL;
	int ret = sqlite3_exec(_db, sql().c_str(), NULL, NULL, &err_msg);
	if (ret != SQLITE_OK) {
		sqlite3_free(err_msg);
		return false;
	}
	return true;
}
AutoRef<ResultSet> Sqlite3PreparedStatement::executeQuery() {
	char * err_msg = NULL;
	AutoRef<ResultSet> rs(new SqliteResultSet);
	int ret = sqlite3_exec(_db, sql().c_str(), cb_query_handler, &rs, &err_msg);
	if (ret != SQLITE_OK) {
		string err(err_msg);
		sqlite3_free(err_msg);
		throw Exception("sqlite3_exec() error - " + err);
	}
	return rs;
}

AutoRef<ResultSet> Sqlite3PreparedStatement::executeStep() {
	return AutoRef<ResultSet>();
}

int Sqlite3PreparedStatement::cb_query_handler(void * user, int argc, char ** args, char ** cols) {
	SqliteResultSet * rs = (SqliteResultSet*)user;
	if (rs->columns().empty()) {
		rs->columns() = Text::toVector(argc, cols);
	}
	rs->rows().push_back(Text::toVector(argc, args));
	return 0;
}


/**
 * @brief 
 */
class Sqlite3Connection : public DatabaseConnection {
private:
	sqlite3 * _db;
public:
	Sqlite3Connection();
	virtual ~Sqlite3Connection();
	virtual void connect(const string & url);
	virtual void connect(const string & url, const string & username, const string & password);
	virtual void disconnect();
	virtual AutoRef<PreparedStatement> prepareStatement(const string & sql);
	virtual void setAutoCommit();
	virtual void beginTransaction();
	virtual void commit();
	virtual void rollback();
};

/**
 * @brief 
 */
Sqlite3Connection::Sqlite3Connection() : _db(NULL) {
}

Sqlite3Connection::~Sqlite3Connection() {
}

void Sqlite3Connection::connect(const string & url) {
	connect(url, "", "");
}

void Sqlite3Connection::connect(const string & url, const string & username, const string & password) {
		
	if (_db) {
		throw Exception("already connected");
	}
		
	int ret = sqlite3_open(url.c_str(), &_db);
	if (ret != SQLITE_OK) {
		disconnect();
		throw Exception("sqlite3 open() error");
	}
}

void Sqlite3Connection::disconnect() {
	if (_db) {
		sqlite3_close(_db);
		_db = NULL;
	}
}

AutoRef<PreparedStatement> Sqlite3Connection::prepareStatement(const string & sql) {
	return AutoRef<PreparedStatement>(new Sqlite3PreparedStatement(_db, sql));
}

void Sqlite3Connection::setAutoCommit() {
	// not implemented
}
void Sqlite3Connection::beginTransaction() {
	// not implemented
}
void Sqlite3Connection::commit() {
	// not implemented
}
void Sqlite3Connection::rollback() {
	// not implemented
}


int main(int argc, char *args[]) {

	Sqlite3Connection conn;
	conn.connect("test.db");
	conn.disconnect();
    
    return 0;
}

#else

int main(int argc, char * args[]) {

	// no sqlite

    return 0;
}


#endif
