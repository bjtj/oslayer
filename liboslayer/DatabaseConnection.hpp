#ifndef __DATABASE_CONNECTION_HPP__
#define __DATABASE_CONNECTION_HPP__

#include <vector>
#include <string>
#include "AutoRef.hpp"

namespace UTIL {

	// 
	DECL_NAMED_EXCEPTION(DatabaseException);
	
	/**
	 * @brief ResultSet
	 */
	class ResultSet {
	private:
	public:
		ResultSet();
		virtual ~ResultSet();
		virtual bool next();
		virtual void close();
		virtual int fieldCount();
		virtual std::string fieldName(size_t idx);
		virtual std::string getString(size_t idx);
		virtual std::string getString(const std::string & column);
	};

	/**
	 * @brief PreparedStatement
	 */
	class PreparedStatement {
	private:
		std::string _sql;
	public:
		PreparedStatement(const std::string & sql);
		virtual ~PreparedStatement();
		std::string & sql();
		virtual bool execute();
		virtual OS::AutoRef<ResultSet> executeQuery();
		virtual size_t executeUpdate();
		virtual void setString(size_t idx, const std::string & str);
		virtual void setInteger(size_t idx, const int & str);
		virtual void setLong(size_t idx, const long & str);
	};

	/**
	 * @brief DatabaseConnection
	 */
	class DatabaseConnection {
	public:
		DatabaseConnection();
		virtual ~DatabaseConnection();
		virtual std::string getDriverName();
		virtual void connect(const std::string & hostname, int port, const std::string & username, const std::string & password, const std::string & dbname);
		virtual void disconnect();
		virtual bool isConnected();
		virtual OS::AutoRef<ResultSet> query(const std::string & sql);
		virtual size_t queryUpdate(const std::string & sql);
		virtual size_t lastInsertId();
		virtual OS::AutoRef<PreparedStatement> prepareStatement(const std::string & sql);
		virtual bool getAutoCommit();
		virtual void setAutoCommit(bool autoCommit);
		virtual void beginTransaction();
		virtual void commit();
		virtual void rollback();
	};
	
}

#endif
