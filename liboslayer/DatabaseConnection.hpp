#ifndef __DATABASE_CONNECTION_HPP__
#define __DATABASE_CONNECTION_HPP__

#include <vector>
#include <string>
#include "AutoRef.hpp"

namespace UTIL {
	
	/**
	 * @brief 
	 */
	class ResultSet {
	public:
		
		/**
		 * @brief 
		 */
		class Row {
		private:
			std::vector<std::string> _row;
		public:
			Row();
			Row(const std::vector<std::string> & row);
			virtual ~Row();
			size_t size();
			std::string & operator[] (size_t idx);
		};

	private:
		std::vector<std::string> _cols;
		std::vector<Row> _rows;
		long _row;
		
	public:
		ResultSet();
		virtual ~ResultSet();
		std::vector<std::string> & cols();
		void append(const std::vector<std::string> & strs);
		size_t size();
		Row & operator[] (size_t idx);
		bool next();
		std::string getString(size_t idx);
		std::string getString(const std::string & column);
	};

	/**
	 * @brief 
	 */
	class PreparedStatement {
	private:
		std::string _sql;
	public:
		PreparedStatement(const std::string & sql);
		virtual ~PreparedStatement();
		std::string & sql();
		virtual bool execute() = 0;
		virtual AutoRef<ResultSet> executeQuery() = 0;
		virtual AutoRef<ResultSet> executeStep() = 0;
	};

	/**
	 * @brief 
	 */
	class DatabaseConnection {
	public:
		DatabaseConnection();
		virtual ~DatabaseConnection();
		virtual void connect(const std::string & url) = 0;
		virtual void connect(const std::string & url, const std::string & username, const std::string & password) = 0;
		virtual void disconnect() = 0;
		virtual AutoRef<PreparedStatement> prepareStatement(const std::string & sql) = 0;
		virtual void setAutoCommit() = 0;
		virtual void beginTransaction() = 0;
		virtual void commit() = 0;
		virtual void rollback() = 0;
	};
	
}

#endif
