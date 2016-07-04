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
	public:
		ResultSet();
		virtual ~ResultSet();
		std::vector<std::string> & cols();
		void append(const std::vector<std::string> & strs);
		size_t size();
		Row & operator[] (size_t idx);
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
		virtual size_t executeQuery(ResultSet & rs) = 0;
	};

	/**
	 * @brief 
	 */
	class DatabaseConnection {
	public:
		DatabaseConnection();
		virtual ~DatabaseConnection();
		virtual void connect(const std::string & url, const std::string & username, const std::string & password) = 0;
		virtual void disconnect() = 0;
		virtual AutoRef<PreparedStatement> prepareStatement(const std::string & sql) = 0;
	};
	
}

#endif
