#ifndef __DATABASE_DRIVER_HPP__
#define __DATABASE_DRIVER_HPP__

#include <string>
#include <map>
#include "AutoRef.hpp"
#include "Library.hpp"
#include "Uri.hpp"
#include "DatabaseConnection.hpp"

namespace osl {

	class DatabaseDriver {
	private:
		std::map< std::string, osl::AutoRef<osl::Library> > _drivers;
		static DatabaseDriver _instance;
	private:
		DatabaseDriver();
		DatabaseDriver(const DatabaseDriver & other);
		DatabaseDriver & operator= (const DatabaseDriver & other);
	public:
		virtual ~DatabaseDriver();
		static DatabaseDriver & instance();
        bool isLoaded(const std::string & name);
		void load(const std::string & name, const osl::AutoRef<osl::Library> & lib);
		void unload(const std::string & name);
		osl::AutoRef<DatabaseConnection> getConnection(const std::string & name);
	};

}

#endif
