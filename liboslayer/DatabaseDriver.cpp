#include "DatabaseDriver.hpp"

namespace UTIL {

	using namespace std;
	using namespace OS;

	DatabaseDriver DatabaseDriver::_instance;

	DatabaseDriver::DatabaseDriver() {
	}
	DatabaseDriver::~DatabaseDriver() {
	}
	DatabaseDriver & DatabaseDriver::instance() {
		return _instance;
	}
	void DatabaseDriver::load(const std::string & name, const AutoRef<Library> & lib) {
		lib->load();
		_drivers[name] = lib;
	}
	void DatabaseDriver::unload(const std::string & name) {
		_drivers.erase(name);
	}
	AutoRef<DatabaseConnection> DatabaseDriver::getConnection(const string & name) {
		return AutoRef<DatabaseConnection>(
			((DatabaseConnection*(*)(void))*_drivers[name]->symbol("get_connector"))());
	}

}
