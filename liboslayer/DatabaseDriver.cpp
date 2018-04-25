#include "DatabaseDriver.hpp"

namespace osl {

	using namespace std;
	

	DatabaseDriver DatabaseDriver::_instance;

	DatabaseDriver::DatabaseDriver() {
	}
	DatabaseDriver::~DatabaseDriver() {
	}
	DatabaseDriver & DatabaseDriver::instance() {
		return _instance;
	}
    bool DatabaseDriver::isLoaded(const string &name) {
        return (_drivers.find(name) != _drivers.end());
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
