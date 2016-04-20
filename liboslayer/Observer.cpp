#include "Observer.hpp"
#include "os.hpp"

namespace UTIL {
	
	using namespace std;
	using namespace OS;
	
	Observable::Observable() {
	}
	Observable::~Observable() {
	}

	void Observable::addObserver(AutoRef<Observer> observer) {
		observers.push_back(observer);
	}
	void Observable::removeObserver(AutoRef<Observer> observer) {
		for (vector<AutoRef<Observer> >::iterator iter = observers.begin(); iter != observers.end();) {
			if ((*iter) == observer) {
				iter = observers.erase(iter);
			} else {
				iter++;
			}
		}
	}
	void Observable::notifyObservers() {
		notifyObservers(this);
	}
	void Observable::notifyObservers(Observable * target) {
		for (vector<AutoRef<Observer> >::iterator iter = observers.begin(); iter != observers.end(); iter++) {
			(*iter)->update(target);
		}
	}


	
	Observer::Observer() {
	}
	Observer::~Observer() {
	}
	void Observer::update(Observable * target) {
	}


	ObserverWrapper::ObserverWrapper(Observer * observer) : observer(observer) {
		if (!observer) {
			throw Exception("null observer instance");
		}
	}
	ObserverWrapper::~ObserverWrapper() {
	}
	
	void ObserverWrapper::update(Observable * target) {
		observer->update(target);
	}
}
