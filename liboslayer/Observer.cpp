#include "Observer.hpp"
#include "os.hpp"

namespace UTIL {
	
	using namespace std;
	using namespace OS;
	
	Observable::Observable() {
	}
	Observable::~Observable() {
	}

	void Observable::addObserver(Observer * observer) {
		observers.push_back(observer);
	}
	void Observable::removeObserver(Observer * observer) {
		for (vector<Observer*>::iterator iter = observers.begin(); iter != observers.end();) {
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
		for (vector<Observer*>::iterator iter = observers.begin(); iter != observers.end(); iter++) {
			(*iter)->update(target);
		}
	}

	
	Observer::Observer() {
	}
	Observer::~Observer() {
	}
	void Observer::update(Observable * target) {
	}

}
