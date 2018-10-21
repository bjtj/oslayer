#include "Observer.hpp"
#include "os.hpp"

namespace osl {
	
    using namespace std;
	
	
    Observable::Observable() {
    }
    Observable::~Observable() {
    }
    void Observable::addObserver(Observer * observer) {
	observers.push_back(observer);
	onObserverAdded(observer);
    }
    void Observable::removeObserver(Observer * observer) {
	for (vector<Observer*>::iterator iter = observers.begin(); iter != observers.end();) {
	    if ((*iter) == observer) {
		onObserverRemoved(*iter);
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
	    onNotify(*iter);
	    (*iter)->onUpdate(target);
	}
    }
    size_t Observable::observerCount() {
	return observers.size();
    }
    void Observable::onObserverAdded(Observer * observer) {
	/**/
    }
    void Observable::onObserverRemoved(Observer * observer) {
	/**/
    }
    void Observable::onNotify(Observer * observer) {
	/**/
    }
	
	
    Observer::Observer() : target(NULL) {
	/**/
    }

    Observer::Observer(Observable * target) : target(target) {
	/**/
    }
	
    Observer::~Observer() {
	/**/
    }
	
    void Observer::startObserve(Observable * target) {
	this->target = target;
	target->addObserver(this);
    }
	
    void Observer::stopObserve() {
	target = NULL;
    }

    Observable * Observer::getObserveTarget() {
	return target;
    }
}
