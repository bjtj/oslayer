#ifndef __OBSERVER_HPP__
#define __OBSERVER_HPP__

#include "AutoRef.hpp"
#include <vector>

namespace UTIL {

	class Observer;
	
	/**
	 * observable
	 */
	class Observable {
	private:
		std::vector<Observer*> observers;
	public:
		Observable();
		virtual ~Observable();
		void addObserver(Observer * observer);
		void removeObserver(Observer * observer);
		void notifyObservers();
		void notifyObservers(Observable * target);
		size_t observerCount();
		virtual void onObserverAdded(Observer * observer);
		virtual void onObserverRemoved(Observer * observer);
		virtual void onNotify(Observer * observer);
	};

	/**
	 * observer
	 */
	class Observer {
	public:
		Observer();
		virtual ~Observer();
		virtual void update(Observable * target);
	};
}

#endif
