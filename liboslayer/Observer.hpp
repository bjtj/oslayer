#ifndef __OBSERVER_HPP__
#define __OBSERVER_HPP__

#include "AutoRef.hpp"
#include <vector>

namespace UTIL {

	class Observer;
	
	/**
	 *
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
	};

	/**
	 *
	 */
	class Observer {
	public:
		Observer();
		virtual ~Observer();
		virtual void update(Observable * target);
	};
}

#endif
