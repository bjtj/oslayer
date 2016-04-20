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
		std::vector<AutoRef<Observer> > observers;
	public:
		Observable();
		virtual ~Observable();

		void addObserver(AutoRef<Observer> observer);
		void removeObserver(AutoRef<Observer> observer);
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

	/**
	 *
	 */
	class ObserverWrapper : public Observer {
	private:
		Observer * observer;
	public:
		ObserverWrapper(Observer * observer);
		virtual ~ObserverWrapper();
		virtual void update(Observable * target);
	};
}

#endif
