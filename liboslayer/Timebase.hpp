#ifndef __TIME_BASE_HPP__
#define __TIME_BASE_HPP__

#include "os.hpp"
#include <vector>

namespace UTIL {

	/**
	 * @brief 
	 */
	template <typename T>
	class Timebase {
	private:
		T _t;
		unsigned long _creationTick;
		unsigned long _lastUpdateTick;
		unsigned long _timeoutTick;
	public:
		Timebase(T t, unsigned long timeoutTick) : _t(t), _timeoutTick(timeoutTick) {
			_lastUpdateTick = _creationTick = OS::tick_milli();
		}
		Timebase(T t, unsigned long creationTick, unsigned long timeoutTick)
			: _t(t), _creationTick(creationTick), _lastUpdateTick(creationTick), _timeoutTick(timeoutTick) {
		}
		virtual ~Timebase() {
		}
		unsigned long & creationTick() {
			return _creationTick;
		}
		unsigned long & lastUpdateTick() {
			return _lastUpdateTick;
		}
		unsigned long & timeoutTick() {
			return _timeoutTick;
		}
		void prolong() {
			_lastUpdateTick = OS::tick_milli();
		}
		void prolong(unsigned long newTimeout) {
			_lastUpdateTick = OS::tick_milli();
			_timeoutTick = newTimeout;
		}
		bool outdated() {
			return (OS::tick_milli() - _lastUpdateTick >= _timeoutTick);
		}
		unsigned long lifetimeRecent() {
			return OS::tick_milli() - _lastUpdateTick;
		}
		unsigned long lifetimeFull() {
			return OS::tick_milli() - _creationTick;
		}
		bool operator==(const Timebase<T> & other) const {
			return _t == other._t;
		}
	};

	/**
	 * @brief 
	 */
	template <typename T>
	class TimebaseList {
	private:
		std::vector<Timebase<T> > _list;
	public:
		TimebaseList() {
		}
		virtual ~TimebaseList() {
		}
		void add(Timebase<T> t) {
			_list.push_back(t);
		}
		void remove(Timebase<T> t) {
			for (typename std::vector<Timebase<T> >::iterator iter = _list.begin(); iter != _list.end();) {
				if ((*iter) == t) {
					iter = _list.erase(iter);
				} else {
					iter++;
				}
			}
		}
		size_t size() {
			return _list.size();
		}
		void collectOutdated() {
			for (typename std::vector<Timebase<T> >::iterator iter = _list.begin(); iter != _list.end();) {
				if (iter->outdated()) {
					iter = _list.erase(iter);
				} else {
					iter++;
				}
			}
		}
		Timebase<T> & operator[] (size_t idx) {
			return _list[idx];
		}
	};
}

#endif
