#ifndef __TIMER_HPP__
#define __TIMER_HPP__

#include <string>
#include <vector>
#include "AutoRef.hpp"
#include "os.hpp"

namespace UTIL {

	/**
	 * @brief
	 */
	class TimerSchedule {
	private:
		std::string _nickname;
		unsigned long delay;
		unsigned long interval;
		int repeatCount;
	public:
		TimerSchedule();
		TimerSchedule(unsigned long delay, unsigned long interval, int repeatCount);
		virtual ~TimerSchedule();
		std::string & nickname();
		void schedule(unsigned long delay, unsigned long interval, int repeatCount);
		bool testDelay(unsigned long startTick, unsigned long currentTick);
		bool testEvent(unsigned long lastLapseTick, unsigned long currentTick);
		unsigned long fixedLapseTick(unsigned long startTick, unsigned int count);
		int getRepeatCount();
		bool infinite();
	};

	/**
	 * @brief
	 */
	class TimerTask {
	private:
	public:
		TimerTask();
		virtual ~TimerTask();
		virtual void doTask() = 0;
	};

	/**
	 * @brief
	 */
	class TimerSession {
	private:
		TimerSchedule schedule;
		AutoRef<TimerTask> task;
		unsigned int runCount;
		unsigned long startTick;
		unsigned long lastLapseTick;
	
	public:
		TimerSession(TimerSchedule & schedule, AutoRef<TimerTask> task);
		virtual ~TimerSession();
		void start();
		void process();
		bool outdated();
	};

	/**
	 * @brief
	 */
	class TimerLooper {
	private:
		std::vector<TimerSession> sessions;
		bool done;
		OS::Semaphore sem;
	
	public:
		TimerLooper();
		virtual ~TimerLooper();
		void addSession(TimerSession & session);
		void delay(unsigned long delay, UTIL::AutoRef<TimerTask> task);
		void interval(unsigned long interval, UTIL::AutoRef<TimerTask> task);
		void intervalWithCount(unsigned long interval, int count, UTIL::AutoRef<TimerTask> task);
		void delayAndInterval(unsigned long delay, unsigned long interval, UTIL::AutoRef<TimerTask> task);
		void delayAndIntervalWithCount(unsigned long delay, unsigned long interval, int count, UTIL::AutoRef<TimerTask> task);
		void loop();
		void stop();
	};

	/**
	 * @brief
	 */
	class TimerLooperThread : public OS::Thread {
	private:
		TimerLooper _looper;
	public:
		TimerLooperThread();
		virtual ~TimerLooperThread();
		virtual void run();
		TimerLooper & looper();
		void stop();
	};


	/**
	 * @brief
	 */
	class TimePin {
	private:
		unsigned long startTick;
	public:
		TimePin();
		virtual ~TimePin();
		void reset();
		unsigned long elapsed();
	};
	
}

#endif
