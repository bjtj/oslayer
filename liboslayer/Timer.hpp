#ifndef __TIMER_HPP__
#define __TIMER_HPP__

#include <string>
#include <vector>
#include "AutoRef.hpp"
#include "os.hpp"
#include "Semaphore.hpp"
#include "Thread.hpp"
#include "Task.hpp"

namespace osl {


    /**
     * @brief
     */
    class TimerSchedule {
    public:
	static const int INFINITE_LOOP = -1;
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
	bool testDelay(unsigned long startTick, unsigned long currentTick);
	bool testEvent(unsigned long lastLapseTick, unsigned long currentTick);
	unsigned long fixedLapseTick(unsigned long startTick, unsigned int count);
	int getRepeatCount();
	bool infinite();
	static TimerSchedule makeInterval(unsigned long interval);
	static TimerSchedule makeDelay(unsigned long delay);
	static TimerSchedule makeIntervalRepeat(unsigned long interval, int repeatCount);
	static TimerSchedule makeDelayIntervalRepeat(unsigned long delay, unsigned long interval, int repeatCount);
	static TimerSchedule makeDelayInterval(unsigned long delay, unsigned long interval);
    };


    /**
     * @brief
     */
    class TimerTask : public Task {
    private:
    public:
	TimerTask();
	virtual ~TimerTask();
    };

    /**
     * @brief
     */
    class TimerTaskSession {
    private:
	TimerSchedule schedule;
	AutoRef<TimerTask> task;
	unsigned int runCount;
	unsigned long startTick;
	unsigned long lastLapseTick;
	
    public:
	TimerTaskSession(TimerSchedule schedule, AutoRef<TimerTask> task);
	virtual ~TimerTaskSession();
	void start();
	void process();
	bool outdated();
    };

    /**
     * @brief
     */
    class TimerLooper {
    private:
	std::vector<TimerTaskSession> sessions;
	bool done;
	Semaphore sem;
	
    public:
	TimerLooper();
	virtual ~TimerLooper();
	void addTaskSession(TimerTaskSession session);
	void delay(unsigned long delay, AutoRef<TimerTask> task);
	void interval(unsigned long interval, AutoRef<TimerTask> task);
	void intervalRepeat(unsigned long interval, int repeat, AutoRef<TimerTask> task);
	void delayInterval(unsigned long delay, unsigned long interval, AutoRef<TimerTask> task);
	void delayIntervalRepeat(unsigned long delay, unsigned long interval, int repeat, AutoRef<TimerTask> task);
	void loop();
	void stop();
    };

    /**
     * @brief
     */
    class TimerLooperThread : public Thread {
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
	TimePin(unsigned long initialTick);
	virtual ~TimePin();
	unsigned long & tick();
	void reset();
	unsigned long elapsed();
    };

    /**
     * @brief
     */
    class Duration {
    private:
	unsigned long _milli;
    public:
	Duration();
	Duration(unsigned long milli);
	virtual ~Duration();
	void add(unsigned long milli);
	unsigned long & milli();
    };

    /**
     * @brief
     */
    class Timeout {
    private:
	unsigned long _timeout;
	unsigned long _tick;
    public:
	Timeout();
	Timeout(unsigned long timeout);
	virtual ~Timeout();
	unsigned long & value();
	void reset();
	bool expired();
    };

}

#endif
