#include "Timer.hpp"
#include "os.hpp"

namespace osl {

	
    using namespace std;

    //
       
    TimerSchedule::TimerSchedule()
	: delay(0), interval(0), repeatCount(0) {
    }
	
    TimerSchedule::TimerSchedule(unsigned long delay, unsigned long interval, int repeatCount)
	: delay(delay), interval(interval), repeatCount(repeatCount) {
    }
	
    TimerSchedule::~TimerSchedule() {
    }
	
    string & TimerSchedule::nickname() {
	return _nickname;
    }

    bool TimerSchedule::testDelay(unsigned long startTick, unsigned long currentTick) {
	return currentTick - startTick >= delay;
    }
	
    bool TimerSchedule::testEvent(unsigned long lastLapseTick, unsigned long currentTick) {
	return currentTick - lastLapseTick >= interval;
    }
	
    unsigned long TimerSchedule::fixedLapseTick(unsigned long startTick, unsigned int count) {
	return startTick + delay + (count * interval);
    }
	
    int TimerSchedule::getRepeatCount() {
	return repeatCount;
    }
	
    bool TimerSchedule::infinite() {
	return repeatCount < 0;
    }
	
    TimerSchedule TimerSchedule::makeInterval(unsigned long interval) {
	return TimerSchedule(0, interval, INFINITE);
    }
	
    TimerSchedule TimerSchedule::makeDelay(unsigned long delay) {
	return TimerSchedule(delay, 0, 0);
    }
	
    TimerSchedule TimerSchedule::makeIntervalRepeat(unsigned long interval, int repeatCount) {
	return TimerSchedule(0, interval, repeatCount);
    }
	
    TimerSchedule TimerSchedule::makeDelayIntervalRepeat(unsigned long delay, unsigned long interval, int repeatCount) {
	return TimerSchedule(delay, interval, repeatCount);
    }
	
    TimerSchedule TimerSchedule::makeDelayInterval(unsigned long delay, unsigned long interval) {
	return TimerSchedule(delay, interval, INFINITE);
    }

    //

    TimerTask::TimerTask() {
    }
	
    TimerTask::~TimerTask() {
    }


    TimerTaskSession::TimerTaskSession(TimerSchedule schedule, AutoRef<TimerTask> task) : schedule(schedule), task(task), runCount(0),startTick(0), lastLapseTick(0) {
	start();
    }
    TimerTaskSession::~TimerTaskSession() {}

    void TimerTaskSession::start() {
	lastLapseTick = startTick = tick_milli();
	runCount = 0;
    }

    void TimerTaskSession::process() {

	if (outdated()) {
	    return;
	}

	if (startTick == lastLapseTick) {
	    if (!schedule.testDelay(startTick, tick_milli())) {
		return;
	    }
	    lastLapseTick = schedule.fixedLapseTick(startTick, 0);
	}
		
	if (schedule.testEvent(lastLapseTick, tick_milli())) {
	    task->run();
	    lastLapseTick = schedule.fixedLapseTick(startTick, ++runCount);
	}
    }

    bool TimerTaskSession::outdated() {
	return (!schedule.infinite() && runCount > (unsigned int)schedule.getRepeatCount());
    }

    //

    TimerLooper::TimerLooper()
	: done(false), sem(1) {
    }
	
    TimerLooper::~TimerLooper() {
    }

    void TimerLooper::addTaskSession(TimerTaskSession session) {
	sem.wait();
	sessions.push_back(session);
	sem.post();
    }

    void TimerLooper::delay(unsigned long delay, AutoRef<TimerTask> task) {
	addTaskSession(TimerTaskSession(TimerSchedule::makeDelay(delay), task));
    }

    void TimerLooper::interval(unsigned long interval, AutoRef<TimerTask> task) {
	addTaskSession(TimerTaskSession(TimerSchedule::makeInterval(interval), task));
    }

    void TimerLooper::intervalRepeat(unsigned long interval, int repeat, AutoRef<TimerTask> task) {
	addTaskSession(TimerTaskSession(TimerSchedule::makeIntervalRepeat(interval, repeat), task));
    }
	
    void TimerLooper::delayInterval(unsigned long delay, unsigned long interval, AutoRef<TimerTask> task) {
	addTaskSession(TimerTaskSession(TimerSchedule::makeDelayInterval(delay, interval), task));
    }
	
    void TimerLooper::delayIntervalRepeat(unsigned long delay, unsigned long interval, int repeat, AutoRef<TimerTask> task) {
	addTaskSession(TimerTaskSession(TimerSchedule::makeDelayIntervalRepeat(delay, interval, repeat), task));
    }

    void TimerLooper::loop() {
	done = false;
	while (!done) {
	    sem.wait();
	    for (vector<TimerTaskSession>::iterator iter = sessions.begin(); iter != sessions.end(); iter++) {
		iter->process();
	    }
	    sem.post();
	    idle(10);
	}
    }

    void TimerLooper::stop() {
	done = true;
    }

    //

    TimerLooperThread::TimerLooperThread() {
    }
    TimerLooperThread::~TimerLooperThread() {
    }
    void TimerLooperThread::run() {
	_looper.loop();
    }
    TimerLooper & TimerLooperThread::looper() {
	return _looper;
    }
    void TimerLooperThread::stop() {
	_looper.stop();
    }

    //

    TimePin::TimePin()
	: startTick(tick_milli()) {
    }

    TimePin::TimePin(unsigned long initialTick)
	: startTick(initialTick) {
    }
	
    TimePin::~TimePin() {
    }

    unsigned long & TimePin::tick() {
	return startTick;
    }

    void TimePin::reset() {
	startTick = tick_milli();
    }

    unsigned long TimePin::elapsed() {
	return tick_milli() - startTick;
    }

    //

    Duration::Duration() : _milli(0) {
    }
    Duration::Duration(unsigned long milli) : _milli(milli) {
    }
    Duration::~Duration() {
    }
    void Duration::add(unsigned long milli) {
	_milli += milli;
    }
    unsigned long & Duration::milli() {
	return _milli;
    }

    //

    Timeout::Timeout() : _timeout(0) {
	_tick = tick_milli();
    }
    Timeout::Timeout(unsigned long timeout) : _timeout(timeout) {
	_tick = tick_milli();
    }
    Timeout::~Timeout() {
    }
    unsigned long & Timeout::value() {
	return _timeout;
    }
    void Timeout::reset() {
	_tick = tick_milli();
    }
    bool Timeout::expired() {
	return (tick_milli() - _tick) >= _timeout;
    }
}
