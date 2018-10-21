#include "Timer.hpp"
#include "os.hpp"

namespace osl {

	
    using namespace std;

    //
	
    TimerSchedule::TimerSchedule() : delay(0), interval(0), repeatCount(0) {}
    TimerSchedule::TimerSchedule(unsigned long delay, unsigned long interval, int repeatCount) : delay(delay), interval(interval), repeatCount(repeatCount) {
    }
    TimerSchedule::~TimerSchedule() {}
    string & TimerSchedule::nickname() {
	return _nickname;
    }
    void TimerSchedule::schedule(unsigned long delay, unsigned long interval, int repeatCount) {
	this->delay = delay;
	this->interval = interval;
	this->repeatCount = repeatCount;
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

    //

    TimerTask::TimerTask() {}
    TimerTask::~TimerTask() {}


    TimerSession::TimerSession(TimerSchedule & schedule, AutoRef<TimerTask> task) : schedule(schedule), task(task), runCount(0),startTick(0), lastLapseTick(0) {
	start();
    }
    TimerSession::~TimerSession() {}

    void TimerSession::start() {
	lastLapseTick = startTick = tick_milli();
	runCount = 0;
    }

    void TimerSession::process() {

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

    bool TimerSession::outdated() {
	return (!schedule.infinite() && runCount > (unsigned int)schedule.getRepeatCount());
    }

    //

    TimerLooper::TimerLooper() : done(false), sem(1) {}
    TimerLooper::~TimerLooper() {}

    void TimerLooper::addSession(TimerSession & session) {
	sem.wait();
	sessions.push_back(session);
	sem.post();
    }

    void TimerLooper::delay(unsigned long delay, AutoRef<TimerTask> task) {
	TimerSchedule schedule(delay, 0, 0);
	TimerSession session(schedule, task);
	addSession(session);
    }
    void TimerLooper::interval(unsigned long interval, AutoRef<TimerTask> task) {
	TimerSchedule schedule(0, interval, -1);
	TimerSession session(schedule, task);
	addSession(session);
    }
    void TimerLooper::intervalWithCount(unsigned long interval, int count, AutoRef<TimerTask> task) {
	TimerSchedule schedule(0, interval, count);
	TimerSession session(schedule, task);
	addSession(session);
    }
    void TimerLooper::delayAndInterval(unsigned long delay, unsigned long interval, AutoRef<TimerTask> task) {
	TimerSchedule schedule(delay, interval, -1);
	TimerSession session(schedule, task);
	addSession(session);
    }
    void TimerLooper::delayAndIntervalWithCount(unsigned long delay, unsigned long interval, int count, AutoRef<TimerTask> task) {
	TimerSchedule schedule(delay, interval, count);
	TimerSession session(schedule, task);
	addSession(session);
    }

    void TimerLooper::loop() {
	done = false;
	while (!done) {
	    sem.wait();
	    for (vector<TimerSession>::iterator iter = sessions.begin(); iter != sessions.end(); iter++) {
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

    TimePin::TimePin() : startTick(tick_milli()) {}
    TimePin::~TimePin() {}

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

    TimeoutChecker::TimeoutChecker() : _timeout(0) {
	_tick = tick_milli();
    }
    TimeoutChecker::TimeoutChecker(unsigned long timeout) : _timeout(timeout) {
	_tick = tick_milli();
    }
    TimeoutChecker::~TimeoutChecker() {
    }
    unsigned long & TimeoutChecker::timeout() {
	return _timeout;
    }
    void TimeoutChecker::reset() {
	_tick = tick_milli();
    }
    bool TimeoutChecker::trigger() {
	return (tick_milli() - _tick) >= _timeout;
    }
}
