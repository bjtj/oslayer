#include "Timer.hpp"
#include "os.hpp"

namespace UTIL {

	using namespace OS;
	using namespace std;

	//
	
	TimerSchedule::TimerSchedule() : delay(0), interval(0), repeatCount(0) {}
	TimerSchedule::TimerSchedule(unsigned long delay, unsigned long interval, int repeatCount) : delay(0), interval(0), repeatCount(0) {
		schedule(delay, interval, repeatCount);
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

		if (startTick == lastLapseTick && schedule.testDelay(startTick, tick_milli())) {
			lastLapseTick = schedule.fixedLapseTick(startTick, 0);
		}
		
		if (schedule.testEvent(lastLapseTick, tick_milli())) {
			task->doTask();
			lastLapseTick = schedule.fixedLapseTick(startTick, ++runCount);
		}
	}

	bool TimerSession::outdated() {
		return (!schedule.infinite() && runCount > (unsigned int)schedule.getRepeatCount());
	}

	//

	TimerLooper::TimerLooper() : done(false) {}
	TimerLooper::~TimerLooper() {}

	void TimerLooper::addSession(TimerSession & session) {
		sessions.push_back(session);
	}

	void TimerLooper::loop() {
		done = false;
		while (!done) {
			for (vector<TimerSession>::iterator iter = sessions.begin(); iter != sessions.end(); iter++) {
				iter->process();
				idle(10);
			}
		}
	}

	void TimerLooper::stop() {
		done = true;
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

	
}
