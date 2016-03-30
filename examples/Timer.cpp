#include <iostream>
#include <liboslayer/os.hpp>
#include <liboslayer/Timer.hpp>
#include <liboslayer/FileStream.hpp>

using namespace std;
using namespace OS;
using namespace UTIL;

class TimerThread : public Thread {
private:
	TimerLooper _looper;
public:
    TimerThread() {
	}
    virtual ~TimerThread() {
	}
	virtual void run() {
		_looper.loop();
	}
	void quit() {
		_looper.stop();
	}
	TimerLooper & looper() {
		return _looper;
	}
};

class PrintTask : public TimerTask {
private:
	string text;
public:
    PrintTask(string text) : text(text) {}
    virtual ~PrintTask() {}
	virtual void doTask() {
		cout << text << endl;
	}
};


string readline() {
	FileStream fs(stdin);
	return fs.readline();
}

int main(int argc, char *args[]) {

	TimerThread tt;

	tt.start();

	while (1) {
		string line = readline();
		if (line == "q") {
			break;
		} else if (line == "s") {
			tt.looper().delay(1000, AutoRef<TimerTask>(new PrintTask("delay")));
		} else if (line == "i") {
			tt.looper().interval(500, AutoRef<TimerTask>(new PrintTask("interval")));
		} else if (line == "ic") {
			tt.looper().intervalWithCount(500, 2, AutoRef<TimerTask>(new PrintTask("interval with count")));
		}
	}

	tt.quit();
	tt.join();
	
    return 0;
}
