#include <iostream>
#include <liboslayer/os.hpp>
#include <liboslayer/ThreadPool.hpp>

using namespace std;
using namespace OS;
using namespace UTIL;

class WorkingThread : public FlaggableThread {
private:
	int id;
public:
	WorkingThread(int id) : FlaggableThread(false), id(id) {}
	virtual ~WorkingThread() {}

	virtual void run() {

		printf("[%d] started\n", id);

		while (!interrupted()) {

			if (!flagged()) {
				idle(10);
				continue;
			}

			printf("[ID: %d] Job Start.\n", id);
			idle(1000);
			printf("[ID: %d] Job Done.\n", id);

			setFlag(false);
		}

		printf("[%d] done\n", id);
	}
};

size_t readline(char * buffer, size_t max) {
	fgets(buffer, (int)max - 1, stdin);
	buffer[strlen(buffer) - 1] = 0;
	return strlen(buffer);
}

class CollectorThread : public Thread {
private:
	ThreadPool & pool;
public:
	CollectorThread(ThreadPool & pool) : pool(pool) {
	}
	virtual ~CollectorThread() {
	}

	virtual void run() {
		while (!interrupted()) {
			pool.collectUnflaggedThreads();
			idle(10);
		}
	}
};


void do_job(ThreadPool & pool, int jobCount) {
	for (int i = 0; i < jobCount; i++) {
		FlaggableThread * thread = pool.acquire();
		if (thread) {
			pool.enqueue(thread);
		}
	}
}

void do_job_must(ThreadPool & pool, int jobCount) {
	for (int i = 0; i < jobCount; i++) {

		FlaggableThread * thread = NULL;
		while (!thread) {
			thread = pool.acquire();
			if (thread) {
				pool.enqueue(thread);
				continue;
			}
		}
	}
}

class CrazyBossThread : public Thread {
private:
	ThreadPool & pool;
public:
	CrazyBossThread(ThreadPool & pool) : pool(pool) {
	}
	virtual ~CrazyBossThread() {
	}

	virtual void run() {
		while (!interrupted()) {
			do_job_must(pool, 1);
			idle(100);
		}
	}
};

void test() {

	bool done = false;

	class MyInstanceCreator : public InstanceCreator<FlaggableThread*> {
	private:
		int id;
	public:
		MyInstanceCreator() : id(0) {
		}
		virtual ~MyInstanceCreator() {}
		FlaggableThread * createInstance() {
			printf("create instance: %d\n", id);
			return new WorkingThread(id++);
		}

		void releaseInstance(FlaggableThread * inst) {
			delete inst;
		}
	};
	MyInstanceCreator creator;
	ThreadPool pool(5, creator);

	pool.start();

	CollectorThread collector(pool);
	collector.start();

	CrazyBossThread * boss = NULL;

	while (!done) {

		char buffer[1024] = {0,};
		readline(buffer, sizeof(buffer));
		if (!strcmp(buffer, "q")) {
			done = true;
			break;
		}

		if (!strcmp(buffer, "j")) {
			do_job(pool, 10);
		}

		if (!strcmp(buffer, "jm")) {
			do_job_must(pool, 10);
		}

		if (!strcmp(buffer, "bs")) {
			if (!boss) {
				boss = new CrazyBossThread(pool);
				boss->start();
			}
		}

		if (!strcmp(buffer, "bt")) {
			if (boss) {
				boss->interrupt();
				boss->join();
				boss = NULL;
			}
		}
	}

	if (boss) {
		boss->interrupt();
		boss->join();
	}

	collector.interrupt();
	collector.join();

	pool.stop();

}


int main(int argc, char * args[]) {

	test();

	getchar();

	return 0;
}