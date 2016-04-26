#include "TaskThreadPool.hpp"
#include "os.hpp"

namespace UTIL {

	using namespace OS;

	/**
	 * @brief
	 */
	
	TaskThread::TaskThread() : FlaggableThread(false) {
	}
	TaskThread::~TaskThread() {
	}
	void TaskThread::setTask(AutoRef<Task> task) {
		this->task = task;
	}
	void TaskThread::run() {
		while (!interrupted()) {
			if (!flagged()) {
				idle(10);
				continue;
			}
			task->doTask();
			setFlag(false);
		}
	}

	/**
	 * @brief
	 */
	class TaskThreadCreator : public InstanceCreator<FlaggableThread*> {
	private:
	public:
		TaskThreadCreator() {}
		virtual ~TaskThreadCreator() {}
		virtual FlaggableThread * createInstance() {
			return new TaskThread;
		}
		virtual void releaseInstance(FlaggableThread * instance) {
			delete instance;
		}
	};

	static TaskThreadCreator s_creator;

	/**
	 * @brief
	 */

	TaskThreadPool::TaskThreadPool(size_t count) : ThreadPool(count, s_creator) {
	}
	TaskThreadPool::~TaskThreadPool() {
	}
	void TaskThreadPool::setTask(AutoRef<Task> task) {
		TaskThread * thread = (TaskThread*)acquire();
		if (!thread) {
			throw Exception("no available thread");
		}
		thread->setTask(task);
		enqueue(thread);
	}
	void TaskThreadPool::setTaskWaitIfFull(AutoRef<Task> task) {
		TaskThread * thread = NULL;
		while ((thread = (TaskThread*)acquire()) == NULL) {
			idle(10);
		}
		thread->setTask(task);
		enqueue(thread);
	}
	void TaskThreadPool::setTaskWaitIfFullWithTimeout(AutoRef<Task> task, unsigned long timeout) {
		TaskThread * thread = NULL;
		unsigned long tick = tick_milli();
		while ((thread = (TaskThread*)acquire()) == NULL) {
			if (tick_milli() - tick >= timeout) {
				throw Exception("timeout occurred");
			}
			idle(10);
		}
		thread->setTask(task);
		enqueue(thread);
	}
}
