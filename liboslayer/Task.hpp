#ifndef __TASK_HPP__
#define __TASK_HPP__

namespace osl {

	/**
	 * Task
	 */
	class Task {
	private:
		bool _canceled;
	public:
		Task() : _canceled(false) {}
		virtual ~Task() {}
		void reset() {
			_canceled = false;
		}
		virtual void onTask() = 0;
		virtual void run() {
			reset();
			onTask();
		}
		void cancel() {
			_canceled = true;
		}
		bool isCanceled() const {
			return _canceled;
		}
	};
}

#endif
