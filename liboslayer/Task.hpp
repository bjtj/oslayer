#ifndef __TASK_HPP__
#define __TASK_HPP__

namespace UTIL {
	
	class Task {
	private:
	public:
		Task() {}
		virtual ~Task() {}
		virtual void doTask() = 0;
	};

	
}

#endif