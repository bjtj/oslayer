#ifndef __RANDOM_PORT_BINDERS_HPP__
#define __RANDOM_PORT_BINDERS_HPP__

#include <vector>
#include "os.hpp"

namespace UTIL {

	/**
     * @brief RangeRandomPortBinder
     */
    class RangeRandomPortBinder : public OS::RandomPortBinder {
	private:
		int startPort;
		int currentPort;
		int endPort;
		int selectedPort;

    public:
        RangeRandomPortBinder(int startPort, int endPort);
        virtual ~RangeRandomPortBinder();
        virtual void start();
        virtual int getNextPort();
        virtual bool wantFinish();
        virtual int getSelectedPort();
    };


	/**
     * @brief ListRandomPortBinder
     */
    class ListRandomPortBinder : public OS::RandomPortBinder {
	private:
		std::vector<int> portLists;
		size_t currentIndex;
		int selectedPort;

    public:
        ListRandomPortBinder(const std::vector<int> & portLists);
        virtual ~ListRandomPortBinder();
        virtual void start();
        virtual int getNextPort();
        virtual bool wantFinish();
        virtual int getSelectedPort();
    };
}

#endif