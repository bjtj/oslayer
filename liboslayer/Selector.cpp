#include "Selector.hpp"

namespace OS {

	using namespace std;

	/* SELECTION */
    
    Selection::Selection(int fd, bool readable, bool writeable, bool except)
		: fd(fd), readable(readable), writeable(writeable), except(except) {
        
    }
    Selection::~Selection() {
        
    }
    
    int Selection::getFd() {
        return fd;
    }
    
    bool Selection::isReadable() {
        return readable;
    }
    
    bool Selection::isWritable() {
        return writeable;
    }

	bool Selection::isExcept() {
		return except;
	}

	/* Selectable */

	Selectable::Selectable() {
	}
	
	Selectable::~Selectable() {
	}
    
	void Selectable::registerSelector(Selector & selector, unsigned char flags) {
		selector.set(getFd(), flags);
	}
	
	void Selectable::unregisterSelector(Selector & selector, unsigned char flags) {
		selector.unset(getFd(), flags);
	}
	
	bool Selectable::isReadable(Selector & selector) {
		return selector.isReadable(getFd());
	}

	bool Selectable::isWritable(Selector & selector) {
		return selector.isWritable(getFd());
	}
	
	bool Selectable::isExcept(Selector & selector) {
		return selector.isExcept(getFd());
	}

	/* Selector */

	Selector::Selector() : maxfds(0) {
		FD_ZERO(&readfds);
        FD_ZERO(&writefds);
		FD_ZERO(&exceptfds);
		FD_ZERO(&curreadfds);
        FD_ZERO(&curwritefds);
		FD_ZERO(&curexceptfds);
	}
	
	Selector::~Selector() {
	}
	void Selector::set(int fd, unsigned char flags) {

		if (fd < 0) {
			throw IOException("invalid fd", fd, 0);
		}
		
		if (fd > maxfds) {
			maxfds = fd;
		}

		if (flags & Selector::READ) {
			FD_SET(fd, &readfds);
		}
		if (flags & Selector::WRITE) {
			FD_SET(fd, &writefds);
		}
		if (flags & Selector::EXCEPT) {
			FD_SET(fd, &exceptfds);
		}
	}
	void Selector::unset(int fd, unsigned char flags) {

		if (fd < 0) {
			throw IOException("Invalid fd", fd, 0);
		}

		if (flags & Selector::READ) {
			FD_CLR(fd, &readfds);
		}
		if (flags & Selector::WRITE) {
			FD_CLR(fd, &writefds);
		}
		if (flags & Selector::EXCEPT) {
			FD_CLR(fd, &exceptfds);
		}
	}
	int Selector::select(unsigned long timeout_milli) {

		struct timeval timeout;
		timeout.tv_sec = timeout_milli / 1000;
		timeout.tv_usec = (timeout_milli % 1000) * 1000;
		
		curreadfds = readfds;
        curwritefds = writefds;
		curexceptfds = exceptfds;
		
		return ::select(maxfds + 1, &curreadfds, &curwritefds, &curexceptfds, &timeout);
	}
	vector<Selection> & Selector::getSelections() {
		selections.clear();
		for (int i = 0; i < maxfds + 1; i++) {
            
            bool readable = FD_ISSET(i, &curreadfds) ? true : false;
            bool writeable = FD_ISSET(i, &curwritefds) ? true : false;
			bool error = FD_ISSET(i, &curexceptfds) ? true : false;
            
            if (readable || writeable || error) {
                selections.push_back(Selection(i, readable, writeable, error));
            }
		}
		return selections;
	}
    
    bool Selector::isRegistered(int fd, unsigned char type) {
        switch (type) {
        case READ:
            return FD_ISSET(fd, &readfds) ? true : false;
        case WRITE:
            return FD_ISSET(fd, &writefds) ? true : false;
        case EXCEPT:
            return FD_ISSET(fd, &exceptfds) ? true : false;
        default:
            return false;
        }
    }
    
    bool Selector::isReadable(int fd) {
        return FD_ISSET(fd, &curreadfds) ? true : false;
    }
    
    bool Selector::isReadable(Selectable & selectable) {
        return isReadable(selectable.getFd());
    }
    
    bool Selector::isWritable(int fd) {
        return FD_ISSET(fd, &curwritefds) ? true : false;
    }
    
    bool Selector::isWritable(Selectable & selectable) {
        return isWritable(selectable.getFd());
    }

	bool Selector::isExcept(int fd) {
        return FD_ISSET(fd, &curexceptfds) ? true : false;
    }
    
    bool Selector::isExcept(Selectable & selectable) {
        return isExcept(selectable.getFd());
    }



	SharedSelector::SharedSelector() : semSet(1), semCur(1) {
	}
	SharedSelector::~SharedSelector() {
	}
	void SharedSelector::set(int fd, unsigned char flags) {
		AutoLock lock(semSet);
		Selector::set(fd, flags);
	}
	void SharedSelector::unset(int fd, unsigned char flags) {
		AutoLock lock(semSet);
		Selector::unset(fd, flags);
	}
	int SharedSelector::select(unsigned long timeout_milli) {
		AutoLock lock(semCur);
		return Selector::select(timeout_milli);
	}
	
	std::vector<Selection> & SharedSelector::getSelections() {
		AutoLock lock(semCur);
		return Selector::getSelections();
	}

	bool SharedSelector::isReadable(int fd) {
		AutoLock lock(semCur);
		return Selector::isReadable(fd);
	}
	bool SharedSelector::isReadable(Selectable & selectable) {
		AutoLock lock(semCur);
		return Selector::isReadable(selectable);
	}
	bool SharedSelector::isWritable(int fd) {
		AutoLock lock(semCur);
		return Selector::isWritable(fd);
	}
	bool SharedSelector::isWritable(Selectable & selectable) {
		AutoLock lock(semCur);
		return Selector::isWritable(selectable);
	}
	bool SharedSelector::isExcept(int fd) {
		AutoLock lock(semCur);
		return Selector::isExcept(fd);
	}
	bool SharedSelector::isExcept(Selectable & selectable) {
		AutoLock lock(semCur);
		return Selector::isExcept(selectable);
	}
}
