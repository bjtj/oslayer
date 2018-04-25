#ifndef __SELECTOR_HPP__
#define __SELECTOR_HPP__

#include "os.hpp"
#include "Semaphore.hpp"

namespace osl {

	/**
     * @brief selection
     */
    class Selection {
        
    private:
        int fd;
        bool readable;
        bool writeable;
		bool except;
        
    public:
        Selection(int fd, bool readable, bool writeable, bool except);
        virtual ~Selection();
        
        int getFd();
        bool isReadable();
        bool isWritable();
		bool isExcept();
    };
    
    class Selector;
    
    /**
     * @brief Selectable interface
     */
    class Selectable {
    private:
    public:
		Selectable();
        virtual ~Selectable();
        virtual int getFd() = 0;
		virtual void registerSelector(Selector & selector, unsigned char flags);
		virtual void unregisterSelector(Selector & selector, unsigned char flags);
		virtual bool isReadable(Selector & selector);
		virtual bool isWritable(Selector & selector);
		virtual bool isExcept(Selector & selector);
    };

	/**
	 * @brief selector
	 */
	class Selector {
	public:
		const static unsigned char READ = 0x01;
		const static unsigned char WRITE = 0x02;
		const static unsigned char EXCEPT = 0x04;
		const static unsigned char FULL = 0x07;
		
	private:
		int maxfds;
        fd_set readfds;
        fd_set writefds;
		fd_set exceptfds;
		fd_set curreadfds;
        fd_set curwritefds;
		fd_set curexceptfds;
		std::vector<Selection> selections;
        
	public:
		Selector();
		virtual ~Selector();

		virtual void set(int fd, unsigned char flags);
		virtual void unset(int fd, unsigned char flags);
		virtual int select(unsigned long timeout_milli);
		virtual std::vector<Selection> & getSelections();
        virtual bool isRegistered(int fd, unsigned char type);
		virtual bool isReadable(int fd);
		virtual bool isReadable(Selectable & selectable);
		virtual bool isWritable(int fd);
		virtual bool isWritable(Selectable & selectable);
		virtual bool isExcept(int fd);
		virtual bool isExcept(Selectable & selectable);
	};

	/**
	 * @brief 
	 */
	class SharedSelector : public Selector {
	private:
		Semaphore semSet;
		Semaphore semCur;
	public:
		SharedSelector();
		virtual ~SharedSelector();

		virtual void set(int fd, unsigned char flags);
		virtual void unset(int fd, unsigned char flags);
		virtual int select(unsigned long timeout_milli);
		virtual std::vector<Selection> & getSelections();
		virtual bool isReadable(int fd);
		virtual bool isReadable(Selectable & selectable);
		virtual bool isWritable(int fd);
		virtual bool isWritable(Selectable & selectable);
		virtual bool isExcept(int fd);
		virtual bool isExcept(Selectable & selectable);
	};
}

#endif
