#ifndef __MESSAGE_QUEUE_HPP__
#define __MESSAGE_QUEUE_HPP__

#include "AutoRef.hpp"
#include "Object.hpp"
#include <deque>

namespace osl {

    /**
     * message
     */
    template <typename T>
    class Message {
    private:
	int _what;
	int _arg1;
	int _arg2;
	T _obj;
    public:
	Message() : _what(0), _arg1(0), _arg2(0) {}
	Message(int what) : _what(what), _arg1(0), _arg2(0) {}
	Message(int what, int arg1, int arg2) : _what(what), _arg1(arg1), _arg2(arg2) {}
	Message(int what, int arg1, int arg2, T obj) : _what(what), _arg1(arg1), _arg2(arg2), _obj(obj) {}
	Message(int what, T obj) : _what(what), _arg1(0), _arg2(0), _obj(obj) {}
	virtual ~Message() {}
	int & what() {return _what;}
	int & arg1() {return _arg1;}
	int & arg2() {return _arg2;}
	T & obj() {return _obj;}
    };

    /**
     * message queue
     */
    template <typename T>
    class MessageQueue {
    private:
	std::deque<Message<T> > messages;
    public:
	MessageQueue() {}
	virtual ~MessageQueue() {}
	bool empty() {
	    return messages.empty();
	}
	void enqueue(const Message<T> & msg) {
	    messages.push_back(msg);
	}
	Message<T> dequeue() {
	    Message<T> msg = messages.front();
	    messages.pop_front();
	    return msg;
	}
	Message<T> front() {
	    return messages.front();
	}
	size_t size() {
	    return messages.size();
	}
    };
}

#endif
