#ifndef __AUTO_RELEASE_HPP__
#define __AUTO_RELEASE_HPP__

namespace osl {

#define DECL_AUTO_RELEASE(CLS_NAME, TYPE, RELEASE_FUNC) \
class CLS_NAME { \
private: \
	TYPE * _t; \
public: \
	CLS_NAME(TYPE * _t) : _t(_t) {} \
	virtual ~CLS_NAME() { if (_t) {RELEASE_FUNC(_t);} } \
	void forget() { _t = NULL; } \
	TYPE & operator*() { return *_t; } \
	TYPE * operator->() { return _t; } \
	TYPE * operator&() { return _t; } \
}

}

#endif
