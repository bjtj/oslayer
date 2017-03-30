#ifndef __REF_HPP__
#define __REF_HPP__

namespace UTIL {

	template <typename T>
	class Ref {
	private:
		T * _t;
	public:
		explicit Ref();
		explicit Ref(T * t);
		Ref(const Ref<T> & other);
		virtual ~Ref();
		Ref<T> & operator= (T * t);
		Ref<T> & operator= (const Ref<T> & other);
		bool operator== (T * t);
		bool operator== (const Ref<T> & other);
		T & operator* ();
        T * operator-> () const;
        T * operator& ();
		bool nil() const;
	};

	template <typename T>
	Ref<T>::Ref() : _t(NULL) {
	}
	template <typename T>
	Ref<T>::Ref(T * t) : _t(t) {
	}
	template <typename T>
	Ref<T>::Ref(const Ref<T> & other) : _t(NULL) {
		_t = other._t;
	}
	template <typename T>
	Ref<T>::~Ref() {
	}
	template <typename T>
	Ref<T> & Ref<T>::operator= (T * t) {
		_t = t;
		return *this;
	}
	template <typename T>
	Ref<T> & Ref<T>::operator= (const Ref<T> & other) {
		_t = other._t;
		return *this;
	}
	template <typename T>
	bool Ref<T>::operator== (T * t) {
		return _t == t;
	}
	template <typename T>
	bool Ref<T>::operator== (const Ref<T> & other) {
		return _t == other._t;
	}
	template <typename T>
	T & Ref<T>::operator* () {
		return *_t;
	}
	template <typename T>
	T * Ref<T>::operator-> () const {
		return _t;
	}
	template <typename T>
	T * Ref<T>::operator& () {
		return _t;
	}
	template <typename T>
	bool Ref<T>::nil() const {
		return _t == NULL;
	}

	template<>
    class Ref<void> {
    private:
    public:
    };
}

#endif
