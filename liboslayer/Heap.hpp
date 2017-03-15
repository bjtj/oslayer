#ifndef __HEAP_HPP__
#define __HEAP_HPP__

#include <cstddef>
#include <map>
#include "os.hpp"

namespace OS {

	template<typename T>
	class Heap;

	/**
	 * @brief 
	 */
	template <typename T>
	class GCRef {
	private:
		Heap<T> * _heap;
		T * _mem;
	public:
		GCRef();
		GCRef(Heap<T> * heap, T * mem);
		virtual ~GCRef();
		GCRef(const GCRef<T> & other);
		GCRef & operator= (const GCRef<T> & other);
		T & operator* ();
		T * operator-> () const;
		T * operator& () const;
		bool nil() const;
	};

	/**
	 * @brief 
	 */
	template<typename T>
	class Heap {
	public:
		class Ref {
		private:
			T * _mem;
			int _ref_count;
		public:
			Ref();
			Ref(T * mem);
			virtual ~Ref();
			int & ref_count();
			void ref();
			void unref();
			void dealloc();
			void * operator* ();
		};
	private:
		std::map<void *, Ref> _mems;
	public:
		Heap();
		virtual ~Heap();
		void clear();
		GCRef<T> alloc(T * mem);
		void ref(void * mem);
		void unref(void * mem);
		unsigned long gc();
		size_t size();
		void print_usage();
	};

	template<typename T>
	GCRef<T>::GCRef() : _heap(NULL), _mem(NULL) {
	}
	template<typename T>
	GCRef<T>::GCRef(Heap<T> * heap, T * mem) : _heap(heap), _mem(mem) {
		_heap->ref(_mem);
	}
	template<typename T>
	GCRef<T>::~GCRef() {
		if (nil()) {
			return;
		}
		_heap->unref(_mem);
	}
	template<typename T>
	GCRef<T>::GCRef(const GCRef<T> & other) : _heap(NULL), _mem(NULL) {
		if (!nil()) {
			_heap->unref(_mem);
		}
		if (other.nil()) {
			return;
		}
		_heap = other._heap;
		_mem = other._mem;
		_heap->ref(_mem);
	}
	template<typename T>
	GCRef<T> & GCRef<T>::operator= (const GCRef<T> & other) {
		if (!nil()) {
			_heap->unref(_mem);
		}
		if (other.nil()) {
			return *this;
		}
		_heap = other._heap;
		_mem = other._mem;
		_heap->ref(_mem);
		return *this;
	}
	template<typename T>
	T & GCRef<T>::operator* () {
		if (nil()) {
			throw Exception("access nil");
		}
		return (T&)*_mem;
	}
	template<typename T>
	T * GCRef<T>::operator-> () const {
		if (nil()) {
			throw Exception("access nil");
		}
		return (T*)_mem;
	}
	template<typename T>
	T * GCRef<T>::operator& () const {
		if (nil()) {
			throw Exception("access nil");
		}
		return (T*)_mem;
	}
	template<typename T>
	bool GCRef<T>::nil() const {
		return _mem == NULL;
	}



	template<typename T>
	Heap<T>::Ref::Ref() : _mem(NULL), _ref_count(0) {
	}
	template<typename T>
	Heap<T>::Ref::Ref(T * mem) : _mem(mem), _ref_count(0) {
	}
	template<typename T>
	Heap<T>::Ref::~Ref() {
	}
	template<typename T>
	int & Heap<T>::Ref::ref_count() {
		return _ref_count;
	}
	template<typename T>
	void Heap<T>::Ref::ref() {
		_ref_count++;
	}
	template<typename T>
	void Heap<T>::Ref::unref() {
		_ref_count--;
	}
	template<typename T>
	void Heap<T>::Ref::dealloc() {
		delete _mem;
	}
	template<typename T>
	void * Heap<T>::Ref::operator* () {
		return _mem;
	}
	

	template <typename T>
	Heap<T>::Heap() {
	}
	template <typename T>
	Heap<T>::~Heap() {
	}
	template <typename T>
	void Heap<T>::clear() {
		for (typename std::map<void *, Ref>::iterator iter = _mems.begin(); iter != _mems.end(); iter++) {
			iter->second.dealloc();
		}
		_mems.clear();
	}
	template <typename T>
	GCRef<T> Heap<T>::alloc(T * mem) {
		_mems[(void *)mem] = Ref(mem);
		return GCRef<T>(this, mem);
	}
	template <typename T>
	void Heap<T>::ref(void * mem) {
		_mems[mem].ref();
	}
	template <typename T>
	void Heap<T>::unref(void * mem) {
		_mems[mem].unref();
	}
	template <typename T>
	unsigned long Heap<T>::gc() {
		bool cont;
		unsigned long tick = OS::tick_milli();
		do {
			cont = false;
			for (typename std::map<void *, Ref>::iterator iter = _mems.begin(); iter != _mems.end();) {
				if (iter->second.ref_count() <= 0) {
					iter->second.dealloc();
					cont = true;
					_mems.erase(iter++);
				} else {
					iter++;
				}
			}
		} while (cont);
		return OS::tick_milli() - tick;
	}
	template <typename T>
	size_t Heap<T>::size() {
		return _mems.size();
	}
	template <typename T>
	void Heap<T>::print_usage() {
		for (typename std::map<void *, Ref>::iterator iter = _mems.begin(); iter != _mems.end(); iter++) {
			printf(" * %p : %d\n", *(iter->second), iter->second.ref_count() );
		}
	}

	/**
	 * @brief semaphore contained heap
	 */
	template<typename T>
	class SharedHeap : public Heap<T> {
	private:
		Semaphore _sem;
	public:
		SharedHeap() : _sem(1) {
		}
		virtual ~SharedHeap() {
		}
		Semaphore & sem() {
			return _sem;
		}
		void lock() {
			_sem.wait();
		}
		void unlock() {
			_sem.post();
		}
	};
}

#endif
