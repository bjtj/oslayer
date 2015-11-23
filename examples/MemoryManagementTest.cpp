#include <iostream>
#include <vector>
#include <algorithm>
#include <liboslayer/AutoRef.hpp>

using namespace std;
using namespace UTIL;

class Item {
private:
    int id;
public:
    Item(int id) : id(id) {
        cout << "construct item" << endl;
    }
    virtual ~Item() {
        cout << "destruct item" << endl;
    }
    void hello() {
        cout << "hello " << id << endl;
    }
};

class Container {
private:
    vector<AutoRef<Item> > vec;
public:
    Container() {
        cout << "construct container" << endl;
    }
    virtual ~Container() {
        cout << "destruct container" << endl;
    }
    void add(AutoRef<Item> item) {
        cout << "add" << endl;
        vec.push_back(item);
    }
    
    AutoRef<Item> getItem(size_t i) {
        return vec[i];
    }
    
    void hello() {
        for (size_t i = 0; i < vec.size(); i++) {
            vec[i]->hello();
        }
    }
};

void s_container() {
    Container container;
    
    container.add(AutoRef<Item>(new Item(0)));
    container.add(AutoRef<Item>(new Item(1)));
    container.add(AutoRef<Item>(new Item(2)));
    
    container.hello();
    
    container.getItem(1)->hello();
}

class Worker {
private:
    AutoRef<Item> item;
public:
    Worker(AutoRef<Item> item) : item(item) {
    }
    virtual ~Worker() {
    }
    
    void doJob() {
        item->hello();
    }
};

void s_work() {
    Worker worker(AutoRef<Item>(new Item(1)));
    worker.doJob();
}

void s_lazy() {
    
    AutoRef<Item> ref;
    
    Item * item = new Item(2);
    
    ref = item;
    
    ref->hello();
}

void s_exchange() {
    AutoRef<Item> ref1;
    AutoRef<Item> ref2(new Item(3));
    
    ref1 = ref2;
    
    ref1->hello();
    ref2->hello();
}

class MyThread : public OS::Thread {
private:
    AutoRef<Item> item;
public:
    MyThread() {
    }
    virtual ~MyThread() {
    }
    
    void setItem(AutoRef<Item> item) {
        this->item = item;
    }
    
    virtual void run() {
        OS::idle(1000);
        item->hello();
    }
};

AutoRef<Item> s_get_item(int id) {
    return AutoRef<Item>(new Item(id));
}

MyThread * s_test_thread(AutoRef<Item> item) {
    MyThread * thread = new MyThread;
    thread->setItem(item);
    thread->start();
    return thread;
}

void s_test_thread_bypass() {
    AutoRef<Item> item = s_get_item(3);
    
    MyThread * thread = s_test_thread(item);
    
    getchar();
    
    delete thread;
}

void s_replace() {
    AutoRef<Item> ref1(new Item(1));
    AutoRef<Item> ref2(new Item(2));
    
    ref1->hello();
    ref2->hello();
    
    ref1 = ref2;
    
    ref1->hello();
    ref2->hello();
}

void s_recycle() {
    
    AutoRef<Item> ref;
    
    for (int i = 0; i < 3; i++) {
        AutoRef<Item> item(new Item(i));
        ref = item;
        
        ref->hello();
    }
}

void s_initialize() {
    AutoRef<Item> ref = AutoRef<Item>(new Item(2));
    ref->hello();
    
    ref = NULL;
    
    cout << "do more..." << endl;
}

void s_nil() {
    AutoRef<Item> ref;
    AutoRef<Item> ref2(new Item(2));
    
    if (ref.empty()) {
        cout << "nil" << endl;
    } else {
        ref->hello();
    }
    
    ref = ref2;
    
    if (ref.empty()) {
        cout << "nil" << endl;
    } else {
        ref->hello();
    }
    
    ref2 = ref;
    
    cout << "done" << endl;
}

void s_ref() {
    AutoRef<Item> ref;
    
    printf("%p\n", &ref);
    
    ref = AutoRef<Item>(new Item(2));
    
    printf("%p\n", &ref);
}

void s_void() {
	//AutoRef<void> ref;
    //AutoRef<void> ref(new Item(1));
}

int main(int argc, char * args[]) {
    
    //s_container();
    //s_work();
    //s_lazy();
    //s_exchange();
    //s_test_thread_bypass();
    //s_replace();
    //s_recycle();
    //s_initialize();
    //s_nil();
    //s_ref();
    //s_void();

	printf("press any key to exit.");
	getchar();
    
    return 0;
}