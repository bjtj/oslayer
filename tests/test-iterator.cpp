#include <liboslayer/TestSuite.hpp>
#include <liboslayer/Iterator.hpp>

using namespace std;
using namespace osl;


class Object {
private:
    string _name;
    string _detail;
public:
    Object() {
    }
    Object(const string & name, const string & detail)
	: _name(name), _detail(detail) {
    }
    virtual ~Object() {
    }
    string & name() {
	return _name;
    }
    string & detail() {
	return _detail;
    }
};

class NameCondition : public Condition {
private:
    string _name;
public:
    NameCondition(const string & name) : _name(name) {
    }
    virtual ~NameCondition() {
    }
    virtual bool test(void * t) const {
	Object * obj = (Object*)t;
	return obj->name() == _name;
    }
};

class DetailCondition : public Condition {
private:
    string _detail;
public:
    DetailCondition(const string & detail) : _detail(detail) {
    }
    virtual ~DetailCondition() {
    }
    virtual bool test(void * t) const {
	Object * obj = (Object*)t;
	return obj->detail() == _detail;
    }
};


class IteratorTestCase : public TestCase {
public:
    IteratorTestCase() : TestCase("iterator test") {
    }
    virtual ~IteratorTestCase() {
    }
    virtual void test() {
	vector<Object> objs;
	objs.push_back(Object("name1", "detail"));
	objs.push_back(Object("name2", "detail"));
		
	Iterator<Object> iter(objs);

	for (;iter.avail();iter++) {
	    cout << "[" << iter.idx() << "] " << iter->name() << " / " << iter->detail() << endl;
	}
    }
};


int main(int argc, char *args[]) {

    TestSuite ts;
    ts.addTestCase(AutoRef<TestCase>(new IteratorTestCase));

    TestReport report(ts.testAll());
    report.validate();
    
    return 0;
}
