#include <liboslayer/TestSuite.hpp>
#include <liboslayer/Optional.hpp>

using namespace std;
using namespace osl;

class Obj
{
private:
    int _i;
public:
    Obj(int i) : _i(i) {
	cout << "construct -- i is '" << _i << "'" << endl;
    }
    Obj(const Obj & other) {
	_i = other._i;
	cout << "copy -- i is '" << _i << "'" << endl;
    }
    virtual ~Obj() {
	cout << "destroy -- i is '" << _i << "'" << endl;
    }
    int & i() {
	return _i;
    }
};


class OptionalTestCase : public TestCase
{
private:
    vector<Obj> objs;
public:
    OptionalTestCase() : TestCase("optional-testcase") {
    }
    virtual ~OptionalTestCase() {
    }
    virtual void test() {
	Optional<Obj> obj = getObj(0);
	ASSERT(obj.nil(), ==, true);
	objs.push_back(Obj(1));
	obj = getObj(1);
	ASSERT(obj.nil(), ==, false);
	ASSERT(obj->i(), ==, 1);
	ASSERT((*obj).i(), ==, 1);
	ASSERT((&obj)->i(), ==, 1);
    }

    Optional<Obj> getObj(int i) {
	vector< Obj>::iterator iter = objs.begin();
	for (; iter != objs.end(); ++iter) {
	    if (iter->i() == i) {
		return Optional<Obj>(*iter);
	    }
	}
	return Optional<Obj>();
    }
};


int main(int argc, char *argv[])
{
    TestSuite ts;
    ts.addTestCase(AutoRef<TestCase>(new OptionalTestCase));

    TestReport report(ts.testAll());
    ASSERT(report.failed(), ==, 0);
    
    return 0;
}
