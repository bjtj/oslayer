#include <liboslayer/TestSuite.hpp>
#include <liboslayer/Ref.hpp>

using namespace osl;


class _cls {
private:
    Ref<int> _n;
public:
    _cls(const Ref<int> & n) : _n(n) {
    }
    virtual ~_cls() {}
    Ref<int> & n() {
	return _n;
    }
};

/**
 * ref test case
 */
class RefTestCase : public TestCase {
public:
    RefTestCase() : TestCase("ref-test-case") {
    }
    virtual ~RefTestCase() {
    }
    virtual void test() {
	int i = 10;
	Ref<int> num;
	ASSERT(num.nil(), ==, true);
	num = &i;
	ASSERT(num.nil(), ==, false);
	ASSERT(*num, ==, 10);

	Ref<int> num2(num);
	ASSERT(num2.nil(), ==, false);
	ASSERT(*num2, ==, 10);

	*num2 = 20;
	ASSERT(*num2, ==, 20);
	ASSERT(*num, ==, 20);

	Ref<int> r(&num);
	_cls c(r);
	// _cls c(Ref<int>(&num)); // not working -- TODO: fixme
	ASSERT(*c.n(), ==, 20);
    }
};


int main(int argc, char *argv[])
{
    TestSuite ts;
    ts.addTestCase(AutoRef<TestCase>(new RefTestCase));
    TestReport report(ts.testAll());
    report.validate();
    
    return 0;
}
