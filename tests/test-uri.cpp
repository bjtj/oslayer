#include <liboslayer/Uri.hpp>
#include <liboslayer/TestSuite.hpp>

using namespace std;
using namespace OS;
using namespace UTIL;


class UriTestCase : public TestCase {
public:
    UriTestCase() :  TestCase("uri-test") {
	}
    virtual ~UriTestCase() {
	}
	virtual void test() {
		Uri uri = Uri::readFromString("urn:");
		ASSERT(uri.scheme(), ==, "urn");

		uri = Uri::readFromString("http://google.com/");
		ASSERT(uri.scheme(), ==, "http");
		ASSERT(uri.username(), ==, "");
		ASSERT(uri.password(), ==, "");
		ASSERT(uri.hostname(), ==, "google.com");
		ASSERT(uri.port(), ==, 0);
		ASSERT(uri.path(), ==, "/");

		uri = Uri::readFromString("http://google.com/hello");
		ASSERT(uri.scheme(), ==, "http");
		ASSERT(uri.hostname(), ==, "google.com");
		ASSERT(uri.port(), ==, 0);
		ASSERT(uri.path(), ==, "/hello");

		uri = Uri::readFromString("http://google.com/hello?a=b&c=d");
		ASSERT(uri.scheme(), ==, "http");
		ASSERT(uri.username(), ==, "");
		ASSERT(uri.password(), ==, "");
		ASSERT(uri.hostname(), ==, "google.com");
		ASSERT(uri.port(), ==, 0);
		ASSERT(uri.path(), ==, "/hello");
		ASSERT(uri.query(), ==, "a=b&c=d");
		ASSERT(uri.fragment(), ==, "");

		uri = Uri::readFromString("http://user:pass@google.com/hello?a=b&c=d");
		ASSERT(uri.scheme(), ==, "http");
		ASSERT(uri.username(), ==, "user");
		ASSERT(uri.password(), ==, "pass");
		ASSERT(uri.hostname(), ==, "google.com");
		ASSERT(uri.port(), ==, 0);
		ASSERT(uri.path(), ==, "/hello");
		ASSERT(uri.query(), ==, "a=b&c=d");
		ASSERT(uri.fragment(), ==, "");

		uri = Uri::readFromString("http://user:pass@google.com/hello?a=b&c=d#myfrag");
		ASSERT(uri.scheme(), ==, "http");
		ASSERT(uri.username(), ==, "user");
		ASSERT(uri.password(), ==, "pass");
		ASSERT(uri.hostname(), ==, "google.com");
		ASSERT(uri.port(), ==, 0);
		ASSERT(uri.path(), ==, "/hello");
		ASSERT(uri.query(), ==, "a=b&c=d");
		ASSERT(uri.fragment(), ==, "myfrag");

		uri = Uri::readFromString("http://user:pass@google.com:80/hello?a=b&c=d#myfrag");
		ASSERT(uri.scheme(), ==, "http");
		ASSERT(uri.username(), ==, "user");
		ASSERT(uri.password(), ==, "pass");
		ASSERT(uri.hostname(), ==, "google.com");
		ASSERT(uri.port(), ==, 80);
		ASSERT(uri.path(), ==, "/hello");
		ASSERT(uri.query(), ==, "a=b&c=d");
		ASSERT(uri.fragment(), ==, "myfrag");
	}
};


/**
 * @brief 
 */
int main(int argc, char *args[]) {

	TestSuite ts;
	ts.addTestCase(AutoRef<TestCase>(new UriTestCase));

	TestReport report(ts.testAll());
	report.validate();
    
    return 0;
}
