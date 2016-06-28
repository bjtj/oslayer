#include <liboslayer/Register.hpp>
#include <liboslayer/TestSuite.hpp>
#include <liboslayer/Text.hpp>

using namespace std;
using namespace UTIL;

/**
 * @brief 
 */
class User {
private:
	string _name;
	string _password;
public:
	User(const string & name, const string & password) : _name(name), _password(password) {
	}
	virtual ~User() {
	}
	string & name() {
		return _name;
	}
	string & password() {
		return _password;
	}
	bool operator== (const User & other) {
		return (other._name == _name && other._password == _password);
	}
};

/**
 * @brief 
 */
class RegisterTestCase : public TestCase {
public:
	RegisterTestCase() : TestCase("reigster test") {
	}
	virtual ~RegisterTestCase() {
	}
	virtual void test() {
		Register<User> users;

		ASSERT(users.size(), ==, 0);
		ASSERT(users.reg(User("tester", "password")), ==, 0);
		ASSERT(users.reg(User("tester2", "password2")), ==, 1);
		ASSERT(users.size(), ==, 2);
		for (size_t i = 0; i < users.size(); i++) {
			switch (i) {
			case 0:
				ASSERT(users[i].name(), ==, "tester");
				ASSERT(users[i].password(), ==, "password");
				break;
			case 1:
				ASSERT(users[i].name(), ==, "tester2");
				ASSERT(users[i].password(), ==, "password2");
				break;
			default:
				break;
			}
		}

		vector<User> lst = users.list();
		ASSERT(lst.size(), ==, 2);
		for (size_t i = 0; i < lst.size(); i++) {
			switch (i) {
			case 0:
				ASSERT(lst[i].name(), ==, "tester");
				ASSERT(lst[i].password(), ==, "password");
				break;
			case 1:
				ASSERT(lst[i].name(), ==, "tester2");
				ASSERT(lst[i].password(), ==, "password2");
				break;
			default:
				break;
			}
		}
		
		users.clear();
		ASSERT(users.size(), ==, 0);
		ASSERT(users.reg(User("tester", "password")), ==, 0);
		ASSERT(users.reg(User("tester2", "password2")), ==, 1);
		ASSERT(users.size(), ==, 2);

		users.unreg(0);
		ASSERT(users.size(), ==, 1);
		ASSERT(users[0].name(), ==, "tester2");
		ASSERT(users[0].password(), ==, "password2");

		// 
		ASSERT(users.reg(User("tester", "password")), ==, 2);
		ASSERT(users.reg(User("x-tester", "password")), ==, 3);
		ASSERT(users.size(), ==, 3);

		class MyTester : public Register<User>::Tester {
		public:
			MyTester() {
			}
			virtual ~MyTester() {
			}
			virtual bool test(User & user) const {
				return Text::startsWith(user.name(), "tester");
			}
		};
		lst = users.query(MyTester());
		ASSERT(lst.size(), ==, 2);
		for (size_t i = 0; i < lst.size(); i++) {
			switch (i) {
			case 0:
				ASSERT(lst[i].name(), ==, "tester2");
				ASSERT(lst[i].password(), ==, "password2");
				break;
			case 1:
				ASSERT(lst[i].name(), ==, "tester");
				ASSERT(lst[i].password(), ==, "password");
				break;
			default:
				break;
			}
		}
		users.seed() = 100;
		ASSERT(users.reg(User("y-man", "0000")), ==, 100);
		ASSERT(users.reg(User("y-woman", "1234")), ==, 101);

		ASSERT(users.containsById(100), ==, true);

		users.unreg(100);

		ASSERT(users.containsById(100), ==, false);

		ASSERT(users.contains(User("y-woman", "1234")), ==, true);
	}
};


int main(int argc, char *args[]) {

	TestSuite ts;
	ts.addTestCase(AutoRef<TestCase>(new RegisterTestCase));

	TestReport report(ts.testAll());
	report.validate();
	
    return 0;
}
