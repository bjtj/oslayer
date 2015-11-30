#include <iostream>
#include <liboslayer/Text.hpp>

using namespace std;
using namespace UTIL;

void s_test(const string & str) {
	string ltrim = Text::ltrim(str);
	cout << "\"" << ltrim << "\" - " << ltrim.size() << endl;
}

int main(int argc, char * args[]) {

	s_test("");
	s_test("# sample");
	s_test(" wow");
	s_test("\n");
	s_test(" \t\r\n");
	s_test(" \t\t\tabc");

	getchar();

	return 0;
}