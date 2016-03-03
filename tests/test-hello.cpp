#include <iostream>
#include <liboslayer/os.hpp>

using namespace std;
using namespace OS;

int main(int argc, char *args[]) {

	cout << endl;
	cout << " **************** hello ****************" << endl;
	cout << "tick: " << tick_milli() << endl;
	Date now = Date::now();
	if (now.getYear() != 2016) {
		exit (1);
	}
	cout << endl;
	
    return 0;
}
