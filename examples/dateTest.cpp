#include <liboslayer/os.hpp>

using namespace std;
using namespace OS;

int main(int argc, char * args[]) {
	while (1) {
		Date date = Date::now();
		printf("%04d-%02d-%02d %02d:%02d:%02d.%d", date.getYear(), date.getMonth(), date.getDay(), date.getHour(), date.getMinute(), date.getSecond(), date.getMillisecond());
		getchar();
	}	
}