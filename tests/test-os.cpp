#include <iostream>
#include <liboslayer/os.hpp>
#include <liboslayer/FileStream.hpp>
#include "utils.hpp"

using namespace std;
using namespace osl;

static void test_time() {
	osl_time_t ti = osl_get_time();
	printf("%lu.%lu (0x%lx.0x%lx)\n", ti.sec, ti.nano, ti.sec, ti.nano);
}

static void test_file_io() {

	File file("big");

	FileStream writer(file, "wb");
	string text = "abcdefghijklmnopqrstuvwxyz";
	ASSERT(writer.write(text.c_str(), text.length()), ==, text.length());
	writer.close();
	
	FileStream reader(file, "rb+");
	char buffer[1024] = {0,};
	reader.read(buffer, sizeof(buffer));
	ASSERT(string(buffer), ==, "abcdefghijklmnopqrstuvwxyz");

	reader.seek(5);
	memset(buffer, 0, sizeof(buffer));
	reader.read(buffer, sizeof(buffer));
	ASSERT(string(buffer), ==, "fghijklmnopqrstuvwxyz");
	reader.close();

	file.remove();
}

static void test_library() {
#if defined(USE_MS_WIN)

	Library lib("..\\Debug", "libhello");

	/*void (*func)() = (void (*)())lib.getSymbol("hello").getHandle();
	func();*/
	lib.getSymbol("hello").asFunc()();
#endif
}

static void print_date(const Date & date) {
	cout << Date::format(date) << "/" << Date::formatRfc1123(date) << endl;
}

static void test_date() {
	Date date = Date::now();

	cout << "[now]" << endl;
	print_date(date);
	cout << "[gmt]" << endl;
	print_date(date.toGmt());
	cout << "[gmt now-1]" << endl;
	date.setGmtOffset(date.getGmtOffset() - 60);
	print_date(date.toGmt());
	cout << "[gmt 0]" << endl;
	date.setGmtOffset(0);
	print_date(date.toGmt());

	cout << "[osl date test]" << endl;
	cout << " * offset: " << date.getGmtOffset() << " (" << ((double)date.getGmtOffset() / 60.0) << ")" << endl;
	cout << " * time: " << date.getTime().sec << endl;
}

static void test_c_date() {
	time_t currtime;
	struct tm * timeinfo;
	time_t utc;
	time_t local;
	time_t base = {0,};
	double offset;

	printf(" * CLOCKS_PER_SEC : %ld\n", CLOCKS_PER_SEC);

	time(&currtime);
	timeinfo = gmtime(&currtime);
	utc = mktime(timeinfo);
	timeinfo = localtime(&currtime);
	local = mktime(timeinfo);

	offset = difftime(local, utc);

	printf("offset : %f => %f\n", offset, offset / (60.0 * 60.0));

	printf("curr : %ld, base : %f (diff: %f)\n", currtime, difftime(utc, base), currtime - difftime(utc, base));
	printf("calc : %f\n", difftime(utc, base) + offset);

	ASSERT(currtime, ==, (long long)(difftime(utc, base) + offset));
}

int main(int argc, char *args[]) {

	try {
		test_time();
		test_file_io();
		test_library();
		test_date();
		test_c_date();
	} catch (Exception e) {
		cerr << e.toString() << endl;
		return 1;
	}
    
    return 0;
}
