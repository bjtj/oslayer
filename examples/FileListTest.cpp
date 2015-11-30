#include <iostream>
#include <liboslayer/os.hpp>

using namespace std;
using namespace OS;

#if defined(USE_MS_WIN)

void s_list_test(const string & path) {

	// https://msdn.microsoft.com/en-us/library/windows/desktop/aa365200%28v=vs.85%29.aspx

	string dir = path;
	dir.append("\\*");

	WIN32_FIND_DATAA ffd;
	LARGE_INTEGER filesize;
	HANDLE hFind = INVALID_HANDLE_VALUE;
	DWORD dwError = 0;

	hFind = FindFirstFileA(dir.c_str(), &ffd);
	if (hFind == INVALID_HANDLE_VALUE) {
		return;
	}

	do {

		if (ffd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) {
			cout << ffd.cFileName << "/" << endl;
		} else {
			filesize.LowPart = ffd.nFileSizeLow;
			filesize.HighPart = ffd.nFileSizeHigh;
			cout << ffd.cFileName << " - " << filesize.QuadPart << " bytes" << endl;
		}

	} while (FindNextFileA(hFind, &ffd) != 0);

	FindClose(hFind);
}

#endif

void s_list_test_oslayer(const string & path) {
	
	vector<File> files = File::list(path);
	for (vector<File>::iterator iter = files.begin(); iter != files.end(); iter++) {
		bool dir = iter->isDirectory();
		cout << iter->getName() << (dir ? "/" : "") << endl;
	}
}

int main(int argc, char * args[]) {
	
	//s_list_test(".");
	s_list_test_oslayer(".");

	getchar();

	return 0;
}