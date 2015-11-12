#include <iostream>
#include <liboslayer/os.hpp>

using namespace std;

void func() {
    OS::System::getInstance();
}

int main(int argc, char * args[]) {
    
    OS::System::getInstance();
    func();
    
    cout << "program" << endl;
    
	getchar();

    return 0;
}
