#include <iostream>
#include <liboslayer/os.hpp>

using namespace std;
using namespace OS;

int main(int argc, char * args[]) {
    
    unsigned long t = tick_milli();
    for (int i = 0; i < 100; i++) {
        idle(10);
        cout << "..." << endl;
    }
    unsigned long dur = tick_milli() - t;
    cout << dur << endl;
    
    return 0;
}