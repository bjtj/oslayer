#include <iostream>
#include <liboslayer/Text.hpp>

using namespace std;
using namespace UTIL;


int main(int argc, char * args[]) {
    
    short int si = -1;
    int i = -1;
    long int li = -1;
    long long int lli = -1;
    
    unsigned short int usi = ~0;
    unsigned int ui = ~0;
    unsigned long int uli = ~0;
    unsigned long long int ulli = ~0;

    cout << Text::toString(si) << endl;
    cout << Text::toString(i) << endl;
    cout << Text::toString(li) << endl;
    cout << Text::toString(lli) << endl;
    cout << Text::toString(usi) << endl;
    cout << Text::toString(ui) << endl;
    cout << Text::toString(uli) << endl;
    cout << Text::toString(ulli) << endl;
    
    return 0;
}