#include <iostream>
#include <liboslayer/os.hpp>
#include <liboslayer/StringElement.hpp>

using namespace std;
using namespace UTIL;

int main(int argc, char * args[]) {
    
    StringMap props;
    
    props["a"] = "A";
    props["b"] = "B";
    props["c"] = "C";
    
    for (StringMap::const_iterator iter = props.begin(); iter != props.end(); iter++) {
        cout << iter->first << " : " << iter->second << endl;
    }
    
    return 0;
}