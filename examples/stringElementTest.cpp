#include <iostream>
#include <liboslayer/os.hpp>
#include <liboslayer/Text.hpp>
#include <liboslayer/StringElement.hpp>

using namespace std;
using namespace UTIL;

void s_test_stringmap() {
    StringMap props;
    
    props["a"] = "A";
    props["b"] = "B";
    props["c"] = "C";
    
    for (StringMap::const_iterator iter = props.begin(); iter != props.end(); iter++) {
        cout << iter->first << " : " << iter->second << endl;
    }
}

void s_print_stringlistmap(StringListMap & parameters) {
    for (StringListMap::iterator iter = parameters.begin(); iter != parameters.end(); iter++) {
        string name = iter->first;
        vector<string> & values = iter->second;
        
        for (size_t i = 0; i < values.size(); i++) {
            cout << name  << " : " << values[i] << endl;
        }
    }
}

void s_print_namevaluelist(const NameValueList & lst) {
    
    for (NameValueList::const_iterator iter = lst.begin(); iter != lst.end(); iter++) {
        const string & name = iter->getName();
        const string & value = iter->getValue();
        cout << name << " : " << value << endl;
    }
}

void s_test_stringlistmap() {
    StringListMap parameters;
    
    parameters.append("a", "a1");
    parameters.append("a", "a2");
    parameters.append("a", "a3");
    
    parameters.append("b", "b1");
    
    s_print_stringlistmap(parameters);
    cout << endl;
    
    cout << Text::toString(parameters.toNameValueList()) << endl;
    cout << endl;
    
    cout << Text::toString(parameters.toNameValueList(), "=") << endl;
    cout << endl;
    
    cout << Text::toString(parameters.toNameValueList(), "=", "&") << endl;
    cout << endl;
    
    parameters.erase("a");
    
    s_print_namevaluelist(parameters.toNameValueList());
}

int main(int argc, char * args[]) {
    
    s_test_stringlistmap();
    
    return 0;
}