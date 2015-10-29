//
//  trycatch.cpp
//  http-server-console
//
//  Created by TJ on 2015. 10. 29..
//  Copyright © 2015년 TJ. All rights reserved.
//

#include <iostream>

using namespace std;

class ScopedConnection {
public:
    ScopedConnection() {
        cout << "scope begin" << endl;
    }
    virtual ~ScopedConnection() {
        cout << "scope end" << endl;
    }
};

bool failed() {
    return true;
}

void troublemaker() {
    ScopedConnection conn;
    cout << "troublemaker 1" << endl;
    if (failed()) {
        throw -1;
    }
    cout << "troublemaker 2" << endl;
}

int main(int argc, char * args[]) {
    
    try {
//        ScopedConnection conn;
        cout << "my code 1" << endl;
//        if (failed()) {
//            throw -1;
//        }
        troublemaker();
        cout << "my code 2" << endl;
    } catch (...) {
        cout << "exception" << endl;
    }
    return 0;
}