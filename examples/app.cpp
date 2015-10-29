//
//  app.cpp
//  http-server-console
//
//  Created by TJ on 2015. 10. 29..
//  Copyright © 2015년 TJ. All rights reserved.
//

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
    
    return 0;
}
