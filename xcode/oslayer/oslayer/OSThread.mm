//
//  OSThread.m
//  oslayer
//
//  Created by TJ on 2015. 10. 20..
//  Copyright © 2015년 TJ. All rights reserved.
//

#import "oslayer.h"
#import "os.hpp"

using namespace OS;

/*
 * @brief thread wrapper
 */
class ThreadWrapper : public Thread {
private:
    OSThread * runnable;
    
public:
    ThreadWrapper(OSThread * runnable) : runnable(runnable) {
    }
    virtual ~ThreadWrapper() {
    }
    virtual void run() {
        [runnable run];
    }
};

@interface OSThread ()
{
    ThreadWrapper * wrapper;
}

@end

@implementation OSThread

- (id)init
{
    self = [super init];
    if (self) {
        wrapper = new ThreadWrapper(self);
    }
    return self;
}

- (void)dealloc
{
    if (wrapper) {
        delete wrapper;
        wrapper = NULL;
    }
}

- (void)run
{
    // http://stackoverflow.com/a/1034464
    [NSException raise:NSInternalInconsistencyException format:@"You must override %@ in a subclas", NSStringFromSelector(_cmd)];
}

- (void)start
{
    wrapper->start();
}

- (bool)interrupted
{
    return wrapper->interrupted();
}

- (void)interrupt
{
    wrapper->interrupt();
}

- (void)join
{
    wrapper->join();
}

@end
