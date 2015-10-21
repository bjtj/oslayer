//
//  OSSocket.m
//  oslayer
//
//  Created by TJ on 2015. 10. 21..
//  Copyright © 2015년 TJ. All rights reserved.
//

#import "oslayer.h"
#import "os.hpp"

using namespace OS;

@interface OSSocket ()
{
    Socket * socket;
}

@property (nonatomic) NSString * hostname;
@property (nonatomic) int port;

@end

@implementation OSSocket

- (id)initWithAddress:(NSString *)hostname :(int)port
{
    self = [super init];
    if (self) {
        socket = new Socket([hostname UTF8String], port);
    }
    return self;
}

- (void)connect
{
    socket->connect();
}

- (NSInteger)send:(const char *)buffer :(int)max
{
    return socket->send(buffer, max);
}

- (NSInteger)recv:(char *)buffer :(int)max
{
    return socket->recv(buffer, max);
}

- (void)close
{
    socket->close();
}

@end