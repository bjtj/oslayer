//
//  oslayer.h
//  oslayer
//
//  Created by TJ on 2015. 10. 19..
//  Copyright © 2015년 TJ. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface oslayer : NSObject

+ (NSString*)nomeaningfulVesion;
+ (void)idle:(unsigned long)timeout;
+ (unsigned long)tick_milli;

@end

@interface OSThread : NSObject

- (void)run;
- (void)start;
- (bool)interrupted;
- (void)interrupt;
- (void)join;

@end

@interface OSSocket : NSObject

- (id)initWithAddress:(NSString*)hostname :(int)port;
- (void)connect;
- (NSInteger)send:(const char*)buffer :(int)max;
- (NSInteger)recv:(char*)buffer :(int)max;
- (void)close;

@end
