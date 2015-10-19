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
