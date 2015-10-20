//
//  oslayer.mm
//  oslayer
//
//  Created by TJ on 2015. 10. 19..
//  Copyright © 2015년 TJ. All rights reserved.
//

#import "oslayer.h"
#import "os.hpp"

@implementation oslayer

+ (NSString*)nomeaningfulVesion
{
    return @(OS::nomeaningfulVesion().c_str());
}

+ (void)idle:(unsigned long)timeout
{
    OS::idle(timeout);
}

+ (unsigned long)tick_milli
{
    return OS::tick_milli();
}

@end
