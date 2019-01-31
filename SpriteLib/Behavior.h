// Very simple behavior types


#ifndef _BEHAVIOR_H
#define _BEHAVIOR_H

#include "cdecls.h"
#include "GlobalVar.h"

typedef double SpriteTime;

EXT_API(SpriteTime) CurrentSpriteTime();

// When a behavior is updated with SetGoal(goalTime,goalVal), should the
// sprite engine interpolate from the *current* time and value, or the
// previous goal time and value.  Ideally, they would be the same.
// Default FALSE.

declare_global(BOOL, behaviorMakeContinuous);

// When a behavior is sampled past its end, should it continue sampling
// its linear function (true) or stop (false)?  Default FALSE.
declare_global(BOOL, behaviorSamplePastGoal);


#ifdef __cplusplus
// C++ interfaces to constructors, destructors, and methods.

// Simple behavior classes

class AFX_EXT_CLASS BehaviorDouble {
public:
    // Just the sampling method.  Out for now to avoid the virtual call.
    // virtual double at(SpriteTime t) = 0;
};


class AFX_EXT_CLASS LinearDouble : public BehaviorDouble {
public:
    // Construct as a constant function
    // To do: make startTime be a required constructor argument
    // here and in container classes like Sprite
    LinearDouble(double startVal, SpriteTime startTime = CurrentSpriteTime());
    int atToInt (SpriteTime t);
    void SetMotion(double startVal, SpriteTime startTime,
                   double goalVal, SpriteTime goalTime);
    void SetGoalValue (double goalVal, SpriteTime goalTime);
    double at (SpriteTime t);
private:
    SpriteTime m_startTime, m_goalTime;
    double m_startVal, m_goalVal, m_speed;
    CCriticalSection m_critSec;
};


#endif __cplusplus
#endif /* _BEHAVIOR_H */
