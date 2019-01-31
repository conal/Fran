// Simple behavior implementation

#include "StdAfx.h"
#include "Behavior.h"

// C interface

static DWORD startTimeMS = timeGetTime();

EXT_API SpriteTime CurrentSpriteTime()
{
    return (timeGetTime() - startTimeMS) / 1000.0;
}

// When a behavior is updated with SetGoal(goalTime,goalVal), should the
// sprite engine interpolate from the *current* time and value, or the
// previous goal time and value.  Ideally, they would be the same.
BOOL behaviorMakeContinuous = FALSE;

// When a behavior is sampled past its end, should it continue sampling
// its linear function (true) or stop (false)?
BOOL behaviorSamplePastGoal = FALSE;

// end of C interface


// Construct as a constant function
LinearDouble::LinearDouble(double startVal, SpriteTime startTime)
{ 
    SetMotion (startVal, startTime, startVal, startTime);
}

void LinearDouble::SetMotion(double startVal, SpriteTime startTime,
                             double goalVal, SpriteTime goalTime)
{
    // Protect from at()
    m_critSec.Lock();
    
    m_startTime = startTime;
    m_startVal  = startVal;
    m_goalTime  = goalTime;
    m_goalVal   = goalVal;
    m_speed     = goalTime==startTime ? 0 : 
                    (goalVal - startVal)/(goalTime - startTime),

    m_critSec.Unlock();
}


void LinearDouble::SetGoalValue (double goalVal, SpriteTime goalTime)
{
    SpriteTime startTime;
    double startVal;

    // Decide on new start time and value.
    if (behaviorMakeContinuous) {        // See comment above
        startTime = CurrentSpriteTime();
        startVal = at(startTime);
    } else {
        startTime = m_goalTime;
        startVal = m_goalVal;
    }
    //printf("SetGoalValue(%.3f at %.3f), from %.3f at %.3f.  Projected %.3f\n", goalVal, goalTime, startVal, startTime, at(goalTime));fflush(stdout);

    // And then set the motion.
    SetMotion(startVal, startTime, goalVal, goalTime);
}

// Almost not needed externally.  We should be able to exploit this fact
// in optimizing the representation for fast atToInt.  Consider
// later, say when doing incremental sampling.  Perhaps use a very
// fast fixpoint method.  (Currently used by Sprite::Paint for scaling.)
double LinearDouble::at (SpriteTime t)
{
    // Protect from SetMotion()
    m_critSec.Lock();

    // Calculate value at t.  If the current time has caught up with our
    // goal time and we're not asked to sample past the goal, then just
    // use the final value.  Otherwise use the linear computation.
    double val = (!behaviorSamplePastGoal && t >= m_goalTime)
        ? m_goalVal
        : m_startVal + (t-m_startTime) * m_speed ;

    m_critSec.Unlock();

    return val;
}

int LinearDouble::atToInt (SpriteTime t)
{ return (int) (at(t) + 0.5); }
