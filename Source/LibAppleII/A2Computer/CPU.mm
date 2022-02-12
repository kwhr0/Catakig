/*	class A2Computer (category CPU)

	Routines and tables for emulating the 65c02 microprocessor, and the
	low-level I/O behavior of the Apple II.
*/
#import "LibAppleII-Priv.h"
#import "RW.h"
#import "W65C02.h"

@implementation A2Computer (CPU)
//---------------------------------------------------------------------------

static uint8_t		gOldTimer,		// residual cycles from last step
					gSpkrState;		// speaker state: 0 or 0xFF

uint32_t		gSpkrOut[kA2SamplesPerStep + kTapRatio - 1];

//---------------------------------------------------------------------------

+ (void)_InitCPU
{/*
	Initializes various lookup tables used in method '-RunForOneStep',
	defined below.  Called only once, from '+initialize'.
*/
}

//---------------------------------------------------------------------------

- (void)RunForOneStep:(int)loopN audio:(uint8_t [])audioOut
{/*
	Executes 65c02 instructions for one time step: about 17,030 CPU cyles,
	or 1/60th of an emulated second.  Also computes the 8-bit audio waveform
	for the time step and writes it to the array 'audioOut'.

	No attempt is made here to keep emulation time in sync with real time.
	It's up to the library user to call this method every 60th of a second,
	and keep the audio stream playing.
*/
	if (mHalts) // != 0, then emulation is forbidden just now
	{
		if (audioOut) [self _DefaultAudio:audioOut:kA2SamplesPerStep];
		return;
	}
	const int SLOW_DET = 2;
	if (self.running) {
		if (++mSlow == SLOW_DET) fprintf(stderr, "slowdown detected.\n");
		return;
	}
	else {
		if (mSlow >= SLOW_DET) fprintf(stderr, "slowdown recovered.\n");
		mSlow = 0;
	}
	self.running = TRUE;
	mAudioActive = audioOut != nil;
	for (int loop = 0; loop < loopN; loop++) {
		mT = gOldTimer;
		mMpu->Remap(mFlags);

		mCycles += 17030; // CPU cycles per step

		for (int scanLine = 0;  scanLine < 262;)
		{
			//mVideoFlags[scanLine++] = mFlags;
			mScanLine = ++scanLine;
			mT -= 65;
			mT = mMpu->Execute(-mT);
		}

		unsigned p;
		gOldTimer	= mT;
		if (mAudioActive) {
			p = gSpkrState;
			for (int i = 0;  i < kA2SamplesPerStep;  ++i)
			{
				mT = gSpkrOut[i];
				p ^= mT>>8;
				audioOut[i] = p ^ mT>>24;
				p ^= mT;
			}
			gSpkrState = p;

			for (int i = kTapRatio-1;  --i >= 0;)
				gSpkrOut[i] = (gSpkrOut + kA2SamplesPerStep)[i];

			mT = A2T.audio.flat;
			for (int i = kA2SamplesPerStep;  --i >= 0;)
				(gSpkrOut + kTapRatio - 1)[i] = mT;
		}
	}
	self.running = FALSE;
}

//---------------------------------------------------------------------------
@end
