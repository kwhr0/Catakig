#import "Catakig-Cocoa.h"
#import "MyDocument.h"
#import "ScreenView.h"
#import "IndicatorLight.h"

@implementation MyDocument (Audio)
//---------------------------------------------------------------------------

enum
{
	kSampleRate			= 22050,
	kNumSpeeds			= 8,
	kSpeedMask			= kNumSpeeds - 1,
	kBytesPerChannel	= 1,
	kUnsigned			= YES,
	kFormatFlags		= 0,

#if 0
	kFormatFlags2		= kAudioFormatFlagIsSignedInteger
	#if __BIG_ENDIAN__
						| kAudioFormatFlagIsBigEndian
	#endif
						| kAudioFormatFlagIsNonInterleaved
						| kAudioFormatFlagIsPacked,
#endif
};

//---------------------------------------------------------------------------
int gAudioCallbackCount;

static int loopN;

static OSStatus InputProc(
	ScreenView*					_screen,
	AudioUnitRenderActionFlags*	ioActionFlags,
	const AudioTimeStamp*		timeStamp,
	UInt32						busNumber,
	UInt32						nFrames,
	AudioBufferList*			ioData)
{
	@autoreleasepool {
		BOOL generated = FALSE;
		NSEnumerator *e = [[G.docMgr documents] objectEnumerator];
		MyDocument *doc;
		while ((doc = [e nextObject])) {
			ScreenView *screen = ((MyDocument *)doc)->mScreen;
			if (!screen) continue;
			A2Computer *a2 = screen->mA2;
			if (loopN < 10) {
				BOOL f = screen == G.activeScreen;
				screen->mRunForOneStep(a2, nil, loopN, f ? ioData->mBuffers[0].mData : nil);
				generated |= f;
			}
			else dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
				screen->mRunForOneStep(a2, nil, loopN, nil);
			});
		}
		if (!generated) memset(ioData->mBuffers[0].mData, 128, kA2SamplesPerStep);
		gAudioCallbackCount++;
	}
	return noErr;
}

//---------------------------------------------------------------------------

- (OSStatus)_PrepareAudioUnit:(int)speed
{
	static int st[] = { 0, 1, 2, 6, 40, 80, 200, 400 };
	speed &= 7;
	G.prefs.speed = speed;
	loopN = speed > 1 ? st[speed] >> 1 : st[speed];
	AURenderCallbackStruct  input =
	{
		.inputProcRefCon	= mScreen,
		.inputProc			= (AURenderCallback)InputProc,
	};
	AudioStreamBasicDescription  format =
	{
		.mSampleRate		= speed > 1 ? 2 * kSampleRate : kSampleRate,
		.mFormatID			= kAudioFormatLinearPCM,
		.mFormatFlags		= kFormatFlags,
		.mFramesPerPacket	= 1, // must be 1 for uncompressed data
		.mChannelsPerFrame	= 1, // or 2 for stereo??
		.mBitsPerChannel	= kBytesPerChannel * 8,
		.mBytesPerFrame		= kBytesPerChannel,
		.mBytesPerPacket	= kBytesPerChannel,
	};
	OSStatus		sts;

	sts = AU_SetBufferFrameSize(G.audioUnit,
								speed > 1 ? kA2SamplesPerStep : 2 * kA2SamplesPerStep);
	if (sts != noErr)
		return sts;

	sts = AudioUnitSetProperty(G.audioUnit,
		kAudioUnitProperty_StreamFormat, kAudioUnitScope_Input,
		0, &format, sizeof(format));
	if (sts != noErr)
		return sts;

	sts = AudioUnitSetProperty(G.audioUnit, 
		kAudioUnitProperty_SetRenderCallback, kAudioUnitScope_Input,
		0, &input, sizeof(input));
	if (sts != noErr)
		return sts;

// kAudioUnitProperty_MaximumFramesPerSlice (UInt32)
// kAudioUnitProperty_SetExternalBuffer (AudioUnitExternalBuffer)

	return noErr;
}

//---------------------------------------------------------------------------

- (void)_SetRunState:(int)newState
{/*
	Sets the run-state of this document to a new value.  The run state is
	an integer: a combination of the user-selected emulation speed (lower
	2 bits) and the pause level (remaining upper bits).  Emulation occurs
	only when the run-state value is greater than zero.
*/
	int			speed = newState & kSpeedMask;
	OSStatus	sts;

	[mSpeedLight setIntValue:speed];
	sts = AudioOutputUnitStop(G.audioUnit);

	if ((mRunState = newState) > 0)
	{
		sts = [self _PrepareAudioUnit:speed];
		sts = AudioOutputUnitStart(G.audioUnit);
	}
}

//---------------------------------------------------------------------------

- (void)awakeFromNib
{
	NSWindow*	mainWindow = [mScreen window];

//	NSLog(@"doc window key? %c", "ny"[[mainWindow isKeyWindow]]); //!!

	[mModelEmblem setStringValue:[mA2 ModelName]];
	[self setHasUndoManager:NO];
	[self _SetRunState:G.prefs.speed | ~kSpeedMask];

	[mScreen setNextResponder:mA2];
	[mA2 setNextResponder:mainWindow];

	[mainWindow useOptimizedDrawing:YES];
//	[mainWindow setBackgroundColor:[NSColor blackColor]];
//	[mainWindow setAcceptsMouseMovedEvents:YES];
//	[mainWindow setResizeIncrements:NSMakeSize(0, 100)];
	[self Unpause];
}

//---------------------------------------------------------------------------

- (IBAction)HitSpeedControl:(id)sender
{/*
	Responds to the user invoking one of the speed control commands.
*/
	[[sender menu].itemArray enumerateObjectsUsingBlock:^(id obj, NSUInteger idx, BOOL *stop) {
		[obj setState:[sender tag] == [obj tag]];
	}];
	[self _SetRunState:(mRunState & ~kSpeedMask) | [sender tag]];
}

//---------------------------------------------------------------------------

- (void)windowDidResignKey:(NSNotification*)note
{
	G.activeScreen = nil;
//	[self Pause];
	[ScreenView FullScreenOff];
}

//---------------------------------------------------------------------------

- (void)windowDidBecomeKey:(NSNotification*)note
{
	[self Unpause];
	G.activeScreen = mScreen;
	while (mA2.running)
		;
	for (int i = 0; i < 2; i++)
		if (G.prefs.diskImagePath[i].length) {
			NSString *path = G.prefs.diskImagePath[i];
			struct stat st;
			if (!stat([path fileSystemRepresentation], &st) && G.loadTime[i] < st.st_mtimespec.tv_sec) {
				G.loadTime[i] = st.st_mtimespec.tv_sec;
				[self loadDrive:i path:path];
				[mA2 SignalReboot:nil];
			}
		}
}

//---------------------------------------------------------------------------

- (void)windowWillMiniaturize:(NSNotification*)note
{/*
	Called when this window is about to be miniturized and put in the Dock.
	Here we make sure that the window's dock image looks like its content.
	We must do this ourselves because NSOpenGLViews don't co-operate with
	Quartz.

	Calling '-setOpaque' is required to make the Quartz underlay and the
	window shadow appear correctly.  We restore the opaque-ness property
	to YES in '-windowDidMiniaturize'.
*/
	NSWindow*	window = [note object];

	[self Pause];
	[mScreen PrepareToMiniaturize];
	[window setOpaque:NO];
}

//---------------------------------------------------------------------------

- (void)Pause
	{ [self _SetRunState:(mRunState - kNumSpeeds)]; }

- (void)Unpause
	{ if (mRunState < 0)  [self _SetRunState:(mRunState + kNumSpeeds)]; }

- (BOOL)IsRunning
	{ return mRunState > 0; }

- (void)windowWillBeginSheet:(NSNotification*)note
	{ [self Pause]; }

- (void)windowDidEndSheet:(NSNotification*)note
	{ [self Unpause]; }

- (void)windowWillClose:(NSNotification*)note
	{ [self windowDidResignKey:note]; }

- (void)windowDidMiniaturize:(NSNotification*)note
	{ [[note object] setOpaque:YES];  [self Unpause]; }
	 // see '-windowWillMiniaturize'

- (BOOL)keepBackupFile
	{ return G.prefs.keepBackupFiles; }

//---------------------------------------------------------------------------
@end
