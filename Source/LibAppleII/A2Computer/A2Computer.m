/*	class A2Computer

	An object representing an Apple II computer.  Methods in this source
	file do object allocation, initialization, deallocation, and
	(eventually) serialization.
*/
#import "LibAppleII-Priv.h"
#import "A2DiskDrive.h"
#import "Catakig-Cocoa.h"

@implementation A2Computer
//---------------------------------------------------------------------------

+ (void)initialize
{
	if (self != [A2Computer class])
		return; // ensures this routine executes no more than once

	[A2Computer _InitAudio];
	[A2Computer _InitVideo];
	[A2Computer _InitCPU];
	[A2Computer _InitROM];
	[A2Computer _InitPrinting];

	[A2Computer SetMouseRangeTo:NSMakeRect(0, 0, 640, 480)];
	[A2Computer setVersion:1]; //??
	mlock(&A2T, sizeof(A2T));

	[NSTimer scheduledTimerWithTimeInterval:1
		target:			[A2Computer class]
		selector:		@selector(_UpdateClock:)
		userInfo:		nil
		repeats:		YES ];

#if 0
	if (NSPageSize() > 0x2000)
		NSLog(@"Warning: VM page size = %ld (> 0x2000)", (unsigned long)NSPageSize());
	NSLog(@"A2Computer size = %lu", sizeof(struct{@defs(A2Computer)}));
	NSLog(@"VM page size = 0x%X", NSPageSize());
	NSLog(@"A2T size = %lu", sizeof(A2T));
#endif
}

//---------------------------------------------------------------------------

static void RandomizeVideoMemory(void* ram, unsigned model)
{
	for (int i = 0x6000;  --i >= 0;)
		((uint16_t*)ram)[i] = A2Random16();

	if (model == kA2ModelIIo)
	{
		uint32_t*	p = (uint32_t*)ram;

		for (int i = 0x400/4;  i < 0xC00/4;  ++i)
			p[i] = p[i] & 0x01010101 | 0xBEBEBEBE;
	}
}

//---------------------------------------------------------------------------

- (id)init
{/*
	"Add your subclass-specific initialization here.  If an error occurs,
	send a [self release] message and return nil."

	Not robust enough against failures!!
*/
	if (nil == (self = [super init]))
		return nil;

	A2G.defaultModel = G.prefs.model;
	A2G.defaultExtraRAM = G.prefs.ram;
	if (GetCurrentEventKeyModifiers() & 1 << shiftKeyBit) {
		[G.appController setup];
		G.prefs.diskImagePath[0] = G.prefs.diskImagePath[1] = @"";
	}
	
	mModel				= A2G.defaultModel;
	mFlags				= kfTEXT;
	mHalts				= kfHaltNoPower | kfHaltReset;
	mMemorySize			= sizeof(A2Memory);
	mPrinter.session	= tmpfile();
	mSlinky.mask		= (1UL << A2G.defaultExtraRAM) - 1;
	mSlinky.rNowhere	= 0xA0;
	mSlinky.rBase		= &mSlinky.rNowhere;
	mSlinky.wBase		= &mSlinky.wNowhere;

	if (mSlinky.mask != 0)
		mMemorySize += (mSlinky.mask + 1);
	mMemory = NSAllocateMemoryPages(mMemorySize);

	if (not mMemory  or  not mPrinter.session)
		return [self Release];

	if (mSlinky.mask != 0)
		mSlinky.rBase = mSlinky.wBase =
			(uint8_t*)mMemory + sizeof(A2Memory);

//	Create the disk drives, and give every one a track buffer to
//	work with.

	for (int dd = 4;  --dd >= 0;)
		mIWM[dd>>1].drive[dd&1] = [[A2DiskDrive alloc] Init];

	RandomizeVideoMemory(mMemory->RAM, mModel);
	madvise(mMemory, mMemorySize, MADV_SEQUENTIAL);
	[self _PrepareModel];
	return self;
}

- (void)powerOn {
	mHalts &= ~kfHaltNoPower;
}

//---------------------------------------------------------------------------

- (void)_TestThePrinter:(BOOL)sampleOutput
{
	// Called only for debugging!!

	if (sampleOutput)
	{
		fputs(
			"---------1---------2---------3---------4"
			"---------5---------6---------7---------8\r\n\r\n"
			" !\"#$%&'()*+,-./0123456789:;<=>?\r\n"
			"@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_\r\n"
			"`abcdefghijklmnopqrstuvwxyz{|}~\r\n\r\n"
			"\x1B\x34Hello world!\x1B@\r\n", mPrinter.session);
		fprintf(mPrinter.session,
			"\x1BK\x07%c\1\2\3\4\5\6\7 |\r\n", 0);
		for (int i = 0;  i < 100;  ++i)
			fprintf(mPrinter.session, "%d\t%d\r\n", i, i*i);
	}

	[self SavePrintSessionAs:kA2PFVerbatim
		toFile:@"/Users/klipsch/Desktop/printout.raw"];
	[self SavePrintSessionAs:kA2PFPlain
		toFile:@"/Users/klipsch/Desktop/printout.txt"];
	[self SavePrintSessionAs:kA2PFEpsonToPS
		toFile:@"/Users/klipsch/Desktop/printout.ps"];
}

//---------------------------------------------------------------------------

- (void)dealloc
{
//	[self _TestThePrinter:NO];

	for (int dd = 4;  --dd >= 0;)
		[mIWM[dd>>1].drive[dd&1] release];

	if (mMemory != nil)
		NSDeallocateMemoryPages(mMemory, mMemorySize);

	fclose(mPrinter.session);
	[super dealloc];
}

@end
