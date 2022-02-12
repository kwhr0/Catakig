/*	class AppController

	A dispatcher for many high-level application events.  Only one instance
	exists at runtime.
*/
#import "Catakig-Cocoa.h"
#import "AppController.h"
#import "ScreenView.h"

static CVReturn MyDisplayLinkCallback(CVDisplayLinkRef displayLink, const CVTimeStamp *now, const CVTimeStamp *outputTime, CVOptionFlags flagsIn, CVOptionFlags *flagsOut, void *context) {
	[MyDocument performSelectorOnMainThread:@selector(AllNeedDisplay) withObject:nil waitUntilDone:NO];
	return kCVReturnSuccess;
}

@implementation AppController
//---------------------------------------------------------------------------

+ (void)initialize
{
	OSStatus		sts;

	// Need these retains??
	G.bundle	= [[NSBundle mainBundle] retain];
	G.docMgr	= [[NSDocumentController sharedDocumentController] retain];
	G.fileMgr	= [[NSFileManager defaultManager] retain];
	G.helpMgr	= [[NSHelpManager sharedHelpManager] retain];
//	G.null		= [[NSNull null] retain];
	G.pboard	= [[NSPasteboard generalPasteboard] retain];
	G.workspace	= [[NSWorkspace sharedWorkspace] retain];

	SetMouseRange();

	if (noErr != (sts = AU_Open(&G.audioUnit)))
	{
		FatalErrorAlert(@"Cannot initialize audio",
			@"Error from AU_Open.");
	}

	if (NO) //!!
	{
		NSArray*	arr;

		arr = [G.fileMgr directoryContentsAtPath:@"/System/Library/Sounds"];
		NSLog(@"Found sounds: %@", arr);
	}
}

//---------------------------------------------------------------------------
- (void)applicationWillFinishLaunching:(NSNotification *)notification {
	G.appController = self;
}
- (void)applicationDidFinishLaunching:(NSNotification*)note
{/*
	"Sent by the default notification center after the application has been
	launched and initialized but before it has received its first event."
*/
	[[NSApplication.sharedApplication.mainMenu.itemArray[2] submenu].itemArray enumerateObjectsUsingBlock:^(id obj, NSUInteger idx, BOOL *stop) {
		[obj setState:[obj tag] == G.prefs.speed];
	}];
	CVDisplayLinkCreateWithActiveCGDisplays(&displayLink);
	CVDisplayLinkSetOutputCallback(displayLink, MyDisplayLinkCallback, nil);
	CVDisplayLinkStart(displayLink);
}

//---------------------------------------------------------------------------

- (NSApplicationTerminateReply)
	applicationShouldTerminate:(NSApplication*)app
{
	AU_Close(&G.audioUnit);
	[ScreenView FullScreenOff];

	CVDisplayLinkStop(displayLink);
	CVDisplayLinkRelease(displayLink);
	return NSTerminateNow;
}

//---------------------------------------------------------------------------
#if 0
- (BOOL)applicationShouldOpenUntitledFile:(NSApplication*)sender
{/*
	Returns whether the application should automatically create a new
	document after launching.  (Default behavior is YES.)
*/
	return NO;
}
#endif
//---------------------------------------------------------------------------

- (void)applicationDidChangeScreenParameters:(NSNotification*)note
{/*
	Responds to changes in screen configuration: either resolutions, or
	relative positions of multiple screens.
*/
	SetMouseRange();
}

//---------------------------------------------------------------------------

- (void)setup
{/*
	Called when the user invokes the "New" menu command.  (Normally the
	global DocumentController gets this message first.)
*/
	NSEnumerator*	e = [[mNewA2Model cells] objectEnumerator];
	NSColor*		warnColor = [NSColor colorWithDeviceHue:0.
						saturation:0.4 brightness:1. alpha:1.];

//	Scan for Apple II ROMs.

	[A2Computer ScanDirectoryForROM:nil];

	for (NSButtonCell* cell;  (cell = [e nextObject]);)
	{
		BOOL	hasROM = [A2Computer ModelHasROM:[cell tag]];

		[cell setBackgroundColor:(hasROM? nil : warnColor)]; // 10.4 only!!
	}

//	Run the "New Apple II" dialog.

	if ([mNewA2Panel RunModal])
	{
		A2G.defaultModel    = [mNewA2Model selectedTag];
		A2G.defaultExtraRAM = [mNewA2ExtraRAM selectedTag];
	}
}

//---------------------------------------------------------------------------
@end
