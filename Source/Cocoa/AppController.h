
@interface AppController : NSObject
{
	//---------------- "New Apple II" dialog ----------------

	IBOutlet	NSPanel*		mNewA2Panel;
	IBOutlet	NSMatrix*		mNewA2Model;
	IBOutlet	NSControl*		mNewA2ExtraRAM;
	CVDisplayLinkRef displayLink;
}

//- (IBAction)	newDocument:(id)sender;

@end
