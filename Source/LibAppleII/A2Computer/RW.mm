#import "LibAppleII-Priv.h"
#import "W65C02.h"

#define CYCLES			(mCycles + mT + mScanLine + (mScanLine<<6))

extern uint32_t gSpkrOut[];

typedef uint32_t (*ReadFunc)(void *, void *, unsigned);
typedef void (*WriteFunc)(void *, void *, unsigned, uint32_t);

static ReadFunc readfunc = (ReadFunc)[A2Computer instanceMethodForSelector:@selector(read:)];
static WriteFunc writefunc = (WriteFunc)[A2Computer instanceMethodForSelector:@selector(write:data:)];

void RW::Stop() {
#if W65C02_TRACE
	W65C02 *mpu = dynamic_cast<W65C02 *>(this);
	if (mpu) mpu->StopTrace();
#endif
}

void RW::Remap(uint32_t sel) {
	zp = memory->RAM[0][sel >> ksALTZP & 1];
	rmap = A2T.rmaps[sel >> ksRMap & kmRMap];
	wmap = A2T.wmaps[sel >> ksWMap & kmWMap];
}

uint32_t RW::Read(uint16_t adr) {
	return readfunc(a2, nil, adr);
}

void RW::Write(uint16_t adr, uint8_t data) {
	writefunc(a2, nil, adr, data);
}

@implementation A2Computer (RW)

- (uint32_t)read:(unsigned)ea {
	uint32_t d;
#if defined(CURMPU) && defined(NEWMPU)
	if (mMpu->ReadStart(ea, d)) return d;
#endif

		#define DONE			goto Epilog
		#define DONE_F			goto R_Floater8
		#define LABEL(NAME)		R_##NAME

	#define CASE(N)		case 0x##N + 1

	#define VF_CASES(N, FLAGS) \
		CASE(N  ):  mFlags &= ~((FLAGS) & mMutableFlags);  DONE_F; \
		CASE(N+1):  mFlags |=  ((FLAGS) & mMutableFlags);  DONE_F;
		// for possibly changing video flags; and memory map not affected

	#define MF_CASES(N, FLAGS) \
		CASE(N): \
			d = mFlags & ~((FLAGS) & mMutableFlags);  goto LABEL(Remap); \
		CASE(N+1): \
			d = mFlags |  ((FLAGS) & mMutableFlags);  goto LABEL(Remap);
		// for possibly changing memory flags; memory map affected

	//---------------------------------------------------------------------------

	switch ((ea ^ 0xC800) - 0x7FF)
	{
		enum
		{
			kfIOU		= kfXYMASK | kfVBLMASK | kfX0EDGE | kfY0EDGE,
			kmLCEven	= ~(kfLCBANK2 | kfLCRD | kfLCWRThi | kfLCWRTlo),
			kmHotSlot	= ~(7UL << ksHotSlot),
		};

	//-------------------------------------------------- $C00x, $C01x

		CASE(00): CASE(01): CASE(02): CASE(03):
		CASE(04): CASE(05): CASE(06): CASE(07):
		CASE(08): CASE(09): CASE(0A): CASE(0B):
		CASE(0C): CASE(0D): CASE(0E): CASE(0F):
			if (mKeyQ.tail == mKeyQ.head) // then char queue is empty
				d = 0; // should be last char queued??
			else // queue has one or more characters
				d = 0x80 | mKeyQ.buf[mKeyQ.head];
			DONE;

		CASE(10):
			if (mKeyQ.tail != mKeyQ.head)
				mKeyQ.head += 1;
			d = A2G.buttons << 3 & 0x80; // or'd with previous char!!
			DONE;

		CASE(11): d = ksLCBANK2;   goto R_Flag;
		CASE(12): d = ksLCRD;      goto R_Flag;
		CASE(13): d = ksRAMRD;     goto R_Flag;
		CASE(14): d = ksRAMWRT;    goto R_Flag;
		CASE(15): d = ksCXROM;     goto R_MuFlag;
		CASE(16): d = ksALTZP;     goto R_Flag;
		CASE(17): d = ksC3ROM;     goto R_MuFlag;
		CASE(18): d = ks80STOREv;  goto R_Flag;

		CASE(19): d = (mScanLine >> 6) - 3;  goto R_Floater7;

		CASE(1A): d = ksTEXT;     goto R_Flag;
		CASE(1B): d = ksMIXED;    goto R_Flag;
		CASE(1C): d = ksPAGE2v;   goto R_Flag;
		CASE(1D): d = ksHIRESv;   goto R_Flag;
		CASE(1E): d = ksALTCHAR;  goto R_Flag;
		CASE(1F): d = ks80COL;    goto R_Flag;

	//-------------------------------------------------- $C02x, $C03x

		CASE(20): CASE(21): CASE(22): CASE(23):
		CASE(24): CASE(25): CASE(26): CASE(27):
		CASE(28): CASE(29): CASE(2A): CASE(2B):
		CASE(2C): CASE(2D): CASE(2E): CASE(2F):
			if (mModel >= kA2ModelIIc)
			{
				d = mFlags ^ kfCXROM;
				goto LABEL(Remap);
			}
			if (not A2G.hearC02x)
				DONE_F;
			// else fall into C03x handler

		CASE(30): CASE(31): CASE(32): CASE(33):
		CASE(34): CASE(35): CASE(36): CASE(37):
		CASE(38): CASE(39): CASE(3A): CASE(3B):
		CASE(3C): CASE(3D): CASE(3E): CASE(3F):
		if (mAudioActive) {
			uint32_t	*out, *delta;

			d = ((mT + 65*mScanLine) * (kA2SamplesPerStep << 7)) / 17030;
			out = gSpkrOut + (d >> 7);
			delta = A2T.audio.delta + (d & 127);

			out[0] =  ~out[0] + delta[kFilterRes * 4];
			out[1] =  ~out[1] + delta[kFilterRes * 3];
			out[2] =  ~out[2] + delta[kFilterRes * 2];
			out[3] =  ~out[3] + delta[kFilterRes];
			out[4] = (~out[4] + delta[0]) ^ 0xFF;

		}	DONE_F;

	//-------------------------------------------------- $C04x, $C06x

		CASE(40):	d = ksXYMASK;	goto R_Flag;
		CASE(41):	d = ksVBLMASK;	goto R_Flag;
		CASE(42):	d = ksX0EDGE;	goto R_Flag;
		CASE(43):	d = ksY0EDGE;	goto R_Flag;

		CASE(60): CASE(68):
			d = 0; // cassette-in ??
			goto R_Floater7;

		CASE(61): CASE(69):
		CASE(62): CASE(6A):
		CASE(63): CASE(6B):
			d = A2G.buttons << (-ea & 7);
			goto R_Floater7;

		CASE(64): CASE(6C):
		CASE(65): CASE(6D):
		CASE(66): CASE(6E):
		CASE(67): CASE(6F):
			d = (CYCLES - mWhenC07x - A2G.paddle[ea&3]) >> 24;
			goto R_Floater7;

		CASE(44): CASE(45): CASE(46): CASE(47):
		CASE(48): CASE(49): CASE(4A): CASE(4B):
		CASE(4C): CASE(4D): CASE(4E): CASE(4F):
			DONE;

	//-------------------------------------------------- $C05x

		CASE(50): mFlags &= ~kfTEXT;   DONE_F;
		CASE(51): mFlags |=  kfTEXT;   DONE_F;
		CASE(52): mFlags &= ~kfMIXED;  DONE_F;
		CASE(53): mFlags |=  kfMIXED;  DONE_F;

		CASE(54): d = mFlags & ~(kfPAGE2m | kfPAGE2v);  goto LABEL(Remap);
		CASE(55): d = mFlags |  (kfPAGE2m | kfPAGE2v);  goto LABEL(Remap);
		CASE(56): d = mFlags & ~(kfHIRESm | kfHIRESv);  goto LABEL(Remap);
		CASE(57): d = mFlags |  (kfHIRESm | kfHIRESv);  goto LABEL(Remap);

		VF_CASES(58, kfSINGRES | kfXYMASK)
		VF_CASES(5A, kfSINGRES | kfVBLMASK)
		VF_CASES(5C, kfSINGRES | kfX0EDGE)
		VF_CASES(5E, kfSINGRES | kfY0EDGE)

	//-------------------------------------------------- $C07x

		CASE(77):	d = 0; // whether in graphics scanline!!
					goto R_Floater7;

		CASE(7E):	d = (mMutableFlags >> ksSINGRES) << 7;
					goto R_Floater7;

		CASE(7F):	d = ksSINGRES;
					goto R_Flag;

		CASE(70): CASE(71): CASE(72): CASE(73):
		CASE(74): CASE(75): CASE(76):
		CASE(78): CASE(79): CASE(7A): CASE(7B):
		CASE(7C): CASE(7D):
		LABEL(HitC07x):
			mWhenC07x = CYCLES;
			DONE_F;

	//-------------------------------------------------- $C08x (lang card)

		CASE(80):
		CASE(84):	d = (mFlags & kmLCEven) | (kfLCBANK2 | kfLCRD);
					goto LABEL(Remap);
		CASE(82):
		CASE(86):	d = (mFlags & kmLCEven) | kfLCBANK2;
					goto LABEL(Remap);
		CASE(88):
		CASE(8C):	d = (mFlags & kmLCEven) | kfLCRD;
					goto LABEL(Remap);
		CASE(8A):
		CASE(8E):	d = mFlags & kmLCEven;
					goto LABEL(Remap);

		CASE(81):
		CASE(85):	d = kfLCBANK2;
					goto LABEL(HitC08xOdd);
		CASE(83):
		CASE(87):	d = kfLCBANK2 | kfLCRD;
					goto LABEL(HitC08xOdd);
		CASE(89):
		CASE(8D):	d = 0;
					goto LABEL(HitC08xOdd);
		CASE(8B):
		CASE(8F):	d = kfLCRD;
					goto LABEL(HitC08xOdd);

		LABEL(HitC08xOdd):
			d |= mFlags & ~(kfLCBANK2 | kfLCRD);
			d += ~d >> 1 & kfLCWRTlo;
			goto LABEL(Remap);

	//-------------------------------------------------- $C09x (printer)

		CASE(90): CASE(91): CASE(92): CASE(93):
		CASE(94): CASE(95): CASE(96): CASE(97):
		CASE(98): CASE(99): CASE(9A): CASE(9B):
		CASE(9C): CASE(9D): CASE(9E): CASE(9F):
			d = mPrinter.reg[ea & 15];
			DONE;

	//-------------------------------------------------- $C0Ax, $C0Bx

		CASE(A0): CASE(A1): CASE(A2): CASE(A3):
		CASE(A4): CASE(A5): CASE(A6): CASE(A7):
		CASE(A8): CASE(A9): CASE(AA): CASE(AB):
		CASE(AC): CASE(AD): CASE(AE): CASE(AF):

		CASE(B0): CASE(B1): CASE(B2): CASE(B3):
		CASE(B4): CASE(B5): CASE(B6): CASE(B7):
		CASE(B8): CASE(B9): CASE(BA): CASE(BB):
		CASE(BC): CASE(BD): CASE(BE):

			DONE;


	CASE(BF):
		d = A2T.curTime[mClock.index-- & 31];

		DONE;

	//-------------------------------------------------- $C0Cx (Slinky RAM)

		CASE(C0):	d = mSlinky.pos;  DONE;
		CASE(C1):	d = mSlinky.pos >> 8;  DONE;
		CASE(C2):	d = mSlinky.pos >> 16 | 0xF0;  DONE;
		CASE(C3):	d = mSlinky.rBase[mSlinky.pos++ & mSlinky.mask];  DONE;
		CASE(CF):	d = (mSlinky.mask + 1) >> (9+8);  DONE;

		CASE(C4): CASE(C5): CASE(C6): CASE(C7):
		CASE(C8): CASE(C9): CASE(CA): CASE(CB):
		CASE(CC): CASE(CD): CASE(CE):
			DONE_F;

	//-------------------------------------------------- $C0D0-EF (IWMs)

		CASE(D0): CASE(D1): CASE(D2): CASE(D3):
		CASE(D4): CASE(D5): CASE(D6): CASE(D7):
		CASE(D8): CASE(D9): CASE(DA): CASE(DB):
		CASE(DC): CASE(DD): CASE(DE): CASE(DF):

		CASE(E0): CASE(E1): CASE(E2): CASE(E3):
		CASE(E4): CASE(E5): CASE(E6): CASE(E7):
		CASE(E8): CASE(E9): CASE(EA): CASE(EB):
		CASE(EC): CASE(ED): CASE(EE): CASE(EF):

			d = A2HitIWM(mIWM + (ea>>4 & 1), ea&31, 0);

			DONE;

	//-------------------------------------------------- $CFFF


		case 0:
			d = mMpu->READ(ea);
			DONE;

	//-------------------------------------------------- Misc epilogs

	//	Arrive at R_Remap or W_Remap with 'd' equal to the new value for
	//	'mFlags'.  Arrive at R_Flag with 'd' equal to one of the 'ks'
	//	flag constants.  Arrive at R_Floater7 with 'd' having a value to
	//	return in bit 7.

		R_Remap:	if (mFlags != d) {
						mFlags = d;  mMpu->Remap(d);
					}
		R_Floater8:	d = mMpu->FLOATER(mT);
					DONE;

		R_MuFlag:	d = ((mFlags & mMutableFlags) >> d) << 7;
					goto R_Floater7;
		R_Flag:		d = (mFlags >> d) << 7;
		R_Floater7:	d = (d & 0x80) | (mMpu->FLOATER(mT) & 0x7F);
					DONE;

	} // end of big switch on 'ea'

	//---------------------------------------------------------------------------

	#undef DONE
	#undef DONE_F
	#undef LABEL
	#undef CASE
	#undef VF_CASES
	#undef MF_CASES
	
	d = mMpu->READ(ea); // default read behavior, when 'ea' not in I/O area
	   // fall into Epilog...
Epilog:
#if defined(CURMPU) && defined(NEWMPU)
	mMpu->ReadEnd(d);
#endif
	return d;
}

- (void)write:(unsigned)ea data:(uint32_t)d {
#if defined(CURMPU) && defined(NEWMPU)
	if (mMpu->WriteStart(ea, d)) return;
#endif

#define DONE			goto Done
#define DONE_F			goto Done;
#define LABEL(NAME)		W_##NAME

	#define CASE(N)		case 0x##N + 1

	#define VF_CASES(N, FLAGS) \
		CASE(N  ):  mFlags &= ~((FLAGS) & mMutableFlags);  DONE_F; \
		CASE(N+1):  mFlags |=  ((FLAGS) & mMutableFlags);  DONE_F;
		// for possibly changing video flags; and memory map not affected

	#define MF_CASES(N, FLAGS) \
		CASE(N): \
			d = mFlags & ~((FLAGS) & mMutableFlags);  goto LABEL(Remap); \
		CASE(N+1): \
			d = mFlags |  ((FLAGS) & mMutableFlags);  goto LABEL(Remap);
		// for possibly changing memory flags; memory map affected

	//---------------------------------------------------------------------------

	switch ((ea ^ 0xC800) - 0x7FF)
	{
		enum
		{
			kfIOU		= kfXYMASK | kfVBLMASK | kfX0EDGE | kfY0EDGE,
			kmLCEven	= ~(kfLCBANK2 | kfLCRD | kfLCWRThi | kfLCWRTlo),
			kmHotSlot	= ~(7UL << ksHotSlot),
		};

	//-------------------------------------------------- $C00x, $C01x

		MF_CASES(00, kf80STOREm | kf80STOREv)
		MF_CASES(02, kfRAMRD)
		MF_CASES(04, kfRAMWRT)
		MF_CASES(06, kfCXROM)
		MF_CASES(08, kfALTZP)
		MF_CASES(0A, kfC3ROM)

		VF_CASES(0C, kf80COL)
		VF_CASES(0E, kfALTCHAR)

		CASE(10): CASE(11): CASE(12): CASE(13):
		CASE(14): CASE(15): CASE(16): CASE(17):
		CASE(18): CASE(19): CASE(1A): CASE(1B):
		CASE(1C): CASE(1D): CASE(1E): CASE(1F):
			if (mKeyQ.tail != mKeyQ.head)
				mKeyQ.head += 1;
			DONE;

	//-------------------------------------------------- $C02x, $C03x

		CASE(20): CASE(21): CASE(22): CASE(23):
		CASE(24): CASE(25): CASE(26): CASE(27):
		CASE(28): CASE(29): CASE(2A): CASE(2B):
		CASE(2C): CASE(2D): CASE(2E): CASE(2F):
			if (mModel >= kA2ModelIIc)
			{
				d = mFlags ^ kfCXROM;
				goto LABEL(Remap);
			}
			if (not A2G.hearC02x)
				DONE_F;
			// else fall into C03x handler

		CASE(30): CASE(31): CASE(32): CASE(33):
		CASE(34): CASE(35): CASE(36): CASE(37):
		CASE(38): CASE(39): CASE(3A): CASE(3B):
		CASE(3C): CASE(3D): CASE(3E): CASE(3F):
		if (mAudioActive) {
			uint32_t	*out, *delta;

			d = ((mT + 65*mScanLine) * (kA2SamplesPerStep << 7)) / 17030;
			out = gSpkrOut + (d >> 7);
			delta = A2T.audio.delta + (d & 127);

			out[0] =  ~out[0] + delta[kFilterRes * 4];
			out[1] =  ~out[1] + delta[kFilterRes * 3];
			out[2] =  ~out[2] + delta[kFilterRes * 2];
			out[3] =  ~out[3] + delta[kFilterRes];
			out[4] = (~out[4] + delta[0]) ^ 0xFF;

		}	DONE_F;

	//-------------------------------------------------- $C04x, $C06x

		CASE(40): CASE(41): CASE(42): CASE(43):
		CASE(60): CASE(61): CASE(62): CASE(63):
		CASE(64): CASE(65): CASE(66): CASE(67):
		CASE(68): CASE(69): CASE(6A): CASE(6B):
		CASE(6C): CASE(6D): CASE(6E): CASE(6F):
			// fall into DONE

		CASE(44): CASE(45): CASE(46): CASE(47):
		CASE(48): CASE(49): CASE(4A): CASE(4B):
		CASE(4C): CASE(4D): CASE(4E): CASE(4F):
			DONE;

	//-------------------------------------------------- $C05x

		CASE(50): mFlags &= ~kfTEXT;   DONE_F;
		CASE(51): mFlags |=  kfTEXT;   DONE_F;
		CASE(52): mFlags &= ~kfMIXED;  DONE_F;
		CASE(53): mFlags |=  kfMIXED;  DONE_F;

		CASE(54): d = mFlags & ~(kfPAGE2m | kfPAGE2v);  goto LABEL(Remap);
		CASE(55): d = mFlags |  (kfPAGE2m | kfPAGE2v);  goto LABEL(Remap);
		CASE(56): d = mFlags & ~(kfHIRESm | kfHIRESv);  goto LABEL(Remap);
		CASE(57): d = mFlags |  (kfHIRESm | kfHIRESv);  goto LABEL(Remap);

		VF_CASES(58, kfSINGRES | kfXYMASK)
		VF_CASES(5A, kfSINGRES | kfVBLMASK)
		VF_CASES(5C, kfSINGRES | kfX0EDGE)
		VF_CASES(5E, kfSINGRES | kfY0EDGE)

	//-------------------------------------------------- $C07x

		CASE(7E):	if (mModel >= kA2ModelIIc)
						mMutableFlags = (mMutableFlags & ~kfIOU) | kfSINGRES;
					goto LABEL(HitC07x);

		CASE(7F):	if (mModel >= kA2ModelIIc)
						mMutableFlags = (mMutableFlags & ~kfSINGRES) | kfIOU;
					goto LABEL(HitC07x);

		CASE(77):
			// fall thru

		CASE(70): CASE(71): CASE(72): CASE(73):
		CASE(74): CASE(75): CASE(76):
		CASE(78): CASE(79): CASE(7A): CASE(7B):
		CASE(7C): CASE(7D):
		LABEL(HitC07x):
			mWhenC07x = CYCLES;
			DONE_F;

	//-------------------------------------------------- $C08x (lang card)

		CASE(80):
		CASE(84):	d = (mFlags & kmLCEven) | (kfLCBANK2 | kfLCRD);
					goto LABEL(Remap);
		CASE(82):
		CASE(86):	d = (mFlags & kmLCEven) | kfLCBANK2;
					goto LABEL(Remap);
		CASE(88):
		CASE(8C):	d = (mFlags & kmLCEven) | kfLCRD;
					goto LABEL(Remap);
		CASE(8A):
		CASE(8E):	d = mFlags & kmLCEven;
					goto LABEL(Remap);

		CASE(81):
		CASE(85):	d = kfLCBANK2;
					goto LABEL(HitC08xOdd);
		CASE(83):
		CASE(87):	d = kfLCBANK2 | kfLCRD;
					goto LABEL(HitC08xOdd);
		CASE(89):
		CASE(8D):	d = 0;
					goto LABEL(HitC08xOdd);
		CASE(8B):
		CASE(8F):	d = kfLCRD;
					goto LABEL(HitC08xOdd);

		LABEL(HitC08xOdd):
			d |= mFlags & ~(kfLCBANK2 | kfLCRD);
			d += ~d >> 1 & kfLCWRTlo;
			goto LABEL(Remap);

	//-------------------------------------------------- $C09x (printer)

		CASE(98):	mPrinter.lights = kLightSustain | 1;
					putc(d & 0x7F, mPrinter.session);
					DONE;
		CASE(9A):
		CASE(9B):	mPrinter.reg[ea & 15] = d;
					DONE;

		CASE(90): CASE(91): CASE(92): CASE(93):
		CASE(94): CASE(95): CASE(96): CASE(97): CASE(99):
		CASE(9C): CASE(9D): CASE(9E): CASE(9F):
			DONE;

	//-------------------------------------------------- $C0Ax, $C0Bx

		CASE(A0): CASE(A1): CASE(A2): CASE(A3):
		CASE(A4): CASE(A5): CASE(A6): CASE(A7):
		CASE(A8): CASE(A9): CASE(AA): CASE(AB):
		CASE(AC): CASE(AD): CASE(AE): CASE(AF):

		CASE(B0): CASE(B1): CASE(B2): CASE(B3):
		CASE(B4): CASE(B5): CASE(B6): CASE(B7):
		CASE(B8): CASE(B9): CASE(BA): CASE(BB):
		CASE(BC): CASE(BD): CASE(BE):

			DONE;


	CASE(BF):
		mClock.index = d;

		DONE;

	//-------------------------------------------------- $C0Cx (Slinky RAM)

		CASE(C0):	mSlinky.pos = (mSlinky.pos & 0xFFFF00) | d;  DONE;
		CASE(C1):	mSlinky.pos = (mSlinky.pos & 0xFF00FF) | d<< 8;  DONE;
		CASE(C2):	mSlinky.pos = (mSlinky.pos & 0x00FFFF) | d<<16;  DONE;
		CASE(C3):	mSlinky.wBase[mSlinky.pos++ & mSlinky.mask] = d;  DONE;
		CASE(CF):	// fall thru

		CASE(C4): CASE(C5): CASE(C6): CASE(C7):
		CASE(C8): CASE(C9): CASE(CA): CASE(CB):
		CASE(CC): CASE(CD): CASE(CE):
			DONE_F;

	//-------------------------------------------------- $C0D0-EF (IWMs)

		CASE(D0): CASE(D1): CASE(D2): CASE(D3):
		CASE(D4): CASE(D5): CASE(D6): CASE(D7):
		CASE(D8): CASE(D9): CASE(DA): CASE(DB):
		CASE(DC): CASE(DD): CASE(DE): CASE(DF):

		CASE(E0): CASE(E1): CASE(E2): CASE(E3):
		CASE(E4): CASE(E5): CASE(E6): CASE(E7):
		CASE(E8): CASE(E9): CASE(EA): CASE(EB):
		CASE(EC): CASE(ED): CASE(EE): CASE(EF):

			A2HitIWM(mIWM + (ea>>4 & 1), ea|32, d);
			DONE;


		CASE(FF): putc(d, stderr); // log output

	//-------------------------------------------------- $CFFF


		case 0:
			d = mMpu->READ(ea);
			DONE;

	//-------------------------------------------------- Misc epilogs

	//	Arrive at R_Remap or W_Remap with 'd' equal to the new value for
	//	'mFlags'.  Arrive at R_Flag with 'd' equal to one of the 'ks'
	//	flag constants.  Arrive at R_Floater7 with 'd' having a value to
	//	return in bit 7.

		W_Remap:	if (mFlags != d) {
						mFlags = d;  mMpu->Remap(d);
					}
					DONE;

	} // end of big switch on 'ea'

	//---------------------------------------------------------------------------

	#undef DONE
	#undef DONE_F
	#undef LABEL
	#undef CASE
	#undef VF_CASES
	#undef MF_CASES
	
	mMpu->WRITE(ea, d); // default write, when 'ea' not in I/O area

Done:
	;
}
@end
