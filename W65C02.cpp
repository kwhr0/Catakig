// W65C02
// Copyright 2022,2023 © Yasuo Kuwahara
// MIT License

#include "W65C02.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum {
	LC, LZ, LI, LD, LB, LX, LV, LN
};

enum {
	MC = 1 << LC, MZ = 1 << LZ, MI = 1 << LI, MD = 1 << LD,
	MB = 1 << LB, MX = 1 << LX, MV = 1 << LV, MN = 1 << LN
};

enum {
	FB = 1, F0, F1, F8, FADD, FSUB, FLEFT, FRIGHT, FBIT
};

#define F(flag, type)	flag##type = F##type << (L##flag << 2)
enum {
	F(C, B), F(C, 0), F(C, 1), F(C, ADD), F(C, SUB), F(C, LEFT), F(C, RIGHT),
	F(Z, B), F(Z, 8),
	F(D, B), F(D, 0), F(D, 1),
	F(V, B), F(V, 0), F(V, ADD), F(V, SUB),
	F(N, B), F(N, 8)
};

#define fmnt()			(++fp < fbuf + FBUFMAX ? 0 : ResolvFlags())
#define fclc()			(fp->dm = C0, fmnt())
#define fsec()			(fp->dm = C1, fmnt())
#define fcld()			(fp->dm = D0, fmnt())
#define fsed()			(fp->dm = D1, fmnt())
#define fclv()			(fp->dm = V0, fmnt())
#define fld(x)			(fp->dm = N8 | Z8, fp->a = (x), fmnt())
#define fadd(x, y, z)	(fp->dm = N8 | VADD | Z8 | CADD, fp->b = (x), fp->s = (y), fp->a = (z), fmnt())
#define fsub(x, y, z)	(fp->dm = N8 | VSUB | Z8 | CSUB, fp->b = (x), fp->s = (y), fp->a = (z), fmnt())
#define fcmp(x, y, z)	(fp->dm = N8 | Z8 | CSUB, fp->b = (x), fp->s = (y), fp->a = (z), fmnt())
#define fleft(x, y)		(fp->dm = N8 | Z8 | CLEFT, fp->b = (x), fp->a = (y), fmnt())
#define fright(x, y)	(fp->dm = N8 | Z8 | CRIGHT, fp->b = (x), fp->a = (y), fmnt())
#define fbit(x, y)		(fp->dm = NB | VB | Z8, fp->b = (x), fp->a = (y), fmnt())
#define fz(x)			(fp->dm = Z8, fp->a = (x), fmnt())

#define CY				(ResolvC())

enum { W_WAI = 1, W_STP };

static void error() {
	fprintf(stderr, "internal error\n");
	exit(1);
}

W65C02::W65C02() {
#if W65C02_TRACE
	memset(tracebuf, 0, sizeof(tracebuf));
	tracep = tracebuf;
#endif
}

void W65C02::Reset() {
	s = irq = waitflags = 0;
	SetupFlags(MI);
	// Catakig		pc = ld16(0xfffc);
}

int W65C02::Execute(int n) {
	auto lda = [&](uint8_t d) { fld(a = d); };
	auto ldx = [&](uint8_t d) { fld(x = d); };
	auto ldy = [&](uint8_t d) { fld(y = d); };
	auto sta = [&] { return a; };
	auto stx = [&] { return x; };
	auto sty = [&] { return y; };
	auto stz = [] { return 0; };
	auto adc = [&](uint8_t d) { fadd(a, d, a += d + CY); };
	auto sbc = [&](uint8_t d) { fsub(a, d, a -= d + !CY); };
	auto cmp = [&](uint8_t d) { fcmp(a, d, a - d); };
	auto cpx = [&](uint8_t d) { fcmp(x, d, x - d); };
	auto cpy = [&](uint8_t d) { fcmp(y, d, y - d); };
	auto inc = [&](uint8_t d) { fld(++d); return d; };
	auto dec = [&](uint8_t d) { fld(--d); return d; };
	auto _and = [&](uint8_t d) { fld(a &= d); };
	auto ora = [&](uint8_t d) { fld(a |= d); };
	auto eor = [&](uint8_t d) { fld(a ^= d); };
	auto bit = [&](uint8_t d) { fbit(d, a & d); };
	auto biti = [&](uint8_t d) { fz(a & d); };
	auto tsb = [&](uint8_t d) { fz(a & d); return a | d; };
	auto trb = [&](uint8_t d) { fz(a & d); return ~a & d; };
	auto asl = [&](uint8_t d) { fleft(d, d <<= 1); return d; };
	auto rol = [&](uint8_t d) { fleft(d, d = d << 1 | CY); return d; };
	auto lsr = [&](uint8_t d) { fright(d, d >>= 1); return d; };
	auto ror = [&](uint8_t d) { fright(d, d = d >> 1 | CY << 7); return d; };
	auto br = [&](uint8_t cond) {
		if (cond) {
			uint16_t last = pc;
			pc += (int8_t)imm8();
			clock += 2 + ((last & 0xff00) != (pc & 0xff00));
		}
		else {
			pc++;
			clock += 2;
		}
	};
	clock = 0;
	do {
		if (irq) {
			if (waitflags & W_WAI) {
				waitflags &= ~W_WAI;
				pc++;
			}
			if (irq & M_NMI) {
				irq &= ~M_NMI;
				st8(0x100 | s--, pc >> 8);
				st8(0x100 | s--, pc);
				st8(0x100 | s--, ResolvFlags() & ~MB);
				intflags |= MI;
				pc = ld16(0xfffa);
				clock += 14; // p.142
			}
			else if (irq & M_IRQ && !(intflags & MI)) {
				irq &= ~M_IRQ;
				st8(0x100 | s--, pc >> 8);
				st8(0x100 | s--, pc);
				st8(0x100 | s--, ResolvFlags());
				intflags |= MI;
				pc = ld16(0xfffe);
				clock += 17; // p.139
			}
		}
#if W65C02_TRACE
		tracep->pc = pc;
		tracep->index = tracep->opn = 0;
#endif
		uint8_t t8;
		switch (imm8()) {
			case 0x00: ++pc; st8(0x100 | s--, pc >> 8); st8(0x100 | s--, pc); st8(0x100 | s--, ResolvFlags()); pc = ld16(0xfffe); break; // brk
			case 0x01: rindx(ora); break;
			case 0x02: pc++; clock += 2; break;
			case 0x03: clock++; break;
			case 0x04: mzp(tsb); break;
			case 0x05: rzp(ora); break;
			case 0x06: mzp(asl); break;
			case 0x07: mzp([](uint8_t d) { return d & ~1; }); break;
			case 0x08: st8(0x100 | s--, ResolvFlags()); clock += 3; break; // php
			case 0x09: imm(ora); break;
			case 0x0a: ma(asl); break;
			case 0x0b: clock++; break;
			case 0x0c: mabs(tsb); break;
			case 0x0d: rabs(ora); break;
			case 0x0e: mabs(asl); break;
			case 0x0f: rzp([&](uint8_t d) { br(!(d & 1)); }); break;
			case 0x10: br(!ResolvN()); break; // bpl
			case 0x11: rindy(ora); break;
			case 0x12: rzpp(ora); break;
			case 0x13: clock++; break;
			case 0x14: mzp(trb); break;
			case 0x15: rzpx(ora); break;
			case 0x16: mzpx(asl); break;
			case 0x17: mzp([](uint8_t d) { return d & ~2; }); break;
			case 0x18: fclc(); clock += 2; break; // clc
			case 0x19: rabsy(ora); break;
			case 0x1a: ma(inc); break;
			case 0x1b: clock++; break;
			case 0x1c: mabs(trb); break;
			case 0x1d: rabsx(ora); break;
			case 0x1e: mabsx(asl); break;
			case 0x1f: rzp([&](uint8_t d) { br(!(d & 2)); }); break;
			case 0x20: st8(0x100 | s--, (pc + 1) >> 8); st8(0x100 | s--, pc + 1); pc = imm16(); clock += 6; break; // jsr
			case 0x21: rindx(_and); break;
			case 0x22: pc++; clock += 2; break;
			case 0x23: clock++; break;
			case 0x24: rzp(bit); break;
			case 0x25: rzp(_and); break;
			case 0x26: mzp(rol); break;
			case 0x27: mzp([](uint8_t d) { return d & ~4; }); break;
			case 0x28: SetupFlags(ld8(0x100 | ++s)); clock += 4; break; // plp
			case 0x29: imm(_and); break;
			case 0x2a: ma(rol); break;
			case 0x2b: clock++; break;
			case 0x2c: rabs(bit); break;
			case 0x2d: rabs(_and); break;
			case 0x2e: mabs(rol); break;
			case 0x2f: rzp([&](uint8_t d) { br(!(d & 4)); }); break;
			case 0x30: br(ResolvN()); break; // bmi
			case 0x31: rindy(_and); break;
			case 0x32: rzpp(_and); break;
			case 0x33: clock++; break;
			case 0x34: rzpx(bit); break;
			case 0x35: rzpx(_and); break;
			case 0x36: mzpx(rol); break;
			case 0x37: mzp([](uint8_t d) { return d & ~8; }); break;
			case 0x38: fsec(); clock += 2; break; // sec
			case 0x39: rabsy(_and); break;
			case 0x3a: ma(dec); break;
			case 0x3b: clock++; break;
			case 0x3c: rabsx(bit); break;
			case 0x3d: rabsx(_and); break;
			case 0x3e: mabsx(rol); break;
			case 0x3f: rzp([&](uint8_t d) { br(!(d & 8)); }); break;
			case 0x40: SetupFlags(ld8(0x100 | ++s)); t8 = ld8(0x100 | ++s); pc = t8 | ld8(0x100 | ++s) << 8; clock += 6; break; // rti
			case 0x41: rindx(eor); break;
			case 0x42: pc++; clock += 2; break;
			case 0x43: clock++; break;
			case 0x44: pc++; clock += 3; break;
			case 0x45: rzp(eor); break;
			case 0x46: mzp(lsr); break;
			case 0x47: mzp([](uint8_t d) { return d & ~0x10; }); break;
			case 0x48: st8(0x100 | s--, a); clock += 3; break; // pha
			case 0x49: imm(eor); break;
			case 0x4a: ma(lsr); break;
			case 0x4b: clock++; break;
			case 0x4c: pc = imm16(); clock += 3; break; // jmp
			case 0x4d: rabs(eor); break;
			case 0x4e: mabs(lsr); break;
			case 0x4f: rzp([&](uint8_t d) { br(!(d & 0x10)); }); break;
			case 0x50: br(!ResolvV()); break; // bvc
			case 0x51: rindy(eor); break;
			case 0x52: rzpp(eor); break;
			case 0x53: clock++; break;
			case 0x54: pc++; clock += 4; break;
			case 0x55: rzpx(eor); break;
			case 0x56: mzpx(lsr); break;
			case 0x57: mzp([](uint8_t d) { return d & ~0x20; }); break;
			case 0x58: intflags &= ~MI; clock += 2; break; // cli
			case 0x59: rabsy(eor); break;
			case 0x5a: st8(0x100 | s--, y); clock += 3; break;
			case 0x5b: clock++; break;
			case 0x5c: pc += 2; clock += 8; break;
			case 0x5d: rabsx(eor); break;
			case 0x5e: mabsx(lsr); break;
			case 0x5f: rzp([&](uint8_t d) { br(!(d & 0x20)); }); break;
			case 0x60: t8 = ld8(0x100 | ++s); pc = (t8 | ld8(0x100 | ++s) << 8) + 1; clock += 6; break; // rts
			case 0x61: rindx(adc); break;
			case 0x62: pc++; clock += 2; break;
			case 0x63: clock++; break;
			case 0x64: wzp(stz); break;
			case 0x65: rzp(adc); break;
			case 0x66: mzp(ror); break;
			case 0x67: mzp([](uint8_t d) { return d & ~0x40; }); break;
			case 0x68: fld(a = ld8(0x100 | ++s)); clock += 4; break; // pla
			case 0x69: imm(adc); break;
			case 0x6a: ma(ror); break;
			case 0x6b: clock++; break;
			case 0x6c: pc = ld16(imm16()); clock += 5; break;
			case 0x6d: rabs(adc); break;
			case 0x6e: mabs(ror); break;
			case 0x6f: rzp([&](uint8_t d) { br(!(d & 0x40)); }); break;
			case 0x70: br(ResolvV()); break; // bvs
			case 0x71: rindy(adc); break;
			case 0x72: rzpp(adc); break;
			case 0x73: clock++; break;
			case 0x74: wzpx(stz); break;
			case 0x75: rzpx(adc); break;
			case 0x76: mzpx(ror); break;
			case 0x77: mzp([](uint8_t d) { return d & ~0x80; }); break;
			case 0x78: intflags |= MI; clock += 2; break; // sei
			case 0x79: rabsy(adc); break;
			case 0x7a: fld(y = ld8(0x100 | ++s)); clock += 4; break;
			case 0x7b: clock++; break;
			case 0x7c: pc = ld16(imm16() + x); clock += 6; break; // jmp (a,x)
			case 0x7d: rabsx(adc); break;
			case 0x7e: mabsx(ror); break;
			case 0x7f: rzp([&](uint8_t d) { br(!(d & 0x80)); }); break;
			case 0x80: br(1); break; // bra
			case 0x81: windx(sta); break;
			case 0x82: pc++; clock += 2; break;
			case 0x83: clock++; break;
			case 0x84: wzp(sty); break;
			case 0x85: wzp(sta); break;
			case 0x86: wzp(stx); break;
			case 0x87: mzp([](uint8_t d) { return d | 1; }); break;
			case 0x88: fld(--y); clock += 2; break; // dey
			case 0x89: imm(biti); break;
			case 0x8a: fld(a = x); clock += 2; break; // txa
			case 0x8b: clock++; break;
			case 0x8c: wabs(sty); break;
			case 0x8d: wabs(sta); break;
			case 0x8e: wabs(stx); break;
			case 0x8f: rzp([&](uint8_t d) { br(d & 1); }); break;
			case 0x90: br(!CY); break; // bcc
			case 0x91: windy(sta); break;
			case 0x92: wzpp(sta); break;
			case 0x93: clock++; break;
			case 0x94: wzpx(sty); break;
			case 0x95: wzpx(sta); break;
			case 0x96: wzpy(stx); break;
			case 0x97: mzp([](uint8_t d) { return d | 2; }); break;
			case 0x98: fld(a = y); clock += 2; break; // tya
			case 0x99: wabsy(sta); break;
			case 0x9a: fld(s = x); clock += 2; break; // txs
			case 0x9b: clock++; break;
			case 0x9c: wabs([] { return 0; }); break;
			case 0x9d: wabsx(sta); break;
			case 0x9e: wabsx(stz); break;
			case 0x9f: rzp([&](uint8_t d) { br(d & 2); }); break;
			case 0xa0: imm(ldy); break;
			case 0xa1: rindx(lda); break;
			case 0xa2: imm(ldx); break;
			case 0xa3: clock++; break;
			case 0xa4: rzp(ldy); break;
			case 0xa5: rzp(lda); break;
			case 0xa6: rzp(ldx); break;
			case 0xa7: mzp([](uint8_t d) { return d | 4; }); break;
			case 0xa8: fld(y = a); clock += 2; break; // tay
			case 0xa9: imm(lda); break;
			case 0xaa: fld(x = a); clock += 2; break; // tax
			case 0xab: clock++; break;
			case 0xac: rabs(ldy); break;
			case 0xad: rabs(lda); break;
			case 0xae: rabs(ldx); break;
			case 0xaf: rzp([&](uint8_t d) { br(d & 4); }); break;
			case 0xb0: br(CY); break; // bcs
			case 0xb1: rindy(lda); break;
			case 0xb2: rzpp(lda); break;
			case 0xb3: clock++; break;
			case 0xb4: rzpx(ldy); break;
			case 0xb5: rzpx(lda); break;
			case 0xb6: rzpy(ldx); break;
			case 0xb7: mzp([](uint8_t d) { return d | 8; }); break;
			case 0xb8: fclv(); clock += 2; break; // clv
			case 0xb9: rabsy(lda); break;
			case 0xba: fld(x = s); clock += 2; break;
			case 0xbb: clock++; break;
			case 0xbc: rabsx(ldy); break;
			case 0xbd: rabsx(lda); break;
			case 0xbe: rabsy(ldx); break;
			case 0xbf: rzp([&](uint8_t d) { br(d & 8); }); break;
			case 0xc0: imm(cpy); break;
			case 0xc1: rindx(cmp); break;
			case 0xc2: pc++; clock += 2; break;
			case 0xc3: clock++; break;
			case 0xc4: rzp(cpy); break;
			case 0xc5: rzp(cmp); break;
			case 0xc6: mzp(dec); break;
			case 0xc7: mzp([](uint8_t d) { return d | 0x10; }); break;
			case 0xc8: fld(++y); clock += 2; break; // iny
			case 0xc9: imm(cmp); break;
			case 0xca: fld(--x); clock += 2; break; // dex
			case 0xcb: waitflags |= W_WAI; pc--; break;
			case 0xcc: rabs(cpy); break;
			case 0xcd: rabs(cmp); break;
			case 0xce: mabs(dec); break;
			case 0xcf: rzp([&](uint8_t d) { br(d & 0x10); }); break;
			case 0xd0: br(!ResolvZ()); break; // bne
			case 0xd1: rindy(cmp); break;
			case 0xd2: rzpp(cmp); break;
			case 0xd3: clock++; break;
			case 0xd4: pc++; clock += 4; break;
			case 0xd5: rzpx(cmp); break;
			case 0xd6: mzpx(dec); break;
			case 0xd7: mzp([](uint8_t d) { return d | 0x20; }); break;
			case 0xd8: fcld(); clock += 2; break;
			case 0xd9: rabsy(cmp); break;
			case 0xda: st8(0x100 | s--, x); clock += 3; break;
			case 0xdb: waitflags |= W_STP; break;
			case 0xdc: pc += 2; clock += 4; break;
			case 0xdd: rabsx(cmp); break;
			case 0xde: mabsx(dec); break;
			case 0xdf: rzp([&](uint8_t d) { br(d & 0x20); }); break;
			case 0xe0: imm(cpx); break;
			case 0xe1: rindx(sbc); break;
			case 0xe2: pc++; clock += 2; break;
			case 0xe3: clock++; break;
			case 0xe4: rzp(cpx); break;
			case 0xe5: rzp(sbc); break;
			case 0xe6: mzp(inc); break;
			case 0xe7: mzp([](uint8_t d) { return d | 0x40; }); break;
			case 0xe8: fld(++x); clock += 2; break; // inx
			case 0xe9: imm(sbc); break;
			case 0xea: clock += 2; break;
			case 0xeb: clock++; break;
			case 0xec: rabs(cpx); break;
			case 0xed: rabs(sbc); break;
			case 0xee: mabs(inc); break;
			case 0xef: rzp([&](uint8_t d) { br(d & 0x40); }); break;
			case 0xf0: br(ResolvZ()); break; // beq
			case 0xf1: rindy(sbc); break;
			case 0xf2: rzpp(sbc); break;
			case 0xf3: clock++; break;
			case 0xf4: pc++; clock += 4; break;
			case 0xf5: rzpx(sbc); break;
			case 0xf6: mzpx(inc); break;
			case 0xf7: mzp([](uint8_t d) { return d | 0x80; }); break;
			case 0xf8: fsed(); clock += 2; break; // sed
			case 0xf9: rabsy(sbc); break;
			case 0xfa: fld(x = ld8(0x100 | ++s)); clock += 4; break;
			case 0xfb: clock++; break;
			case 0xfc: pc += 2; clock += 4; break;
			case 0xfd: rabsx(sbc); break;
			case 0xfe: mabsx(inc); break;
			case 0xff: rzp([&](uint8_t d) { br(d & 0x80); }); break;
		}
#if W65C02_TRACE
		tracep->p = ResolvFlags();
		tracep->a = a;
		tracep->x = x;
		tracep->y = y;
		tracep->s = s;
#if W65C02_TRACE > 1
		if (++tracep >= tracebuf + TRACEMAX - 1) StopTrace();
#else
		if (++tracep >= tracebuf + TRACEMAX) tracep = tracebuf;
#endif
#endif
	} while (!waitflags && clock < n);
	return waitflags ? 0 : clock - n;
}

int W65C02::ResolvC() {
	uint32_t sw = 0;
	FlagDecision *p;
	for (p = fp - 1; p >= fbuf && !(sw = p->dm & 0xf); p--)
		;
	if (p < fbuf) error();
	switch (sw) {
		case F0:
			break;
		case F1:
			return MC;
		case FB:
			return p->b & MC;
		case FADD:
			return ((p->s & p->b) | (~p->a & p->b) | (p->s & ~p->a)) >> 7 & MC;
		case FSUB:
			return ~((p->s & ~p->b) | (p->a & ~p->b) | (p->s & p->a)) >> 7 & MC;
		case FLEFT:
			return p->b >> 7 << LC;
		case FRIGHT:
			return (p->b & 1) << LC;
		default:
			error();
			break;
	}
	return 0;
}

int W65C02::ResolvZ() {
	uint32_t sw = 0;
	FlagDecision *p;
	for (p = fp - 1; p >= fbuf && !(sw = p->dm & 0xf0); p--)
		;
	if (p < fbuf) error();
	switch (sw >> 4) {
		case FB:
			return p->b & MZ;
		case F8:
			return !(p->a & 0xff) << LZ;
		default:
			error();
			break;
	}
	return 0;
}

int W65C02::ResolvD() {
	uint32_t sw = 0;
	FlagDecision *p;
	for (p = fp - 1; p >= fbuf && !(sw = p->dm & 0xf000); p--)
		;
	if (p < fbuf) error();
	switch (sw >> 12) {
		case F0:
			break;
		case F1:
			return MD;
		case FB:
			return p->b & MD;
		default:
			error();
			break;
	}
	return 0;
}

int W65C02::ResolvV() {
	uint32_t sw = 0;
	FlagDecision *p;
	for (p = fp - 1; p >= fbuf && !(sw = p->dm & 0xf000000); p--)
		;
	if (p < fbuf) error();
	switch (sw >> 24) {
		case F0:
			break;
		case FB:
			return p->b & MV;
		case FADD:
			return ((p->b & p->s & ~p->a) | (~p->b & ~p->s & p->a)) >> (7 - LV) & MV;
		case FSUB:
			return ((p->b & ~p->s & ~p->a) | (~p->b & p->s & p->a)) >> (7 - LV) & MV;
		default:
			error();
			break;
	}
	return 0;
}

int W65C02::ResolvN() {
	uint32_t sw = 0;
	FlagDecision *p;
	for (p = fp - 1; p >= fbuf && !(sw = p->dm & 0xf0000000); p--)
		;
	if (p < fbuf) error();
	switch (sw >> 28) {
		case FB:
			return p->b & MN;
		case F8:
			return ((p->a & 0x80) != 0) << LN;
		default:
			error();
			break;
	}
	return 0;
}

void W65C02::SetupFlags(int x) {
	fp = fbuf;
	fp->dm = NB | VB | DB | ZB | CB;
	fp++->b = intflags = x | MX;
}

int W65C02::ResolvFlags() {
	int r = ResolvN() | ResolvV() | ResolvD() | ResolvZ() | ResolvC();
	r |= (intflags & MI) | MX;
	SetupFlags(r);
	return r;
}

#if W65C02_TRACE
#include <string>
void W65C02::StopTrace() {
	TraceBuffer *endp = tracep;
	int i = 0, j;
	FILE *fo;
	if (!(fo = fopen((std::string(getenv("HOME")) + "/Desktop/trace.txt").c_str(), "w"))) exit(1);
	do {
		if (++tracep >= tracebuf + TRACEMAX) tracep = tracebuf;
		fprintf(fo, "%4d %04X  ", i++, tracep->pc);
		for (j = 0; j < tracep->opn; j++) fprintf(fo, "%02X ", tracep->op[j]);
		for (; j < OPMAX; j++) fprintf(fo, "   ");
		fprintf(fo, "%02X %02X %02X %02X %c%c%c%c%c%c%c%c ",
				tracep->a, tracep->x, tracep->y, tracep->s,
				tracep->p & 0x80 ? 'N' : '-',
				tracep->p & 0x40 ? 'V' : '-',
				tracep->p & 0x20 ? '+' : '-',
				tracep->p & 0x10 ? 'B' : '-',
				tracep->p & 0x08 ? 'D' : '-',
				tracep->p & 0x04 ? 'I' : '-',
				tracep->p & 0x02 ? 'Z' : '-',
				tracep->p & 0x01 ? 'C' : '-');
		for (Acs *p = tracep->acs; p < tracep->acs + tracep->index; p++) {
			switch (p->type) {
				case acsLoad8:
					fprintf(fo, "L %04X %02X ", p->adr, p->data & 0xff);
					break;
				case acsLoad16:
					fprintf(fo, "L %04X %04X ", p->adr, p->data);
					break;
				case acsStore8:
					fprintf(fo, "S %04X %02X ", p->adr, p->data & 0xff);
					break;
				case acsStore16:
					fprintf(fo, "S %04X %04X ", p->adr, p->data);
					break;
			}
		}
		fprintf(fo, "\n");
	} while (tracep != endp);
	fclose(fo);
	fprintf(stderr, "trace dumped.\n");
	exit(1);
}
#endif	// W65C02_TRACE
